library(tidyverse)
library(lubridate)
library(glue)
library(httr)
library(jsonlite)
library(multidplyr)

# card = c(1, 2, 3, 5, 6)
# date_start = "YYYY-mm-dd"
# date_end = "YYYY-mm-dd"
# type = c("Кредит Дніпро", "Cash", "Fondy", "Fondy Sub", "Oschad EUR", "Oschad GBP", "Oschad PLN",
#          "Oschad UAH", "Oschad USD", "Privat CHF", "Privat CZK", "Privat EUR", "Privat GBP",
#          "Privat NOK", "Privat PLN", "Privat PLZ", "Privat SEK", "Privat UAH", "Privat USD",
#          "Solidgate Foreign", "Solidgate UKR", "Universal")
# filter = character()
# value = c(numeric(), numeric())

make_cba_req <- \(card = 1, date_start = NULL, date_end = NULL, type = NULL, string_filter = NULL, case_sensitive = FALSE, value_between = NULL) {
  # Base URL
  cba_url_base <- glue("https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/{card}/card/{card}")

  ### Build query
  cba_url_query <- list()
  ## date
  cba_url_query_date_comp <- ""

  if (!is.null(date_start)) {
    cba_url_query_date_comp <- paste0(cba_url_query_date_comp, date_start)
  }

  if (!is.null(date_end)) {
    cba_url_query_date_comp <- paste0(cba_url_query_date_comp, "~", date_end)
  }

  if (cba_url_query_date_comp != "") {
    cba_url_query_date <- list(type = unbox("date/all-options"), value = unbox(cba_url_query_date_comp), target = list(unbox("dimension"), c("field", 77, "null")), id = unbox("b6f7e9ea"))
    cba_url_query <- append(cba_url_query, list(cba_url_query_date))
  }

  ## type
  if (!is.null(type)) {
    cba_url_query_type <- list(type = unbox("string/="), value = type, target = list(unbox("dimension"), c("field", 78, "null")), id = unbox("64daade5"))
    cba_url_query <- append(cba_url_query, list(cba_url_query_type))
  }

  ## string_filter
  if (!is.null(string_filter)) {
    cba_url_query_filter <- list(type = unbox("string/contains"), value = string_filter, target = list(unbox("dimension"), c("field", 81, "null")), options = list("case-sensitive" = unbox(case_sensitive)), id = unbox("d41494b"))
    cba_url_query <- append(cba_url_query, list(cba_url_query_filter))
  }

  ## value
  if (!is.null(value_between)) {
    cba_url_query_value <- list(type = unbox("number/between"), value = value_between, target = list(unbox("dimension"), c("field", 83, "null")), id = unbox("413eae65"))
    cba_url_query <- append(cba_url_query, list(cba_url_query_value))
  }

  cba_url_query <- toJSON(cba_url_query)

  message(glue("Making request with date: {cba_url_query_date_comp}, type: {type}, string_filter: {string_filter}, value_between: {format(value_between[1], scientific = FALSE)}-{format(value_between[2], scientific = FALSE)}", .null = "none"))

  cba_req <- GET(cba_url_base, query = list(parameters = cba_url_query))
  cba_res <- content(cba_req, "parsed")$data$rows
}

transform_rows <- \(rows_raw) {
  rows_raw %>%
    tibble(rows = .) %>%
    hoist(rows, type = 1, date = 2, value_uah = 3, comment = 4)
}

# Given date & type, find maximum transaction value
# date <- "2022-02-25"
# type <- "Privat UAH"
# parent_count <- 30350
find_max_value <- \(date, type, parent_count) {
  message(glue("Finding maximum value for date: {date}, type: {type}, count: {parent_count}"))

  # Initial guess = 500000
  last_lower_bound <- 0
  lower_bound <- 0
  upper_bound <- 500000

  repeat {
    # Make guess of upper bound
    count_upp <- make_cba_req(card = 1, date_start = date, type = type, value_between = c(0, upper_bound))[[1]][[1]]

    if (count_upp < parent_count) {
      # Maximum is above this, increase lower bound, increase upper bound
      lower_bound <- upper_bound
      # Too small, increase upper bound
      upper_bound <- 2 * upper_bound
    } else {
      # Maximums is this or below, verify
      break
    }
  }

  # Maximum is either this or below this, need to verify, bring up lower bound
  repeat {
    last_lower_bound <- lower_bound
    lower_bound <- round(lower_bound + (upper_bound - lower_bound) / 2, 3)
    count_between <- make_cba_req(card = 1, date_start = date, type = type, value_between = c(lower_bound, upper_bound))[[1]][[1]]

    # Use all.equal due to floating point arithmetic
    if (isTRUE(all.equal(lower_bound, upper_bound, scale = 1, tolerance = 0.01))) {
      # Bounds match, lower bound is maximum
      return(lower_bound)
    } else {
      if (count_between == 0) {
        # Too big, set upper bound to lower bound, lower bound to previous successful lower bound
        upper_bound <- lower_bound
        lower_bound <- last_lower_bound
      }
    }
  }
}

split_by_value <- \(date, type, parent_count, value_max) {
  # Init lists
  results <- list()
  queue <- list(list(date = date, type = type, count = parent_count, value_min = 0, value_max = value_max))

  # Main loop
  while (length(queue) > 0) {
    # Pop first period
    period <- queue[[1]]
    queue <- queue[-1]

    message(glue_data(period, "Splitting period date: {date}, type: {type}, value_between: {format(value_min, scientific = FALSE)}-{format(value_max, scientific = FALSE)}"))

    period$count <- make_cba_req(card = 1, date_start = period$date, type = period$type, value_between = c(period$value_min, period$value_max))[[1]][[1]]

    # If under cutoff
    if (period$count > 2000) {
      # Check if overlap (reached a singularity)
      if (isTRUE(all.equal(period$value_min, period$value_max, scale = 1, tolerance = 0.01))) {
        # Can't do better than this, push to results
        results <- append(results, list(period))
      } else {
        # Can do better than this, need to split in half
        # First period, value_min, half of value_max
        # queue <- append(queue, list(list(date = date, type = type, value_min = period$value_min, value_max = round(period$value_min + (period$value_max - period$value_min) / 2, digits = 3))))
        queue <- append(queue, list(list(date = date, type = type, value_min = period$value_min, value_max = period$value_min + (period$value_max - period$value_min) / 2)))
        # Second period, half of value_min, value_max
        # queue <- append(queue, list(list(date = date, type = type, value_min = round(period$value_max - (period$value_max - period$value_min) / 2, digits = 3), value_max = period$value_max)))
        queue <- append(queue, list(list(date = date, type = type, value_min = period$value_max - (period$value_max - period$value_min) / 2, value_max = period$value_max)))
      }
    } else {
      results <- append(results, list(period))
    }
  }

  results
}

get_overlaps <- \(f_date, f_type, data_verify) {
  filter(data_verify, date == f_date & f_type == type) %>%
    pull(d)
}

# First pass with alphabet characters
alph_english <- "abcdefghijklmnopqrstuvwxyz"
alph_ukrainian <- "абвгґдеєжзиіїйклмнопрстуфхцчшщьюя"
alph <- paste0(alph_ukrainian, toupper(alph_ukrainian), alph_english, toupper(alph_english))

get_rows_by_text <- \(date, type, value_min, value_max, text, case_sensitive = TRUE) {
  map(strsplit(text, "")[[1]], ~ make_cba_req(card = 5, date_start = date, type = type, value_between = c(value_min, value_max), string_filter = ., case_sensitive = case_sensitive ))
}

collate_rows_by_text <- \(rows_set) {
  # Tag actual duplicates
  map(rows_set, \(rows) {
    rows %>%
      group_by(date, type, value_uah, comment) %>%
      mutate(id = row_number())
  }) %>%
    # Naively bind all rows together :)
    # bind_rows() %>%
    # distinct(date, type, value_uah, comment, id)
    # Or antijoin
    reduce(\(acc, rows) {
      bind_rows(acc, anti_join(rows, acc, by = c("date", "type", "value_uah", "comment", "id")))
    })
}

split_by_text <- \(date, type, parent_count, value_min, value_max) {
  alph_english <- "abcdefghijklmnopqrstuvwxyz"
  alph_ukrainian <- "абвгґдеєжзиіїйклмнопрстуфхцчшщьюя"
  alph <- strsplit(paste0(alph_ukrainian, alph_english), "")[[1]]

  result <- list(list(date = date, type = type, value_min = value_min, value_max = value_max, count = 2000))

  message(glue("Splitting period date: {date}, type: {type}, value_between: {format(value_min, scientific = FALSE)}-{format(value_max, scientific = FALSE)}"))

  # Get naive query
  rows <- make_cba_req(5, date_start = date, type = type, value_between = c(value_min, value_max)) %>%
    transform_rows()

  # Set baseline row count
  count_curr <- nrow(rows)

  for (text in alph) {

    # Make guess, eliminate duplicate rows
    rows <- make_cba_req(5, date_start = date, type = type, value_between = c(value_min, value_max), string_filter = text) %>%
      transform_rows() %>%
      bind_rows(rows) %>%
      distinct(type, date, value_uah, comment)

    count_add <- nrow(rows) - count_curr

    if (count_add > 0) {
      # Query adds information, add to list
      result <- append(result, list(date = date, type = type, value_min = value_min, value_max = value_max, count = count_add))
    }

    count_curr <- nrow(rows)
    message(glue("Have {count_curr} rows out of {parent_count}"))
  }

  result
}
