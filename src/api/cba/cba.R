library(tidyverse)
library(httr)
library(glue)
library(jsonlite)
library(priceR)
library(lubridate)

# TODO: Think about parallelization
# library(doParallel)
# cl <- parallel::makeCluster(16, outfile="")
# registerDoParallel(cl)
# stopCluster(cl)

# library(doFuture)
# plan(multisession)
# registerDoFuture()

library(foreach)
registerDoSEQ()

# TODO: Check out potential time zone problems
# TODO: Fix Solidgate Card ukrainian vs. foreign (Get row-level data)

START_DATE <- Sys.getenv("CBA_START")
END_DATE <- Sys.getenv("CBA_END")

## Aggregated graph level ----
get_data_cba_value <- \(start, end) {

  # Make request to graph endpoint & parse response
  url_cba_query <- '[{"type":"date/all-options","value":"<<start>>~<<end>>","target":["dimension",["field",77,null]],"id":"b6f7e9ea"}]'

  res <- GET("https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/6/card/6",
             query = list(parameters = glue(url_cba_query, .open = "<<", .close = ">>")))
  content <- content(res, "parsed")$data$rows %>%
    reduce(\(acc, row) {
      add_row(acc, date = row[[1]], type = row[[2]], value_uah = row[[3]])
    }, .init = data.frame(date = character(), type = character(), value_uah = numeric()))

  # Make request to for domestic Solidgate donations
  string_type <- "Solidgate Card"
  string_filter <- "UKR"
  url_cba_query_sgukr <- '[{"type":"date/all-options","value":"<<start>>~<<end>>","target":["dimension",["field",77,null]],"id":"b6f7e9ea"},{"type":"string/=","value":["<<string_type>>"],"target":["dimension",["field",78,null]],"id":"64daade5"},{"type":"string/contains","value":["<<string_filter>>"],"target":["dimension",["field",81,null]],"options":{"case-sensitive":false},"id":"d41494b"}]'

  res_sgukr <- GET("https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/6/card/6",
                          query = list(parameters = glue(url_cba_query_sgukr, .open = "<<", .close = ">>")))
  content_sgukr <- content(res_sgukr, "parsed")$data$rows %>%
    reduce(\(acc, row) {
      add_row(acc, date = row[[1]], type = "Solidgate UKR", value_uah = row[[3]])
    }, .init = data.frame(date = character(), type = character(), value_uah = numeric()))

  # Process response
  data_cba_value <- bind_rows(content, content_sgukr) %>%
    # Parse dates into days, convert UAH to USD
    mutate(date = as_date(ymd_hms(date, tz = "Europe/Kiev", quiet = TRUE), tz = "Europe/Kiev"),
           value_usd = convert_currencies(as.numeric(value_uah), from = "UAH", to = "USD", date = date)) %>%
    select(-value_uah) %>%
    # Expand values to all days, fill missing values with 0
    right_join(expand(., type, date = full_seq(date, 1)), by = c("date", "type")) %>%
    mutate(value_usd = replace_na(value_usd, 0)) %>%
    # Calculate Solidgate Foreign
    pivot_wider(names_from = type, values_from = value_usd) %>%
    mutate(`Solidgate Foreign` = `Solidgate Card` - `Solidgate UKR`) %>%
    select(-`Solidgate Card`) %>%
    pivot_longer(-date, names_to = "type", values_to = "value_usd") %>%
    arrange(type, date)
}

data_cba_value <- get_data_cba_value(START_DATE, END_DATE)
saveRDS(data_cba_value, "data/cba/data_cba_value.RDS")
# data_cba_value <- readRDS("data/cba/data_cba_value.RDS")

## Individual daily count level ----
get_data_cba_count <- \(start, end) {
  # Get list of types
  url_cba_types <- "https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/params/64daade5/values"
  types_req <- GET(url_cba_types)
  types <- content(types_req, "parsed")$values %>%
    # Add additional Solidgate types
    c(., "Solidgate UKR")

  # Create list of dates
  dates <- seq(as_date(start), as_date(end), 1)

  # Base query parameters - date, string_type, string_filter
  url_cba_query <- '[{"type":"date/all-options","value":"<<date>>","target":["dimension",["field",77,null]],"id":"b6f7e9ea"},{"type":"string/=","value":["<<string_type>>"],"target":["dimension",["field",78,null]],"id":"64daade5"}]'

  # Loop over type and then date, requesting daily count
  counts <- types %>%
    reduce(\(acc, type) {

      # Change settings depending on Solidgate type
      if (type == "Solidgate UKR") {
        url_cba_query <- '[{"type":"date/all-options","value":"<<date>>","target":["dimension",["field",77,null]],"id":"b6f7e9ea"},{"type":"string/=","value":["<<string_type>>"],"target":["dimension",["field",78,null]],"id":"64daade5"},{"type":"string/contains","value":["<<string_filter>>"],"target":["dimension",["field",81,null]],"options":{"case-sensitive":false},"id":"d41494b"}]'
        string_type <- "Solidgate Card"
        string_filter <- "UKR"
      } else {
        string_type <- type
        string_filter <- ""
      }

      counts <- foreach(date = dates, .export = c("type", "url_cba_query"), .combine = "c", .packages = c("tidyverse", "glue", "httr")) %dopar% {
        print(glue("type: {type}, date: {date}, string_filter: {string_filter}"))

        res <- GET("https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/1/card/1",
                   query = list(parameters = glue(url_cba_query, .open = "<<", .close = ">>")))
        content <- content(res, "parsed")

        content$data$rows[[1]][[1]]
      }

      bind_rows(acc, data.frame(date = dates, type = type, count = counts))
    }, .init = NULL)

  # Calculate Solidgate Foreign data
  counts_ex <- counts %>%
    pivot_wider(names_from = type, values_from = count) %>%
    mutate(`Solidgate Foreign` = `Solidgate Card` - `Solidgate UKR`) %>%
    select(-`Solidgate Card`) %>%
    pivot_longer(-date, names_to = "type", values_to = "count") %>%
    arrange(type, date)
}

data_cba_count <- get_data_cba_count(START_DATE, END_DATE)
saveRDS(data_cba_count, "data/cba/data_cba_count.RDS")
# data_cba_count <- readRDS("data/cba/data_cba_count.RDS")

data_cba <- data_cba_count %>%
  left_join(data_cba_value, by = c("date", "type")) %>%
  mutate(across(c(count, value_usd), ~ replace_na(., 0)),
         mean_usd = if_else(is.nan(value_usd / count), 0, value_usd / count)) %>%
  select(-value_usd)

saveRDS(data_cba, "data/cba/data_cba.RDS")
