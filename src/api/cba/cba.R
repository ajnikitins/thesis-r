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

START_DATE <- Sys.getenv("CBA_START")
END_DATE <- Sys.getenv("CBA_END")

## Aggregated graph level ----
get_data_cba_value <- \(start, end) {

  # Make request to graph endpoint & parse response
  url_base_cba <- glue("https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/6/card/6?parameters=%5B%7B%22type%22%3A%22date%2Fall-options%22%2C%22value%22%3A%22{start}~{end}%22%2C%22target%22%3A%5B%22dimension%22%2C%5B%22field%22%2C77%2Cnull%5D%5D%2C%22id%22%3A%22b6f7e9ea%22%7D%5D")
  res <- GET(url_base_cba)
  content <- content(res, "parsed")

  # Process response
  data_cba_value <- content$data$rows %>%
    # Turn individual row lists into a data frame
    reduce(\(acc, row) {
      add_row(acc, date = row[[1]], type = row[[2]], value_uah = row[[3]])
    }, .init = data.frame(date = character(), type = character(), value_uah = numeric())) %>%
    # Parse dates into days, convert UAH to USD
    mutate(date = as_date(ymd_hms(date, tz = "Europe/Kiev", quiet = TRUE), tz = "Europe/Kiev"),
           value_usd = convert_currencies(as.numeric(value_uah), from = "UAH", to = "USD", date = date)) %>%
    select(-value_uah) %>%
    # Expand values to all days, fill missing values with 0
    right_join(expand(data_cba_value, type, date = full_seq(date, 1))) %>%
    mutate(value_usd = replace_na(value_usd, 0)) %>%
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
  types <- content(types_req, "parsed")$values

  # Create list of dates
  dates <- seq(as_date(start), as_date(end), 1)

  # Base query parameters
  url_cba_query <- '[{"type":"date/all-options","value":"<<date>>","target":["dimension",["field",77,null]],"id":"b6f7e9ea"},{"type":"string/=","value":["<<type>>"],"target":["dimension",["field",78,null]],"id":"64daade5"}]'

  # Loop over type and then date, requesting daily count
  counts <- types %>%
    reduce(\(acc, type) {
      counts <- foreach(date = dates, .export = c("type", "url_cba_query"), .combine = "c", .packages = c("tidyverse", "glue", "httr")) %dopar% {
        print(glue("type: {type}, date: {date}"))

        res <- GET("https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/1/card/1",
                   query = list(parameters = glue(url_cba_query, .open = "<<", .close = ">>")))
        content <- content(res, "parsed")

        content$data$rows[[1]][[1]]
      }

      bind_rows(acc, data.frame(date = dates, type = type, count = counts))
    }, .init = NULL)
}

data_cba_count <- get_data_cba_count(START_DATE, END_DATE)
saveRDS(data_cba_count, "data/cba/data_cba_count.RDS")
# data_cba_count <- readRDS("data/cba/data_cba_count.RDS")

data_cba <- data_cba_count %>%
  left_join(data_cba_value, by = c("date", "type")) %>%
  mutate(mean_usd = if_else(is.nan(value_usd / count), 0, value_usd / count))

saveRDS(data_cba, "data/cba/data_cba.RDS")
