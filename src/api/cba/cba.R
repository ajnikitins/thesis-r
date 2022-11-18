library(tidyverse)
library(httr)
library(glue)
library(jsonlite)
library(priceR)
library(lubridate)

START_DATE <- Sys.getenv("CBA_START")
END_DATE <- Sys.getenv("CBA_END")

## Row-level data
# cba_url_base <- "https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/5/card/5?parameters=%5B%7B%22type%22%3A%22date%2Fall-options%22%2C%22value%22%3A%22{start}~{end}%22%2C%22target%22%3A%5B%22dimension%22%2C%5B%22field%22%2C77%2Cnull%5D%5D%2C%22id%22%3A%22b6f7e9ea%22%7D%5D"
#
# get_data_cba <- \(start, end) {
#   data_cba <- list()
#
#   start <- START_DATE
#   end <- END_DATE
#
#   res_cba <- GET(glue(cba_url_base))
#
#   tmp <- fromJSON(rawToChar(res_cba$content))
# }

## Aggregated graph level ----

get_data_cba_value <- \(start, end) {
  url_base_cba <- "https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/6/card/6?parameters=%5B%7B%22type%22%3A%22date%2Fall-options%22%2C%22value%22%3A%22{start}~{end}%22%2C%22target%22%3A%5B%22dimension%22%2C%5B%22field%22%2C77%2Cnull%5D%5D%2C%22id%22%3A%22b6f7e9ea%22%7D%5D"

  res <- GET(glue(url_base_cba))
  content <- fromJSON(rawToChar(res$content))

  data_cba_value <- content$data$rows %>%
    as.data.frame() %>%
    set_names(content$data$cols$name) %>%
    mutate(sum_usd = convert_currencies(as.numeric(sum), from = "UAH", to = "USD", date = as.Date(date)))
}

data_cba_value <- get_data_cba_value(START_DATE, END_DATE)
saveRDS(data_cba_value, "../../../data/cba/data_cba_value.RDS")

get_data_cba_count <- \(start, end) {
  dates <- seq(as_date(start), as_date(end), 1)

  date <- dates[[1]]
  url_base_cba <- "https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/1/card/1?parameters=%5B%7B%22type%22%3A%22date%2Fall-options%22%2C%22value%22%3A%22{date}%22%2C%22target%22%3A%5B%22dimension%22%2C%5B%22field%22%2C77%2Cnull%5D%5D%2C%22id%22%3A%22b6f7e9ea%22%7D%5D"

  counts <- dates %>%
    map(\(date) {
      res <- GET(glue(url_base_cba))
      content <- fromJSON(rawToChar(res$content))
      content$data$rows[1]
    })

  data_cba_count <- data.frame(date = dates, count = unlist(counts))
}

data_cba_count <- get_data_cba_count(START_DATE, END_DATE)
saveRDS(data_cba_count, "../../../data/cba/data_cba_count.RDS")

