library(tidyverse)
library(httr)
library(glue)
library(jsonlite)
library(priceR)

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

get_data_cba_agg <- \(start, end) {

  url_base_cba_agg <- "https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/6/card/6?parameters=%5B%7B%22type%22%3A%22date%2Fall-options%22%2C%22value%22%3A%22{start}~{end}%22%2C%22target%22%3A%5B%22dimension%22%2C%5B%22field%22%2C77%2Cnull%5D%5D%2C%22id%22%3A%22b6f7e9ea%22%7D%5D"

  res_cba_agg <- GET(glue(url_base_cba_agg))
  full_data_cba_agg <- fromJSON(rawToChar(res_cba_agg$content))

  data_cba_agg <- full_data_cba_agg$data$rows %>%
    as.data.frame() %>%
    set_names(full_data_cba_agg$data$cols$name) %>%
    mutate(sum_usd = convert_currencies(as.numeric(sum), from = "UAH", to = "USD", date = as.Date(date)))
}

data_cba_agg <- get_data_cba_agg(START_DATE, END_DATE)
saveRDS(data_cba_agg, "data/data_cba_agg.RDS")
