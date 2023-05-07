library(tidyverse)
library(furrr)
library(lubridate)
library(rjson)
library(priceR)

plan(multisession, workers = parallel::detectCores())

# Load rows into
data_cba_rows_raw <- list.files("data_manual/cba_rows", full.names = TRUE, pattern = "*.json") %>%
  future_map_dfr(\(file) {
    print(paste0("reading ", file))
    result <- fromJSON(file = file) %>%
      map_dfr("rows")
  })

write_rds(data_cba_rows_raw, "data/cba/data_cba_rows_raw.RDS")

data_cba_rows <- data_cba_rows_raw %>%
  mutate(date = ymd_hms(date),
         value = as.numeric(amount),
         value_usd = convert_currencies(as.numeric(value), from = "UAH", to = "USD", date = floor_date(date, unit = "day"))) %>%
  filter(value >= 0) %>%
  select(-amount, -comment, -source) %>%
  arrange(date)

write_rds(data_cba_rows, "data/cba/data_cba_rows.RDS")

# data_cba_rows <- read_rds("data/cba/data_cba_rows.RDS")
