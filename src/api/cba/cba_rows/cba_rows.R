library(tidyverse)
library(furrr)
library(lubridate)
library(rjson)
library(priceR)

plan(multisession, workers = parallel::detectCores())

data_cba_counts <- fromJSON(file = "src/api/cba/cba_rows/cba_counts.json") %>%
  tibble(tmp = .) %>%
  hoist(tmp, date = 1, count = 2) %>%
  mutate(date = ymd(date)) %>%
  arrange(date)

# Load rows into
data_cba_rows_load <- list.files("data_manual/cba_rows", full.names = TRUE, pattern = "*.json") %>%
  future_map(\(file) {
    print(paste0("reading ", file))
    fromJSON(file = file)
  })

data_cba_rows_raw <- data_cba_rows_load %>%
  unlist(recursive = FALSE) %>%
  tibble(rows = .) %>%
  hoist(rows, rows = 2) %>%
  pull(rows) %>%
  unlist(recursive = FALSE) %>%
  bind_rows()

data_cba_rows <- data_cba_rows_raw %>%
  mutate(date = ymd_hms(date),
         value = as.numeric(amount),
         value_usd = convert_currencies(as.numeric(value), from = "UAH", to = "USD", date = floor_date(date, unit = "day"))) %>%
  filter(value >= 0) %>%
  select(-amount, -comment, -source) %>%
  arrange(date)

saveRDS(data_cba_rows, "data/cba/data_cba_rows.RDS")

# data_cba_rows <- readRDS("data/cba/data_cba_rows.RDS")
