library(tidyverse)
library(lubridate)

# Merge fiat and crypto donations
data_cba <- readRDS("data/cba/data_cba_rows_usd.RDS")
data_crypto <- readRDS("data/crypto/data_crypto.RDS")

data_donations_hourly <- bind_rows(data_cba, data_crypto) %>%
  mutate(date = as_datetime(date),
         date = floor_date(date, unit = "hours")) %>%
  # Setup type names
  mutate(type = case_when(
    str_detect(currency, "UAH") ~ "Ukrainian",
    str_detect(currency, "Bitcoin|Ethereum") ~ "Crypto",
    TRUE ~ "Foreign"
  ), .keep = "unused") %>%
  # Filter to period
  filter(date %within% interval("2022-01-01 00:00:00", "2022-10-31 23:59:59")) %>%
  # Trim bottom and top 1% of donations
  group_by(type) %>%
  filter(between(value_usd, quantile(value_usd, 0.01), quantile(value_usd, 0.99))) %>%
  # Calculate daily counts and means
  group_by(type, date) %>%
  summarise(don_count = n(), don_total = sum(value), don_total_usd = sum(value_usd), don_mean = don_total / don_count, don_mean_usd = don_total_usd / don_count, .groups = "drop") %>%
  right_join(tidyr::expand(., type, date = full_seq(date, 3600)), by = c("type", "date")) %>%
  mutate(across(c(-date, -type), ~ replace_na(., 0))) %>%
  arrange(type, date)

saveRDS(data_donations_hourly, "data/data_donations_hourly.RDS")

data_donations <- data_donations_hourly %>%
  mutate(date = floor_date(date, unit = "days")) %>%
  group_by(type, date) %>%
  summarise(don_count = sum(don_count), don_total = sum(don_total), don_total_usd = sum(don_total_usd), don_mean = don_total / don_count, don_mean_usd = don_total_usd / don_count, .groups = "drop") %>%
  mutate(across(c(-date, -type), ~ replace_na(., 0))) %>%
  arrange(type, date)

saveRDS(data_donations, "data/data_donations.RDS")
