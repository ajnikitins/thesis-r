library(tidyverse)
library(lubridate)

# Merge fiat and crypto donations
data_cba <- readRDS("data/cba/data_cba_rows_usd.RDS")
data_crypto <- readRDS("data/crypto/data_crypto.RDS")

data_donations <- bind_rows(data_cba, data_crypto) %>%
  mutate(date = as_date(date)) %>%
  # Setup type names
  mutate(type = case_when(
    str_detect(currency, "UAH") ~ "Ukrainian",
    str_detect(currency, "Bitcoin|Ethereum") ~ "Crypto",
    TRUE ~ "Foreign"
  ), .keep = "unused") %>%
  # Filter to period
  filter(date %within% interval("2022-01-01", "2022-10-31")) %>%
  # Trim bottom and top 1% of donations
  group_by(type) %>%
  filter(between(value_usd, quantile(value_usd, 0.01), quantile(value_usd, 0.99))) %>%
  # Calculate daily counts and means
  group_by(type, date) %>%
  summarise(don_count = n(), don_total = sum(value), don_total_usd = sum(value_usd), don_mean = don_total / don_count, don_mean_usd = don_total_usd / don_count, .groups = "drop") %>%
  right_join(expand(., type, date = full_seq(date, 1)), by = c("type", "date")) %>%
  mutate(across(c(-date, -type), ~ replace_na(., 0))) %>%
  arrange(type, date)

saveRDS(data_donations, "data/data_donations.RDS")
