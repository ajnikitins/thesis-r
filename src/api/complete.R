library(tidyverse)
library(lubridate)
library(priceR)

# Merge fiat and crypto donations
data_cba <- readRDS("data/cba/data_cba_rows.RDS")
data_crypto <- readRDS("data/crypto/data_crypto.RDS") %>%
  mutate(type = "Crypto")

# TODO: Put this in cba row-data
data_cba_usd <- data_cba  %>%
  mutate(type = case_when(
    currency == "UAH" ~ "Ukrainian",
    TRUE ~ "Foreign"
  ),
         date = floor_date(date, unit = "day"),
         value_usd = convert_currencies(as.numeric(amount), from = "UAH", to = "USD", date = floor_date(date, unit = "day"))) %>%
  select(-amount, -comment, -source, -currency)

data_donations <- bind_rows(data_cba_usd, data_crypto) %>%
  mutate(date = as_date(date)) %>%
  # Filter to period
  filter(date %within% interval("2022-01-01", "2022-10-31")) %>%
  # Trim bottom and top 1% of donations
  group_by(type) %>%
  filter(between(value_usd, quantile(value_usd, 0.01), quantile(value_usd, 0.99))) %>%
  # Calculate daily counts and means
  group_by(type, date) %>%
  summarise(don_count = n(), don_mean_usd = sum(value_usd) / don_count, .groups = "drop") %>%
  right_join(expand(., type, date = full_seq(date, 1)), by = c("type", "date")) %>%
  mutate(across(c(don_count, don_mean_usd), ~ replace_na(., 0))) %>%
  arrange(type, date)

saveRDS(data_donations, "data/data_donations.RDS")

## Load independent variables
data_sirens <- readRDS("data/sirens/data_sirens.RDS")
data_tweet_count <- readRDS("data/tweets/count/data_tweet_count_day.RDS")
data_factiva <- readRDS("data/factiva.RDS")
data_severity <- readRDS("data/severity/data_severity.RDS")

data_complete <- data_donations %>%
  left_join(data_sirens, by = "date") %>%
  left_join(data_tweet_count, by = "date") %>%
  left_join(data_factiva, by = "date") %>%
  left_join(data_severity, by = "date")

data_complete_d <- data_complete %>%
  mutate(across(c(don_count, don_mean_usd, siren_count, siren_mean_duration, siren_prop, tweet_count, factiva_count, cas_civ, cas_rus_mil, confl_evs), ~ . - lag(.), .names = "d_{.col}"))

saveRDS(data_complete_d, "data/data_complete.RDS")
