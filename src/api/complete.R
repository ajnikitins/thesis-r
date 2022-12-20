library(tidyverse)
library(lubridate)
library(readxl)

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
  summarise(don_count = n(), don_mean = sum(value) / don_count, don_mean_usd = sum(value_usd) / don_count, .groups = "drop") %>%
  right_join(expand(., type, date = full_seq(date, 1)), by = c("type", "date")) %>%
  mutate(across(c(don_count, don_mean, don_mean_usd), ~ replace_na(., 0))) %>%
  arrange(type, date)

saveRDS(data_donations, "data/data_donations.RDS")

## Load independent variables
data_sirens <- readRDS("data/sirens/data_sirens.RDS")
data_tweet_count <- readRDS("data/tweets/count/data_tweet_count_day.RDS")
data_factiva <- readRDS("data/factiva.RDS")
data_severity <- readRDS("data/severity/data_severity.RDS")
data_emotions <- readRDS("data/sentiment/data_emotions.RDS")
data_sentiments <- readRDS("data/sentiment/data_sentiments.RDS")
data_events_raw <- read_excel("data/important_events.xlsx")

data_events <- data_events_raw %>%
  mutate(event_positive = if_else(coloring == 0, 1, 0, missing = 0),
         event_negative = if_else(coloring == 1, 1, 0, missing = 0)) %>%
  select(date, event_positive, event_negative)

data_complete <- data_donations %>%
  left_join(data_events, by = "date") %>%
  left_join(data_sirens, by = "date") %>%
  left_join(data_tweet_count, by = "date") %>%
  left_join(data_emotions, by = "date") %>%
  left_join(data_sentiments, by = "date") %>%
  left_join(data_factiva, by = "date") %>%
  left_join(data_severity, by = "date") %>%
  mutate(date = as_date(date)) %>%
  mutate(across(c(-date, -type, -starts_with("event"), -siren_kyiv), ~ . - lag(.), .names = "d_{.col}"),
         across(c(-date, -type, -starts_with("event"), -siren_kyiv, -starts_with("d_")), ~ log(.) - log(lag(.)), .names = "dlog_{.col}"))

saveRDS(data_complete, "data/data_complete.RDS")
