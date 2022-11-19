library(tidyverse)

# Merge fiat and crypto donations
data_cba <- readRDS("../../data/cba/data_cba.RDS")
data_crypto <- readRDS("../../data/crypto/data_crypto.RDS")

data_donations <- bind_rows(data_cba, data_crypto) %>%
  # Filter to period
  filter(date %within% interval("2022-01-01", "2022-10-31")) %>%
  # Assign groups - UA, Foreign, Crypto
  mutate(type = case_when(
    str_detect(type, "Foreign|EUR|GBP|PLN|USD|CHF|CZK|EUR|NOK|PLZ|SEK") ~ "Foreign",
    str_detect(type, "Bitcoin|Ethereum") ~ "Crypto",
    # str_detect(type, "Кредит Дніпро|Cash|Fondy|UAH|UKR|Universal") ~ "Ukrainian",
    TRUE ~ "Ukrainian"
  )) %>%
  # # Aggregate counts, means
  group_by(type, date) %>%
  # summarize(mean_usd = (count * mean_usd) / sum(count), count = sum(count), .groups = "drop") %>%
  summarize(don_mean_usd = sum(count * mean_usd) / sum(count),
            don_mean_usd = if_else(is.nan(don_mean_usd), 0, don_mean_usd),
            don_count = sum(count),
            .groups = "drop") %>%
  arrange(type, date)

saveRDS(data_donations, "../../data/data_donations.RDS")

## Load independent variables
data_sirens <- readRDS("../../data/sirens/data_sirens.RDS")
data_tweet_count <- readRDS("../../data/tweets/count/data_tweet_count_day.RDS")
data_factiva <- readRDS("../../data/factiva.RDS")

data_complete <- data_donations %>%
  left_join(data_sirens, by = "date") %>%
  left_join(data_tweet_count, by = "date") %>%
  left_join(data_factiva, by = "date")

saveRDS(data_complete, "../../data/data_complete.RDS")
