library(tidyverse)
library(lubridate)

data_donations <- readRDS("data/data_donations_hourly.RDS")
data_strikes <- readRDS("data/data_strikes_hourly.RDS")
data_tweet_count <- readRDS("data/tweets/count/data_tweet_count_hour.RDS")

data_intraday <- data_donations %>%
  left_join(data_strikes, by = "date") %>%
  left_join(data_tweet_count, by = "date") %>%
  group_by(type) %>%
  mutate(across(c(-date, -contains("dum")), ~log(.), .names = "log_{.col}"),
         across(c(-date, -contains("dum"), -contains("log_")), ~log(1 + .), .names = "log1_{.col}")) %>%
  filter(date >= ymd("2022-03-16")) %>%
  mutate(across(c(-date, -contains("dum")), ~findInterval(.x, quantile(.x, seq(0, 1, 0.2)), rightmost.closed = TRUE), .names = "{col}_quint")) %>%
  ungroup()

saveRDS(data_intraday, "data/data_intraday.RDS")
