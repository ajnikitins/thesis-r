library(tidyverse)
library(lubridate)

generate_intraday_data <- \(data, grouping = "type") {
  data %>%
    left_join(data_strikes, by = "date") %>%
    left_join(bind_rows(data_tweet_count, data_tweet_ukr_count), by = c("date", "type")) %>%
    arrange(across(all_of(grouping)), date) %>%
    group_by(across(all_of(grouping))) %>%
    mutate(across(c(-date, -contains("dum")), list(log = ~log(.), log1 = ~log(1 + .), dlog = ~log(.) - dplyr::lag(log(.)), dlog1 = ~log(1 + .) - log(1 + dplyr::lag(.)), asinh = asinh), .names = "{.fn}_{.col}")) %>%
    filter(date >= ymd("2022-03-16")) %>%
    # mutate(across(c(-date, -contains("dum")), ~findInterval(.x, quantile(.x, seq(0, 1, 0.2)), rightmost.closed = TRUE), .names = "{col}_quint")) %>%
    ungroup()
}

# Load donations
data_donations <- readRDS("data/data_donations_hourly.RDS")
data_donations_sub <- readRDS("data/data_donations_hourly_sub.RDS")

# Load controls
data_strikes <- readRDS("data/data_strikes_hourly.RDS")
data_tweet_count <- readRDS("data/tweets/count/data_tweet_count_hour.RDS") %>%
  expand_grid(., type = c("Foreign", "Crypto"))
data_tweet_ukr_count <- readRDS("data/tweets/count/data_tweet_ukr_count_hour.RDS") %>%
  mutate(type = "Ukrainian") %>%
  rename(tweet_count = tweet_ukr_count)

data_intraday <- generate_intraday_data(data_donations)
saveRDS(data_intraday, "data/data_intraday.RDS")

data_intraday_sub <- generate_intraday_data(data_donations_sub, c("type", "type_sub"))
saveRDS(data_intraday_sub, "data/data_intraday_sub.RDS")
