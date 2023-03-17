library(tidyverse)
library(lubridate)
library(readxl)

WEEKDAYS <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

deseasonalise_variable <- \(col, col_weekday) {
  mod <- lm(col ~ factor(col_weekday))
  mod$residuals
}

# Load variables
data_donations <- readRDS("data/data_donations.RDS")
data_sirens <- readRDS("data/sirens/data_sirens.RDS")
data_strikes <- readRDS("data/data_strikes.RDS")
data_tweet_count <- readRDS("data/tweets/count/data_tweet_count_day.RDS")
data_news_factiva <- readRDS("data/news/data_news_factiva.RDS")
data_news_euro <- readRDS("data/news/data_news_euro.RDS")
data_severity <- readRDS("data/severity/data_severity.RDS")
data_emotions <- readRDS("data/sentiment/data_emotions.RDS") %>%
  rename(emot_count_neutral = emot_count_mixed, emot_prop_neutral = emot_prop_mixed) %>%
  rename_with(~ paste0(., "_dict"), -date)
data_emotions_ml <- readRDS("data/sentiment/data_emotions_ml.RDS") %>%
  rename_with(~ paste0(., "_ml"), -date)
data_sentiments <- readRDS("data/sentiment/data_sentiments.RDS")
data_events_raw <- read_excel("data/important_events_hourly.xlsx")

data_events <- data_events_raw %>%
  mutate(datetime = with_tz(ymd_hms(datetime, tz = "Europe/Kiev"), tzone = "UTC"),
         date = date(datetime)) %>%
  mutate(event_positive_dum = if_else(coloring == 0, 1, 0, missing = 0),
         event_negative_dum = if_else(coloring == 1, 1, 0, missing = 0)) %>%
  select(date, event_positive_dum, event_negative_dum)

# Aid
data_events_aid_raw <- read_xlsx("data/events_aid.xlsx")
data_events_aid <- data_events_aid_raw %>%
  filter((events_mil == 1 & currency == "USD") | (events_EU_mix == 1)) %>%
  mutate(datetime = ymd_hms(datetime)) %>%
  mutate(date = date(datetime),
         event_aid_dum = if_else(events_mil == 1 | events_EU_mix == 1, 1, 0)
  ) %>%
  group_by(date) %>%
  arrange(desc(amount)) %>%
  summarize(across(everything(), first)) %>%
  select(date, event_aid_dum, aid_amount = amount)

data_complete_full <- data_donations %>%
  left_join(data_events, by = "date") %>%
  left_join(data_events_aid, by = "date") %>%
  left_join(data_sirens, by = "date") %>%
  left_join(data_strikes, by = "date") %>%
  left_join(data_tweet_count, by = "date") %>%
  left_join(data_emotions, by = "date") %>%
  left_join(data_emotions_ml, by = "date") %>%
  left_join(data_sentiments, by = "date") %>%
  left_join(data_news_factiva, by = "date") %>%
  left_join(data_news_euro, by = "date") %>%
  left_join(data_severity, by = "date") %>%
  mutate(across(c(-date, -type), ~ replace_na(., 0))) %>%
  mutate(date = ymd(date)) %>%
  group_by(type) %>%
  mutate(across(c(-date, -contains("dum")), ~log(.), .names = "log_{.col}"),
         across(c(-date, -contains("dum"), -contains("log_")), ~. - dplyr::lag(.), .names = "d_{.col}"),
         across(c(-date, -contains("dum"), -starts_with(c("d_", "log_"))), ~log(.) - log(dplyr::lag(.)), .names = "dlog_{.col}")) %>%
  # Generate weekday dummies
  mutate(weekday = factor(weekdays(date), levels = WEEKDAYS),
         days_since = (as.numeric(date - 19047)))

saveRDS(data_complete_full, "data/data_complete_full.RDS")

data_complete <- data_complete_full %>%
  filter(date >= ymd("2022-03-16")) %>%
  mutate(across(c(dlog_news_factiva_count, dlog_news_euro_count), ~ deseasonalise_variable(., weekday), .names = "{.col}_des")) %>%
  # mutate(across(c(-date, -contains("dum")), ~findInterval(.x, quantile(.x, seq(0, 1, 0.2)), rightmost.closed = TRUE), .names = "{col}_quint")) %>%
  ungroup()

saveRDS(data_complete, "data/data_complete.RDS")
