library(tidyverse)
library(lubridate)
library(readxl)

# Load variables
data_donations <- readRDS("data/data_donations.RDS")
data_sirens <- readRDS("data/sirens/data_sirens.RDS")
data_tweet_count <- readRDS("data/tweets/count/data_tweet_count_day.RDS")
data_factiva <- readRDS("data/factiva.RDS")
data_severity <- readRDS("data/severity/data_severity.RDS")
data_emotions <- readRDS("data/sentiment/data_emotions.RDS")
data_sentiments <- readRDS("data/sentiment/data_sentiments.RDS")
data_events_raw <- read_excel("data/important_events.xlsx")

data_events <- data_events_raw %>%
  mutate(event_positive_dum = if_else(coloring == 0, 1, 0, missing = 0),
         event_negative_dum = if_else(coloring == 1, 1, 0, missing = 0)) %>%
  select(date, event_positive_dum, event_negative_dum)

data_complete <- data_donations %>%
  left_join(data_events, by = "date") %>%
  left_join(data_sirens, by = "date") %>%
  left_join(data_tweet_count, by = "date") %>%
  left_join(data_emotions, by = "date") %>%
  left_join(data_sentiments, by = "date") %>%
  left_join(data_factiva, by = "date") %>%
  left_join(data_severity, by = "date") %>%
  mutate(date = as_date(date)) %>%
  # mutate(across(contains(c("count", "total", "value", "mean")), ~ log(.), .names = "log_{.col}"),
  mutate(across(c(-date, -type, -contains("dum")), ~ log(.), .names = "log_{.col}"),
         across(c(-date, -type, -contains("dum"), -contains("log_")), ~ . - dplyr::lag(.), .names = "d_{.col}"),
         across(c(-date, -type, -contains("dum"), -starts_with(c("d_", "log_"))), ~ log(.) - log(dplyr::lag(.)), .names = "dlog_{.col}"))

saveRDS(data_complete, "data/data_complete.RDS")
