library(academictwitteR)
library(tidyverse)
library(glue)
library(lubridate)

# Count tweets ----
data_tweet_count_raw <- count_all_tweets(
  query = "ukraine war",
  start_tweets = Sys.getenv("TWITTER_TWEET_START"),
  end_tweets = Sys.getenv("TWITTER_TWEET_END"),
  granularity = Sys.getenv("TWITTER_COUNT_GRANULARITY"),
  bearer_token = Sys.getenv("BEARER_TOKEN"),
  n = Inf,
  is_retweet = FALSE,
  is_reply = FALSE,
  is_quote = FALSE,
  remove_promoted = TRUE,
  file = glue("data/tweets/count/data_tweet_count_{Sys.getenv('TWITTER_COUNT_GRANULARITY')}_raw.RDS")
)

# data_tweet_count_raw <- readRDS(glue("data/tweets/count/data_tweet_count_{Sys.getenv('TWITTER_COUNT_GRANULARITY')}_raw.RDS"))

# Reformat raw twitter counts
data_tweet_count <- data_tweet_count_raw %>%
  mutate(date = if (Sys.getenv('TWITTER_COUNT_GRANULARITY') == "day") floor_date(ymd_hms(start), unit = "days") else ymd_hms(start)) %>%
  select(date, tweet_count) %>%
  arrange(date)

saveRDS(data_tweet_count, glue("data/tweets/count/data_tweet_count_{Sys.getenv('TWITTER_COUNT_GRANULARITY')}.RDS"))

# Tweets by "Ukrainians"

data_ukr_accounts <- read.csv("data/tweets/ukrainian_accounts.csv", colClasses = c("character", "character"))

data_tweet_ukr_count_raw <- count_all_tweets(
  # query = c("ukraine", "war"),
  users = data_ukr_accounts$author_id,
  start_tweets = Sys.getenv("TWITTER_TWEET_START"),
  end_tweets = Sys.getenv("TWITTER_TWEET_END"),
  granularity = Sys.getenv("TWITTER_COUNT_GRANULARITY"),
  bearer_token = Sys.getenv("BEARER_TOKEN"),
  n = Inf,
  is_retweet = FALSE,
  is_reply = FALSE,
  is_quote = FALSE,
  remove_promoted = TRUE,
  file = glue("data/tweets/count/data_tweet_ukr_count_{Sys.getenv('TWITTER_COUNT_GRANULARITY')}_raw.RDS")
)

# Reformat raw twitter counts
data_tweet_ukr_count <- data_tweet_ukr_count_raw %>%
  mutate(date = if (Sys.getenv('TWITTER_COUNT_GRANULARITY') == "day") floor_date(ymd_hms(start), unit = "days") else ymd_hms(start)) %>%
  select(date, tweet_count) %>%
  arrange(date)

saveRDS(data_tweet_ukr_count, glue("data/tweets/count/data_tweet_ukr_count_{Sys.getenv('TWITTER_COUNT_GRANULARITY')}.RDS"))
