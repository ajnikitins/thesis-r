library(academictwitteR)
library(tidyverse)
library(lubridate)

# Actual tweets
data_tweet_count <- get_all_tweets(
  query = "ukraine war",
  start_tweets = "2022-01-01T00:00:00Z",
  end_tweets = "2022-10-21T00:00:00Z",
  bearer_token = Sys.getenv("BEARER_TOKEN"),
  n = Inf,
  is_retweet = FALSE,
  is_reply = FALSE,
  is_quote = FALSE,
  remove_promoted = TRUE,
  data_path = "data/tweets/raw/",
  bind_tweets = FALSE,
  export_query = TRUE
)

# Minute count
data_tweet_count <- count_all_tweets(
  query = "ukraine war",
  start_tweets = "2022-01-01T00:00:00Z",
  end_tweets = "2022-10-18T00:00:00Z",
  granularity = "minute",
  bearer_token = Sys.getenv("BEARER_TOKEN"),
  n = Inf,
  is_retweet = FALSE,
  is_reply = FALSE,
  is_quote = FALSE,
  remove_promoted = TRUE,
  file = "data/tweets/count/data_count_filt.RDS",
)

data_tweet_count %>%
  ggplot(aes(x = ymd_hms(end), y = tweet_count)) +
  geom_line()

data_tweet_count <- readRDS("data/tweets/count/data_count.RDS")

data_tweet_count_daily <- count_all_tweets(
  query = "ukraine war",
  start_tweets = "2022-01-01T00:00:00Z",
  end_tweets = "2022-10-20T00:00:00Z",
  granularity = "day",
  bearer_token = Sys.getenv("BEARER_TOKEN"),
  n = Inf,
  is_retweet = FALSE,
  is_reply = FALSE,
  is_quote = FALSE,
  remove_promoted = TRUE
  # file = "data/tweets/count/data_count.RDS"
)

sum(data_tweet_count$tweet_count)
sum(data_tweet_count_daily$tweet_count)
