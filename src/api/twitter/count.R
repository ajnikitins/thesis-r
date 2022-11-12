library(academictwitteR)
library(tidyverse)
library(glue)

# Count tweets ----

data_tweet_count_daily <- count_all_tweets(
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
  file = glue("data/tweets/count/data_count_{Sys.getenv('TWITTER_COUNT_GRANULARITY')}.RDS")
)
