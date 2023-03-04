library(academictwitteR)
library(tidyverse)
library(lubridate)

# Load actual tweets ----

data_tweet_raw <- get_all_tweets(
  query = "ukraine war",
  start_tweets = Sys.getenv("TWITTER_TWEET_START"),
  end_tweets = Sys.getenv("TWITTER_TWEET_END"),
  bearer_token = Sys.getenv("BEARER_TOKEN"),
  n = Inf,
  is_retweet = FALSE,
  is_reply = FALSE,
  is_quote = FALSE,
  remove_promoted = TRUE,
  data_path = "data_manual/tweets/raw/",
  bind_tweets = FALSE,
  export_query = TRUE
)

# Tweets by "Ukrainians"

data_ukr_accounts <- read.csv("data/tweets/ukrainian_accounts.csv", colClasses = c("character", "character"))

data_tweet_ukr_raw <- get_all_tweets(
  # query = "ukraine war",
  users = data_ukr_accounts$author_id,
  start_tweets = Sys.getenv("TWITTER_TWEET_START"),
  end_tweets = Sys.getenv("TWITTER_TWEET_END"),
  bearer_token = Sys.getenv("BEARER_TOKEN"),
  n = Inf,
  is_retweet = FALSE,
  is_reply = FALSE,
  is_quote = FALSE,
  remove_promoted = TRUE,
  data_path = "data_manual/tweets_ukr/raw/",
  bind_tweets = FALSE,
  export_query = TRUE
)
