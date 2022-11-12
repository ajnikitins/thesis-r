library(academictwitteR)
library(tidyverse)
library(lubridate)

# Load actual tweets ----

data_tweet_count <- get_all_tweets(
  query = "ukraine war",
  start_tweets = Sys.getenv("TWITTER_TWEET_START"),
  end_tweets = Sys.getenv("TWITTER_TWEET_END"),
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
