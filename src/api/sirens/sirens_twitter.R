library(academictwitteR)
library(tidyverse)
library(lubridate)
library(glue)

data_air_raid_siren_raw <- get_all_tweets(
  query = c("Ukraine Siren Alert", "All Clear!"),
  users = "ukrainealert",
  start_tweets = Sys.getenv("TWITTER_TWEET_START"),
  end_tweets = Sys.getenv("TWITTER_TWEET_END"),
  bearer_token = Sys.getenv("BEARER_TOKEN"),
  n = Inf,
  is_retweet = FALSE,
  is_reply = FALSE,
  is_quote = FALSE,
  remove_promoted = TRUE,
  data_path = "data/air_raid_sirens/tweets"
)

saveRDS(data_air_raid_siren_raw, "data/sirens/data_air_raid_siren_raw.RDS")

data_air_raid_siren_raw <- readRDS("data/sirens/data_sirens_raw.RDS")

data_sirens <- data_sirens_raw %>%
  select(text) %>%
  filter(str_detect(text, "Ukraine Siren Alert|All [Cc]lear!",)) %>%
  filter(str_detect(text, "THIS IS A TEST", negate = TRUE)) %>%
  separate(text, into = glue("{1:6}"), sep = "\n", extra = "drop", fill = "right") %>%
  extract(1, into = c("status", "date"), regex = "(Alert|All [Cc]lear)!* \\[([0-9\\/]+)")
  I()
