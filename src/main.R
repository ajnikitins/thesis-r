library(tidyverse)
library(lubridate)

# Set locale for ggplot time plots
Sys.setlocale(category = "LC_TIME", locale = "English")

## BTC
# Load txs data
data_btc_txs <- readRDS("data/crypto/data_btc_txs.RDS")

# Format txs data as required
data_btc_txs_filt <- data_btc_txs %>%
  # Daily time
  mutate(date = floor_date(as_datetime(time), unit = "day")) %>%
  # Selected time period
  filter(date >= ymd("2022-02-24"), date <= ymd("2022-10-31")) %>%
  # Selected addresses
  filter(name == "Official crypto wallets of Ukraine" | name == "The Return Alive Foundation") %>%
  # Only incoming transactions
  filter(result_usd > 0) %>%
  # Update CBA name
  mutate(name = if_else(name == "The Return Alive Foundation", "The Come Back Alive Foundation", name)) %>%
  group_by(name, date) %>%
  # Count and total value
  summarize(count = n(), total_value = sum(result_usd))

# Daily log-count over time
data_btc_txs_filt %>%
  ggplot(aes(x = date, y = log(count), color = name)) +
  geom_line() +
  # facet_wrap(~ name, nrow = 2) +
  labs(x = NULL, y = "Log of daily BTC donation count") +
  theme(legend.position = "bottom")

# Daily log-value over time
data_btc_txs_filt %>%
  ggplot(aes(x = date, y = log(total_value), color = name)) +
  geom_line() +
  # facet_wrap(~ name, nrow = 2) +
  labs(x = NULL, y = "Log of daily BTC donation total value (USD)") +
  theme(legend.position = "bottom")

## Same but for ETH donations
# Load txs data
data_eth_txs <- readRDS("data/crypto/data_eth_txs.RDS")

# Format txs data as required
data_eth_txs_filt <- data_eth_txs %>%
  # Daily time
  mutate(date = floor_date(as_datetime(block_signed_at), unit = "day")) %>%
  # Selected time period
  filter(date >= ymd("2022-02-24"), date <= ymd("2022-10-31")) %>%
  # Selected addresses
  filter(name == "Official crypto wallets of Ukraine" | name == "The Return Alive Foundation") %>%
  # Only incoming transactions
  filter(to_address == address) %>%
  # Update CBA name
  mutate(name = if_else(name == "The Return Alive Foundation", "The Come Back Alive Foundation", name)) %>%
  group_by(name, date) %>%
  # Count and total value
  summarize(count = n(), total_value = sum(value_quote))

# Daily log-count over time
data_eth_txs_filt %>%
  ggplot(aes(x = date, y = log(count), color = name)) +
  geom_line() +
  # facet_wrap(~ name, nrow = 2) +
  labs(x = NULL, y = "Log of daily ETH donation count") +
  theme(legend.position = "bottom")

# Daily log-value over time
data_eth_txs_filt %>%
  ggplot(aes(x = date, y = log(total_value), color = name)) +
  geom_line() +
  # facet_wrap(~ name, nrow = 2) +
  labs(x = NULL, y = "Log of daily ETH donation total value (USD)") +
  theme(legend.position = "bottom")

## The same for CBA fiat currencies
data_cba_txs <- readRDS("data/data_cba_agg.RDS")

# Format txs data as required
data_cba_txs_filt <- data_cba_txs %>%
  # Daily time
  mutate(date = floor_date(as_datetime(date), unit = "day")) %>%
  # Selected time period
  filter(date >= ymd("2022-02-24"), date <= ymd("2022-10-31")) %>%
  group_by(date) %>%
  # Count and total value
  summarize(count = n(), total_value = sum(sum_usd))

# Daily log-value over time
data_cba_txs_filt %>%
  ggplot(aes(x = date, y = log(total_value))) +
  geom_line() +
  labs(x = NULL, y = "Log of daily donation total value (USD)") +
  theme(legend.position = "none")

### Twitter
# Load raw daily tweet count data
data_tweet_count_day <- readRDS("data/tweets/count/data_tweet_count_day.RDS")

# Process raw daily tweet count data
data_tweet_count_day_filt <- data_tweet_count_day %>%
  mutate(date = ymd(as_datetime(start)))

# Daily log tweet count
data_tweet_count_day_filt %>%
  ggplot(aes(x = date, y = log(tweet_count))) +
  geom_line() +
  labs(x = NULL, y = "Log of daily tweet count")
