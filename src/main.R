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
data_cba_value <- readRDS("data/cba/data_cba_agg.RDS")
data_cba_count <- readRDS("data/cba/data_cba_count.RDS")

# Format txs data as required
data_cba_value_filt <- data_cba_value %>%
  # Daily time
  mutate(date = floor_date(as_datetime(date), unit = "day")) %>%
  # Selected time period
  filter(date >= ymd("2022-02-24"), date <= ymd("2022-10-31")) %>%
  group_by(date) %>%
  # Count and total value
  summarize(count = n(), total_value = sum(sum_usd))

data_cba_count_filt <- data_cba_count %>%
  filter(date >= ymd("2022-02-24"), date <= ymd("2022-10-31"))

# Daily log-count over time
data_cba_count_filt %>%
  ggplot(aes(x = date, y = log(count))) +
  geom_line() +
  labs(x = NULL, y = "Log of daily donation count (USD)") +
  theme(legend.position = "none")

# Daily log-value over time
data_cba_value_filt %>%
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


### Severity
## Weekly civilian casualties
# Load severity data
data_civ_cas <- readRDS("data/severity/data_civ_cas.RDS")

data_civ_cas_filt <- data_civ_cas %>%
  # Populate pre 27/02/2022 casualties with zeros
  add_row(tribble(
    ~date, ~civ_killed, ~civ_injured, ~civ_both,
    as.Date("2022-02-24"), 0, 0, 0,
    as.Date("2022-02-25"), 0, 0, 0,
    as.Date("2022-02-26"), 0, 0, 0,
  )) %>%
  # Recalculate injured (because og data is messed up)
  mutate(civ_injured = civ_both - civ_killed) %>%
  # Fill in missing dates using last observed value
  right_join(expand(., date = full_seq(date, 1))) %>%
  arrange(date) %>%
  fill(-date) %>%
  # Select weekly data
  mutate(week = isoweek(date)) %>%
  group_by(week) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(-week) %>%
  # Calculate weekly change
  mutate(across(-date, ~ . - lag(.), .names = "d_{.col}"), .keep = "unused")


# Weekly civilian casualties
data_civ_cas_filt %>%
  pivot_longer(-date, names_to = "type", values_to = "value") %>%
  mutate(type = case_when(type == "d_civ_injured" ~ "Injured",
                          type == "d_civ_killed" ~ "Killed",
                          type == "d_civ_both" ~ "Both")) %>%
  group_by(type) %>%
  ggplot(aes(x = date, y = value, color = type)) +
  geom_line() +
  # facet_wrap(~ type, scales = "free_y") +
  labs(x = NULL, y = "Weekly change in casualties", color = "Type") +
  theme(legend.position = "bottom")


## Russian casualties
data_rus_cas <- readRDS("data/severity/data_rus_cas.RDS")

data_rus_cas_filt <- data_rus_cas %>%
  group_by(type) %>%
  mutate(d_value = value - lag(value))

# Daily change in Russian casualties
data_rus_cas_filt %>%
  ggplot(aes(x = date, y = d_value, color = type)) +
  geom_line() +
  facet_wrap(~ type, scales = "free_y", nrow = 4) +
  labs(x = NULL, y = "Daily change in Russian casualties") +
  theme(legend.position = "none")

## Conflict events
data_confl_evs <- readRDS("data/severity/data_confl_evs.RDS")

data_confl_evs_filt <- data_confl_evs %>%
  # Select relevant variables
  select(date = EVENT_DATE, type = EVENT_TYPE) %>%
  # Filter out irrelevant types %>%
  filter(type != "Riots") %>%
  # Count number of events
  group_by(date, type) %>%
  summarize(count = n(), .groups = "drop") %>%
  # Filter to relevant time period
  filter(date >= "2022-02-24" & date <="2022-10-31")

data_confl_evs_filt %>%
  ggplot(aes(x = date, y = count, color = type)) +
  geom_line() +
  facet_wrap(~ type, scales = "free_y") +
  labs(x = NULL, y = "Count of conflict events") +
  theme(legend.position = "none")
