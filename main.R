library(tidyverse)
library(lubridate)
library(dynlm)

## BTC
BTC_ADDRESS <- "357a3So9CbsNfBBgFYACGvxxS6tMaDoa1P"

# Load
data_btc <- readRDS("data_btc.RDS")
data_btc_txs <- data_btc$txs %>%
  mutate(time = as_datetime(time))

# Count over time; select precision with floor_date
data_btc_txs %>%
  mutate(time = floor_date(time, "day")) %>%
  ggplot(aes(x = time)) + geom_line(stat = "count")

# Sum of all incoming transactions; select precision with floor_date
data_btc_txs %>%
  filter(result > 0) %>%
  mutate(time = floor_date(time, "week")) %>%
  group_by(time) %>%
  summarise(sum(result)) %>%
  ggplot(aes(x = time, y = log(`sum(result)`))) + geom_line() +
  # geom_vline(xintercept = dmy("16 May 2022"), size = 1) +
  NULL

data_btc_txs %>%
  filter(result > 0) %>%
  # filter(date(time) > dmy("01 Jul 2022") & date(time) < dmy("22 Jul 2022")) %>%
  # filter(date(time) == dmy("16 Jul 2022") | date(time) == dmy("17 Jul 2022")) %>%
  mutate(time = floor_date(time, "minutes")) %>%
  group_by(time) %>%
  summarise(mean(result)) %>%
  ggplot(aes(x = time, y = log(`mean(result)`))) + geom_line() +
  # geom_vline(xintercept = dmy("16 May 2022"), size = 1) +
  NULL

data_btc_txs %>%
  filter(result > 0) %>%
  filter(date(time) < dmy("1 Apr 2022")) %>%
  # filter(date(time) == dmy("16 Jul 2022") | date(time) == dmy("17 Jul 2022")) %>%
  mutate(time = floor_date(time, "day")) %>%
  count(time) %>%
  ggplot(aes(x = time, y = n)) + geom_col() +
  # geom_vline(xintercept = dmy("16 May 2022"), size = 1) +
  NULL



## ETH

eth_data <- readRDS("data_eth.RDS")

eth_data$items %>%
  mutate(time = floor_date(as_datetime(block_signed_at), "hour")) %>%
  count(time) %>%
  ggplot(aes(x = time, y = log(n))) + geom_line()

eth_data$items %>%
  count(to_address_label) %>%
  arrange(n)

eth_data$items %>%
  filter(to_address_label == "Tether USD (USDT)") %>%
  mutate(time = floor_date(as_datetime(block_signed_at), "hour")) %>%
  count(time) %>%
  ggplot(aes(x = time, y = n)) + geom_line()

eth_data$items %>%
  mutate(time = floor_date(as_datetime(block_signed_at), "second")) %>%
  filter(date(time) > dmy("11/04/2022") & date(time) < dmy("14/04/2022")) %>%
  count(time) %>%
  ggplot(aes(x = time, y = n)) + geom_line()

eth_data$items %>%
  mutate(time = floor_date(as_datetime(block_signed_at), "day")) %>%
  filter(value_quote > 0) %>%
  filter(from_address != "0x165cd37b4c644c2921454429e7f9358d18a45e14") %>%
  group_by(time) %>%
  summarise(mean(value_quote)) %>%
  ggplot(aes(x = time, y = `mean(value_quote)`)) + geom_line() +
  # geom_vline(xintercept = dmy("16 May 2022"), size = 1) +
  NULL

eth_data$items %>%
  mutate(time = floor_date(as_datetime(block_signed_at), "day")) %>%
  filter(value_quote > 0) %>%
  filter(from_address != "0x165cd37b4c644c2921454429e7f9358d18a45e14") %>%
  group_by(time) %>%
  summarise(sum(value_quote)) %>%
  ggplot(aes(x = time, y = log(`sum(value_quote)`))) + geom_line() +
  # geom_vline(xintercept = dmy("16 May 2022"), size = 1) +
  NULL


## Trends
data_trends_donation <- read.csv("trend_donation.csv", header = TRUE, sep = ",") %>%
  mutate(time = ymd(time))

data_btc_count <- data_btc_txs %>%
  mutate(time = floor_date(as_datetime(time), "day")) %>%
  count(time)

data_btc_trend_ts <- data_trends_donation %>%
  left_join(data_btc_count, by = "time") %>%
  mutate(n = replace_na(n, 0)) %>%
  select(-time) %>%
  mutate(d_ukraine.donation = ukraine.donation - lag(ukraine.donation),
         d_n = n - lag(n)) %>%
  ts(start = c(1, 1), freq = 1)

tmp <- data_btc_trend_ts %>%
  dynlm(L(n,3) ~ ukraine.donation, data = .) %>%
    summary()

autoplot(tmp$residuals)

cor(data_btc_trend_ts[, c("ukraine.donation", "n")])

autoplot

library(forecast)
ggAcf(data_btc_trend_ts[, c("d_n")])

library(urca)

data_btc_trend_ts[, c("n")] %>%
  ur.df(type = "drif", selectlags = "AIC") %>%
  summary()
