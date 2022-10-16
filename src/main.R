library(tidyverse)
library(lubridate)
library(dynlm)
library(ggfortify)
library(urca)
library(lmtest)
library(sandwich)

## BTC
# Load txs data
data_btc_txs <- readRDS("../data/data_btc_txs.RDS")
data_btc_txs <- data_btc_txs %>%
  mutate(time = floor_date(as_datetime(time), unit = "day")) %>%
  filter(time > dmy("01/01/2022"))

# Donation count, sum & mean
data_btc_don <- data_btc_txs %>%
  filter(result > 0) %>%
  group_by(address, name, time) %>%
  summarise(count = n(), sum = sum(result), mean = mean(result), .groups = "drop")

data_btc_don %>%
  filter(name == "Official" | name == "ComeBackAlive") %>%
  ggplot(aes(x = time, y = log(count), color = name)) +
  geom_line() +
  xlab(NULL)+
  facet_wrap(~ name) +
  theme(legend.position = "none") +
  NULL

data_btc_don %>%
  ggplot(aes(x = time, y = log(sum), color = name)) +
  geom_line() +
  facet_wrap(~ name) +
  # theme(legend.position = "none") +
  NULL

data_btc_don %>%
  ggplot(aes(x = time, y = log(mean), color = name)) +
  geom_line() +
  # geom_vline(xintercept = dmy_hm("03/07/2022 00:00"), size = 0.5, color = "red") +
  facet_wrap(~ name) +
  # theme(legend.position = "none") +
  NULL

## Trends
data_trends_ukraine <- read.csv("../data/trends_ukraine.csv") %>%
  mutate(time = ymd(time))

## Models
data_trend_cba <- data_btc_don %>%
  filter(name == "ComeBackAlive") %>%
  right_join(data_trends_ukraine, by = "time") %>%
  filter(time <= dmy("11/09/2022") & time >= dmy("24/02/2022")) %>%
  select(count, sum, mean, ukraine) %>%
  mutate(days_since = 0:(length(ukraine)-1)) %>%
  ts(start = 1, frequency = 365)

data_trend_cba[, "count"] %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()

data_trend_cba[, "sum"] %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()

data_trend_cba[, "mean"] %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()

data_trend_cba[, "ukraine"] %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()
# ComeBackAlive count vs ukraine trends
model_count_trend_cba <- data_trend_cba %>%
  dynlm(log(count) ~ log(ukraine), data = .)
model_count_trend_cba %>% summary()
coeftest(model_count_trend_cba, vcov = vcovHAC(model_count_trend_cba), save = TRUE)

data_trend_cba %>%
  ggplot(aes(x = log(ukraine), y = log(count))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

data_trend_cba %>%
  dynlm(log(count) ~ log(ukraine) + days_since, data = .) %>%
  coeftest(vcov = vcovHAC(.), save = TRUE)

# ComeBackAlive sum vs ukraine trends
model_sum_trend_cba <- data_trend_cba %>%
  dynlm(log(sum) ~ log(ukraine), data = .)
model_sum_trend_cba %>% summary()
coeftest(model_sum_trend_cba, vcov = vcovHAC(model_sum_trend_cba), save = TRUE)

data_trend_cba %>%
  ggplot(aes(x = log(ukraine), y = log(sum))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

data_trend_cba %>%
  dynlm(log(sum) ~ log(ukraine) + days_since, data = .) %>%
  coeftest(vcov = vcovHAC(.), save = TRUE)

# ComeBackAlive mean vs ukraine trends
model_mean_trend_cba <- data_trend_cba %>%
  dynlm(log(mean) ~ log(ukraine), data = .)
model_mean_trend_cba %>% summary()
coeftest(model_mean_trend_cba, vcov = vcovHAC(model_mean_trend_cba))

data_trend_cba %>%
  ggplot(aes(x = log(ukraine), y = log(mean))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

data_trend_cba %>%
  dynlm(log(mean) ~ log(ukraine) + days_since, data = .) %>%
  coeftest(vcov = vcovHAC(.), save = TRUE)

# Official vs trends ukraine
# this has NAs, don't use diffs/lags :)
data_trend_off <- data_btc_don %>%
  filter(name == "Official") %>%
  right_join(data_trends_ukraine, by = "time") %>%
  filter(time <= dmy("11/09/2022") & time >= dmy("24/02/2022")) %>%
  select(count, sum, mean, ukraine) %>%
  mutate(days_since = 0:(length(ukraine)-1)) %>%
  filter(if_all(everything(), ~ !is.na(.))) %>%
  ts(start = 1, frequency = 365)

data_trend_off[, "count"] %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()

data_trend_off[, "sum"] %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()

data_trend_off[, "mean"] %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()

data_trend_off[, "ukraine"] %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()
# ComeBackAlive count vs ukraine trends
model_count_trend_off <- data_trend_off %>%
  dynlm(log(count) ~ log(ukraine), data = .)
model_count_trend_off %>% summary()
coeftest(model_count_trend_off, vcov = vcovHAC(model_count_trend_off), save = TRUE)

data_trend_off %>%
  ggplot(aes(x = log(ukraine), y = log(count))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

data_trend_off %>%
  dynlm(log(count) ~ log(ukraine) + days_since, data = .) %>%
  coeftest(vcov = vcovHAC(.), save = TRUE)

# ComeBackAlive sum vs ukraine trends
model_sum_trend_off <- data_trend_off %>%
  dynlm(log(sum) ~ log(ukraine), data = .)
model_sum_trend_off %>% summary()
coeftest(model_sum_trend_off, vcov = vcovHAC(model_sum_trend_off), save = TRUE)

data_trend_off %>%
  ggplot(aes(x = log(ukraine), y = log(sum))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

data_trend_off %>%
  dynlm(log(sum) ~ log(ukraine) + days_since, data = .) %>%
  coeftest(vcov = vcovHAC(.), save = TRUE)

# ComeBackAlive mean vs ukraine trends
model_mean_trend_off <- data_trend_off %>%
  dynlm(log(mean) ~ log(ukraine), data = .)
model_mean_trend_off %>% summary()
coeftest(model_mean_trend_off, vcov = vcovHAC(model_mean_trend_off))

data_trend_off %>%
  ggplot(aes(x = log(ukraine), y = log(mean))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

data_trend_off %>%
  dynlm(log(mean) ~ log(ukraine) + days_since, data = .) %>%
  coeftest(vcov = vcovHAC(.), save = TRUE)

BTC_ADDRESS <- "357a3So9CbsNfBBgFYACGvxxS6tMaDoa1P"

# Load
data_btc <- readRDS("../old/data_btc.RDS")
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

eth_data <- readRDS("../old/data_eth.RDS")

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
data_trends_donation <- read.csv("../data/trend_donation.csv", header = TRUE, sep = ",") %>%
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
