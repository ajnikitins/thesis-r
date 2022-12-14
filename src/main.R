library(tidyverse)
library(GGally)

# Find outliers
data <- readRDS("data/data_complete.RDS")

## Seems that Crypto has 3 days with outliers
data_outliers <- data %>%
  filter(don_mean_usd > 25000)

data_log <- data %>%
  group_by(type) %>%
  mutate(don_total_usd = don_mean_usd * don_count,
    don_total_usd_log = log(don_total_usd),
    don_mean_usd_log = log(don_mean_usd),
         don_count_log = log(don_count))

data_log %>%
  filter(don_mean_usd > 0) %>%
  filter(date > "2022-02-22") %>%
  select(date, type, don_mean_usd, don_mean_usd_log) %>%
  pivot_longer(-c(date, type), names_to = "var", values_to = "val") %>%
  ggplot(aes(x = val)) +
  geom_density() +
  facet_wrap(~ var + type, scale = "free")

data_log %>%
  select(date, type, don_count, don_count_log) %>%
  pivot_longer(-c(date, type), names_to = "var", values_to = "val") %>%
  ggplot(aes(x = val)) +
  geom_density() +
  facet_wrap(~ var + type, scale = "free")

data_log %>%
  select(date, type, don_total_usd, don_total_usd_log) %>%
  pivot_longer(-c(date, type), names_to = "var", values_to = "val") %>%
  ggplot(aes(x = val)) +
  geom_density() +
  facet_wrap(~ var + type, scale = "free")


# Plots
# Visual check
data %>%
  filter(type == "Ukrainian") %>%
  select(-date, -type) %>%
  ggpairs()

data %>%
  filter(type == "Foreign") %>%
  select(-date, -type) %>%
  ggpairs()

data %>%
  filter(type == "Crypto") %>%
  select(-date, -type) %>%
  ggpairs()

data %>%
  filter(don_mean_usd > 0) %>%
  ggplot(aes(x = date, y = don_mean_usd)) +
  geom_line() +
  facet_wrap(~ type, scales = "free_y")


data %>%
  ggplot(aes(x = date, y = d_don_mean_usd)) +
  geom_line() +
  facet_wrap(~ type, scales = "free_y")
