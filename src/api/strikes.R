library(tidyverse)
library(lubridate)

# Load complete event dataset
data_strikes_raw <- read.csv("data/intraday/events_latest.csv")

# Filter to only airstrikes and do further pre-processing
data_strikes_hourly <- data_strikes_raw %>%
  filter(t_airstrike_b == 1 | t_artillery_b == 1) %>%
  select(date, time, t_airstrike_b, t_artillery_b) %>%
  mutate(date = ymd_hm(paste(date, time)),
         date = floor_date(date, unit = "hours")) %>%
  group_by(date) %>%
  summarise(strike_air_count = sum(t_airstrike_b),
            strike_arty_count = sum(t_artillery_b),
            strike_air_arty_count = strike_air_count + strike_arty_count,
            .groups = "drop") %>%
  right_join(tidyr::expand(., date = full_seq(date, 3600)), by = "date") %>%
  mutate(across(-date, ~ replace_na(., 0))) %>%
  arrange(date)

saveRDS(data_strikes_hourly, "data/data_strikes_hourly.RDS")

# Aggregate to daily airstrikes
data_strikes <- data_strikes_hourly %>%
  mutate(date = floor_date(date, unit = "days")) %>%
  group_by(date) %>%
  summarise(across(everything(), sum), .groups = "drop")

saveRDS(data_strikes, "data/data_strikes.RDS")
