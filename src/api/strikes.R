library(tidyverse)
library(lubridate)

# Load complete event dataset
data_strikes_raw <- read.csv("data/intraday/events_latest.csv")

# Filter to only airstrikes and do further pre-processing
data_strikes <- data_strikes_raw %>%
  filter(t_airstrike_b == 1 | t_artillery_b == 1) %>%
  select(date, t_airstrike_b, t_artillery_b) %>%
  mutate(date = ymd(date)) %>%
  group_by(date) %>%
  summarise(strike_air_count = sum(t_airstrike_b),
            strike_arty_count = sum(t_artillery_b),
            strike_air_arty_count = strike_air_count + strike_arty_count,
            .groups = "drop")

saveRDS(data_strikes, "data/data_strikes.RDS")
