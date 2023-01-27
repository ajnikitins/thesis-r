library(tidyverse)
library(lubridate)

# Load complete event dataset
data_strikes_raw <- read.csv("data/intraday/events_latest.csv")

# Filter to only airstrikes and do further pre-processing
data_strikes <- data_strikes_raw %>%
  filter(t_airstrike_b == 1 | t_artillery_b == 1) %>%
  select(date, time) %>%
  mutate(date = ymd_hm(paste(date, time)),
         date = floor_date(date, unit = "hours")) %>%
  group_by(date) %>%
  summarise(strike_count = n(), .groups = "drop")

# data_strikes %>%
#   ggplot(aes(x = datetime, y = strike_count)) + geom_line()

data_donations <- readRDS("data/data_donations_hourly.RDS")

data_intraday <- data_donations %>%
  left_join(data_strikes, by = "date") %>%
  mutate(strike_count = replace_na(strike_count, 0))

saveRDS(data_intraday, "data/data_intraday.RDS")
