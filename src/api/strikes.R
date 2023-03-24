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
  summarise(strike_air_count = sum(t_airstrike_b) + 1,
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

### Experiment

data_strikes_acled <- read_excel("data/severity/Ukraine_Black_Sea_2020_2023_Mar03.xlsx") %>%
  filter(str_detect(SUB_EVENT_TYPE, "missile") | str_detect(SUB_EVENT_TYPE, "Air")) %>%
  select(date = EVENT_DATE, type = SUB_EVENT_TYPE) %>%
  group_by(date, type) %>%
  summarise(count = n() + 1, .groups = "drop") %>%
  mutate(type = case_when(
    str_detect(type, "missile") ~ "strike_shelling_count",
    str_detect(type, "Air") ~ "strike_air_count"
  )) %>%
  pivot_wider(names_from = "type", values_from = "count") %>%
  mutate(across(-date, ~ replace_na(., 1)))

saveRDS(data_strikes_acled, "data/data_strikes.RDS")
