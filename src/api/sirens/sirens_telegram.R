library(tidyverse)
library(lubridate)

# data_sirens_raw_full <- read.csv("src/api/sirens/ukrainian-air-raid-sirens-dataset/datasets/full_data.csv", header = TRUE, encoding = "UTF-8")

# TODO: Convert to minute-by-minute (when possible)

# Load data
data_sirens_oblast_raw <- read.csv("src/api/sirens/ukrainian-air-raid-sirens-dataset/datasets/oblasts_only.csv", header = TRUE, encoding = "UTF-8")

# Format oblast-siren level data
data_sirens_oblast <- data_sirens_oblast_raw %>%
  mutate(#region = as.factor(region),
         interval = interval(started_at, finished_at),
         duration_mins = int_length(interval) / 60,
         interval = interval(floor_date(int_start(interval), "day"), floor_date(int_end(interval), "day")), .keep = "unused")

# Given name of region and date, find sirens
get_sirens <- \(data_sirens, date, name = ".all") {
  data_sirens %>%
    filter((name == ".all" | enc2native(region) == enc2native(name)) & (date %within% interval))
}

# Aggregate to date-siren count/duration
data_sirens <- data.frame(date = as_date(seq(min(int_start(data_sirens_oblast$interval)), max(int_end(data_sirens_oblast$interval)), by = 60 * 60 * 24))) %>%
  rowwise() %>%
  mutate(sirens = list(get_sirens(data_sirens_oblast, date)),
         sirens_kyiv = list(bind_rows(get_sirens(data_sirens_oblast, date, "Київ"), get_sirens(data_sirens_oblast, date, "Київська область"))),
         siren_count_all = nrow(sirens),
         siren_mean_duration_all = mean(sirens$duration_mins),
         siren_count_kyiv = nrow(sirens_kyiv),
         siren_mean_duration_kyiv = mean(sirens_kyiv$duration_mins)) %>%
  ungroup() %>%
  select(-sirens, -sirens_kyiv) %>%
  right_join(expand(., date = seq(dmy("01-01-2022"), max(date), by = 1)), by = "date") %>%
  mutate(across(-date, ~ replace_na(., 0))) %>%
  arrange(date)

saveRDS(data_sirens, "data/sirens/data_sirens.RDS")

## Share of naive sirens
# data_sirens_oblast %>%
#   mutate(date = floor_date(started_at, "dates"),
#          naive = if_else(naive == "True", 1, 0)) %>%
#   group_by(date) %>%
#   summarize(prop_naive = sum(naive) / n()) %>%
#   ggplot(aes(x = date, y = prop_naive)) + geom_line()

