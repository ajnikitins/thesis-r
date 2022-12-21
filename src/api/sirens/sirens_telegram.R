library(tidyverse)
library(lubridate)
library(multidplyr)

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
get_sirens <- \(date, name = ".all", data_sirens) {
  data_sirens %>%
    filter((name == ".all" | region == name) & (date %within% interval))
}

cl_sirens <- new_cluster(parallel::detectCores())
cluster_copy(cl_sirens, c("get_sirens", "data_sirens_oblast"))
cluster_library(cl_sirens, c("dplyr", "purrr", "lubridate"))

# Aggregate to date-siren count/duration
data_sirens <- expand(data_sirens_oblast,
                      date = as_date(seq(min(int_start(data_sirens_oblast$interval)), max(int_end(data_sirens_oblast$interval)), by = 60 * 60 * 24)),
                      region) %>%
  group_by(date) %>%
  partition(cl_sirens) %>%
  mutate(sirens = map2(date, region, get_sirens, data_sirens_oblast),
         siren_count = map_int(sirens, nrow),
         siren_total_duration = map_dbl(sirens, ~ sum(.$duration_mins)),
         siren_has = if_else(siren_count > 0, 1, 0)) %>%
  collect() %>%
  summarise(siren_count = sum(siren_count),
            siren_mean_duration = sum(siren_total_duration) / sum(siren_count),
            siren_mean_duration = if_else(is.nan(siren_mean_duration), 0, siren_mean_duration),
            siren_prop = sum(siren_has) / n(),
            siren_kyiv_dum = if_else(any(siren_has == 1 & (region == "Київ" | region == "Київська область")), 1, 0)) %>%
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

