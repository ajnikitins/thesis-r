library(tidyverse)
library(zoo)
library(readxl)
library(ggnewscale)

data_emotions <- readRDS("data/sentiment/data_emotions.RDS") %>%
  mutate(classifier = "dict") %>%
  rename(emot_count_neutral = emot_count_mixed, emot_prop_neutral = emot_prop_mixed)
data_emotions_ml <- readRDS("data/sentiment/data_emotions_ml.RDS") %>%
  mutate(classifier = "ml")

# data_sentiments <- readRDS("data/sentiment/data_sentiments.RDS")

# Join both emotion datasets together
data_emotions_plot <- bind_rows(data_emotions, data_emotions_ml) %>%
  pivot_longer(-c(date, classifier), names_to = c(NA, ".value", "emotion"), names_sep = "_") %>%
  group_by(classifier, emotion) %>%
  mutate(across(-date, ~ rollmean(., k = 5, fill = list(NA, NA, NA), align = "right"), .names = "{.col}_mv"),
         across(-date, ~ . - lag(.), .names = "d_{.col}"),
         across(c(-date, -starts_with("d_")), ~ log(.) - log(lag(.)), .names = "dlog_{.col}")) %>%
  ungroup() %>%
  filter(date >= "2022-03-16")

#creating important events variables
events <- read_xlsx("data/important_events_hourly.xlsx") %>%
  mutate(date = floor_date(ymd_hms(datetime), unit = "day")) %>%
  mutate(coloring = as.factor(coloring)) %>%
  filter(event_name != "N/A") %>%
  select(date, coloring)

make_plot <- \(.data, dep_var) {
  .data %>%
    ggplot(aes(x = date, y = {{ dep_var }}, color = classifier)) +
    facet_wrap(~ emotion, scales = "free_y") +
    geom_line() +
    new_scale_color() +
    geom_vline(data = events, aes(xintercept = date, color = coloring), linewidth = 0.5, alpha = 0.5) +
    scale_color_manual(values = c(`1` = "red", `0` = "dark green")) +
    NULL
}

# Count
make_plot(data_emotions_plot, count_mv)

# Log-count
make_plot(data_emotions_plot, log(count_mv))

# Diff count
make_plot(data_emotions_plot, d_count_mv)

# Log-diff count
make_plot(data_emotions_plot, dlog_count_mv)

# Prop
make_plot(data_emotions_plot, prop_mv)

# Diff prop
make_plot(data_emotions_plot, d_prop_mv)

# Log-diff prop
make_plot(data_emotions_plot, dlog_prop_mv)
