library(tidyverse)
library(zoo)
library(readxl)
library(ggnewscale)

data_emotions <- readRDS("data/sentiment/data_emotions.RDS")
data_sentiments <- readRDS("data/sentiment/data_sentiments.RDS")

data_emotions_plot <- data_emotions %>%
  pivot_longer(-date, names_to = "emotion", values_to = "count") %>%
  group_by(date) %>%
  mutate(prop = count / sum(count)) %>%
  group_by(emotion) %>%
  mutate(across(-date, rollmean, k = 5, na.pad = TRUE, .names = "{.col}_mv")) %>%
  mutate(across(-date, ~ . - lag(.), .names = "d_{.col}")) %>%
  mutate(across(c(-date, -starts_with("d_")), ~ log(.) - log(lag(.)), .names = "dlog_{.col}")) %>%
  ungroup() %>%
  filter(date >= "2022-03-10")

#creating important events variables
events <- read_xlsx("data/important_events.xlsx") %>%
  mutate(date = floor_date(as_date(date), unit = "day")) %>%
  mutate(coloring = as.factor(coloring)) %>%
  filter(event_name != "N/A")

make_plot <- \(.data, dep_var) {
  .data %>%
    ggplot(aes(x = date, y = {{ dep_var }})) +
    facet_wrap(~ emotion, scales = "free_y") +
    geom_line() +
    new_scale_color() +
    geom_vline(data = events, aes(xintercept = date, color = coloring), size = 0.5, alpha = 0.5) +
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
