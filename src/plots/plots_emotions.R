library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(zoo)
library(RcppRoll)
library(ggpubr)
library(ggnewscale)
library(ggsci)
library(ggrepel)
library(ggpattern)

data <- readRDS("data/data_complete_full.RDS")

## Emotional profile
data_emot <- data %>%
  ungroup() %>%
  filter(type == "Ukrainian") %>%
  select(date, tweet_count, intersect(starts_with("emot_count"), ends_with("ml"))) %>%
  mutate(date = floor_date(date, "week")) %>%
  group_by(date) %>%
  summarise(across(everything(), sum)) %>%
  rowwise() %>%
  mutate(emot_total_count = sum(c_across(starts_with("emot")))) %>%
  ungroup() %>%
  mutate(across(starts_with("emot_count"), ~ ./emot_total_count)) %>%
  select(-tweet_count, -emot_total_count) %>%
  rename_with(.cols = starts_with("emot_count"), .fn = ~ str_extract(., "(?<=nt_).*(?=_)")) %>%
  pivot_longer(-date,
               names_to = "emotion", values_to = "count")

emot_order <- data_emot %>%
  # filter(emotion != "neutral") %>%
  group_by(emotion) %>%
  summarise(count = mean(count)) %>%
  arrange(desc(count)) %>%
  pull(emotion) %>%
  rev()

emot_plot <- data_emot %>%
  mutate(emotion = factor(emotion, levels = emot_order)) %>%
  ggplot(aes(fill = emotion, y = count, x = date, pattern_density = emotion, pattern_shape = emotion)) +
  geom_bar_pattern(position = "stack", stat = "identity", pattern = "pch", pattern_density = 0.3, pattern_spacing = 0.02, pattern_size = 0, pattern_fill = "white") +
  # scale_pattern_density_manual(values = rep_len(c(0, 0.4, 0.45), length(emot_order))) +
  scale_pattern_density_manual(values = c(0.3, 0, 0.4, 0, 0.4, 0.3, 0)) +
  # scale_pattern_shape_manual(values = rep_len(c(NA, 21, 24), length(emot_order))) +
  scale_pattern_shape_manual(values = c(21, NA, 24, NA, 24, 21, NA)) +
  # scale_fill_grey() +
  # scale_fill_brewer(palette = "Pastel1") +
  scale_fill_uchicago() +
  # scale_pattern_fill_manual(values = pal_uchicago()(length(emot_order))) +
  # scale_fill_jco() +
  # viridis::scale_fill_viridis(option = "cividis", discrete = TRUE) +
  scale_x_date(date_labels = "%b/%y", date_breaks = "1 month", limits = as.Date(c("2022-02-01", "2023-03-01"))) +
  theme_classic() +
  ylab("Proportion of tweets by emotion") +
  xlab("") +
  theme(legend.spacing.y = unit(0, "mm"),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 0.2, fill=NA),
        panel.grid.major.x = element_line(color="light grey", linewidth =0.1),
        axis.text.x = element_text(angle = 45, hjust=1, colour = "black", size = 9),
        axis.text.y = element_text(hjust=1, colour = "black", size = 9),
        axis.title = element_text(face="bold", size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=13),
        legend.key.width = unit(1.5,"cm"))

ggsave("pics/emotion_plot.png", width = 210, height = 150, unit = "mm")
