library(tidyverse)

## Ukranian civilian casualties
data_civ_cas <- read.csv("data/severity/Data_ ReliefWeb Crisis Figures Data - historical_figures.csv") %>%
  mutate(figure_name = case_when(
    str_detect(figure_name, fixed("Civilians Killed since 24 Feb 2022")) ~ "civ_killed",
    str_detect(figure_name, fixed("Civilians Injured since 24 Feb 2022")) ~ "civ_injured",
    str_detect(figure_name, fixed("Civilian Casualties since 24 Feb 2022 (killed & injured)")) ~ "civ_both",
    TRUE ~ figure_name
  )) %>%
  filter(crisis_index == 27 & str_detect(figure_name, "civ_")) %>%
  select(date = figure_date, variable = figure_name, value = figure_value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(date = if_else(date == "2021-03-10", "2022-03-10", date)) %>%
  arrange(date)
