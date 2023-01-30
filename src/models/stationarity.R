library(tidyverse)
library(tseries)
library(lubridate)

data <- readRDS("data/data_complete.RDS") %>%
  filter(date >= dmy("16-03-2022")) %>%
  pivot_wider(names_from = type, values_from = contains("don"))

mods_adf <- data %>%
  select(-date) %>%
  summarise(var = names(.),
            # mods = map(., ~ ur.df(., type = "drift", selectlags = "AIC")))
            mods = map(., ~ adf.test(.,)),
            mods_tidy = map(mods, broom::tidy)
  ) %>%
  unnest(mods_tidy)

broom::glance(mods_adf$mods[[1]])
