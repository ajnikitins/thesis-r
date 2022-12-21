library(tidyverse)
library(glue)
library(systemfit)

# Load data
data_raw <- readRDS("data/data_complete.RDS")

# Process data for regressions
data <- data_raw %>%
  # Filter out beginning of war
  filter(date >= "2022-03-10") %>%
  # Pivot out separate counts, total & mean values
  pivot_wider(names_from = type, values_from = contains("don")) %>%
  mutate(weekday = factor(weekdays(date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

# Get all dependent variables
dep_vars <- names(data_raw)[str_detect(names(data_raw), "don")] %>%
  str_extract("don_.*") %>%
  unique()

# Get all types of variable forms
var_forms <- names(data_raw)[str_detect(names(data_raw), "don")] %>%
  str_match("(.*)_don") %>%
  `[`(, 2) %>%
  unique()

mods_template <- expand_grid(dep_vars, var_forms)

# Generic equation
get_eq <- \(dep_var, indep_var, var_form = NA, types = c("Ukrainian", "Foreign", "Crypto"), deseasonalise = TRUE) {
  if (!is.na(var_form)) {
    dep_var <- glue("{var_form}_{dep_var}")
    indep_var <- map(indep_var, ~ if (!str_detect(., "_dum")) glue("{var_form}_{.}") else .)
  }

  if (deseasonalise) {
    indep_var <- c(indep_var, "weekday")
  }

  indep_var <- paste(indep_var, collapse = "+")
  map(types, ~ as.formula(glue("{dep_var}_{.} ~ {indep_var}")))
}

# On event dummies
mods_events <- mods_template %>%
  mutate(eqs = map2(dep_vars, var_forms, get_eq, indep_var = c("event_positive_dum", "event_negative_dum")),
         mods = map(eqs, systemfit, method = "SUR", data = data))
