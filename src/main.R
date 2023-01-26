library(tidyverse)
library(glue)
library(systemfit)
library(texreg)

# Load data
data_raw <- readRDS("data/data_complete.RDS")

# Process data for regressions
data <- data_raw %>%
  # Filter out beginning of war
  filter(date >= "2022-03-10") %>%
  # Pivot out separate counts, total & mean values
  pivot_wider(names_from = type, values_from = contains("don")) %>%
  # Generate weekday dummies
  mutate(weekday = factor(weekdays(date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

# Get all variables
vars <- names(data_raw)

# Get all dependent variables
dep_vars <- names(data_raw)[str_detect(names(data_raw), "(?=.*don)(?=.*count|.*usd)")] %>%
  str_extract("don_.*") %>%
  unique()

# Get all types of variable forms
var_forms <- names(data_raw)[str_detect(names(data_raw), "don")] %>%
  str_match("(.*)_don") %>%
  `[`(, 2) %>%
  unique()

# List of all independent variables for all model specifications
# `name` = `independent variables`
mods_specifications <- list(
  "events" = "event_.*_dum",
  "sirens" = "siren_.*",
  # "tweets_all" = c("siren_.*", "tweet_count", "factiva_count",
  #                  "(emot|sent)_count_.*", "(emot|sent)_prop_(?!mixed).*",
  #                  "cas_civ", "cas_rus_mil", "confl_evs"),
  # "tweets_counts" = c("siren_.*",
  #                     "tweet_count",
  #                     "factiva_count",
  #                     "(emot|sent)_count_.*",
  #                     "cas_civ",
  #                     "cas_rus_mil",
  #                     "confl_evs"),
  "tweets_props" = c("siren_.*", "tweet_count", "factiva_count",
                     "(emot|sent)_prop_(?!mixed).*",
                     "sev_.*")
) %>% modify(\(patterns) {
  reduce(patterns, \(acc, pattern) {
    # Replace patterns with actual variables according to RegExr
    # Including ^ makes sure only level variable forms are included as those are handled by `get_eq`
    append(acc, str_subset(vars, paste0("^", pattern)))
  }, .init = list())
}) %>%
  # Reformat the list into a dataframe
  stack() %>%
  data.frame() %>%
  rename(specification_name = ind, indep_vars = values) %>%
  group_by(specification_name) %>%
  summarise(indep_vars = list(indep_vars), .groups = "drop")

# Generate template variable setup for all models
mods_template <- expand_grid(dep_vars, var_forms, mods_specifications)

# Generic equation
get_eq <- \(dep_var, indep_var, var_form = NA, types = c("Ukrainian", "Foreign", "Crypto"), deseasonalise = TRUE) {
  if (!is.na(var_form)) {
    dep_var <- glue("{var_form}_{dep_var}")
    indep_var <- map(indep_var, ~ {
        if (!str_detect(., "_dum") & (var_form == "d" | any(str_detect(., c("count", "total", "value", "mean"))))) {
          glue("{var_form}_{.}")}
        else .
    })
  }

  if (deseasonalise) {
    indep_var <- c(indep_var, "weekday")
  }

  indep_var <- paste(indep_var, collapse = "+")
  map(types, ~ as.formula(glue("{dep_var}_{.} ~ {indep_var}"))) %>%
    set_names(types)
}

# Run models according to specifications
mods <- mods_template %>%
  mutate(eqs = pmap(list(dep_vars, indep_vars, var_forms), get_eq)) %>%
  mutate(mods = map(eqs, systemfit, method = "SUR", data = data))

# Code for testing individual models
# dep_var <- "don_count"
# var_form <- "d"
# indep_vars <- mods_template %>%
#   filter(dep_vars == dep_va & var_forms == var_form & specification_name == "tweets_counts") %>%
#   pull(indep_vars) %>%
#   `[[`(1)
# eqs <- get_eq(dep_var = dep_var, var_form = var_form, indep_var = indep_vars)
# model <- systemfit(eqs, method = "SUR", data = data)

tmp <- mods %>%
  rowwise() %>%
  mutate(table_header_depvar = str_replace_all(ifelse(is.na(var_forms), dep_vars, glue('{var_forms}_{dep_vars}')), "_", "\\\\_"),
         table_header = list(set_names(list(1:3), paste0(table_header_depvar, ": ", specification_name)))) %>%
  mutate(table = list(texreg(mods, beside = TRUE, dcolumn = TRUE, booktabs = TRUE, custom.header = table_header)))

result <- extract(mods$mods[[1]], beside = TRUE)
screenreg(result)
screenreg(mods$mods[[1]],  beside = TRUE, dcolumn = FALSE, booktabs = TRUE, custom.header = list(a = 1:3))

texPreview::tex_preview(tmp$table[[1]])
