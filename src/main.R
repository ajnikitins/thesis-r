library(tidyverse)
library(glue)
library(systemfit)
library(texreg)

WEEKDAYS <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Load data
data_raw <- readRDS("data/data_complete.RDS")

# Process data for regressions
data <- data_raw %>%
  # Filter out beginning of war
  filter(date >= "2022-03-10") %>%
  # Pivot out separate counts, total & mean values
  pivot_wider(names_from = type, values_from = contains("don")) %>%
  # Generate weekday dummies
  mutate(weekday = factor(weekdays(date), levels = WEEKDAYS))

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

# Generate texreg tables from models
mod_tables <- mods %>%
  # Var form ordering
  mutate(var_forms = replace_na(var_forms, "level"),
         var_forms = factor(var_forms, levels = unique(var_forms))) %>%
  # Combine models into groups for the same specifications and functional forms (i.e., tables will be with all three dependent variables)
  group_by(specification_name, var_forms) %>%
  summarise(dep_vars = list(dep_vars), mods = list(mods)) %>%
  rowwise() %>%
  # Define table parameters for the three specifications, and transform them into LaTeX
  mutate(table_groups = list(switch(as.character(specification_name),
                                    events = list("Event types" = 2:3, "Seasonality" = 4:9),
                                    sirens = list("Air raid sirens" = 2:5, "Seasonality" = 6:11),
                                    tweets_props = list("Air raid sirens" = 2:5, "Media" = 6:7, "Emotion type" = 8:15, "Sentiment type" = 16:17, "War severity" = 18:20, "Seasonality" = 21:26),
         )),
         table_scalebox = list(switch(as.character(specification_name),
                                      tweets_props = 0.65,
                                      1
         )),
         table_sideways = list(switch(as.character(specification_name),
                                      tweets_props = FALSE,
                                      TRUE
         )),
         table_variable_names = list(switch(as.character(specification_name),
                                            events = c("(Intercept)", "Positive events", "Negative events", WEEKDAYS[-1]),
                                            sirens = c("(Intercept)", "Sirens", "Siren duration", "Share of Ukraine", "In Kyiv?", WEEKDAYS[-1]),
                                            tweets_props = c("(Intercept)", "Sirens", "Siren duration", "Share of Ukraine", "In Kyiv?",
                                                       "Tweets", "News articles",
                                                       "Joy", "Anger", "Surprise", "Trust", "Fear", "Anticipation", "Sadness", "Disgust", "Positive", "Negative",
                                                       "Civilian casualties", "Russian mil. casualties", "Conflict events", WEEKDAYS[-1])
         )),
         table_caption_var_forms = list(switch(as.character(var_forms), `level` = "Levels", log = "Logs", d = "First-differences", dlog = "Log-differences")),
         table_caption_specifications = list(switch(as.character(specification_name),
                                                    events = "positive and negative events",
                                                    sirens = "air raid sirens",
                                                    tweets_props = "air raid sirens, tweet \\& media article counts, proportions of tweet emotions, and war severity"
         )),
         table_caption = glue("{table_caption_var_forms} of donation characteristics explained by {table_caption_specifications}."),
         table_label = glue("table:{var_forms}_{specification_name}"),
         table_header_dep_vars = list(map(dep_vars, \(dep_var, var_form) ifelse(var_form == "level", dep_var, glue('{var_form}_{dep_var}')), var_forms)),
         table_header = list(set_names(list(1:3, 4:6, 7:9), str_replace_all(glue("{table_header_dep_vars}"), "_", "\\\\_")))) %>%
  mutate(table = list(texreg(mods, beside = TRUE,
                             dcolumn = TRUE, booktabs = TRUE, sideways = table_sideways,
                             custom.coef.names = table_variable_names,
                             custom.header = table_header, groups = table_groups, scalebox = table_scalebox,
                             caption = table_caption, label = table_label,
                             use.packages = FALSE)))

ltx_file <- paste(mod_tables$table)
write(ltx_file, "src/latex/supplement.tex")
tools::texi2pdf("src/latex/main.tex", clean = TRUE)
file.copy("main.pdf", "data/tables.pdf")
