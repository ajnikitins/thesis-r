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
  filter(date >= "2022-03-16") %>%
  # Pivot out separate counts, total & mean values
  pivot_wider(names_from = type, values_from = contains("don")) %>%
  # Generate weekday dummies
  mutate(weekday = factor(weekdays(date), levels = WEEKDAYS),
         daysSince = as.numeric(date - 19047))

# Get all variables
vars <- names(data_raw)

# Get all dependent variables
dep_vars <- names(data_raw)[str_detect(names(data_raw), "(?=.*don)(?=.*count|.*usd)")] %>%
  str_extract("don_.*") %>%
  unique()

# List of all independent variables for all model specifications
mods_specifications <- list(
  list(name = "events", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum")),
  # list(name = "sirens", dep_var_form = c("dlog"), indep_vars = c("dlog_siren_count", "dlog_siren_mean_duration", "d_siren_prop", "siren_kyiv_dum")),
  list(name = "sirens_dlog", dep_var_form = c("dlog"), indep_vars = c("dlog_siren_count", "dlog_siren_mean_duration", "dlog_siren_prop", "siren_kyiv_dum")),
  # list(name = "tweets_props", dep_var_form = c("dlog"), indep_vars = c("dlog_siren_count", "dlog_siren_mean_duration", "d_siren_prop", "siren_kyiv_dum",
  #                                                                "dlog_tweet_count", "dlog_factiva_count",
  #                                                                "d_emot_prop_joy", "d_emot_prop_anger", "d_emot_prop_surprise", "d_emot_prop_trust", "d_emot_prop_fear", "d_emot_prop_anticip", "d_emot_prop_sadness", "d_emot_prop_disgust",
  #                                                                "d_sent_prop_positive", "d_sent_prop_negative",
  #                                                                "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count")),
  # list(name = "tweets_props_nosentiment", dep_var_form = c("dlog"), indep_vars = c("dlog_siren_count", "dlog_siren_mean_duration", "d_siren_prop", "siren_kyiv_dum",
  #                                                                "dlog_tweet_count", "dlog_factiva_count",
  #                                                                "d_emot_prop_joy", "d_emot_prop_anger", "d_emot_prop_surprise", "d_emot_prop_trust", "d_emot_prop_fear", "d_emot_prop_anticip", "d_emot_prop_sadness", "d_emot_prop_disgust",
  #                                                                "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count")),
  # list(name = "tweets_props_dlog", dep_var_form = c("dlog"), indep_vars = c("dlog_siren_count", "dlog_siren_mean_duration", "dlog_siren_prop", "siren_kyiv_dum",
  #                                                                "dlog_tweet_count", "dlog_factiva_count",
  #                                                                "dlog_emot_prop_joy", "dlog_emot_prop_anger", "dlog_emot_prop_surprise", "dlog_emot_prop_trust", "dlog_emot_prop_fear", "dlog_emot_prop_anticip", "dlog_emot_prop_sadness", "dlog_emot_prop_disgust",
  #                                                                "dlog_sent_prop_positive", "dlog_sent_prop_negative")),
  list(name = "tweets_props_nosentiment_dlog", dep_var_form = c("dlog"), indep_vars = c("dlog_siren_count", "dlog_siren_mean_duration", "dlog_siren_prop", "siren_kyiv_dum",
                                                                 "dlog_tweet_count", "dlog_factiva_count",
                                                                 "dlog_emot_prop_joy", "dlog_emot_prop_anger", "dlog_emot_prop_surprise", "dlog_emot_prop_trust", "dlog_emot_prop_fear", "dlog_emot_prop_anticip", "dlog_emot_prop_sadness", "dlog_emot_prop_disgust",
                                                                 "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count"))
  # list(name = "tweets_counts", dep_var_form = c("dlog"), indep_vars = c("dlog_siren_count", "dlog_siren_mean_duration", "dlog_siren_prop", "siren_kyiv_dum",
  #                                                                           "dlog_tweet_count", "dlog_factiva_count",
  #                                                                           "dlog_emot_count_joy", "dlog_emot_count_anger", "dlog_emot_count_surprise", "dlog_emot_count_trust", "dlog_emot_count_fear", "dlog_emot_count_anticip", "dlog_emot_count_sadness", "dlog_emot_count_disgust",
  #                                                                           "dlog_sent_count_positive", "dlog_sent_count_negative",
  #                                                                           "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count")),
  # list(name = "tweets_counts_nototal", dep_var_form = c("dlog"), indep_vars = c("dlog_siren_count", "dlog_siren_mean_duration", "dlog_siren_prop", "siren_kyiv_dum",
  #                                                                       "dlog_factiva_count",
  #                                                                       "dlog_emot_count_joy", "dlog_emot_count_anger", "dlog_emot_count_surprise", "dlog_emot_count_trust", "dlog_emot_count_fear", "dlog_emot_count_anticip", "dlog_emot_count_sadness", "dlog_emot_count_disgust",
  #                                                                       "dlog_sent_count_positive", "dlog_sent_count_negative",
  #                                                                       "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count")),
  # list(name = "tweets_counts_nosentiment_nototal", dep_var_form = c("dlog"), indep_vars = c("dlog_siren_count", "dlog_siren_mean_duration", "dlog_siren_prop", "siren_kyiv_dum",
  #                                                                       "dlog_factiva_count",
  #                                                                       "dlog_emot_count_joy", "dlog_emot_count_anger", "dlog_emot_count_surprise", "dlog_emot_count_trust", "dlog_emot_count_fear", "dlog_emot_count_anticip", "dlog_emot_count_sadness", "dlog_emot_count_disgust",
  #                                                                       "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count"))
  ) %>%
  reduce(\(acc, x) {
    frame <- data.frame(specification_name = x$name, dep_var_form = x$dep_var_form) %>%
      mutate(indep_vars = list(x$indep_vars))
    bind_rows(acc, frame)
  }, .init = NULL)

# Generate template variable setup for all models
mods_template <- expand_grid(dep_var = dep_vars, mods_specifications)

# Generic equation
get_eq <- \(dep_var, indep_var, dep_var_form = NA, types = c("Ukrainian", "Foreign", "Crypto"), deseasonalise = TRUE) {
  if (!is.na(dep_var_form)) {
    dep_var <- glue("{dep_var_form}_{dep_var}")
  }

  if (deseasonalise) {
    indep_var <- c(indep_var, "weekday")
  }

  indep_var <- paste(indep_var, collapse = "+")
  # map(types, ~ as.formula(glue("{dep_var}_{.} ~ {indep_var} + daysSince + I(daysSince^2)"))) %>%
  map(types, ~ as.formula(glue("{dep_var}_{.} ~ {indep_var} + daysSince"))) %>%
    set_names(types)
}

# Run models according to specifications
mods <- mods_template %>%
  mutate(eq = pmap(list(dep_var, indep_vars, dep_var_form), get_eq)) %>%
  mutate(mod = map(eq, systemfit, method = "SUR", data = data))

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
  # Combine models into groups for the same specifications and functional forms (i.e., tables will be with all three dependent variables)
  group_by(specification_name, dep_var_form) %>%
  summarise(dep_vars = list(dep_var), mod = list(mod)) %>%
  rowwise() %>%
  # Define table parameters for the three specifications, and transform them into LaTeX
  mutate(table_groups = list(switch(as.character(specification_name),
                                    events = list("Event types" = 2:3, "Seasonality" = 4:9),
                                    # sirens = list("Air raid sirens" = 2:5, "Seasonality" = 6:11),
                                    sirens_dlog = list("Air raid sirens" = 2:5, "Seasonality" = 6:11),
                                    # tweets_props = list("Air raid sirens" = 2:5, "Media" = 6:7, "Emotion type" = 8:15, "Sentiment type" = 16:17, "War severity" = 18:20, "Seasonality" = 21:26),
                                    # tweets_props_nosentiment = list("Air raid sirens" = 2:5, "Media" = 6:7, "Emotion type" = 8:15, "War severity" = 16:18, "Seasonality" = 19:24),
                                    # tweets_props_dlog = list("Air raid sirens" = 2:5, "Media" = 6:7, "Emotion type" = 8:15, "Sentiment type" = 16:17, "War severity" = 18:20, "Seasonality" = 21:26),
                                    tweets_props_nosentiment_dlog = list("Air raid sirens" = 2:5, "Media" = 6:7, "Emotion type" = 8:15, "War severity" = 16:18, "Seasonality" = 19:24),
                                    # tweets_counts = list("Air raid sirens" = 2:5, "Media" = 6:7, "Emotion type" = 8:15, "Sentiment type" = 16:17, "War severity" = 18:20, "Seasonality" = 21:26),
                                    # tweets_counts_nototal = list("Air raid sirens" = 2:5, "Media" = 6, "Emotion type" = 7:14, "Sentiment type" = 15:16, "War severity" = 17:19, "Seasonality" = 20:25),
                                    # tweets_counts_nosentiment = list("Air raid sirens" = 2:5, "Media" = 6, "Emotion type" = 7:14, "War severity" = 15:17, "Seasonality" = 18:23),
         )),
         table_scalebox = list(switch(as.character(specification_name),
                                      # tweets_props = 0.65,
                                      # tweets_props_nosentiment = 0.65,
                                      # tweets_props_dlog = 0.65,
                                      tweets_props_nosentiment_dlog = 0.65,
                                      # tweets_counts = 0.65,
                                      # tweets_counts_nototal = 0.65,
                                      # tweets_counts_nosentiment = 0.65,
                                      1
         )),
         table_sideways = list(switch(as.character(specification_name),
                                      # tweets_props = FALSE,
                                      # tweets_props_nosentiment = FALSE,
                                      # tweets_props_dlog = FALSE,
                                      tweets_props_nosentiment_dlog = FALSE,
                                      # tweets_counts = FALSE,
                                      # tweets_counts_nototal = FALSE,
                                      # tweets_counts_nosentiment = FALSE,
                                      TRUE
         )),
         table_variable_names = list(switch(as.character(specification_name),
                                            events = c("(Intercept)", "Positive events", "Negative events", WEEKDAYS[-1], "DaysSince"),
                                            # sirens = c("(Intercept)", "Sirens", "Siren duration", "Share of Ukraine", "In Kyiv?", WEEKDAYS[-1], "DaysSince"),
                                            sirens_dlog = c("(Intercept)", "Sirens", "Siren duration", "Share of Ukraine", "In Kyiv?", WEEKDAYS[-1], "DaysSince"),
                                            # tweets_props = c("(Intercept)", "Sirens", "Siren duration", "Share of Ukraine", "In Kyiv?",
                                            #            "Tweets", "News articles",
                                            #            "Joy", "Anger", "Surprise", "Trust", "Fear", "Anticipation", "Sadness", "Disgust", "Positive", "Negative",
                                            #            "Civilian casualties", "Russian mil. casualties", "Conflict events", WEEKDAYS[-1], "DaysSince"),
                                            # tweets_props_nosentiment = c("(Intercept)", "Sirens", "Siren duration", "Share of Ukraine", "In Kyiv?",
                                            #            "Tweets", "News articles",
                                            #            "Joy", "Anger", "Surprise", "Trust", "Fear", "Anticipation", "Sadness", "Disgust",
                                            #            "Civilian casualties", "Russian mil. casualties", "Conflict events", WEEKDAYS[-1], "DaysSince"),
                                            # tweets_props_dlog = c("(Intercept)", "Sirens", "Siren duration", "Share of Ukraine", "In Kyiv?",
                                            #                  "Tweets", "News articles",
                                            #                  "Joy", "Anger", "Surprise", "Trust", "Fear", "Anticipation", "Sadness", "Disgust", "Positive", "Negative",
                                            #                  "Civilian casualties", "Russian mil. casualties", "Conflict events", WEEKDAYS[-1], "DaysSince"),
                                            tweets_props_nosentiment_dlog = c("(Intercept)", "Sirens", "Siren duration", "Share of Ukraine", "In Kyiv?",
                                                             "Tweets", "News articles",
                                                             "Joy", "Anger", "Surprise", "Trust", "Fear", "Anticipation", "Sadness", "Disgust",
                                                             "Civilian casualties", "Russian mil. casualties", "Conflict events", WEEKDAYS[-1], "DaysSince"),
                                            # tweets_counts = c("(Intercept)", "Sirens", "Siren duration", "Share of Ukraine", "In Kyiv?",
                                            #                       "Tweets", "News articles",
                                            #                       "Joy", "Anger", "Surprise", "Trust", "Fear", "Anticipation", "Sadness", "Disgust", "Positive", "Negative",
                                            #                       "Civilian casualties", "Russian mil. casualties", "Conflict events", WEEKDAYS[-1], "DaysSince"),
                                            # tweets_counts_nototal = c("(Intercept)", "Sirens", "Siren duration", "Share of Ukraine", "In Kyiv?",
                                            #                       "News articles",
                                            #                       "Joy", "Anger", "Surprise", "Trust", "Fear", "Anticipation", "Sadness", "Disgust", "Positive", "Negative",
                                            #                       "Civilian casualties", "Russian mil. casualties", "Conflict events", WEEKDAYS[-1], "DaysSince"),
                                            # tweets_counts_nosentiment = c("(Intercept)", "Sirens", "Siren duration", "Share of Ukraine", "In Kyiv?",
                                            #                       "News articles",
                                            #                       "Joy", "Anger", "Surprise", "Trust", "Fear", "Anticipation", "Sadness", "Disgust",
                                            #                       "Civilian casualties", "Russian mil. casualties", "Conflict events", WEEKDAYS[-1], "DaysSince")
         )),
         table_caption_var_forms = list(switch(as.character(dep_var_form), `level` = "Levels", log = "Logs", d = "First-differences", dlog = "Log-differences")),
         table_caption_specifications = list(switch(as.character(specification_name),
                                                    events = "positive and negative events",
                                                    # sirens = "air raid sirens (prop differences)",
                                                    sirens_dlog = "air raid sirens",
                                                    # tweets_props = "air raid sirens, tweet \\& media article counts, proportions of tweet emotions (differences), and war severity",
                                                    # tweets_props_nosentiment = "air raid sirens, tweet \\& media article counts, proportions of tweet emotions (differences, no sentiment), and war severity",
                                                    # tweets_props_dlog = "air raid sirens, tweet \\& media article counts, proportions of tweet emotions (log-differences), and war severity",
                                                    tweets_props_nosentiment_dlog = "air raid sirens, tweet \\& media article counts, proportions of tweet emotions, and war severity",
                                                    # tweets_counts = "air raid sirens, tweet \\& media article counts, counts of tweet emotions, and war severity",
                                                    # tweets_counts_nototal = "air raid sirens, media article counts, counts of tweet emotions, and war severity",
                                                    # tweets_counts_nosentiment = "air raid sirens, media article counts, counts of tweet emotions (no sentiment), and war severity"
         )),
         table_caption = glue("{table_caption_var_forms} of donation characteristics explained by {table_caption_specifications}."),
         table_label = glue("table:{dep_var_form}_{specification_name}"),
         # table_header_dep_vars = list(map(dep_var, \(dep_var, dep_var_form) ifelse(dep_var_form == "level", dep_var, glue('{dep_var_form}_{dep_var}')), dep_var_form)),
         table_header_dep_vars = list(map(dep_vars, \(dep_var) switch(dep_var, don_count = "Count", don_total_usd = "Total value (USD)", don_mean_usd = "Mean value (USD)"))),
         table_header = list(set_names(list(1:3, 4:6, 7:9), str_replace_all(glue("{table_header_dep_vars}"), "_", "\\\\_")))) %>%
  mutate(table = list(texreg(mod, beside = TRUE,
                             dcolumn = TRUE, booktabs = TRUE, sideways = table_sideways,
                             custom.coef.names = table_variable_names,
                             custom.header = table_header, groups = table_groups, scalebox = table_scalebox,
                             caption = table_caption, label = table_label,
                             use.packages = FALSE)))

ltx_file <- paste(mod_tables$table)
write(ltx_file, "src/latex/supplement.tex")
tools::texi2pdf("src/latex/main.tex", clean = TRUE)
file.copy("main.pdf", "data/models/OLS_results.pdf", overwrite = TRUE)

# Summary of results (sign and significance)
mods_sum <- mods %>%
  rowwise() %>%
  mutate(extracted = list(extract(mod, beside = TRUE, include.rsquared = FALSE, include.adjrs = FALSE, include.nobs = FALSE)),
         summarised = list(map(extracted, \(model) {
           list(name = model@model.name, variable = model@coef.names, coef = model@coef, pvalues = model@pvalues) %>%
             data.frame(row.names = NULL) %>%
             mutate(variable = str_remove_all(variable, "^(log|d|dlog)_"),
                    stars = case_when(pvalues < 0.001 ~ "***", pvalues < 0.01 ~ "**", pvalues < 0.05 ~ "*", pvalues < 0.1 ~ ".", TRUE ~ ""),
                    sign = case_when(coef > 0 ~ "+", coef < 0 ~ "-"), .keep = "unused") %>%
             mutate(coef = paste0("'", sign, stars), .keep = "unused")
         })),
         summarised = list(bind_rows(summarised)),
         # summarised = list(pivot_wider(summarised, names_from = name, values_from = coef))
         ) %>%
  select(dep_var, dep_var_form, specification_name, summarised) %>%
  unnest_wider(summarised) %>%
  unnest_longer(col = c(variable, name, coef)) %>%
  unite(dep_var, dep_var_form, dep_var) %>%
  # dplyr::group_by(dep_vars, name, variable, var_forms) %>%
  # dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  # dplyr::filter(n > 1L)
  pivot_wider(names_from = name, values_from = coef) %>%
  filter(variable != "(Intercept)" & variable != "daysSince" & variable != "I(daysSince^2)" & !str_detect(variable, "weekday"))
  # arrange(dep_vars, name, specification_name)

file.remove("data/models/summary.xlsx")
mods_sum %>%
  group_by(dep_var) %>%
  group_walk(\(data, key) {
    xlsx::write.xlsx(data, "data/models/summary.xlsx", sheetName = key$dep_var, append = TRUE)
  })
