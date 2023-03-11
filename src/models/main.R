library(tidyverse)
library(glue)
library(systemfit)
library(texreg)
library(lmtest)
library(sandwich)

WEEKDAYS <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
EMOTIONS <- c('joy', 'anger', 'surprise', 'trust', 'fear', 'anticip', 'sadness', 'disgust')

# Load data
data_raw <- readRDS("data/data_complete.RDS")

# Process data for regressions
data <- data_raw %>%
  # Filter out beginning of war
  filter(date >= "2022-03-16") %>%
  # Pivot out separate counts, total & mean values
  pivot_wider(names_from = type, values_from = contains("don")) %>%
  fastDummies::dummy_cols(c(glue("emot_prop_{EMOTIONS}_quint"), glue("dlog_emot_count_{EMOTIONS}_quint"))) %>%
  # Generate weekday dummies
  mutate(weekday = factor(weekdays(date), levels = WEEKDAYS),
         days_since = (as.numeric(date - 19047)))

# Get all variables
vars <- names(data_raw)

# Get all dependent variables
dep_vars <- names(data_raw)[str_detect(names(data_raw), "(?=.*don)(?=.*count|.*usd)(?!.*quint)")] %>%
  str_extract("don_.*") %>%
  unique()

# List of all independent variables for all model specifications
mods_specifications <- list(
  list(name = "events", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum")),
  list(name = "sirens", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum", "dlog_siren_count", "dlog_strike_air_count", "siren_prop")),
  list(name = "severity", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum", "dlog_siren_count", "dlog_strike_air_count", "siren_prop",
                                                                   "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count")),
  list(name = "media", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum", "dlog_siren_count", "dlog_strike_air_count", "siren_prop",
                                                                "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count",
                                                                "dlog_tweet_count", "dlog_factiva_count")),
  list(name = "media_int", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum", "dlog_siren_count", "dlog_strike_air_count", "siren_prop",
                                                                    "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count",
                                                                    "dlog_tweet_count", "dlog_factiva_count",
                                                                    "dlog_tweet_count:event_positive_dum", "dlog_tweet_count:event_negative_dum")),
  list(name = "emotions", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum", "dlog_siren_count", "dlog_strike_air_count", "siren_prop",
                                                                   "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count",
                                                                   "dlog_tweet_count", "dlog_factiva_count",
                                                                   glue("emot_prop_{EMOTIONS}")))
  # list(name = "emotions_quints", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum", "dlog_siren_count", "dlog_strike_air_count", "siren_prop", "dlog_tweet_count", "dlog_factiva_count",
  #                                                                         glue("emot_prop_{EMOTIONS}_quint_4"),
  #                                                                         glue("emot_prop_{EMOTIONS}_quint_5"),
  #                                                                         "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count")),
  # list(name = "emotions_counts", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum", "dlog_siren_count", "dlog_strike_air_count", "siren_prop", "dlog_factiva_count",
  #                                                                         glue("dlog_emot_count_{EMOTIONS}"),
  #                                                                         "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count")),
  # list(name = "emotions_counts_quints", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum", "dlog_siren_count", "dlog_strike_air_count", "siren_prop", "dlog_factiva_count",
  #                                                                                glue("dlog_emot_count_{EMOTIONS}_quint_4"),
  #                                                                                glue("dlog_emot_count_{EMOTIONS}_quint_5"),
  #                                                                                "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count")),
  # list(name = "emotions_d", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum", "dlog_siren_count", "dlog_strike_air_count", "siren_prop", "dlog_factiva_count",
  #                                                                    glue("d_emot_count_{EMOTIONS}"),
  #                                                                    "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count")),
  # list(name = "emotions_log", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum", "dlog_siren_count", "dlog_strike_air_count", "siren_prop", "dlog_factiva_count",
  #                                                                    glue("log_emot_count_{EMOTIONS}"),
  #                                                                    "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count"))
) %>%
  reduce(\(acc, x) {
    frame <- data.frame(specification_name = x$name, dep_var_form = x$dep_var_form) %>%
      mutate(indep_vars = list(x$indep_vars))
    bind_rows(acc, frame)
  }, .init = NULL) %>%
  mutate(specification_name = factor(specification_name, levels = specification_name))

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
  map(types, ~as.formula(glue("{dep_var}_{.} ~ {indep_var} + days_since"))) %>%
    set_names(types)
}

# Run models according to specifications
mods <- mods_template %>%
  rowwise() %>%
  mutate(eq = list(get_eq(dep_var, indep_vars, dep_var_form, deseasonalise = TRUE))) %>%
  mutate(mod = list(systemfit(eq, method = "SUR", data = data, maxiter = 500)),
         mod_robust = list(coeftest(mod, vcov = vcovHAC(mod))),
         mod_robust_se = list(split(mod_robust[, 2], cut(seq_along(mod_robust[, 2]), 3, labels = FALSE))),
         mod_robust_p = list(split(mod_robust[, 4], cut(seq_along(mod_robust[, 4]), 3, labels = FALSE)))
  )

emotion_coef_names <- map(EMOTIONS, \(emot) {
  data.frame(
    names = c("emot_prop_{emot}", "emot_prop_{emot}_quint_4", "emot_prop_{emot}_quint_5", "dlog_emot_count_{emot}", "dlog_emot_count_{emot}_quint_4", "dlog_emot_count_{emot}_quint_5", "d_emot_count_{emot}", "log_emot_count_{emot}"),
    pretty_names = c("{str_to_title(emot)} (prop)", "{str_to_title(emot)} (prop) Q4", "{str_to_title(emot)} (prop) Q5", "{str_to_title(emot)} (count)", "{str_to_title(emot)} (count) Q4", "{str_to_title(emot)} (count) Q5", "{str_to_title(emot)} (diff)", "{str_to_title(emot)} (log)")
  ) %>%
    rowwise() %>%
    mutate(names = glue_data(., names),
           # pretty_names = glue_data(., pretty_names)) %>%
           pretty_names = NA) %>%
    pull(pretty_names, names)
}) %>% unlist()

# Generate texreg tables from models
mod_tables <- mods %>%
  # Combine models into groups for the same specifications and functional forms (i.e., tables will be with all three dependent variables)
  group_by(specification_name, dep_var_form) %>%
  summarise(dep_vars = list(dep_var), mod = list(mod), mod_robust_se = list(mod_robust_se), mod_robust_p = list(mod_robust_p)) %>%
  rowwise() %>%
  # Define table parameters for the three specifications, and transform them into LaTeX
  mutate(table_groups = list(switch(as.character(specification_name),
                                    events = list("Event types" = 2:3),
                                    sirens = list("Event types" = 2:3, "Air raid sirens" = 4:6),
                                    severity = list("Event types" = 2:3, "Air raid sirens" = 4:6, "War severity" = 7:9),
                                    media = list("Event types" = 2:3, "Air raid sirens" = 4:6, "War severity" = 7:9, "Media" = 10:11),
                                    media_int = list("Event types" = 2:3, "Air raid sirens" = 4:6, "War severity" = 7:9, "Media" = 10:13),
                                    emotions = list("Event types" = 2:3, "Air raid sirens" = 4:6, "War severity" = 7:9, "Media" = 10:11, "Emotion type" = 12:19),
                                    NULL
  )),
         table_scalebox = list(switch(as.character(specification_name),
                                      emotions = 0.65,
                                      0.7
         )),
         table_sideways = list(switch(as.character(specification_name),
                                      emotions = FALSE,
                                      FALSE
         )),
         table_variable_map = list(c("(Intercept)" = "Intercept", "event_positive_dum" = NA, "event_negative_dum" = NA,
                                     "dlog_siren_count" = NA, "siren_count" = NA, "dlog_siren_mean_duration" = NA, "siren_mean_duration" = NA, "dlog_strike_air_count" = NA, "dlog_siren_prop" = NA, "siren_prop" = NA, "siren_kyiv_dum" = NA,
                                     "dlog_sev_cas_civ_count" = NA, "dlog_sev_cas_rus_mil_count" = NA, "dlog_sev_confl_evs_count" = NA,
                                     "dlog_tweet_count" = NA, "dlog_factiva_count" = NA,
                                     "event_positive_dum:dlog_tweet_count" = NA, "event_negative_dum:dlog_tweet_count" = NA,
                                     emotion_coef_names
                                     # "weekdayMonday" = NA, "weekdayTuesday" = NA, "weekdayWednesday" = NA, "weekdayThursday" = NA, "weekdayFriday" = NA, "weekdaySaturday" = NA, "weekdaySunday" = NA,
                                     # "days_since" = NA
         )),
         # table_variable_map = list(c("(Intercept)" = "Intercept", "event_positive_dum" = "Positive event", "event_negative_dum" = "Negative event",
         #                             "dlog_siren_count" = "Sirens", "siren_count" = "Sirens", "dlog_siren_mean_duration" = "Mean siren duration", "siren_mean_duration" = "Mean siren duration",  "dlog_strike_air_count" = "Air strikes", "dlog_siren_prop" = "Share of Ukraine", "siren_prop" = "Share of Ukraine","siren_kyiv_dum" = "Sirens in Kyiv",
         #                             "dlog_tweet_count" = "Tweets", "dlog_factiva_count" = "News articles",
         #                             emotion_coef_names,
         #                             "dlog_sev_cas_civ_count" = "Civilian casualties", "dlog_sev_cas_rus_mil_count" = "Russian mil. casualties", "dlog_sev_confl_evs_count" = "Conflict events"
         #                             # "weekdayMonday" = "Monday", "weekdayTuesday" = "Tuesday", "weekdayWednesday" = "Wednesday", "weekdayThursday" = "Thursday", "weekdayFriday" = "Friday", "weekdaySaturday" = "Saturday", "weekdaySunday" = "Sunday",
         #                             # "days_since" = "Days since"
         # )),
         table_caption = list(switch(as.character(specification_name),
                                     events = "Donation characteristics and high emotional intensity days.",
                                     sirens = "Donation characteristics and proxies for emotional intensity.",
                                     severity = "Donation characteristics and proxies for emotional intensity.",
                                     media = "Donation characteristics and proxies for emotional intensity and exposure.",
                                     media_int = "Donation characteristics and proxies for emotional intensity and exposure.",
                                     emotions = "Donation characteristics and proxies for emotional intensity, exposure and type of emotion."
         )),
         table_label = glue("table:{dep_var_form}_{specification_name}"),
         # table_header_dep_vars = list(map(dep_vars, \(dep_var) switch(dep_var, don_count = "Count", don_total_usd = "Total value (USD)", don_mean_usd = "Mean value (USD)"))),
         table_header_dep_vars = list(map(dep_vars, \(dep_var) switch(dep_var, don_count = "don_count", don_total_usd = "don_total_usd", don_mean_usd = "don_mean_usd"))),
         table_header = list(set_names(list(1:3, 4:6, 7:9), str_replace_all(glue("{table_header_dep_vars}"), "_", "\\\\_")))) %>%
  mutate(table = list(texreg(mod, override.se = unlist(mod_robust_se, recursive = FALSE), override.pvalues = unlist(mod_robust_p, recursive = FALSE), beside = TRUE,
                             include.nobs = FALSE,
                             dcolumn = TRUE, booktabs = TRUE, sideways = table_sideways, threeparttable = TRUE,
                             custom.coef.map = as.list(table_variable_map),
                             stars = c(0.01, 0.05, 0.10),
                             custom.header = table_header,
                             groups = table_groups,
                             scalebox = table_scalebox,
                             caption.above = TRUE,
                             caption = table_caption, label = table_label,
                             use.packages = FALSE)))

ltx_file <- paste(mod_tables$table)
write(ltx_file, "results/latex_main/supplement.tex")
file.copy("src/latex/main_ols.tex", "results/latex_main/main_ols.tex", overwrite = TRUE)
tools::texi2pdf("results/latex_main/main_ols.tex", clean = TRUE)
file.copy("main_ols.pdf", "results/OLS_results.pdf", overwrite = TRUE)

### Coefficient comparisons (F-tests)
# Whether events are stronger for foreign
test_events <- tibble(
  expand.grid(var = c("don_count", "don_total_usd", "don_mean_usd"), coef = c("event_positive_dum", "event_negative_dum")), type = list(c("Ukrainian", "Foreign")),
  hypothesis = "equal",
) %>%
  rowwise() %>%
  mutate(mod = filter(mods, specification_name == "events", dep_var == var)$mod,
         vars = list(map(type, ~paste0(.x, "_", coef))),
         hypothesis = paste(vars[[1]], "=", vars[[2]])) %>%
  mutate(result = list(linearHypothesis(mod, hypothesis, vcov = vcovHAC))) %>%
  mutate(results_presented = apastats::describe.anova(result))

test_events_table <- test_events %>%
  mutate(p = result[2, 4]) %>%
  group_by(var) %>%
  summarise(across(everything(), list)) %>%
  rowwise() %>%
  mutate(table_base = list(createTexreg(coef.names = hypothesis, coef = p, pvalues = p, model.name = as.character(var)))) %>%
  with({
    texreg(table_base,
              booktabs = TRUE, dcolumn = TRUE, threeparttable = TRUE, use.packages = FALSE, digits = 3,
              stars = c(0.01, 0.05, 0.10),
              caption.above = TRUE,
              label = "table:events-tests"
    )
  })

write(test_events_table, "results/latex_main/event-tests.tex")

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
  pivot_wider(names_from = name, values_from = coef) %>%
  filter(variable != "(Intercept)" &
           variable != "days_since" &
           variable != "I(days_since^2)" &
           !str_detect(variable, "weekday"))

file.remove("data/models/summary.xlsx")
mods_sum %>%
  group_by(dep_var) %>%
  group_walk(\(data, key) {
    xlsx::write.xlsx(data, "results/summary.xlsx", sheetName = key$dep_var, append = TRUE)
  })
