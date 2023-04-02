library(tidyverse)
library(glue)
library(systemfit)
library(texreg)
library(lmtest)
library(sandwich)

EMOTIONS <- c('joy', 'anger', 'surprise', 'trust', 'fear', 'anticip', 'sadness', 'disgust')
EMOTIONS_ml <- c('joy', 'anger', 'surprise', 'fear', 'sadness', 'disgust')

emotion_coef_names <- map(EMOTIONS, \(emot) {
  data.frame(
    names = c("dlog_emot_count_{emot}_dict", "dlog_emot_count_{emot}_ml", "emot_prop_{emot}_dict", "emot_prop_{emot}_ml", "d_emot_prop_{emot}_ml", "emot_prop_{emot}_ml_quint_5"),
    pretty_names = c("dlog_emot_count_{emot}_dict", "dlog_emot_count_{emot}_ml", "emot_prop_{emot}_dict", "emot_prop_{emot}_ml", "d_emot_prop_{emot}_ml", "emot_prop_{emot}_quint_5")
  ) %>%
    rowwise() %>%
    mutate(names = glue_data(., names),
           # pretty_names = glue_data(., pretty_names)) %>%
           pretty_names = NA) %>%
    pull(pretty_names, names)
}) %>% unlist()

# Load data
data_raw <- readRDS("data/data_complete.RDS")

# Process data for regressions
data <- data_raw %>%
  # Pivot out separate counts, total & mean values
  pivot_wider(names_from = type, values_from = contains("don"))
  # fastDummies::dummy_cols(c(glue("emot_prop_{EMOTIONS_ml}_ml_quint"), glue("dlog_emot_count_{EMOTIONS_ml}_ml_quint")))

# Get all variables
vars <- names(data_raw)

# Get all dependent variables
dep_vars <- names(data_raw)[str_detect(names(data_raw), "(?=.*don)(?=.*count|.*total_usd)(?!.*quint)")] %>%
  str_extract("don_.*") %>%
  unique()

# List of all independent variables for all model specifications
mods_specifications <- list(
  list(name = "events", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum")),
  # list(name = "sirens", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum", "dlog_siren_count", "dlog_strike_air_count", "siren_prop")),
  # list(name = "sirens_noevents", dep_var_form = c("dlog"), indep_vars = c("dlog_siren_count", "dlog_strike_air_count", "siren_prop")),
  # list(name = "severity", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum", "dlog_siren_count", "dlog_strike_air_count", "siren_prop",
  #                                                                  "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count")),
  list(name = "severity_nosiren", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum",
                                                                   "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count")),
  list(name = "media", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum",
                                                                    # "dlog_siren_count", "dlog_strike_air_count", "siren_prop",
                                                                "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count",
                                                                "dlog_tweet_count", "dlog_news_euro_count_des")),
  # list(name = "media_int", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum",
  #                                                                           # "dlog_siren_count", "dlog_strike_air_count", "siren_prop",
  #                                                                   "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count",
  #                                                                   "dlog_tweet_count", "dlog_news_euro_count_des",
  #                                                                   "dlog_tweet_count:event_positive_dum", "dlog_tweet_count:event_negative_dum")),
  list(name = "emotions_ml_count", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum",
                                                                                            # "dlog_siren_count", "dlog_strike_air_count", "siren_prop",
                                                                   "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count",
  #                                                                  "dlog_tweet_count",
                                                                   "dlog_news_euro_count_des",
                                                                   glue("dlog_emot_count_{EMOTIONS_ml}_ml"))),
  # list(name = "emotions_ml_count_no_fear", dep_var_form = c("dlog"), indep_vars = c("event_positive_dum", "event_negative_dum", "dlog_siren_count", "dlog_strike_air_count", "siren_prop",
  #                                                                  "dlog_sev_cas_civ_count", "dlog_sev_cas_rus_mil_count", "dlog_sev_confl_evs_count",
  # #                                                                  "dlog_tweet_count",
  #                                                                  "dlog_news_euro_count_des",
  #                                                                  glue("dlog_emot_count_{EMOTIONS_ml[-4]}_ml")))
  NULL
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
                                    severity_nosiren = list("Event types" = 2:3, "War severity" = 4:6),
                                    media = list("Event types" = 2:3, "War severity" = 4:6, "Media" = 7:8),
                                    media_int = list("Event types" = 2:3, "War severity" = 4:6, "Media" = 7:10),
                                    emotions_ml_count = list("Event types" = 2:3, "War severity" = 4:6, "Media" = 7, "Emotion type" = 8:13),
                                    NULL
  )),
         table_scalebox = list(switch(as.character(specification_name),
                                      0.7
         )),
         table_sideways = list(switch(as.character(specification_name),
                                      FALSE
         )),
         # table_variable_map = list(c("(Intercept)" = "Intercept", "event_positive_dum" = NA, "event_negative_dum" = NA, "event_aid_dum" = NA, "event_aid_dum:log_aid_amount" = NA,
         #                             "dlog_siren_count" = NA, "siren_count" = NA, "dlog_siren_mean_duration" = NA, "siren_mean_duration" = NA, "dlog_strike_air_count" = NA, "dlog_strike_air_arty_count" = NA, "dlog_siren_prop" = NA, "siren_prop" = NA, "siren_kyiv_dum" = NA,
         #                             "dlog_sev_cas_civ_count" = NA, "dlog_sev_cas_rus_mil_count" = NA, "dlog_sev_confl_evs_count" = NA,
         #                             "dlog_tweet_count" = NA, "dlog_factiva_count" = NA, "dlog_news_euro_count_des" = NA,
         #                             "event_positive_dum:dlog_tweet_count" = NA, "event_negative_dum:dlog_tweet_count" = NA,
         #                             emotion_coef_names
         #                             # "weekdayMonday" = NA, "weekdayTuesday" = NA, "weekdayWednesday" = NA, "weekdayThursday" = NA, "weekdayFriday" = NA, "weekdaySaturday" = NA, "weekdaySunday" = NA,
         #                             # "days_since" = NA
         # )),
         table_variable_map = list(c("(Intercept)" = "Intercept",
                                     event_positive_dum = "$EventPositive$",
                                     event_negative_dum = "$EventNegative$",
                                     dlog_don_count = "$DonCount$",
                                     dlog_don_total_usd = "$DonTotalUSD$",
                                     dlog_don_mean_usd = "$DonMeanUSD$",
                                     dlog_siren_count = "$SirenCount$",
                                     siren_prop = "$TerritoryAffected$",
                                     dlog_strike_air_count = "$StrikeCount$",
                                     dlog_sev_cas_civ_count = "$CivCasualtiesCount$",
                                     dlog_sev_cas_rus_mil_count = "$RusMilCasualtiesCount$",
                                     dlog_sev_confl_evs_count = "$ConflEvsCount$",
                                     dlog_tweet_count = "$TweetCount$",
                                     dlog_news_euro_count_des = "$NewsCount$",
                                     dlog_emot_count_joy_ml = "$TweetJoyCount$",
                                     dlog_emot_count_anger_ml = "$TweetAngerCount$",
                                     dlog_emot_count_disgust_ml = "$TweetDisgustCount$",
                                     dlog_emot_count_surprise_ml = "$TweetSurpriseCount$",
                                     dlog_emot_count_sadness_ml = "$TweetSadnessCount$",
                                     dlog_emot_count_fear_ml = "$TweetFearCount$",
                                     dlog_emot_count_neutral_ml = "$TweetNeutralCount$",
                                     "event_positive_dum:dlog_tweet_count" = NA, "event_negative_dum:dlog_tweet_count" = NA
         )),
         table_caption = list(switch(as.character(specification_name),
                                     events = "Donation characteristics and high emotional intensity days.",
                                     sirens = "Donation characteristics and proxies for emotional intensity.",
                                     severity = "Donation characteristics and proxies for emotional intensity.",
                                     severity_nosiren = "Donation characteristics and proxies for emotional intensity.",
                                     media = "Donation characteristics and proxies for emotional intensity and exposure.",
                                     media_int = "Donation characteristics and proxies for emotional intensity and exposure.",
                                     emotions_ml_count = "Donation characteristics and proxies for emotional intensity, exposure and type of emotion (count of tweets).",
                                     NULL
         )),
         table_label = glue("table:{dep_var_form}_{specification_name}"),
         table_header_dep_vars = list(c(don_count = "$DonCount$", don_total_usd = "$DonTotalUSD$", don_mean_usd = "$DonMeanUSD$")[dep_vars]),
         table_header = list(set_names(list(1:3, 4:6), str_replace_all(glue("{table_header_dep_vars}"), "_", "\\\\_")))) %>%
  mutate(table = list(texreg(mod, override.se = unlist(mod_robust_se, recursive = FALSE), override.pvalues = unlist(mod_robust_p, recursive = FALSE), beside = TRUE,
                             include.nobs = FALSE,
                             dcolumn = TRUE,
                             # siunitx = TRUE,
                             booktabs = TRUE, sideways = table_sideways,
                             threeparttable = TRUE,
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

#### Extras
### Coefficient comparisons (F-tests)
# Whether events are different for foreign, are positives different from negatives
test_events_hypos <- list(
  "Ukrainian_event_positive_dum = Foreign_event_positive_dum",
  "Ukrainian_event_negative_dum = Foreign_event_negative_dum",
  "Ukrainian_event_positive_dum = Ukrainian_event_negative_dum",
  "Foreign_event_positive_dum = Foreign_event_negative_dum"
)

test_events <- tibble(
  expand.grid(var = c("don_count", "don_total_usd"), hypothesis = test_events_hypos)
) %>%
  mutate(hypothesis = as.character(hypothesis)) %>%
  rowwise() %>%
  mutate(mod = filter(mods, specification_name == "events", dep_var == var)$mod) %>%
  mutate(result = list(linearHypothesis(mod, hypothesis, vcov = vcovHAC))) %>%
  mutate(results_presented = apastats::describe.anova(result))

test_events_table <- test_events %>%
  mutate(coef = result[2, 3], p = result[2, 4]) %>%
  group_by(var) %>%
  summarise(across(everything(), list)) %>%
  rowwise() %>%
  mutate(table_base = list(createTexreg(coef.names = hypothesis, coef = coef, pvalues = p, model.name = switch(as.character(var), don_count = "DonCount", don_total_usd = "DonTotalUSD")))) %>%
  with({
    texreg(table_base,
              booktabs = TRUE, dcolumn = TRUE, threeparttable = TRUE, use.packages = FALSE, digits = 3,
              stars = c(0.01, 0.05, 0.10),
              custom.coef.map = list("Ukrainian_event_positive_dum = Foreign_event_positive_dum" = "UkrainianEventPositive = ForeignEventPositive",
                                     "Ukrainian_event_negative_dum = Foreign_event_negative_dum" = "UkrainianEventNegative = ForeignEventNegative",
                                     "Ukrainian_event_positive_dum = Ukrainian_event_negative_dum" = "UkrainianEventPositive = UkrainianEventNegative",
                                     "Foreign_event_positive_dum = Foreign_event_negative_dum" = "ForeignEventPositive = ForeignEventNegative"),
              caption.above = TRUE,
              label = "table:events-tests",
              custom.note = "\n \\linespread{1}\\selectfont \n \\item %stars \\\\\n \\item Degrees of freedom between groups = 1 \n \\item Degrees of freedom within groups = 1020"
    )
  })

write(test_events_table, "results/latex_extras/event-tests.tex")

# Whether media effects are different
test_media_hypos <- list(
  "Ukrainian_dlog_tweet_count = Foreign_dlog_tweet_count",
  "Ukrainian_dlog_tweet_count = Crypto_dlog_tweet_count",
  "Foreign_dlog_tweet_count = Crypto_dlog_tweet_count"
)

test_media <- tibble(
  expand.grid(var = c("don_count", "don_total_usd"), hypothesis = test_media_hypos)
) %>%
  mutate(hypothesis = as.character(hypothesis)) %>%
  rowwise() %>%
  mutate(mod = filter(mods, specification_name == "media", dep_var == var)$mod) %>%
  mutate(result = list(linearHypothesis(mod, hypothesis, vcov = vcovHAC))) %>%
  mutate(results_presented = apastats::describe.anova(result))

test_media_table <- test_media %>%
  mutate(coef = result[2, 3], p = result[2, 4]) %>%
  group_by(var) %>%
  summarise(across(everything(), list)) %>%
  rowwise() %>%
  mutate(table_base = list(createTexreg(coef.names = hypothesis, coef = coef, pvalues = p, model.name = switch(as.character(var), don_count = "DonCount", don_total_usd = "DonTotalUSD")))) %>%
  with({
    texreg(table_base,
           booktabs = TRUE, dcolumn = TRUE, threeparttable = TRUE, use.packages = FALSE, digits = 3,
           stars = c(0.01, 0.05, 0.10),
           custom.coef.map = list("Ukrainian_dlog_tweet_count = Foreign_dlog_tweet_count" = "UkrainianTweetCount = ForeignTweetCount",
                                  "Ukrainian_dlog_tweet_count = Crypto_dlog_tweet_count" = "UkrainianTweetCount = CryptoTweetCount",
                                  "Foreign_dlog_tweet_count = Crypto_dlog_tweet_count" = "ForeignTweetCount = CryptoTweetCount"),
           caption.above = TRUE,
           label = "table:media-tests",
           custom.note = "\n \\linespread{1}\\selectfont \n \\item %stars \\\\\n \\item Degrees of freedom between groups = 1 \n \\item Degrees of freedom within groups = 1005"
    )
  })

write(test_media_table, "results/latex_extras/media-tests.tex")

### Split Strikes & Sirens
extra_split_strikes_cutoff <- "31/10/2022"

extra_split_strikes_eqs <- map(c("dlog_don_count", "dlog_don_total_usd", "dlog_don_mean_usd"), \(dep_var) {
  interaction(dep_var, c("Ukrainian", "Foreign", "Crypto"), sep = "_") %>%
    interaction(., "event_positive_dum + event_negative_dum + dlog_siren_count + dlog_strike_air_count + siren_prop + weekday + days_since", sep = " ~ ") %>%
    map(as.character) %>%
    map(as.formula) %>%
    set_names(c("Ukrainian", "Foreign", "Crypto"))
})

extra_split_strikes_mods_specifications <- expand_grid(
  eq = extra_split_strikes_eqs,
  tibble(specification_name = c("splits_strikes_pre", "splits_strikes_post"), data = list(filter(data, date <= dmy(extra_split_strikes_cutoff)), filter(data, date > dmy(extra_split_strikes_cutoff))))
)

extra_split_strikes_mods <- extra_split_strikes_mods_specifications %>%
  rowwise() %>%
  mutate(mod = list(systemfit(eq, method = "SUR", data = data, maxiter = 500)),
         mod_robust = list(coeftest(mod, vcov = vcovHAC(mod))),
         mod_robust_se = list(split(mod_robust[, 2], cut(seq_along(mod_robust[, 2]), 3, labels = FALSE))),
         mod_robust_p = list(split(mod_robust[, 4], cut(seq_along(mod_robust[, 4]), 3, labels = FALSE)))
  )

extra_split_strikes_mod_tables <- extra_split_strikes_mods %>%
  group_by(specification_name) %>%
  # Define table parameters for the three specifications, and transform them into LaTeX
  summarise(
    across(c(mod, mod_robust_se, mod_robust_p), list),
    table_variable_map = list(c("(Intercept)" = "Intercept", "event_positive_dum" = NA, "event_negative_dum" = NA,
                                     "dlog_siren_count" = NA, "siren_count" = NA, "dlog_siren_mean_duration" = NA, "siren_mean_duration" = NA, "dlog_strike_air_count" = NA, "dlog_strike_air_arty_count" = NA, "dlog_siren_prop" = NA, "siren_prop" = NA, "siren_kyiv_dum" = NA
                                     # "weekdayMonday" = NA, "weekdayTuesday" = NA, "weekdayWednesday" = NA, "weekdayThursday" = NA, "weekdayFriday" = NA, "weekdaySaturday" = NA, "weekdaySunday" = NA,
                                     # "days_since" = NA
            )),
    table_caption = list(switch(as.character(specification_name[[1]]),
                                splits_strikes_pre = glue("Donation characteristics and proxies for emotional intensity (data before {extra_split_strikes_cutoff})."),
                                splits_strikes_post = glue("Donation characteristics and proxies for emotional intensity (data after {extra_split_strikes_cutoff})."),
                                NULL
    )),
    table_label = glue("table:dlog_{specification_name[[1]]}"),
    table_header_dep_vars = list(map(dep_vars, \(dep_var) switch(dep_var, don_count = "don_count", don_total_usd = "don_total_usd", don_mean_usd = "don_mean_usd"))),
    table_header = list(set_names(list(1:3, 4:6, 7:9), str_replace_all(glue("{unlist(table_header_dep_vars)}"), "_", "\\\\_"))),
    .groups = "keep",
    table = list(texreg(unlist(mod, recursive = FALSE), override.se = unlist(mod_robust_se[[1]], recursive = FALSE), override.pvalues = unlist(mod_robust_p[[1]], recursive = FALSE), beside = TRUE,
                             include.nobs = FALSE,
                             dcolumn = TRUE, booktabs = TRUE, sideways = FALSE, threeparttable = TRUE,
                             custom.coef.map = as.list(unlist(table_variable_map, recursive = FALSE)),
                             stars = c(0.01, 0.05, 0.10),
                             custom.header = unlist(table_header, recursive = FALSE),
                             groups = list("Event types" = 2:3, "Air raid sirens" = 4:6),
                             scalebox = 0.7,
                             caption.above = TRUE,
                             caption = table_caption, label = table_label,
                             use.packages = FALSE)))

extra_split_strikes_ltx_file <- paste(extra_split_strikes_mod_tables$table)
write(extra_split_strikes_ltx_file, "results/latex_extras/split-strikes.tex")

### US/EU donations vs US/EU aid
WEEKDAYS <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Load data
extra_aid_data_raw <- readRDS("data/data_donations_sub.RDS")

# Aid
extra_aid_data_events_raw <- read_xlsx("data/events_aid.xlsx")
extra_aid_data_events <- extra_aid_data_events_raw %>%
  filter((events_mil == 1 & currency == "USD") | (events_EU_mix == 1)) %>%
  mutate(datetime = ymd_hms(datetime)) %>%
  mutate(date = date(datetime),
         event_aid_usd_dum = if_else((events_mil == 1 & currency == "USD"), 1, 0),
         event_aid_eur_dum = if_else(events_EU_mix == 1, 1, 0),
         log_aid_amount = log(amount)
  ) %>%
  group_by(date) %>%
  arrange(desc(amount)) %>%
  summarize(across(everything(), first)) %>%
  select(date, event_aid_usd_dum, event_aid_eur_dum, log_aid_amount)

# Process data for regressions
extra_aid_data <- extra_aid_data_raw %>%
  select(-type, -don_total, -don_mean) %>%
  filter(type_sub %in% c("USD", "EUR")) %>%
  left_join(extra_aid_data_events, by = "date") %>%
  mutate(across(c(-date, -type_sub), ~ replace_na(., 0))) %>%
  mutate(date = ymd(date)) %>%
  group_by(type_sub) %>%
  mutate(across(c(-date, -contains("dum")), list(dlog = ~log(.) - log(dplyr::lag(.)), dlog1 = ~log(. + 1) - log(dplyr::lag(. + 1))), .names = "{.fn}_{.col}")) %>%
  # Generate weekday dummies
  mutate(weekday = factor(weekdays(date), levels = WEEKDAYS),
         days_since = (as.numeric(date - 19047))) %>%
  filter(date >= "2022-05-24") %>%
  # Pivot out separate counts, total & mean values
  pivot_wider(names_from = type_sub, values_from = contains("don"))

extra_aid_eqs <- map(c("dlog_don_count", "dlog_don_total_usd", "dlog_don_mean_usd"), \(dep_var) {
  interaction(dep_var, c("USD", "EUR"), sep = "_") %>%
    interaction(c("event_aid_usd_dum + weekday + days_since", "event_aid_eur_dum + weekday + days_since"), sep = " ~ ") %>%
    map(as.character) %>%
    map(as.formula) %>%
    set_names(c("USD", "EUR"))
})

extra_aid_specifications <- tibble(specification_name = "donations_aid", eq = extra_aid_eqs)

extra_aid_mods <- extra_aid_specifications %>%
  rowwise() %>%
  mutate(mod = list(systemfit(eq, method = "SUR", data = extra_aid_data, maxiter = 500)),
         mod_robust = list(coeftest(mod, vcov = vcovHAC(mod))),
         mod_robust_se = list(split(mod_robust[, 2], cut(seq_along(mod_robust[, 2]), 2, labels = FALSE))),
         mod_robust_p = list(split(mod_robust[, 4], cut(seq_along(mod_robust[, 4]), 2, labels = FALSE)))
  )

extra_aid_mods_table <- extra_aid_mods %>%
  with({
    texreg(mod, override.se = unlist(mod_robust_se, recursive = FALSE), override.pvalues = unlist(mod_robust_p, recursive = FALSE), beside = TRUE,
           include.nobs = FALSE,
           dcolumn = TRUE, booktabs = TRUE, sideways = FALSE, threeparttable = TRUE,
           custom.coef.map = list("(Intercept)" = "Intercept", "event_aid_usd_dum" = "event_aid_dum", "event_aid_eur_dum" = "event_aid_dum",
                                     "event_aid_usd_dum:log_aid_amount" = "event_aid_dum:log_aid_amount", "event_aid_eur_dum:log_aid_amount" = "event_aid_dum:log_aid_amount"
                                     # "weekdayMonday" = NA, "weekdayTuesday" = NA, "weekdayWednesday" = NA, "weekdayThursday" = NA, "weekdayFriday" = NA, "weekdaySaturday" = NA, "weekdaySunday" = NA,
                                     # "days_since" = NA
           ),
           stars = c(0.01, 0.05, 0.10),
           scalebox = 0.7,
           custom.header = list("don\\_count" = 1:2, "don\\_total\\_usd" = 3:4, "don\\_mean\\_usd" = 5:6),
           caption.above = TRUE,
           caption = "USD and EUR donation characteristics and US and EU aid announcements (starting from 24/05/2022).", label = "table:dlog_aid",
           use.packages = FALSE
    )
  })

write(extra_aid_mods_table, "results/latex_extras/aid.tex")

### VIF
extra_vif_variable_map <-  c("(Intercept)" = "Intercept",
                             "event_positive_dum" = "$EventPositive$", "event_negative_dum" = "$EventNegative$",
                             dlog_don_count = "$DonCount$",
                             dlog_don_total_usd = "$DonTotalUSD$",
                             dlog_don_mean_usd = "$DonMeanUSD$",
                             dlog_siren_count = "$SirenCount$",
                             siren_prop = "$TerritoryAffected$",
                             dlog_strike_air_count = "$StrikeCount$",
                             dlog_sev_cas_civ_count = "$CivCasualtiesCount$",
                             dlog_sev_cas_rus_mil_count = "$RusMilCasualtiesCount$",
                             dlog_sev_confl_evs_count = "$ConflEvsCount$",
                             dlog_tweet_count = "$TweetCount$",
                             dlog_news_euro_count_des = "$NewsCount$",
                             dlog_emot_count_joy_ml = "$TweetJoyCount$",
                             dlog_emot_count_anger_ml = "$TweetAngerCount$",
                             dlog_emot_count_disgust_ml = "$TweetDisgustCount$",
                             dlog_emot_count_surprise_ml = "$TweetSurpriseCount$",
                             dlog_emot_count_sadness_ml = "$TweetSadnessCount$",
                             dlog_emot_count_fear_ml = "$TweetFearCount$",
                             dlog_emot_count_neutral_ml = "$TweetNeutralCount$",
                             weekday = "$Weekdays$",
                             days_since = "$DaysSince$"
)

extra_vif <- vif(mods$mod[[4]]$eq[[1]])
rownames(extra_vif) <- extra_vif_variable_map[rownames(extra_vif)]

extra_vif_table_vif <- createTexreg(rownames(extra_vif), extra_vif[, 1], model.name = "VIF")
extra_vif_table_df <- createTexreg(rownames(extra_vif), extra_vif[, 2], model.name = "Df")

extra_vif_table_ltx <- texreg(list(extra_vif_table_vif, extra_vif_table_df),
                              dcolumn = TRUE, booktabs = TRUE, sideways = FALSE, threeparttable = TRUE,
                              caption.above = TRUE,
                              caption = "Variance inflation factor values for SUR regression variables.", label = "table:vif",
                              custom.note = "\\item Note: VIF for $Weekdays$ is a combined VIF for the 6 weekday dummies.",
                              use.packages = FALSE)

write(extra_vif_table_ltx, "results/latex_extras/vif.tex")
