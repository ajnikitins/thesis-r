library(tidyverse)
library(readxl)
library(lubridate)
library(plm)
library(lmtest)
library(sandwich)
library(texreg)
library(glue)
library(fixest)
library(ggnewscale)
library(ggsci)

# Utility for generating times for events
generate_hours <- \(day, start_h, pre_offset_h, post_offset_h, ignore_night = FALSE) {
  # Generates a sequence of hours, given a start, finish and whether nighttime (00-06) should be ignored
  generate_time_sequence <- \(from, to, ignore_night = FALSE, ignore_direction = "forward") {
    # Generate base sequence
    sequence <- seq(from, to, by = "hours")

    if (ignore_night) {
      # Need to reverse sequence if we're going "back" in time (for per-periods)
      if (ignore_direction == "backward") {
        sequence <- rev(sequence)
      }

      # offset is a vector of hours each date in `sequence` needs to be adjusted by to get no hours between (00:06) and still be a "continuous" sequence
      offset <- accumulate(sequence, \(acc, x) {
        if (dplyr::between(hour(x + 3600 * acc), 0, 5)) {
          # 1 and 6 is the amount of hours that midnight needs to be adjusted by, the `hour(.)` term increases/decreases the offset for post-midnight hours
          if (ignore_direction == "backward") {
            acc - 1 - hour(x + 3600 * acc)
          } else if (ignore_direction == "forward") {
            acc + 6 - hour(x + 3600 * acc)
          }
        } else acc
      }, .init = 0)

      # Calculate the new sequence
      res <- sequence + offset[-1] * 3600

      # Reverse the sequence back to make it ascending
      if (ignore_direction == "backward") {
        rev(res)
      } else {
        res
      }

    } else {
      sequence
    }

  }

  start_date <- day

  # Adjust the starting hour in case it's during night (set it to the next morning)
  if (dplyr::between(start_h, 0, 5)) {
    start_h <- 6
  }

  hour(start_date) <- start_h

  pre_start_date <- day
  hour(pre_start_date) <- start_h - pre_offset_h
  pre <- data.frame(date = generate_time_sequence(pre_start_date, start_date - 3600, ignore_night, "backward"), rel_time = (-pre_offset_h):(-1), is_pre = 1)
  # pre <- data.frame(date = seq(pre_start_date, start_date - 3600, by = "hours"), rel_time = (-pre_offset_h):(-1), is_pre = 1)

  post_date <- day
  hour(post_date) <- start_h + post_offset_h
  # post <- data.frame(date = seq(start_date, post_date - 3600, by = "hours"), rel_time = 0:(post_offset_h - 1), is_post = 1)
  post <- data.frame(date = generate_time_sequence(start_date, post_date - 3600, ignore_night, "forward"), rel_time = 0:(post_offset_h - 1), is_post = 1)

  dates <- bind_rows(pre, post) %>%
    mutate(across(c(is_pre, is_post), ~ replace_na(., 0)),
           is_pre = if_else(rel_time == -1, 0, is_pre),
           group = glue("{year(day)}-{month(day)}-{day(day)}--{start_h}"))
}

generate_models <- \(data, specifications, estimate_fixef = FALSE, fixef = "type_sub") {
  mods <- specifications %>%
    rowwise() %>%
    mutate(dep_var = if (dep_var_form == "level") as.factor(dep_var) else paste0(dep_var_form, "_", dep_var),
           indep_vars = ifelse(include_lag, list(c(glue("lag({dep_var})"), indep_vars)), list(indep_vars)))

  mods <- if (estimate_fixef) {
    mods %>%
      mutate(eq = list(formula(paste(dep_var, "~", paste(indep_vars, collapse = "+"), glue("| {fixef} + rel_time")))),
             mod = list(feols(eq, data = data, panel.id = ~type_sub + rel_time, vcov = "DK")))
  } else {
    mods %>%
      mutate(eq = list(formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))),
             mod = list(lm(eq, data = data)))
  }

  pull(mods, mod)
}

generate_data <- \(events, own_start_h, data, treatment_type, pre_offset_h, post_offset_h, ignore_night, omit_crypto, distinct_dates) {
  dates <- events %>%
    rowwise() %>%
    mutate(start_h = if (!is.na(own_start_h)) own_start_h else start_h) %>%
    transmute(date = list(generate_hours(date, start_h, pre_offset_h, post_offset_h, ignore_night = ignore_night))) %>%
    unnest(date) %>%
    filter(date >= dmy("16-03-2022") & date <= dmy("28-02-2023"))

  if (distinct_dates) {
    dates <- distinct(dates, date, .keep_all = TRUE)
  }

  data_did <- dates %>%
    left_join(data, by = "date", multiple = "all") %>%
    filter(!omit_crypto | type != "Crypto") %>%
    mutate(is_treatment = if_else(type == treatment_type, 1, 0)) %>%
    mutate(is_treated = if_else(is_treatment == 1 & is_post == 1, 1, 0)) %>%
    mutate(is_daylight = if_else(dplyr::between(hour(date), 6, 22), 1, 0)) %>%
    mutate(time = hour(date), .after = date)
}

data_intraday <- readRDS("data/data_intraday.RDS")
data_intraday_sub <- readRDS("data/data_intraday_sub.RDS")

## Event times are in GMT+2
data_events_raw <- read_excel("data/important_events_hourly.xlsx")
data_events <- data_events_raw %>%
  mutate(datetime = with_tz(ymd_hms(datetime, tz = "Europe/Kiev"), tzone = "UTC")) %>%
  filter(is_aid == 0) %>%
  transmute(date = date(datetime),
            start_h = hour(datetime),
            event_dum = if_else(!is.na(coloring), 1, 0, missing = 0))

# Aid
data_events_aid_raw <- read_xlsx("data/events_aid.xlsx")
data_events_aid <- data_events_aid_raw %>%
  filter((events_mil == 1 & currency == "USD") | (events_EU_mix == 1)) %>%
  mutate(datetime = ymd_hms(datetime)) %>%
  mutate(date = date(datetime),
         start_h = hour(datetime),
         event_dum = if_else((events_mil == 1 & currency == "USD") | events_EU_mix == 1, 1, 0)
  ) %>%
  select(date, start_h, event_dum, type = currency, amount)

create_spec <- \(name, dep_var_form = "level", dep_var = c("don_count", "don_total_usd"), include_lag = FALSE, indep_vars = c("is_treatment", "is_post", "is_post:is_treatment"), controls = NULL) {
  tibble(specification_name = name, expand_grid(dep_var_form, dep_var), include_lag = include_lag, indep_vars = list(c(indep_vars, controls)))
}

did_specs <- list(
  tibble(name = "different_two", ignore_night = TRUE, omit_crypto = TRUE, distinct_dates = TRUE, specifications = list(bind_rows(
    create_spec("normal", dep_var_form = c("level")),
    create_spec("normal", dep_var_form = c("level"), controls = "log1_tweet_count"),
  ))),
  tibble(name = "aid_usd", events = list(filter(data_events_aid, type == "USD")), data = list(filter(data_intraday_sub, str_detect(type_sub, "USD|UAH"))), treatment_type = "Foreign", ignore_night = TRUE, specifications = list(bind_rows(
    create_spec("normal", dep_var_form = c("level")),
    create_spec("normal", dep_var_form = c("level"), controls = "log1_tweet_count")
  ))),
  tibble(name = "aid_eur", events = list(filter(data_events_aid, type == "EUR")), data = list(filter(data_intraday_sub, str_detect(type_sub, "EUR|UAH"))), treatment_type = "Foreign", ignore_night = TRUE, specifications = list(bind_rows(
    create_spec("normal", dep_var_form = c("level")),
    create_spec("normal", dep_var_form = c("level"), controls = "log1_tweet_count"),
  )))
) %>%
  bind_rows(tibble(name = character(), events = list(), own_start_h = numeric(), data = list(), treatment_type = character(), pre_offset_h = numeric(), post_offset_h = numeric(), ignore_night = logical(), omit_crypto = logical(), distinct_dates = logical(), estimate_fixef = logical(), specifications = list()), .) %>%
  replace_na(list(events = list(data_events), data = list(data_intraday), treatment_type = "Ukrainian", pre_offset_h = 48, post_offset_h = 48, ignore_night = FALSE, omit_crypto = TRUE, distinct_dates = TRUE, estimate_fixef = FALSE)) %>%
  rowwise() %>%
  mutate(name = factor(name, levels = name), specifications = list(mutate(specifications, across(c(specification_name, dep_var, dep_var_form), ~factor(., levels = unique(.))))))

did_mods <- did_specs %>%
  mutate(data = list(generate_data(events, own_start_h, data, treatment_type, pre_offset_h, post_offset_h, ignore_night, omit_crypto, distinct_dates))) %>%
  mutate(mod = list(generate_models(data, specifications, estimate_fixef, aggregation)),
         mod_robust = list(map(mod, ~ coeftest(.x, vcov = vcovHAC(.x)))))

did_tables <- did_mods %>%
  select(name, specifications, mod, mod_robust) %>%
  unnest(c(specifications, mod, mod_robust)) %>%
  arrange(name, dep_var, specification_name) %>%
  group_by(name, dep_var_form) %>%
  summarise(across(everything(), list)) %>%
  rowwise() %>%
  mutate(mod_robust_se = list(map(mod_robust, ~.[, 2])),
         mod_robust_p = list(map(mod_robust, ~.[, 4]))) %>%
  mutate(table_caption = switch(as.character(name),
                                different_two = "DiD after war events: Comparison of different transformations for count of tweets (exposure).",
                                aid_usd = "DiD after US military aid events: USD (treatment) vs. UAH (control) donations.",
                                aid_eur = "DiD after EU mixed (military, humanitarian, and financial) aid events: EUR (treatment) vs. UAH (control) donations.",
                                NULL
  ),
         table_variable_map = switch(as.character(name),
                                     different_two = list(c(is_treatment = "$Ukrainian$", is_post = "$After$", "is_treatment:is_post" = "$Ukrainian \\times After$", log1_tweet_count = "$TweetCount$")),
                                     aid_usd = list(c(is_treatment = "$USD$", is_post = "$After$", "is_treatment:is_post" = "$USD \\times After$", log1_tweet_count = "$TweetCount$")),
                                     aid_eur = list(c(is_treatment = "$EUR$", is_post = "$After$", "is_treatment:is_post" = "$EUR \\times After$", log1_tweet_count = "$TweetCount$")),
                                     NULL
  ),
         table_model_names = list(c(don_count = "$DonCount$", don_total_usd = "$DonTotalUSD$", don_mean_usd = "$DonMeanUSD$")[dep_var])
  ) %>%
  mutate(table = list(texreg(set_names(mod, str_replace_all(table_model_names, "_", "\\\\_")),
                             override.se = mod_robust_se,
                             override.pvalues = mod_robust_p,
                             include.nobs = FALSE,
                             use.packages = FALSE,
                             threeparttable = FALSE,
                             booktabs = TRUE,
                             dcolumn = TRUE,
                             scalebox = 0.8,
                             custom.coef.map = as.list(table_variable_map),
                             caption.above = TRUE,
                             caption = table_caption,
                             stars = c(0.01, 0.05, 0.1))))

ltx_file <- paste(did_tables$table)
write(ltx_file, "results/latex_did/supplement.tex")
file.copy("src/latex/main_did.tex", "results/latex_did/main_did.tex", overwrite = TRUE)
tools::texi2pdf("results/latex_did/main_did.tex", clean = TRUE)
file.copy("main_did.pdf", "results/DiD_results.pdf", overwrite = TRUE)

### PLOTS
## DiD plot
scaling_count <- 10
scaling_total <- 1.6
# scaling_mean <- 4.3

# Select last specification's data
data_did <- did_mods$data[[1]] %>%
  # Average over treatment/control groups
  group_by(is_treatment, rel_time) %>%
  # summarise(across(c(don_count, don_total_usd, don_mean_usd), mean), .groups = "drop") %>%
  summarise(across(c(don_count, don_total_usd), mean), .groups = "drop") %>%
  mutate(don_count = if_else(is_treatment == 0, don_count * scaling_count, don_count)) %>%
  # mutate(don_mean_usd = if_else(is_treatment == 0, don_mean_usd / scaling_mean, don_mean_usd)) %>%
  mutate(don_total_usd = if_else(is_treatment == 0, don_total_usd * scaling_total, don_total_usd)) %>%
  pivot_longer(contains("don")) %>%
  group_by(is_treatment,
           is_post = if_else(rel_time < 0, 0, 1),
           name) %>%
  mutate(mean = mean(value)) %>%
  group_by(is_treatment, name) %>%
  mutate(value = rollapply(value, 5, mean, partial = TRUE)) %>%
  ungroup()

plot_1 <- data_did %>%
  mutate(group = factor(is_treatment, levels = c(0, 1), labels = c("Foreign", "Ukrainian")),
         name = factor(name, levels = unique(name))) %>%
  filter(name == "don_count") %>%
  ggplot(aes(x = rel_time, y = value, color = group, group = group, linetype = group)) +
  scale_colour_manual(name = "Type", values = c("#800000FF", "#CC8214FF")) +
  scale_linetype_manual(name = "Type", values = c("41", "solid")) +
  geom_line(linewidth=1.5) +
  geom_line(aes(y = mean, color = group, group = interaction(is_post, group)), linetype = "dashed") +
  geom_vline(aes(xintercept = 0)) +
  ylab("Donation count (Ukrainian)") +
  xlab("") +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),
                                    color = "#CC8214FF", face= "bold", size = 20),
        axis.line.y = element_line(color = "#CC8214FF", linewidth=1),
        axis.ticks.y = element_line(color = "#CC8214FF"),
        axis.text.y = element_text(color = "#CC8214FF", size=13),
        legend.position = "bottom",
        legend.title = element_text(size=19),
        legend.text = element_text(size=17),
        legend.key.width = unit(1.5,"cm"),
        axis.text.x = element_text(size=13))


plot_1 <- plot_1 + new_scale_color() +
  scale_y_continuous(sec.axis = sec_axis(~ ./scaling_count, name = "Donation count (Foreign)")) +
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                                          color = "#800000FF"),
        axis.line.y.right = element_line(color = "#800000FF"),
        axis.ticks.y.right = element_line(color = "#800000FF"),
        axis.text.y.right = element_text(color = "#800000FF"))


plot_2 <- data_did %>%
  mutate(group = factor(is_treatment, levels = c(0, 1), labels = c("Foreign", "Ukrainian")),
         name = factor(name, levels = unique(name))) %>%
  filter(name == "don_total_usd") %>%
  ggplot(aes(x = rel_time, y = value, color = group, group = group, linetype = group)) +
  scale_colour_manual(name = "Type", values = c("#800000FF", "#CC8214FF")) +
  scale_linetype_manual(name = "Type", values = c("41", "solid")) +
  geom_line(linewidth=1.5) +
  geom_line(aes(y = mean, color = group, group = interaction(is_post, group)), linetype = "dashed") +
  geom_vline(aes(xintercept = 0)) +
  ylab("Donation total USD value (Ukrainian)") +
  xlab("Event timeline (number of hours before and after the event)") +
  #scale_y_continuous(sec.axis = sec_axis(~ ., name = "Donation total USD value (Foreign)")) +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),face  = "bold", size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),
                                    color = "#CC8214FF", face= "bold", size = 20),
        axis.line.y = element_line(color = "#CC8214FF", linewidth=1),
        axis.ticks.y = element_line(color = "#CC8214FF"),
        axis.text.y = element_text(color = "#CC8214FF", size=13),
        legend.position = "bottom",
        legend.title = element_text(size=19),
        legend.text = element_text(size=17),
        legend.key.width = unit(1.5,"cm"),
        axis.text.x = element_text(size=13))

plot_2 <- plot_2 + new_scale_color() +
  scale_y_continuous(sec.axis = sec_axis(~ ./scaling_total, name = "Donation total USD value (Foreign)")) +
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                                          color = "#800000FF"),
        axis.line.y.right = element_line(color = "#800000FF"),
        axis.ticks.y.right = element_line(color = "#800000FF"),
        axis.text.y.right = element_text(color = "#800000FF"))

# plot_3 <- data_did %>%
#   mutate(group = factor(is_treatment, levels = c(0, 1), labels = c("Foreign", "Ukrainian")),
#          name = factor(name, levels = unique(name))) %>%
#   filter(name == "don_mean_usd") %>%
#   ggplot(aes(x = rel_time, y = value, color = group, group = group)) +
#   scale_color_uchicago() +
#   geom_line( linewidth=1.5) +
#   geom_line(aes(y = mean, color = group, group = interaction(is_post, group)), linetype = "dashed") +
#   geom_vline(aes(xintercept = 0)) +
#   scale_colour_manual(values = c("#800000FF", "#4747ba")) +
#   ylab("Donation mean USD value (Ukrainian)") +
#   xlab("Event timeline (number of hours before and after the event)") +
#   #scale_y_continuous(sec.axis = sec_axis(~ .*scaling, name = "Donation mean USD value (Foreign)"))+
#   theme_classic() +
#   theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), face= "bold", size = 15),
#         axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),
#                                     color = "#4747ba", face= "bold", size = 15),
#         axis.line.y = element_line(color = "#4747ba", linewidth=1),
#         axis.ticks.y = element_line(color = "#4747ba"),
#         axis.text.y = element_text(color = "#4747ba"))
#
# plot_3 <- plot_3 + new_scale_color() +
#   scale_y_continuous(sec.axis = sec_axis(~ .*scaling_mean, name = "Donation mean USD value (Foreign)")) +
#   theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
#                                           color ="#800000FF"),
#         axis.line.y.right = element_line(color = "#800000FF"),
#         axis.ticks.y.right = element_line(color = "#800000FF"),
#         axis.text.y.right = element_text(color = "#800000FF"))

# ggpubr::ggarrange(plot_1, plot_2, plot_3, ncol = 1, legend = FALSE)
ggpubr::ggarrange(plot_1, plot_2, ncol = 1, common.legend = TRUE, legend = "top")
