library(tidyverse)
library(readxl)
library(lubridate)
library(plm)
library(lmtest)
library(sandwich)
library(texreg)
library(glue)
library(fixest)

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
    mutate(across(c(is_pre, is_post), replace_na, 0),
           is_pre = if_else(rel_time == -1, 0, is_pre),
           group = glue("{year(day)}{month(day)}{day(day)}-{start_h}"))
}

generate_models <- \(data, specifications, estimate_fixef = FALSE, fixef = "type_sub") {
  mods <- if (estimate_fixef) {
    specifications %>%
      rowwise() %>%
      mutate(dep_var = if (dep_var_form == "level") dep_var else paste0(dep_var_form, "_", dep_var),
             eq = list(formula(paste(dep_var, "~", paste(indep_vars, collapse = "+"), glue("| {fixef} + rel_time")))),
             mod = list(feols(eq, data = data, panel.id = ~type_sub + rel_time, vcov = "DK")))
  } else {
    specifications %>%
      rowwise() %>%
      mutate(dep_var = if (dep_var_form == "level") dep_var else paste0(dep_var_form, "_", dep_var),
             eq = list(formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))),
             mod = list(lm(eq, data = data)),
             mod = list(coeftest(mod, vcov = vcovHAC(mod))))
  }

  pull(mods, mod)
}

generate_data <- \(events, own_start_h, aggregation, pre_offset_h, post_offset_h, ignore_night, omit_crypto, distinct_dates) {
  dates <- events %>%
    rowwise() %>%
    mutate(start_h = if (!is.na(own_start_h)) own_start_h else start_h) %>%
    transmute(date = list(generate_hours(date, start_h, pre_offset_h, post_offset_h, ignore_night = ignore_night))) %>%
    unnest(date) %>%
    filter(date >= dmy("16-03-2022") & date < dmy("01-11-2022"))

  if (distinct_dates) {
    dates <- distinct(dates, date, .keep_all = TRUE)
  }

  variables <- if (aggregation == "type") {
    data_intraday
  } else if (aggregation == "type_sub") {
    data_intraday_sub
  }

  data <- dates %>%
    left_join(variables, by = "date") %>%
    filter(!omit_crypto | type != "Crypto") %>%
    mutate(is_treatment = if_else(type == "Ukrainian", 1, 0)) %>%
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
  transmute(date = date(datetime),
            start_h = hour(datetime),
            event_dum = if_else(!is.na(coloring), 1, 0, missing = 0)) %>%
  filter(date != dmy("11-10-2022"))

did_specs <- list(
  tibble(name = "single_two", own_start_h = 12, aggregation = "type", omit_crypto = TRUE, distinct_dates = TRUE, specifications = list(bind_rows(
    tibble(specification_name = "normal", expand_grid(dep_var_form = c("level", "log1"), dep_var = c("don_count", "don_total_usd", "don_mean_usd")), indep_vars = list(c("is_treatment", "is_post", "is_post:is_treatment"))),
    tibble(specification_name = "pre", expand_grid(dep_var_form = c("level", "log1"), dep_var = c("don_count", "don_total_usd", "don_mean_usd")), indep_vars = list(c("is_treatment", "is_pre", "is_post", "is_pre:is_treatment", "is_post:is_treatment"))),
    # tibble(specification_name = "daylight", expand_grid(dep_var_form = c("level", "log1"), dep_var = c("don_count", "don_total_usd", "don_mean_usd")), indep_vars = list(c("is_treatment", "is_daylight", "is_pre", "is_post", "is_daylight:is_treatment", "is_pre:is_treatment", "is_post:is_treatment")))
  ))),
  tibble(name = "different_two", aggregation = "type", omit_crypto = TRUE, distinct_dates = TRUE, specifications = list(bind_rows(
    tibble(specification_name = "normal", expand_grid(dep_var_form = c("level", "log1"), dep_var = c("don_count", "don_total_usd", "don_mean_usd")), indep_vars = list(c("is_treatment", "is_post", "is_post:is_treatment"))),
    tibble(specification_name = "pre", expand_grid(dep_var_form = c("level", "log1"), dep_var = c("don_count", "don_total_usd", "don_mean_usd")), indep_vars = list(c("is_treatment", "is_pre", "is_post", "is_pre:is_treatment", "is_post:is_treatment"))),
    # tibble(specification_name = "daylight", expand_grid(dep_var_form = c("level", "log1"), dep_var = c("don_count", "don_total_usd", "don_mean_usd")), indep_vars = list(c("is_treatment", "is_daylight", "is_pre", "is_post", "is_daylight:is_treatment", "is_pre:is_treatment", "is_post:is_treatment")))
  ))),
  tibble(name = "different_many", aggregation = "type_sub", ignore_night = TRUE, omit_crypto = FALSE, distinct_dates = TRUE, estimate_fixef = TRUE, specifications = list(bind_rows(
    tibble(specification_name = "normal", expand_grid(dep_var_form = c("level", "log1"), dep_var = c("don_count", "don_total_usd", "don_mean_usd")), indep_vars = list(c("is_treated")))
  ))),
  tibble(name = "different_many_nofixef", aggregation = "type_sub", ignore_night = TRUE, omit_crypto = FALSE, distinct_dates = TRUE, estimate_fixef = FALSE, specifications = list(bind_rows(
    tibble(specification_name = "normal", expand_grid(dep_var_form = c("level", "log1"), dep_var = c("don_count", "don_total_usd", "don_mean_usd")), indep_vars = list(c("is_treatment", "is_post", "is_treated")))
  )))
  # tibble(name = "different_many", aggregation = "type_sub", ignore_night = TRUE, omit_crypto = FALSE, distinct_dates = FALSE, estimate_fixef = TRUE, specifications = list(bind_rows(
  #   tibble(specification_name = "dupes", expand_grid(dep_var_form = c("level", "log1"), dep_var = c("don_count", "don_total_usd", "don_mean_usd")), indep_vars = list(c("is_treated")))
  # )))
) %>%
  bind_rows(tibble(name = character(), own_start_h = numeric(), aggregation = character(), pre_offset_h = numeric(), post_offset_h = numeric(), ignore_night = logical(), omit_crypto = logical(), distinct_dates = logical(), estimate_fixef = logical(), specifications = list()), .) %>%
  replace_na(list(aggregation = "type", pre_offset_h = 48, post_offset_h = 48, ignore_night = FALSE, omit_crypto = TRUE, distinct_dates = TRUE, estimate_fixef = FALSE)) %>%
  rowwise() %>%
  mutate(name = factor(name, levels = name), specifications = list(mutate(specifications, across(c(specification_name, dep_var, dep_var_form), ~factor(., levels = unique(.))))))

did_mods <- did_specs %>%
  mutate(data = list(generate_data(data_events, own_start_h, aggregation, pre_offset_h, post_offset_h, ignore_night, omit_crypto, distinct_dates))) %>%
  mutate(mods = list(generate_models(data, specifications, estimate_fixef, aggregation)))

did_tables <- did_mods %>%
  select(name, specifications, mods) %>%
  unnest(c(specifications, mods)) %>%
  arrange(name, dep_var, specification_name) %>%
  group_by(name, dep_var_form) %>%
  summarise(across(.fns = list)) %>%
  rowwise() %>%
  mutate(table = list(texreg(mods,
                             threeparttable = TRUE,
                             booktabs = TRUE,
                             dcolumn = TRUE,
                             stars = c(0.01, 0.05, 0.1))))

did_tables$table[[7]] %>% write("results/latex_did/main.tex")

# TODO: Re-check the DiD specification, the way standard errors are calculated, whether to use date or relative time fixed effects, whether to use "event" fixed effects
# Current specification `different_many_nofixef` is a "basic" (no-fixed effect) DiD regression *with duplicate dates removed!!!!*

### PLOT
scaling <- 40

# Select last specification's data
data_did <- did_mods$data[[4]] %>%
  # Average over treatment/control groups
  group_by(is_treatment, rel_time) %>%
  summarise(across(c(don_count, don_total_usd, don_mean_usd), mean), .groups = "drop") %>%
  mutate(don_count = if_else(is_treatment == 0, don_count * scaling, don_count)) %>%
  mutate(don_mean_usd = if_else(is_treatment == 0, don_mean_usd / scaling, don_mean_usd)) %>%
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
  ggplot(aes(x = rel_time, y = value, color = group, group = group)) +
  geom_line() +
  geom_line(aes(y = mean, color = group, group = interaction(is_post, group)), linetype = "dashed") +
  geom_vline(aes(xintercept = 0)) +
  ylab("don_count_Ukrainian") +
  scale_y_continuous(sec.axis = sec_axis(~ ./scaling, name = "don_count_Foreign"))

plot_2 <- data_did %>%
  mutate(group = factor(is_treatment, levels = c(0, 1), labels = c("Foreign", "Ukrainian")),
         name = factor(name, levels = unique(name))) %>%
  filter(name == "don_total_usd") %>%
  ggplot(aes(x = rel_time, y = value, color = group, group = group)) +
  geom_line() +
  geom_line(aes(y = mean, color = group, group = interaction(is_post, group)), linetype = "dashed") +
  geom_vline(aes(xintercept = 0)) +
  ylab("don_total_usd_Ukrainian") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "don_total_usd_Foreign"))

plot_3 <- data_did %>%
  mutate(group = factor(is_treatment, levels = c(0, 1), labels = c("Foreign", "Ukrainian")),
         name = factor(name, levels = unique(name))) %>%
  filter(name == "don_mean_usd") %>%
  ggplot(aes(x = rel_time, y = value, color = group, group = group)) +
  geom_line() +
  geom_line(aes(y = mean, color = group, group = interaction(is_post, group)), linetype = "dashed") +
  geom_vline(aes(xintercept = 0)) +
  ylab("don_mean_usd_Ukrainian") +
  scale_y_continuous(sec.axis = sec_axis(~ .*scaling, name = "don_mean_usd_Foreign"))

ggpubr::ggarrange(plot_1, plot_2, plot_3, ncol = 3, common.legend = TRUE, legend = "bottom")

# Select last specification's data
data_did <- did_mods$data[[4]] %>%
  # Average over treatment/control groups
  group_by(is_treatment, rel_time) %>%
  summarise(across(c(don_count, don_total_usd, don_mean_usd), mean), .groups = "drop") %>%
  pivot_longer(contains("don")) %>%
  group_by(is_treatment,
           is_post = if_else(rel_time < 0, 0, 1),
           name) %>%
  mutate(mean = mean(value)) %>%
  group_by(is_treatment, name) %>%
  mutate(value = rollapply(value, 5, mean, partial = TRUE)) %>%
  ungroup()

data_did %>%
  mutate(group = factor(is_treatment, levels = c(0, 1), labels = c("Foreign", "Ukrainian")),
         name = factor(name, levels = unique(name))) %>%
  ggplot(aes(x = rel_time, y = value, color = group, group = group)) +
  geom_line() +
  geom_line(aes(y = mean, color = group, group = interaction(is_post, group)), linetype = "dashed") +
  geom_vline(aes(xintercept = 0)) +
  facet_wrap(~name + group, ncol = 2, scales = "free_y")
# facet_grid(cols = vars(group), rows = vars(name), scales = "free")
