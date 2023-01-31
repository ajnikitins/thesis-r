library(tidyverse)
library(readxl)
library(lubridate)
library(plm)
library(lmtest)
library(texreg)

generate_hours <- \(day, start_h, end_h, offset_h) {
  pre_start_date <- day
  hour(pre_start_date) <- start_h - offset_h
  start_date <- day
  hour(start_date) <- start_h
  pre <- data.frame(date = seq(pre_start_date, start_date - 3600, by = "hours"), rel_time = (-offset_h):(-1), is_pre = 1)

  end_date <- day
  hour(end_date) <- end_h
  # post_end_date <- day
  # hour(post_end_date) <- end_h + offset_h
  # post <- data.frame(date = seq(end_date + 3600, post_end_date, by = "hours"), is_post = 1)

  active <- data.frame(date = seq(start_date, end_date, by = "hours"), rel_time = 0:(end_h - start_h), is_active = 1)

  bind_rows(pre, active) %>%
    mutate(across(c(is_pre, is_active), replace_na, 0)) %>%
    mutate(group = paste0(year(day), month(day), day(day)))
}

data_intraday <- readRDS("data/data_intraday.RDS")
data_events_raw <- read_excel("data/important_events.xlsx")
data_events <- data_events_raw %>%
  mutate(event_dum = if_else(!is.na(coloring), 1, 0, missing = 0)) %>%
  select(date, event_dum) %>%
  mutate(event_dum = if_else(date == dmy("11-10-2022") | date == dmy("01-06-2022"), 0, event_dum))

data <- data_intraday %>%
  # filter(date >= dmy("16-03-2022")) %>%
  filter(type != "Crypto") %>%
  group_by(type) %>%
  mutate(group_n = 4,
         group = ifelse(hour(date) %% group_n == 0, row_number(), row_number() - hour(date) %% group_n)) %>%
  group_by(type, group) %>%
  summarise(date = date[[1]], group_n = group_n[[1]], across(c(don_count, don_total, don_total_usd, strike_count), sum), don_mean = don_total / don_count, don_mean_usd = don_total_usd / don_count) %>%
  mutate(across(don_mean_usd, replace_na, 0)) %>%
  left_join(data_events, by = "date") %>%
  fill(event_dum, .direction = "down") %>%
  mutate(start_h = 6, end_h = start_h + 24, pre_offset_h = 24,
         is_active = if_else(event_dum == 1 & dplyr::between(hour(date), start_h[[1]], end_h[[1]]), 1, 0),
         is_pre = if_else(reduce(lapply(1:(floor(pre_offset_h[[1]] / group_n[[1]])), \(x) dplyr::lead(is_active, x) == 1), `|`) & is_active == 0, 1, 0),
         # is_post = if_else(lag(is_active, 14) == 1, 1, 0),
         is_treatment = if_else(type == "Ukrainian", 1, 0),
         # across(c(is_pre, is_post), replace_na, 0)
         across(is_pre, replace_na, 0)
  ) %>%
  mutate(is_daylight = if_else(dplyr::between(hour(date), 6, 22), 1, 0)) %>%
  group_by(type) %>%
  # Normalization?
  mutate(across(c(don_count, don_total_usd, don_mean_usd), ~ (.x - mean(.x)) / sd(.x))) %>%
  # mutate(across(c(don_count, don_total_usd, don_mean_usd), log)) %>%
  # mutate(across(c(don_count, don_total_usd, don_mean_usd), ~ log(.) - log(dplyr::lag(., 1)))) %>%
  filter(date >= dmy("16-03-2022"))

# data %>%
#   ungroup() %>%
#   mutate(across(c(don_count, don_total_usd, don_mean_usd), log)) %>%
#   filter(if_any(c(don_count, don_total_usd, don_mean_usd), ~ is.na(.) | is.nan(.) | is.infinite(.)))

did <- data.frame(
  specification = c("normal", "daylight"),
  expand_grid(
  dep_var = c("don_count", "don_total_usd", "don_mean_usd"),
  indep_vars = list(c("is_treatment", "is_pre", "is_active", "is_pre:is_treatment", "is_active:is_treatment"),
                    c("is_treatment", "is_daylight", "is_pre", "is_active", "is_daylight:is_treatment", "is_pre:is_treatment", "is_active:is_treatment"))
)) %>%
  rowwise() %>%
  mutate(eq = list(formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))),
            mod = list(lm(eq, data = data)),
            mod_robust = list(coeftest(mod, vcov = vcovHAC(mod))))

did %>%
  mutate(dep_var = list(switch(dep_var, don_count = "Count", don_total_usd = "Total value (USD)", don_mean_usd = "Mean value (USD)"))) %>%
  with({ texreg(mod_robust,
                custom.header = set_names(split(seq_along(mod), cut(seq_along(mod), length(unique(dep_var)), labels = FALSE)), unique(dep_var)),
                custom.model.names = rep(" ", length(mod)),
                dcolumn = TRUE, booktabs = TRUE, threeparttable = TRUE,
                stars = c(0.001, 0.01, 0.05, 0.10),
                caption = "D-i-D analysis for hourly donation characteristics (levels).",
                custom.note = "
                \\item %stars \\\\
                \\item $is\\_treatment = 1$ for Ukrainian donations, $is\\_active = 1$ for 06:00 until 23:59 for days with events, $is\\_pre = 1$ for 06:00 previous day until 05:59 for days with events, $is\\_daylight = 1$ for 06:00 until 22:00 for all days."
    )})


did_count <- lm(don_count ~ is_treatment + is_daylight + is_pre + is_active + is_daylight:is_treatment + is_pre:is_treatment + is_active:is_treatment, data = data)
did_count %>% summary()
did_count_robust <- coeftest(did_stacked_count, vcov = vcovHAC(did_stacked_count))

did_total <- lm(don_total_usd ~ is_treatment + is_pre + is_active + is_pre:is_treatment + is_active:is_treatment, data = data)
did_total %>% summary()
did_total_robust <- coeftest(did_stacked_total, vcov = vcovHAC(did_stacked_total))

did_mean <- nlme::gls(don_mean_usd ~ is_treatment + is_pre + is_active + is_pre:is_treatment + is_active:is_treatment, data = data)
did_mean %>% summary()

screenreg(list(did_count, did_total, did_mean),
                  include.aic = FALSE, include.bic = FALSE, include.loglik = FALSE,
                  custom.model.names = c("Count", "Total", "Mean"),
                  dcolumn = TRUE, booktabs = TRUE, threeparttable = TRUE,
                  stars = c(0.001, 0.01, 0.05, 0.10),
                  caption = "D-i-D analysis for hourly donation characteristics (levels).",
                  custom.note = "\\item %stars \\\\
                  \\item $is\\_treatment = 1$ for Ukrainian donations, $is\\_active = 1$ for 06:00 (including) till 22:00 (incl.) for days with events, $is\\_pre = 1$ for 03:00 (incl.) till 06:00 (excl.) for days with events.\\item $is\\_treatment = 1$ for Ukrainian donations, $is\\_active = 1$ for 06:00 till 22:00 (including) for days with events, $is\\_pre = 1$ for 03:00 till 06:00 (excluding)."
)

data_stacked <- data_events %>%
  filter(date >= dmy("16-03-2022")) %>%
  filter(event_dum == 1) %>%
  mutate(start_h = 6, end_h = 24, offset_h = 24) %>%
  rowwise() %>%
  transmute(time = list(generate_hours(date, start_h, end_h, offset_h))) %>%
  unnest(time) %>%
  left_join(data_intraday, by = "date") %>%
  filter(type != "Crypto") %>%
  mutate(is_treatment = if_else(type == "Ukrainian", 1, 0)) %>%
  mutate(is_daylight = if_else(dplyr::between(hour(date), 6, 22), 1, 0)) %>%
  mutate(time = rel_time + 24) %>%
  group_by(type) #%>%
  # Normalization?
  # mutate(across(c(don_count, don_total_usd, don_mean_usd), ~ (.x - mean(.x)) / sd(.x)))

data_stacked_panel <- data_stacked %>%
  # mutate(group = paste0(group, "-", is_treatment)) %>%
  pdata.frame(index = c("group", "rel_time"), row.names = FALSE)

# lm(don_count ~ is_treatment + factor(rel_time) + is_treatment:factor(rel_time), data = data_stacked) %>% summary()
# tmp <- lm(don_count ~ is_daylight + is_daylight:is_treatment + factor(group) + factor(rel_time) + is_treatment:factor(rel_time), data = data_stacked)
# lm(don_total_usd ~ 0 + factor(group) + factor(rel_time) + is_treatment:factor(rel_time), data = data_stacked) %>% summary()
# lm(don_mean_usd ~ factor(group) + factor(rel_time) + is_treatment:factor(rel_time), data = data_stacked) %>% summary()

did_stacked_count <- plm(don_count ~ is_daylight:is_treatment + is_treatment:factor(rel_time), data = data_stacked_panel, model = "within", effect = "twoways")
did_stacked_count %>% summary()
did_stacked_count_robust <- coeftest(did_stacked_count, vcov = vcovHC(did_stacked_count, type = "HC1", cluster = "group"))

screenreg(list(did_stacked_count, did_stacked_count_robust))


did_stacked_total <- plm(don_total_usd ~ is_treatment:factor(rel_time), data = data_stacked_panel, model = "within", effect = "twoways") %>% summary()
did_stacked_mean <-  plm(don_mean_usd ~ is_treatment:factor(rel_time), data = data_stacked_panel, model = "within", effect = "twoways") %>% summary()

fixest::feols(don_count ~ is_daylight:is_treatment + is_treatment:factor(rel_time) | group + time, data = data_stacked) %>% summary()
lfe::felm(don_count ~ is_daylight:is_treatment + is_treatment:factor(rel_time) | group + time | 0 | group, data = data_stacked) %>% summary()

did_stacked <- data.frame(
  specification = c("normal", "daylight"),
  expand_grid(
    dep_var = c("don_count", "don_total_usd", "don_mean_usd"),
    indep_vars = list("is_treatment:factor(rel_time)",
                      c("is_daylight:is_treatment", "is_treatment:factor(rel_time)")),
    # robust = c(FALSE, TRUE)
  )) %>%
  rowwise() %>%
  mutate(eq = list(formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))),
         mod = list(plm(eq, data = data_stacked_panel, model = "within", effect = "twoways")),
         # mod_robust = list(ifelse(robust, coeftest(mod, vcov = vcovHC(mod, type = "HC3", cluster = "group")), mod)))
         mod_robust = list(coeftest(mod, vcov = vcovHC(mod, type = "HC3", cluster = "group"))))

did_stacked %>%
  with({ screenreg(mod,
                custom.header = set_names(split(seq_along(mod), cut(seq_along(mod), length(unique(dep_var)), labels = FALSE)), unique(dep_var)),
                custom.model.names = rep(" ", length(mod)),
                dcolumn = TRUE, booktabs = TRUE, threeparttable = TRUE,
                stars = c(0.001, 0.01, 0.05, 0.10),
                caption = "D-i-D analysis for hourly donation characteristics (levels).",
                # custom.note = "
                # \\item %stars \\\\
                # \\item $is\\_treatment = 1$ for Ukrainian donations, $is\\_active = 1$ for 06:00 until 23:59 for days with events, $is\\_pre = 1$ for 06:00 previous day until 05:59 for days with events, $is\\_daylight = 1$ for 06:00 until 22:00 for all days."
  )})


