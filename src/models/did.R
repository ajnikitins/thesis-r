library(tidyverse)
library(readxl)
library(lubridate)
library(plm)
library(lmtest)
library(sandwich)
library(texreg)
library(glue)

generate_hours <- \(day, start_h, pre_offset_h, post_offset_h) {
  start_date <- day
  hour(start_date) <- start_h

  pre_start_date <- day
  hour(pre_start_date) <- start_h - pre_offset_h
  pre <- data.frame(date = seq(pre_start_date, start_date - 3600, by = "hours"), rel_time = (-pre_offset_h):(-1), is_pre = 1)

  post_date <- day
  hour(post_date) <- start_h + post_offset_h
  post <- data.frame(date = seq(start_date, post_date - 3600, by = "hours"), rel_time = 0:(post_offset_h - 1), is_post = 1)

  dates <- bind_rows(pre, post) %>%
    mutate(across(c(is_pre, is_post), replace_na, 0),
           is_pre = if_else(rel_time == -1, 0, is_pre),
           group = paste0(year(day), month(day), day(day)))
}

data_intraday <- readRDS("data/data_intraday.RDS")
data_events_raw <- read_excel("data/important_events_daily.xlsx")
data_events <- data_events_raw %>%
  mutate(event_dum = if_else(!is.na(coloring), 1, 0, missing = 0)) %>%
  select(date, event_dum) %>%
  mutate(event_dum = if_else(date == dmy("11-10-2022") | date == dmy("01-06-2022"), 0, event_dum))

data <- data_events %>%
  filter(date >= dmy("16-03-2022")) %>%
  filter(event_dum == 1) %>%
  mutate(start_h = 12, pre_offset_h = 48, post_offset_h = 48) %>%
  rowwise() %>%
  transmute(date = list(generate_hours(date, start_h, pre_offset_h, post_offset_h))) %>%
  unnest(date) %>%
  left_join(data_intraday, by = "date") %>%
  filter(type != "Crypto") %>%
  mutate(is_treatment = if_else(type == "Ukrainian", 1, 0)) %>%
  mutate(is_daylight = if_else(dplyr::between(hour(date), 6, 22), 1, 0)) %>%
  mutate(time = rel_time + 24) %>%
  group_by(type)

# data %>%
#   ungroup() %>%
#   mutate(across(c(don_count, don_total_usd, don_mean_usd), log)) %>%
#   filter(if_any(c(don_count, don_total_usd, don_mean_usd), ~ is.na(.) | is.nan(.) | is.infinite(.)))

did <- data.frame(
  specification = c("normal", "pre", "daylight"),
  expand_grid(
    # dep_var = c("log1_don_count", "log1_don_total_usd", "log1_don_mean_usd"),
    dep_var = c("don_count", "don_total_usd", "don_mean_usd"),
    indep_vars = list(
      c("is_treatment", "is_post", "is_post:is_treatment"),
      c("is_treatment", "is_pre", "is_post", "is_pre:is_treatment", "is_post:is_treatment"),
      c("is_treatment", "is_daylight", "is_pre", "is_post", "is_daylight:is_treatment", "is_pre:is_treatment", "is_post:is_treatment")
    )
  )) %>%
  rowwise() %>%
  mutate(eq = list(formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))),
         mod = list(lm(eq, data = data)),
         mod_robust = list(coeftest(mod, vcov = vcovHAC(mod))))

did %>%
  # mutate(dep_var = list(switch(dep_var, don_count = "Count", don_total_usd = "Total value (USD)", don_mean_usd = "Mean value (USD)"))) %>%
  with({ screenreg(mod_robust,
                custom.header = set_names(split(seq_along(mod), cut(seq_along(mod), length(unique(dep_var)), labels = FALSE)), unique(dep_var)),
                custom.model.names = rep(" ", length(mod)),
                dcolumn = TRUE, booktabs = TRUE, threeparttable = TRUE,
                stars = c(0.001, 0.01, 0.05, 0.10),
                caption = "D-i-D analysis for hourly donation characteristics (levels).",
                custom.note = "
                \\item %stars \\\\
                \\item $is\\_treatment = 1$ for Ukrainian donations, $is\\_active = 1$ for 06:00 until 23:59 for days with events, $is\\_pre = 1$ for 06:00 previous day until 05:59 for days with events, $is\\_daylight = 1$ for 06:00 until 22:00 for all days."
  ) })

### Dynamic
data_dynamic <- data %>%
  mutate(rel_time = apply(cbind(sapply((-floor(pre_offset_h[[1]] / group_n[[1]])):(floor((end_offset_h[[1]] - 1) / group_n[[1]])), \(x) ifelse(data.table::shift(is_active, x, type = "lag", fill = 0) == 1 & hour(data.table::shift(date, x, type = "lag", fill = 0)) == start_h[[1]], x, NA))), 1, \(x) if(all(is.na(x))) NA else sum(x, na.rm = TRUE))) %>%
  fastDummies::dummy_cols("rel_time") %>%
  filter(!is.na(rel_time)) %>%
  mutate(across(starts_with("rel_time"), replace_na, 0))

did_dynamic <- data.frame(
  # specification = c("normal", "daylight"),
  specification = c("normal"),
  expand_grid(
    dep_var = c("don_count", "don_total_usd", "don_mean_usd"),
    indep_vars = list(c("is_treatment", glue("`rel_time_{setdiff((min(data_dynamic$rel_time, na.rm = TRUE)):(max(data_dynamic$rel_time, na.rm = TRUE)), -1)}`"), glue("is_treatment:`rel_time_{setdiff((min(data_dynamic$rel_time, na.rm = TRUE)):(max(data_dynamic$rel_time, na.rm = TRUE)), -1)}`")))
  )) %>%
  rowwise() %>%
  mutate(eq = list(formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))),
         mod = list(lm(eq, data = data_dynamic)),
         mod_robust = list(coeftest(mod, vcov = vcovHAC(mod))),
         mod_robust_ci = list(coefci(mod, vcov = vcovHAC(mod)))
  )

screenreg(did_dynamic$mod_robust)
summary(did_dynamic$mod[[1]])

did_dynamic %>%
  mutate(coef_name = list(names(mod$coefficients)),
         coef = list(mod$coefficients),
         coef_ci_low = list(mod_robust_ci[, 1]),
         coef_ci_high = list(mod_robust_ci[, 2])
         ) %>%
  select(specification, dep_var, starts_with("coef")) %>%
  unnest_longer(starts_with("coef"), indices_include = FALSE) %>%
  filter(str_detect(coef_name, "is_treatment")) %>%
  mutate(coef_name = if_else(coef_name == "is_treatment", "is_treatment:`rel_time_-1`", coef_name)) %>%
  mutate(coef_name = as.numeric(str_extract(coef_name, "-?\\d+"))) %>%
  group_by(specification, dep_var) %>%
  arrange(coef_name, .by_group = TRUE) %>%
  ggplot(aes(x = coef_name, y = coef)) +
  geom_line() +
  geom_errorbar(aes(ymin = coef_ci_low, ymax = coef_ci_high)) +
  facet_grid(vars(dep_var), vars(specification)) +
  geom_hline(aes(yintercept = 0))


coeftest(did_dynamic$mod[[1]], vcov = vcovHAC(did_dynamic$mod[[1]]))
coefci(did_dynamic$mod[[1]], vcov = vcovHAC(did_dynamic$mod[[1]]))

### Stacked

data_stacked <- data_events %>%
  filter(date >= dmy("16-03-2022")) %>%
  filter(event_dum == 1) %>%
  mutate(start_h = 6, pre_offset_h = 24, post_offset_h = 24) %>%
  rowwise() %>%
  transmute(time = list(generate_hours(date, start_h, pre_offset_h, post_offset_h))) %>%
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

# did_stacked_count <- plm(don_count ~ is_daylight:is_treatment + is_treatment:factor(rel_time), data = data_stacked_panel, model = "within", effect = "twoways")
# did_stacked_count %>% summary()
# did_stacked_count_robust <- coeftest(did_stacked_count, vcov = vcovHC(did_stacked_count, type = "HC1", cluster = "group"))
#
# screenreg(list(did_stacked_count, did_stacked_count_robust))
#
#
# did_stacked_total <- plm(don_total_usd ~ is_treatment:factor(rel_time), data = data_stacked_panel, model = "within", effect = "twoways") %>% summary()
# did_stacked_mean <-  plm(don_mean_usd ~ is_treatment:factor(rel_time), data = data_stacked_panel, model = "within", effect = "twoways") %>% summary()
#
# fixest::feols(don_count ~ is_daylight:is_treatment + is_treatment:factor(rel_time) | group + time, data = data_stacked) %>% summary()
# lfe::felm(don_count ~ is_daylight:is_treatment + is_treatment:factor(rel_time) | group + time | 0 | group, data = data_stacked) %>% summary()

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
  ) })


