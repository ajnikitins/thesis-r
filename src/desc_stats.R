library(tidyverse)
library(stargazer)

# Media attention:
# - total news count, mean, sd, median, max/min
# - total tweet count, mean, sd, median, max/min
# (good to know dates of max/min?)
# War severity:
# - total air raid sirens count, mean, sd, median?, max/min (good to know dates of max/min?);
# - mean, sd?, max/min of territory affected; mean of Kyiv affected;
# - air raid sirens duration mean, sd, max/min; (good to know dates of max/min?)
# - total count of conflict events by type (battles, remote violence, civilian targeting), mean, sd?, median, max/min (good to know dates of max/min?)
# Donations:
# - total count, mean, sd, median, max/min for each type (UKR, foreign, crypto) separately
# (good to know dates of max / min?)
# - total value, mean, sd, median, max/min for each type (UKR, foreign, crypto) separately
# (good to know dates of max / min?)
#
# ("?" is where I question usefulness conceptually, maybe it's good practice to have those)
#
# I think we can add Russian casualties (by type) & UN civilian casualties (?) total count, mean, sd, median, min/max in Appendix, dunno how useful those are

data <- readRDS("data/data_complete.RDS")

data %>%
  filter(date >= "2022-02-24") %>%
  select(-date, -type, -don_count, -don_mean_usd) %>%
  mutate(across(c(siren_count, tweet_count), as.double)) %>%
  as.data.frame() %>%
  stargazer(type = "html",
            align = TRUE,
            title = "Descriptive Statistics for Independent Variables",
            out = "pics/desc_stats_ind.html",
            digits = 2,
            digits.extra = 2,
            column.sep.width = "100pt")

data %>%
  filter(date >= "2022-02-24") %>%
  filter(type == "Ukrainian") %>%
  select(don_count, don_mean_usd) %>%
  as.data.frame() %>%
  stargazer(type = "html",
            align = TRUE,
            title = "Descriptive Statistics for Dependent Variables (Ukrainian)",
            out = "pics/desc_stats_ukr.html",
            digits = 2,
            digits.extra = 2)

data %>%
  filter(date >= "2022-02-24") %>%
  filter(type == "Foreign") %>%
  select(don_count, don_mean_usd) %>%
  as.data.frame() %>%
  stargazer(type = "html",
            align = TRUE,
            title = "Descriptive Statistics for Dependent Variables (Foreign)",
            out = "pics/desc_stats_for.html",
            digits = 2,
            digits.extra = 2)

data %>%
  filter(date >= "2022-02-24") %>%
  filter(type == "Crypto") %>%
  select(don_count, don_mean_usd) %>%
  as.data.frame() %>%
  stargazer(type = "html",
            align = TRUE,
            title = "Descriptive Statistics for Dependent Variables (Crypto)",
            out = "pics/desc_stats_crypto.html",
            digits = 2,
            digits.extra = 2)

data %>%
  filter(date >= "2022-02-24") %>%
  pivot_wider(names_from = "type", values_from = c("don_count", "don_mean_usd")) %>%
  as.data.frame() %>%
  stargazer(type = "html",
            align = TRUE,
            title = "Descriptive Statistics",
            out = "pics/desc_stats.html",
            digits = 2,
            digits.extra = 2)
