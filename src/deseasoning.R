library(tidyverse)

# data <- readRDS("data_complete.RDS") %>%
data <- readRDS("data/data_complete.RDS") %>%
  filter(date >= ymd("2022-03-10")) %>%
  select(date, type, d_don_count, d_don_mean_usd, d_siren_count, d_siren_mean_duration, d_siren_prop, siren_kyiv, d_tweet_count, d_factiva_count, d_cas_civ, d_cas_rus_mil, d_confl_evs)

#using regression residuals to deseasonalise absolute values
#create weekday dummies
data_dummies <- data %>%
  select(date, type, d_don_count, d_don_mean_usd) %>%
  mutate(weekday = weekdays(date),
         monday = ifelse(weekday == "Monday",1,0),
         tuesday = ifelse(weekday == "Tuesday",1,0),
         wednesday = ifelse(weekday == "Wednesday",1,0),
         thursday = ifelse(weekday == "Thursday",1,0),
         friday = ifelse(weekday == "Friday",1,0),
         saturday = ifelse(weekday == "Saturday",1,0),
         sunday = ifelse(weekday == "Sunday",1,0),
         banking = ifelse(date < "2022-05-26" & (monday == 1 | saturday == 1 | sunday == 1), 1, 0)) %>%
  # mutate (outbreak = ifelse(date<="2022-03-10", 1,0)) %>%
  pivot_wider(names_from = type, values_from = c(d_don_count, d_don_mean_usd))

mod_don_count_des <- data_dummies %>%
  lm(cbind(d_don_count_Ukrainian, d_don_count_Foreign, d_don_count_Crypto) ~ monday + tuesday + wednesday + thursday + friday + saturday + banking + banking:(monday + saturday), data=.)
summary(mod_don_count_des)

mod_don_mean_usd_des <- data_dummies %>%
  lm(cbind(d_don_mean_usd_Ukrainian, d_don_mean_usd_Foreign, d_don_mean_usd_Crypto) ~ monday + tuesday + wednesday + thursday + friday + saturday + banking + banking:(monday + saturday), data=.)
summary(mod_don_mean_usd_des)

## Deseasonalised counts
# Ukrainian
mod_don_count_des$fitted.values %>%
  as.data.frame() %>%
  ggplot(aes(x = data_dummies$date, y = d_don_count_Ukrainian)) + geom_line()

mod_don_count_des$residuals %>%
  as.data.frame() %>%
  ggplot(aes(x = data_dummies$date, y = d_don_count_Ukrainian)) + geom_line()

# Foreign
mod_don_count_des$fitted.values %>%
  as.data.frame() %>%
  ggplot(aes(x = data_dummies$date, y = d_don_count_Foreign)) + geom_line()

mod_don_count_des$residuals %>%
  as.data.frame() %>%
  ggplot(aes(x = data_dummies$date, y = d_don_count_Foreign)) + geom_line()

# Crypto donations
mod_don_count_des$fitted.values %>%
  as.data.frame() %>%
  ggplot(aes(x = data_dummies$date, y = d_don_count_Crypto)) + geom_line()

mod_don_count_des$residuals %>%
  as.data.frame() %>%
  ggplot(aes(x = data_dummies$date, y = d_don_count_Crypto)) + geom_line()

#add residuals to original dataframe

data_res <- mod_don_count_des$residuals %>%
  as.data.frame() %>%
  mutate(date = data_dummies$date) %>%
  pivot_longer(-date, names_to = "type", values_to = "d_don_count_des", names_prefix = "d_don_count_") %>%
  right_join(data, by = c("date", "type"))

data_res <- mod_don_mean_usd_des$residuals %>%
  as.data.frame() %>%
  mutate(date = data_dummies$date) %>%
  pivot_longer(-date, names_to = "type", values_to = "d_don_mean_usd_des", names_prefix = "d_don_mean_usd_") %>%
  right_join(data_res, by = c("date", "type"))

#plot to check
data_res %>%
  ggplot(aes(x=date, y=don_count_des)) +
  geom_line() +
  facet_wrap(~ type, scales = "free_y")

data %>%
  ggplot(aes(x=date, y=don_count)) +
  geom_line()

# saveRDS(data_res, "data_deseasoned.RDS")
saveRDS(data_res, "data/data_deseasoned.RDS")
