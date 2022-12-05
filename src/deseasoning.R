library(tidyverse)

# data <- readRDS("data_complete.RDS") %>%
data <- readRDS("data/data_complete.RDS")

#using regression residuals to deseasonalise absolute values
#create weekday dummies
# Given a variable and a dataset, calculate the deseasonalised values
deseasonalise <- \(.data, var_y) {
  # Get weekend dummies
  data <- .data %>%
    mutate(weekday = weekdays(date),
           monday = ifelse(weekday == "Monday",1,0),
           tuesday = ifelse(weekday == "Tuesday",1,0),
           wednesday = ifelse(weekday == "Wednesday",1,0),
           thursday = ifelse(weekday == "Thursday",1,0),
           friday = ifelse(weekday == "Friday",1,0),
           saturday = ifelse(weekday == "Saturday",1,0),
           sunday = ifelse(weekday == "Sunday",1,0))

  formula <- as.formula(paste0(var_y, "~ monday + tuesday + wednesday + thursday + friday + saturday"))
  model <- lm(formula, data = data)

  model$residuals
}

data_res <- data %>%
  filter(date >= ymd("2022-03-10")) %>%
  pivot_wider(names_from = type, values_from = c(don_count, don_mean_usd, d_don_count, d_don_mean_usd)) %>%
  mutate(across(c(-date, -siren_kyiv), ~ deseasonalise(cur_data(), cur_column()))) %>%
  pivot_longer(contains(c("don_count", "don_mean_usd")), names_to = c(".value", "type"), names_pattern = "(d?_?don_count|d?_?don_mean_usd)_([A-Z].*)")

# ## Deseasonalised counts
# # Ukrainian
# mod_don_count_des$fitted.values %>%
#   as.data.frame() %>%
#   ggplot(aes(x = data_dummies$date, y = d_don_count_Ukrainian)) + geom_line()
#
# mod_don_count_des$residuals %>%
#   as.data.frame() %>%
#   ggplot(aes(x = data_dummies$date, y = d_don_count_Ukrainian)) + geom_line()
#
# # Foreign
# mod_don_count_des$fitted.values %>%
#   as.data.frame() %>%
#   ggplot(aes(x = data_dummies$date, y = d_don_count_Foreign)) + geom_line()
#
# mod_don_count_des$residuals %>%
#   as.data.frame() %>%
#   ggplot(aes(x = data_dummies$date, y = d_don_count_Foreign)) + geom_line()
#
# # Crypto donations
# mod_don_count_des$fitted.values %>%
#   as.data.frame() %>%
#   ggplot(aes(x = data_dummies$date, y = d_don_count_Crypto)) + geom_line()
#
# mod_don_count_des$residuals %>%
#   as.data.frame() %>%
#   ggplot(aes(x = data_dummies$date, y = d_don_count_Crypto)) + geom_line()
#
# #add residuals to original dataframe
#
# data_res <- mod_don_count_des$residuals %>%
#   as.data.frame() %>%
#   mutate(date = data_dummies$date) %>%
#   pivot_longer(-date, names_to = "type", values_to = "d_don_count_des", names_prefix = "d_don_count_") %>%
#   right_join(data, by = c("date", "type"))
#
# data_res <- mod_don_mean_usd_des$residuals %>%
#   as.data.frame() %>%
#   mutate(date = data_dummies$date) %>%
#   pivot_longer(-date, names_to = "type", values_to = "d_don_mean_usd_des", names_prefix = "d_don_mean_usd_") %>%
#   right_join(data_res, by = c("date", "type"))
#
# #plot to check
# data_res %>%
#   ggplot(aes(x=date, y=don_count_des)) +
#   geom_line() +
#   facet_wrap(~ type, scales = "free_y")
#
# data %>%
#   ggplot(aes(x=date, y=don_count)) +
#   geom_line()

# saveRDS(data_res, "data_deseasoned.RDS")
saveRDS(data_res, "data/data_deseasoned.RDS")
