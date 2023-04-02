library(tidyverse)

data <- readRDS("data/data_complete.RDS")

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
  select(date, type, starts_with("don")) %>%
  group_by(type) %>%
  mutate(across(c(-date, -ends_with("dum"), -ends_with("quint"), -ends_with("des")), ~ deseasonalise(cur_data(), cur_column()))) %>%
  ungroup()

saveRDS(data_res, "data/data_deseasoned.RDS")
