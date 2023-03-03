library(tidyverse)
library(broom)

data_news_euro <- read.csv("data/news/europresse.csv", header = TRUE) %>%
  mutate(date = seq(ymd("2022-01-01"), ymd("2023-02-28"), by = 1), .before = 1) %>%
  mutate(dlog_news_euro_count = log(news_euro_count) - log(lag(news_euro_count)),
         weekday = weekdays(date)) %>%
  filter(if_all(everything(), ~ !is.na(.)))

mod_euro <- data_news_euro %>%
  lm(dlog_news_euro_count ~ factor(weekday), data = .)

data_news_euro <- data_news_euro %>%
  mutate(dlog_news_euro_count_des = mod_euro$residuals) %>%
  select(-weekday)

data_news_euro %>%
  select(-news_euro_count) %>%
  pivot_longer(-c(date, weekday)) %>%
  ggplot(aes(x = date, y = value)) + geom_col() +
  facet_wrap(~ name)

saveRDS(data_news_euro, "data/news/data_news_euro.RDS")
