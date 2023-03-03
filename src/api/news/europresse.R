library(tidyverse)

data_news_euro <- read.csv("data/news/europresse.csv", header = TRUE) %>%
  mutate(date = seq(ymd("2022-01-01"), ymd("2023-02-28"), by = 1), .before = 1)

saveRDS(data_news_euro, "data/news/data_news_euro.RDS")
