library(tidyverse)
library(lubridate)
library(glue)
library(rvest)
library(readxl)

## Ukranian civilian casualties
# From https://data.humdata.org/dataset/reliefweb-crisis-figures
data_civ_cas <- read.csv("data/severity/Data_ ReliefWeb Crisis Figures Data - historical_figures.csv") %>%
  mutate(figure_name = case_when(
    str_detect(figure_name, fixed("Civilians Killed since 24 Feb 2022")) ~ "civ_killed",
    str_detect(figure_name, fixed("Civilians Injured since 24 Feb 2022")) ~ "civ_injured",
    str_detect(figure_name, fixed("Civilian Casualties since 24 Feb 2022 (killed & injured)")) ~ "civ_both",
    TRUE ~ figure_name
  )) %>%
  filter(crisis_index == 27 & str_detect(figure_name, "civ_")) %>%
  select(date = figure_date, type = figure_name, value = figure_value) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(date = ymd(if_else(date == "2021-03-10", "2022-03-10", date))) %>%
  arrange(date)

saveRDS(data_civ_cas, "data/severity/data_civ_cas.RDS")


## Russian casualties
get_data_rus_cas <- \(start, end) {
  months <- seq(ym(start), ym(end), by = "month") %>%
    map_chr(~ paste(year(.), month(.), sep = "-"))

  data_rus_cas_bare <- reduce(months, \(acc, res_month) {
    res <- read_html(glue("https://index.minfin.com.ua/en/russian-invading/casualties/month.php?month={res_month}"))
    list <- html_elements(res, ".gold")

    dates <- list %>%
      html_element("span") %>%
      html_text2()

    casualties <- list %>%
      html_elements(".casualties ul") %>%
      map(html_elements, "li") %>%
      map(html_text2)

    data_part <- tibble(date = dates, casualties = casualties)

    bind_rows(data_part, acc)
  }, .init = tibble())

  data_rus_cas <- data_rus_cas_bare %>%
    unnest(casualties) %>%
    separate(casualties, into = c("type", "value"), sep = " \u2014 ") %>%
    # There also is the number of captives in some "Military personnel" values
    # Some groups were merged later
    mutate(date = dmy(date),
           type = case_when(
             # `Cisterns with fuel` & `Cars` to `Cars and cisterns`
             str_detect(type, "isterns with") ~ "Cars and cisterns",
             str_detect(type, "Cars") ~ "Cars and cisterns",
             # `MLRS Grad` to `MLRS`
             str_detect(type, "MLRS Grad") ~ "MLRS",
             # `BUK missile system` & `Mobile SRBM` to `Cruise missiles
             str_detect(type, "BUK missile system") ~ "Cruise missiles",
             str_detect(type, "Mobile SRBM") ~ "Cruise missiles",
             TRUE ~ type
           ),
           value = as.numeric(str_extract(value, "[0-9]+"))) %>%
    group_by(date, type) %>%
    summarize(value = sum(value), .groups = "drop")
}

data_rus_cas <- get_data_rus_cas(start = Sys.getenv("RUS_CAS_START"), end = Sys.getenv("RUS_CAS_END"))

saveRDS(data_rus_cas, "data/severity/data_rus_cas.RDS")

## Conflict events
# From https://acleddata.com/ukraine-crisis/#data
data_confl_evs <- read_excel("data/severity/Ukraine_Black_Sea_2020_2022_Nov04.xlsx")

saveRDS(data_confl_evs, "data/severity/data_confl_evs.RDS")
