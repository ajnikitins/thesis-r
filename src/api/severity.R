library(tidyverse)
library(lubridate)
library(glue)
library(rvest)
library(readxl)

## Ukranian civilian casualties
# From https://data.humdata.org/dataset/reliefweb-crisis-figures
data_sev_cas_civ <- read.csv("data/severity/Data_ ReliefWeb Crisis Figures Data - historical_figures.csv") %>%
  mutate(figure_name = case_when(
    # str_detect(figure_name, fixed("Civilians Killed since 24 Feb 2022")) ~ "civ_killed",
    # str_detect(figure_name, fixed("Civilians Injured since 24 Feb 2022")) ~ "civ_injured",
    str_detect(figure_name, fixed("Civilian Casualties since 24 Feb 2022 (killed & injured)")) ~ "sev_cas_civ_count",
    TRUE ~ figure_name
  )) %>%
  filter(crisis_index == 28 & str_detect(figure_name, "sev_cas_civ_count")) %>%
  select(date = figure_date, type = figure_name, value = figure_value) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(date = if_else(date == "2021-03-10", "2022-03-10", date),
         date = if_else(date == "2022-01-29", "2023-01-29", date),
         date = ymd(date)) %>%
  arrange(date) %>%
  mutate(sev_cas_civ_count = sev_cas_civ_count - dplyr::lag(sev_cas_civ_count)) %>%
  add_row(date = ymd("2023-02-28")) %>%
  right_join(tidyr::expand(., date = full_seq(date, 1)), by = "date") %>%
  arrange(date) %>%
  fill(sev_cas_civ_count) %>%
  mutate(sev_cas_civ_count = replace_na(sev_cas_civ_count, 0))
  arrange(date)

# # From https://www.statista.com/statistics/1296924/ukraine-war-casualties-daily/
# data_sev_cas_civ <- read_excel("data/severity/UN_civ_casualties.xlsx") %>%
#   transmute(date = mdy(date),
#             sev_cas_civ_count = killed + injured) %>%
#   add_row(date = dmy("24-02-2022"), sev_cas_civ_count = 0, .before = 1) %>%
#   mutate(sev_cas_civ_count = sev_cas_civ_count - dplyr::lag(sev_cas_civ_count)) %>%
#   right_join(tidyr::expand(., date = full_seq(date, 1)), by = "date") %>%
#   arrange(date) %>%
#   fill(sev_cas_civ_count) %>%
#   mutate(sev_cas_civ_count = replace_na(sev_cas_civ_count, 0))

saveRDS(data_sev_cas_civ, "data/severity/data_civ_cas.RDS")

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

data_sev_cas_rus <- get_data_rus_cas(start = Sys.getenv("RUS_CAS_START"), end = Sys.getenv("RUS_CAS_END"))

saveRDS(data_sev_cas_rus, "data/severity/data_rus_cas.RDS")

# data_sev_cas_rus <- readRDS("data/severity/data_rus_cas.RDS")

data_sev_cas_rus_agg <- data_sev_cas_rus %>%
  group_by(type) %>%
  mutate(d_value = value - dplyr::lag(value),
         d_value = if_else(is.na(d_value), value, d_value)) %>%
  ungroup() %>%
  filter(type == "Military personnel") %>%
  select(date, sev_cas_rus_mil_count = d_value)

## Conflict events
# From https://acleddata.com/ukraine-crisis/#data
data_sev_confl_evs_count <- read_excel("data/severity/Ukraine_Black_Sea_2020_2022_Nov04.xlsx") %>%
  select(date = EVENT_DATE) %>%
  group_by(date) %>%
  summarise(sev_confl_evs_count = n())

saveRDS(data_sev_confl_evs_count, "data/severity/data_confl_evs.RDS")

data_severity <- data_sev_cas_civ %>%
  select(date, sev_cas_civ_count) %>%
  left_join(data_sev_cas_rus_agg, by = "date") %>%
  left_join(data_sev_confl_evs_count, by = "date") %>%
  filter(date <= "2022-10-31")

saveRDS(data_severity, "data/severity/data_severity.RDS")
