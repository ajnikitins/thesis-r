library(tidyverse)
library(lubridate)
library(glue)

# Merge fiat and crypto donations
data_cba <- read_rds("data/cba/data_cba_rows.RDS")
data_crypto <- read_rds("data/crypto/data_crypto.RDS")

data_donations_base <- bind_rows(data_cba, data_crypto) %>%
  mutate(date = ymd_hms(date),
         date = floor_date(date, unit = "hours")) %>%
  # Setup type names
  mutate(
    type = case_when(
      grepl("UAH", currency) ~ "Ukrainian",
      grepl("Bitcoin|Ethereum", currency) ~ "Crypto",
      TRUE ~ "Foreign"
    ),
    type_sub = case_when(
      grepl( "Bitcoin", currency) ~ "BTC",
      grepl( "Ethereum", currency) ~ "ETH",
      TRUE ~ currency
    ),
    .keep = "unused") %>%
  # Filter to period
  filter(date %within% interval("2022-01-01 00:00:00", "2023-02-28 23:59:59")) %>%
  # Trim bottom and top 1% of donations
  group_by(type, type_sub) %>%
  # filter(dplyr::between(value_usd, quantile(value_usd, 0.01), quantile(value_usd, 0.99))) %>%
  filter(dplyr::between(value_usd, 0, quantile(value_usd, 0.99))) %>%
  ungroup()

aggregate_donations <- \(main, grouping_vars, date_floor, date_expand) {
    date_unit <- switch(date_floor, days = 24 * 3600, hours = 3600)

    result <- main %>%
      mutate(date = floor_date(date, unit = date_floor)) %>%
      group_by(across(all_of(grouping_vars)), date) %>%
      summarise(don_count = n(), don_total = sum(value), don_total_usd = sum(value_usd), don_mean = don_total / don_count, don_mean_usd = don_total_usd / don_count, .groups = "drop")

    if (date_expand) {
        result <- result %>%
          right_join(tidyr::expand(., nesting(!!!syms(grouping_vars)), date = full_seq(date, date_unit)), by = c(grouping_vars, "date")) %>%
          mutate(across(-c(date, all_of(grouping_vars)), ~ replace_na(., 0)))
    }

    arrange(result, across(all_of(grouping_vars)), date)
}

data_donations_aggregations <- tribble(
  ~name,                        ~grouping_vars,                 ~date_floor,    ~date_expand,
  "data_donations_hourly_sub",  c("type", "type_sub"),          "hours",        TRUE,
  "data_donations_hourly",      "type",                         "hours",        TRUE,
  "data_donations_id",          c("type", "type_sub", "id"),    "days",         FALSE,
  "data_donations_sub",         c("type", "type_sub"),          "days",         TRUE,
  "data_donations",             "type",                         "days",         TRUE
) %>%
  rowwise() %>%
  mutate(aggregated = list(aggregate_donations(data_donations_base, grouping_vars, date_floor, date_expand)))

walk2(data_donations_aggregations$aggregated, data_donations_aggregations$name, ~ write_rds(.x, glue("data/{.y}.RDS")))
