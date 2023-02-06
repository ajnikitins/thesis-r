library(tidyverse)
library(lubridate)

# Merge fiat and crypto donations
data_cba <- readRDS("data/cba/data_cba_rows_usd.RDS")
data_crypto <- readRDS("data/crypto/data_crypto.RDS")

data_donations_hourly_sub <- bind_rows(data_cba, data_crypto) %>%
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
      grepl("UAH|EUR|USD", currency) ~ currency,
      grepl( "Bitcoin", currency) ~ "BTC",
      grepl( "Ethereum", currency) ~ "ETH",
      TRUE ~"Other"
    ),
    .keep = "unused") %>%
  # Filter to period
  filter(date %within% interval("2022-01-01 00:00:00", "2022-10-31 23:59:59")) %>%
  # Trim bottom and top 1% of donations
  group_by(type, type_sub) %>%
  filter(dplyr::between(value_usd, quantile(value_usd, 0.01), quantile(value_usd, 0.99))) %>%
  # Calculate daily counts and means
  group_by(type, type_sub, date) %>%
  summarise(don_count = n(), don_total = sum(value), don_total_usd = sum(value_usd), don_mean = don_total / don_count, don_mean_usd = don_total_usd / don_count, .groups = "drop") %>%
  right_join(tidyr::expand(., nesting(type, type_sub), date = full_seq(date, 3600)), by = c("type", "type_sub", "date")) %>%
  mutate(across(c(-date, -type, -type_sub), ~ replace_na(., 0))) %>%
  arrange(type, type_sub, date)

saveRDS(data_donations_hourly_sub, "data/data_donations_hourly_sub.RDS")

aggregate_donations <- \(main, grouping_vars, date_floor) {
    main %>%
      mutate(date = floor_date(date, unit = date_floor)) %>%
      group_by(across(all_of(grouping_vars)), date) %>%
      summarise(across(c(don_count, don_total, don_total_usd), sum), don_mean = don_total / don_count, don_mean_usd = don_total_usd / don_count, .groups = "drop") %>%
      mutate(across(-c(date, grouping_vars), ~ replace_na(., 0))) %>%
      arrange(across(grouping_vars), date)
}

aggregations_donations <- data.frame(
  name = c("data_donations_hourly", "data_donations_sub", "data_donations"),
  grouping_vars = I(list(c("type"), c("type", "type_sub"), c("type"))),
  date_floor = c("hours", "days", "days")
) %>%
  rowwise() %>%
  mutate(aggregated = list(aggregate_donations(data_donations_hourly_sub, grouping_vars, date_floor)))

walk2(aggregations_donations$aggregated, aggregations_donations$name, ~ saveRDS(.x, glue("data/{.y}.RDS")))
