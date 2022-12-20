library(tidyverse)

# Load crypto data
data_btc_txs <- readRDS("data/crypto/data_btc_txs.RDS")
data_eth_txs <- readRDS("data/crypto/data_eth_txs.RDS")

# TODO: Rename United24 and CBA addresses in .csv

# Merge and aggregate
data_crypto_merged <- bind_rows(data_btc_txs, data_eth_txs) %>%
  filter(name == "Official crypto wallets of Ukraine" | name == "The Return Alive Foundation") %>%
  mutate(name = case_when(
           name == "The Return Alive Foundation" ~ "CBA",
           name == "Official crypto wallets of Ukraine" ~ "United24"
         ),
         date = as_date(floor_date(time, "day")), .keep = "unused") %>%
  unite(currency, name, currency, sep = "_") %>%
  # Remove outliers
  # 2022-04-04 (2 >1 000 000 donations for U24 eth and btc wallets)
  # 2022-07-08 (2 >250 000 donation for CBA btc wallet)
  # 2022-07-30 (2 >250 000 donation for U24 btc wallet)
  # filter((date != "2022-04-04" & date != "2022-07-08" & date != "2022-07-30") | value_usd < 250000) %>%
  I()

data_crypto_agg <- data_crypto_merged %>%
  # group_by(type, date) %>%
  # summarise(count = n(), mean_usd = sum(value_usd) / count, .groups = "drop") %>%
  # right_join(expand(., type, date = seq(dmy("01-01-2022"), max(date), by = 1)), by = c("type", "date")) %>%
  # mutate(across(c(count, mean_usd), ~ replace_na(., 0))) %>%
  # mutate(across(c(value_usd), ~ replace_na(., 0))) %>%
  arrange(currency, date)

saveRDS(data_crypto_agg, "data/crypto/data_crypto.RDS")
