library(tidyverse)

# Load crypto data
data_btc_txs <- readRDS("data/crypto/data_btc_txs.RDS")
data_eth_txs <- readRDS("data/crypto/data_eth_txs.RDS")

# TODO: Rename United24 and CBA addresses in .csv

data_crypto <- bind_rows(data_btc_txs, data_eth_txs) %>%
  filter(name == "Official crypto wallets of Ukraine" | name == "The Return Alive Foundation") %>%
  mutate(name = case_when(
           name == "The Return Alive Foundation" ~ "CBA",
           name == "Official crypto wallets of Ukraine" ~ "United24"
         ),
         date = as_date(floor_date(time, "day")), .keep = "unused") %>%
  unite(type, name, type, sep = "_") %>%
  group_by(type, date) %>%
  summarise(count = n(), mean_usd = sum(value_usd) / count, .groups = "drop") %>%
  right_join(expand(., type, date = seq(dmy("01-01-2022"), max(date), by = 1)), by = c("type", "date")) %>%
  mutate(across(c(count, mean_usd), ~ replace_na(., 0))) %>%
  arrange(type, date)

saveRDS(data_crypto, "data/crypto/data_crypto.RDS")
