library(tidyverse)
library(httr)
library(glue)
library(jsonlite)
library(priceR)
library(lubridate)

# Load addresses
btc_addresses <- read.csv("data/crypto/addresses.csv", header = TRUE, sep = ",") %>%
  filter(coin == "Bitcoin")

# Query Blockchain.info API to get transactions
get_btc_data <- \(address) {
  print(glue("Loading txs for {address}"))

  btc_data <- list()
  btc_offset <- 0
  repeat {
    print(glue("Offset {btc_offset}"))

    btc_res <- GET("https://blockchain.info/",
                   path = glue("address/{address}"),
                   query = list(limit = 5000, offset = btc_offset, format = "json"))

    btc_data_part <- fromJSON(rawToChar(btc_res$content))

    btc_data_part$txs <- rbind(btc_data$txs, btc_data_part$txs)
    btc_data <- btc_data_part

    btc_offset <- btc_offset + 5000
    if (btc_offset >= btc_data_part$n_tx) break
  }

  btc_data
}

# Get raw BTX transaction data
btc_data <- lapply(btc_addresses$address, get_btc_data)
saveRDS(btc_data, "data/crypto/data_btc.RDS")

# btc_data <- readRDS("data/crypto/data_btc.RDS")

# Load BTC/USD exchange rate data
btc_data_rate <- read.csv("data/crypto/Bitstamp_BTCUSD_2022_minute.csv", skip = 1, header = TRUE) %>%
  mutate(date = as_datetime(date))

# Collect txs data into a single data frame
btc_data_txs_raw <- lapply(btc_data, \(set) {
  txs <- set$txs
  if (length(txs) == 0) return(NULL)

  txs$time <- as_datetime(txs$time)

  # Converts to USD using minute exchange rates
  txs$exch_rate <- btc_data_rate[match(floor_date(txs$time, unit = "minute"), btc_data_rate$date), "close"]
  txs$result_usd <- txs$result / 100000000 * txs$exch_rate
  txs$name <- (subset(btc_addresses, address == tolower(set$address)))[["name"]]

  txs
}) %>%
  reduce(bind_rows)

# Process txs data for saving
btc_data_txs <- btc_data_txs_raw %>%
  mutate(type = "Bitcoin") %>%
  select(type, name, time, value_usd = result_usd) %>%
  # Filter to incoming transactions
  filter(value_usd > 0)

saveRDS(btc_data_txs, "data/crypto/data_btc_txs.RDS")
