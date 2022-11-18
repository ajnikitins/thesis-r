library(tidyverse)
library(httr)
library(jsonlite)
library(priceR)
library(glue)
library(lubridate)

# TODO: Filter out token transactions

# Load addresses
eth_addresses <- read.csv("data/crypto/addresses.csv", header = TRUE, sep = ",") %>%
  filter(coin == "Ethereum")

# Query CovalentHQ API to get transactions
get_eth_data <- \(address) {
  print(glue("Loading txs for {address}"))

  eth_res <- GET("https://api.covalenthq.com/",
                 path = glue("v1/1/address/{address}/transactions_v2/"),
                 query = list(key = Sys.getenv("COVALENT_API_KEY"), "no-logs" = "true", "page-size" = 99999))

  eth_data <- fromJSON(rawToChar(eth_res$content))
}

eth_data <- lapply(eth_addresses$address, get_eth_data)

saveRDS(eth_data, "data/crypto/data_eth.RDS")

# eth_data <- readRDS("data/crypto/data_eth.RDS")

# Collect txs data into a single data frame
eth_data_txs_raw <- lapply(eth_data, \(set) {
  txs <- set$data$items
  if (length(txs) == 0) return(NULL)

  txs$time <- as_datetime(txs$block_signed_at)
  txs$address <- set$data$address
  txs$name <- (subset(eth_addresses, address == set$data$address))[["name"]]

  txs
}) %>%
  reduce(rbind)

eth_data_txs <- eth_data_txs_raw %>%
  mutate(type = "Ethereum") %>%
  # Filter to incoming transactions
  filter(tolower(to_address) == address) %>%
  # Potentially filter out token transactions?
  filter(value_quote > 0) %>%
  select(type, name, time, value_usd = value_quote)

saveRDS(eth_data_txs, "data/crypto/data_eth_txs.RDS")
