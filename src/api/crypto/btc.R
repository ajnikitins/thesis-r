library(httr)
library(glue)
library(jsonlite)
library(purrr)
library(priceR)
library(lubridate)

# Load addresses
btc_addresses <- read.csv("data/addresses.csv", header = TRUE, sep = ",") %>%
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

btc_data <- lapply(btc_addresses$address, get_btc_data)

saveRDS(btc_data, "data/crypto/addresses.csv")

# Collect txs data into a single data frame
btc_data_txs <- lapply(btc_data, \(set) {
  txs <- set$txs
  if (length(txs) == 0) return(NULL)

  # Converts to USD using daily exchange rates
  # TODO: Check whether need to convert to instantaneous USD
  txs$result_usd <- convert_currencies(txs$result / 100000000, from = "BTC", to = "USD", date = as_date(as_datetime(txs$time)))
  txs$address <- set$address
  txs$name <- (subset(btc_addresses, address == set$address))[["name"]]

  txs
}) %>%
  reduce(bind_rows)

saveRDS(btc_data_txs, "data/crypto/data_btc_txs.RDS")
