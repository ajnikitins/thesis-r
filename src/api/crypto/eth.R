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
  page <- 0

  repeat {
    print(glue("Loading page {page} of txs for {address}"))
    eth_res <- GET("https://api.covalenthq.com/",
                   path = glue("v1/1/address/{address}/transactions_v2/"),
                   query = list(key = Sys.getenv("COVALENT_API_KEY"), "page-size" = 500, "page-number" = page, "block-signed-at-asc" = TRUE))
    eth_data_text <- content(eth_res, "text", encoding = "UTF-8")
    write(eth_data_text, glue("data_manual/crypto/eth/{address}_{page}.json"))

    if(str_detect(eth_data_text, fixed('error":true'))) {
      warning("Failed to get data, try a smaller page size; retrying...")
    } else if (str_detect(eth_data_text, fixed("504: Gateway time-out"))) {
      warning("Gateway timeout; Sleeping for 10 seconds & retrying...")
      Sys.sleep(10)
    } else if (!str_detect(eth_data_text, fixed('has_more":true'))) {
      message("Done! Moving on...")
      break
    } else {
      page <- page + 1
    }
  }
}

# Gather ETH txs data into a single data frame
load_eth_data <- \() {
  files <- list.files("data_manual/crypto/eth", full.names = TRUE)

  data_eth_raw <- map(files, read_json)

  data_eth <- reduce(data_eth_raw, \(acc, part) {
    address <- part$data$address
    address_name <- eth_addresses$name[which(eth_addresses$address == address)]

    if (length(part$data$items) == 0) {
      return(acc)
    }

    txs <- part$data$items %>%
      tibble(txs = .) %>%
      unnest_wider(txs) %>%
      mutate(address = address,
             name = address_name,
             to_address = tolower(to_address),
             time = ymd_hms(block_signed_at),
             .keep = "unused")

    acc <- bind_rows(acc, txs)
  }, .init = NULL)

  data_eth %>%
    arrange(name, time)
}

# Collect ETH data
walk(eth_addresses$address, get_eth_data)

# Load ETH data
data_eth <- load_eth_data()
saveRDS(data_eth, "data/crypto/data_eth.RDS")

# data_eth <- readRDS("data/crypto/data_eth.RDS")

# Get all transactions with log events (assume those are token transactions)
data_eth_tok_full <- data_eth %>%
  filter(!map_lgl(log_events, is.null))

write_json(data_eth_tok_full, "data/crypto/data_eth_tok_full.json")

# Process token transactions
data_eth_tok <- data_eth_tok_full %>%
  # filter(value > 0) %>%
  select(time, name, address, tx_hash, from_address, to_address, value, value_quote, log_events) %>%
  unnest_longer(log_events) %>%
  # hoist(log_events, log_offset = "log_offset", contract_decimals = "sender_contract_decimals", contract_name = "sender_name", contract_ticker = "sender_contract_ticker_symbol", contract_address = "sender_address", "decoded") %>%
  hoist(log_events, "log_offset", "sender_contract_decimals", "sender_name", "sender_contract_ticker_symbol", "sender_address", "decoded") %>%
  select(-log_events) %>%
  # hoist(decoded, contract_action = "name", contract_action_params = "params") %>%
  hoist(decoded, decoded_action = "name", decoded_action_signature = "signature", decoded_action_params = "params") %>%
  # select(-decoded) %>%
  I()

# # Get unique examples of contract actions
# tmp <- data_eth_tok %>%
#   distinct(decoded_action, decoded_action_signature, .keep_all = TRUE) %>%
#   unnest_longer(decoded_action_params) %>%
#   hoist(decoded_action_params, param_name = "name", param_type = "type", param_value = "value") %>%
#   select(-decoded_action_params)

data_eth_txs <- data_eth %>%
  mutate(currency = "Ethereum",
         value = as.numeric(value) / 10^18) %>%
  # Filter to incoming transactions
  filter(from_address != address) %>%
  # Potentially filter out token transactions?
  filter(value_quote > 0) %>%
  select(currency, name, time, value, value_usd = value_quote)

saveRDS(data_eth_txs, "data/crypto/data_eth_txs.RDS")
