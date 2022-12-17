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

data_eth_txs <- data_eth %>%
  mutate(type = "Ethereum") %>%
  # Filter to incoming transactions
  filter(to_address == address) %>%
  # Potentially filter out token transactions?
  filter(value_quote > 0) %>%
  select(type, name, time, value_usd = value_quote)

saveRDS(data_eth_txs, "data/crypto/data_eth_txs.RDS")
