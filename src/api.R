library(httr)
library(glue)
library(jsonlite)
library(purrr)

## Addresses
addresses <- read.csv("../data/addresses.csv", header = TRUE, sep = ",")

## BTC
btc_addresses <- addresses %>%
  filter(coin == "BTC")

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

saveRDS(btc_data, "../data/data_btc.RDS")

btc_data_txs <- lapply(btc_data, \(set) {
  txs <- set$txs
  if (length(txs) == 0 | set$n_tx < 100) return(NULL)

  txs$address <- set$address
  txs$name <- (subset(addresses, address == set$address))[["name"]]

  txs
}) %>%
  reduce(rbind)

saveRDS(btc_data_txs, "../data/data_btc_txs.RDS")

## ETHER
# eth_res <- GET("https://api.etherscan.io/api",
#                query = list(module = "account",
#                             action = "txlist",
#                             address = "0x165CD37b4C644C2921454429E7F9358d18A45e14",
#                             offset = 0,
#                             page = 3,
#                             sort = "desc",
#                             apikey = "ZMEFGIGKHAQZ8W48EQED8WPCMH41QD579F"
#                ))

eth_res <- GET("https://api.covalenthq.com/",
               path = "v1/1/address/0x165CD37b4C644C2921454429E7F9358d18A45e14/transactions_v2/",
               query = list(key = "ckey_7c4c50530dd4418fbb30d995325", "no-logs" = "true", "page-size" = 99999)
)

# curl -X GET https://api.covalenthq.com/v1/1/address/0x165CD37b4C644C2921454429E7F9358d18A45e14/transactions_v2/?quote-currency=USD&format=JSON&block-signed-at-asc=false&no-logs=true&page-size=10&page-number=1&key=ckey_7c4c50530dd4418fbb30d995325
# \ -H "Accept: application/json"

eth_data <- fromJSON(rawToChar(eth_res$content))


## USDT

usdt_res <- GET("https://api.covalenthq.com/",
                path = "v1/1/address/0x165CD37b4C644C2921454429E7F9358d18A45e14/transfers_v2/",
                query = list(key = "ckey_7c4c50530dd4418fbb30d995325",
                             # "no-logs" = "true",
                             "page-size" = 99999,
                             "contract-address" = "0xdac17f958d2ee523a2206206994597c13d831ec7")
)

usdt_data <- fromJSON(rawToChar(usdt_res$content))


## Twitter

dotenv::load_dot_env()

bearer_token <- Sys.getenv("BEARER_TOKEN")
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))


##
