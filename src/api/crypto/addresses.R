library(tidyverse)
library(rvest)

get_crypto_addresses <- \() {
  charities <- read_html("https://crystalblockchain.com/verified-ukraine-donation-programs-list/") %>%
    html_elements(".un-block_content--organizations") %>%
    map(html_elements, ".g-row") %>%
    reduce(\(acc, charity) {
      # Get name of charity
      name <- charity %>% html_elements("h3") %>% html_text2()
      # Get coins
      coins <- charity[-1] %>% html_element(".g-cols--2") %>% html_text2()
      # Get addresses
      addresses <- charity[-1] %>% html_element(".g-cols--10") %>% html_text2() %>% tolower()

      if (length(addresses) > 0) {
        out <- data.frame(name = name, coin = coins, address = addresses)
        bind_rows(acc, out)
      } else {
        acc
      }
    }, .init = NULL) %>%
    separate_rows(coin, sep = ", ") %>%
    separate_rows(address, sep = "\\n")
}

crypto_addresses <- get_crypto_addresses()
write.csv(crypto_addresses, "data/crypto/addresses.csv", row.names = FALSE)
