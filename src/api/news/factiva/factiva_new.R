library(tidyverse)
library(lubridate)
library(glue)
library(RSelenium)

res <- data.frame(date = Date(), factiva_count = numeric())

dates <- seq(dmy("01012022"), dmy("28022023"), by = 1)

## Login into Factiva
browser <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  browserName = "firefox"
)
browser$open()

url_1 <- "https://www.sciencespo.fr/bibliotheque/fr/rechercher/eressources/#/resource/26"
browser$navigate(url_1)

elem_button <- browser$findElement(using = "css", value = "a[class = 'layout-gt-sm-row layout-column']")
browser$navigate(elem_button$getElementAttribute("href")[[1]])

browser$findElement(using = "id", value = "username")$sendKeysToElement(list(Sys.getenv("FACTIVA_USERNAME")))
browser$findElement(using = "id", value = "password")$sendKeysToElement(list(Sys.getenv("FACTIVA_PWD"), "\uE007"))

Sys.sleep(15)

walk(dates, \(date) {
  date_string <- format(date, "%d%m%Y")

  url_2 <- "https://global-factiva-com.acces-distant.sciencespo.fr/en/du/headlines.asp?napc=S&searchText=ukraine+and+war&exclude=Obituaries|Recurring|Republished&dateRangeMenu=custom&dateFormat=dmy&dateFrom={date_string}&dateTo={date_string}&sortBy=y&currentSources=U%7c%7eMRKRE%2c%7eNFINCE&currentSourcesDesc=sc_u_MRKRE%2cMarketResearch.com+(Abstracts)%3bsc_u_NFINCE%2cCE+NoticiasFinancieras+(Latin+America)&searchLanguage=custom&searchLang=&dedupe=2&srchuiver=2&accountid=9FON000600&namespace=16"

  browser$navigate(url_2)
  Sys.sleep(15)

  result <- browser$findElements(using = "xpath", value = "//script") %>%
    map(~ unlist((.)$getElementAttribute("innerHTML"))) %>%
    `[[`(73) %>%
    str_extract("(?<=C:)\\d*")

  print(glue("Date: {date}, factiva_count: {result}"))
  res <<- add_row(res, date = date, factiva_count = as.numeric(result))

})

browser$close()

saveRDS(res, "data/news/factiva.RDS")
