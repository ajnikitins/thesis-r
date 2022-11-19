library(tidyverse)
library(lubridate)
library(glue)
library(RSelenium)
# library(doParallel)

# cl <- makeCluster(16)
# registerDoParallel(cl)
#
# dates_tot <- seq(dmy("01012022"), dmy("31102022"), by = 1) %>%
#   split(ceiling(seq_along(.)%%16))
#
# foreach(dates = dates_tot, .combine = "c", .packages = c("tidyverse", "lubridate", "glue", "RSelenium")) %dopar% {
#
# }
#
# dates <- dates_tot
#
# map(dates, \(date) {
#
# })
#
# date <- dates[1]

## Currently, this is a semi-automated setup
# Remember to start the selenium server
# Start with the broadest date period needed, wait until it crashes and the restart from a later date

res <- data.frame(date = Date(), factiva_count = numeric())

# dates <- seq(dmy("01012022"), dmy("31102022"), by = 1)
dates <- seq(dmy("22102022"), dmy("31102022"), by = 1)

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

Sys.sleep(20)

browser$findElement(using = "class", value = "ace_text-input")$sendKeysToElement(list("ukraine and war"))
browser$findElement(using = "css", value = "option[value = 'Custom']")$clickElement()

walk(dates, \(date) {
  elems <- c("frd", "frm", "fry", "tod", "tom", "toy") %>%
    map(~ browser$findElement(using = "id", value = .))

  walk(elems, ~ (.)$clearElement())

  walk(elems[c(1,4)], ~ (.)$sendKeysToElement(list(format(date, "%d"))))
  walk(elems[c(2,5)], ~ (.)$sendKeysToElement(list(format(date, "%m"))))
  walk(elems[c(3,6)], ~ (.)$sendKeysToElement(list(format(date, "%Y"))))

  browser$findElement(using = "id", value = "btnSBSearch")$clickElement()

  Sys.sleep(5)

  result <- browser$findElements(using = "xpath", value = "//script") %>%
    map(~ unlist((.)$getElementAttribute("innerHTML"))) %>%
    `[[`(73) %>%
    str_extract("(?<=C:)\\d*")

  print(glue("Date: {date}, factiva_count: {result}"))
  res <<- add_row(res, date = date, factiva_count = as.numeric(result))

  Sys.sleep(4)
  browser$findElement(using = "id", value = "btnModifySearch")$clickElement()
  Sys.sleep(1)
})

browser$close()

saveRDS(res, "data/factiva.RDS")
