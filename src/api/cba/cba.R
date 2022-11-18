library(tidyverse)
library(httr)
library(glue)
library(jsonlite)
library(priceR)
library(lubridate)
library(foreach)
# library(doFuture)
# library(doParallel)

# cl <- parallel::makeCluster(16, outfile="")
# registerDoParallel(cl)
# stopCluster(cl)

registerDoSEQ()

# plan(multisession)
# registerDoFuture()



START_DATE <- Sys.getenv("CBA_START")
END_DATE <- Sys.getenv("CBA_END")

## Aggregated graph level ----

get_data_cba_value <- \(start, end) {
  url_base_cba <- "https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/6/card/6?parameters=%5B%7B%22type%22%3A%22date%2Fall-options%22%2C%22value%22%3A%22{start}~{end}%22%2C%22target%22%3A%5B%22dimension%22%2C%5B%22field%22%2C77%2Cnull%5D%5D%2C%22id%22%3A%22b6f7e9ea%22%7D%5D"

  res <- GET(glue(url_base_cba))
  content <- fromJSON(rawToChar(res$content))

  data_cba_value <- content$data$rows %>%
    as.data.frame() %>%
    set_names(content$data$cols$name) %>%
    mutate(sum_usd = convert_currencies(as.numeric(sum), from = "UAH", to = "USD", date = as.Date(date)))
}

data_cba_value <- get_data_cba_value(START_DATE, END_DATE)
saveRDS(data_cba_value, "data/cba/data_cba_value.RDS")

get_data_cba_count <- \(start, end) {
  url_cba_types <- "https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/params/64daade5/values"
  types_req <- GET(url_cba_types)
  types <- content(types_req, "parsed")$values

  dates <- seq(as_date(start), as_date(end), 1)

  url_cba_query <- '[{"type":"date/all-options","value":"<<date>>","target":["dimension",["field",77,null]],"id":"b6f7e9ea"},{"type":"string/=","value":["<<type>>"],"target":["dimension",["field",78,null]],"id":"64daade5"}]'

  counts <- types %>%
    reduce(\(acc, type) {
      counts <- foreach(date = dates, .export = c("type", "url_cba_query"), .combine = "c", .packages = c("tidyverse", "glue", "httr")) %dopar% {
        print(glue("type: {type}, date: {date}"))

        res <- GET("https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/1/card/1",
                   query = list(parameters = glue(url_cba_query, .open = "<<", .close = ">>")))
        content <- content(res, "parsed")

        # tryCatch(\() content$data, error = print(content))

        content$data$rows[[1]][[1]]
      }
      bind_rows(acc, data.frame(date = dates, type = type, count = counts))
    }, .init = NULL)
}

data_cba_count <- get_data_cba_count(START_DATE, END_DATE)
saveRDS(data_cba_count, "data/cba/data_cba_count.RDS")
