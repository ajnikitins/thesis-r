library(tidyverse)
library(lubridate)
library(glue)
library(httr)
library(jsonlite)
library(future)

# source("taskq.R")

# Problem is with the relative time — it shifts as the program runs
# Need to implement overlaps between periods to allow for changes in minutes

plan(multisession)

buffer <- 0
cutoff <- 2000

time_end <- Sys.time()
time_start <- as_datetime("2021-12-31")

min_diff <- \(time1, time2) floor(difftime(time1, time2, units = "mins"))

get_count <- \(start_offset, duration) {
  url_1 <- glue('https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/1/card/1?parameters=%5B%7B%22type%22%3A%22date%2Fall-options%22%2C%22value%22%3A%22past{duration}minutes-from-{start_offset}minutes%22%2C%22target%22%3A%5B%22dimension%22%2C%5B%22field%22%2C77%2Cnull%5D%5D%2C%22id%22%3A%22b6f7e9ea%22%7D%5D')
  req <- GET(url_1)

  res <- fromJSON(rawToChar(req$content))

  res$data$rows[[1]]
}

get_period <- \(start_offset, duration, time_begin) {
  time_exec <- Sys.time()
  start_offset <- start_offset + min_diff(time_exec, time_begin)

  cat(glue("start_offset: {start_offset}, duration: {duration}"))
  if (start_offset < 0) start_offset <- 0

  count <- get_count(start_offset, duration)
  cat(glue(", count: {count} \n", .trim = FALSE))

  list(start_offset = start_offset, duration = duration, count = count, time_exec = time_exec, time_begin = time_begin)
}

periods <- list()

queue <- list(futureCall(get_period, list(0, min_diff(time_end, time_start), time_end)))

while (length(queue) > 0) {
  resolved_tasks <- which(resolved(queue))

  if (length(resolved_tasks) == 0) {
    print(glue("Waiting on {length(queue)} running tasks..."))
    Sys.sleep(0.01)
    next
  }

  it_resolved_tasks <- resolved_tasks
  while (length(it_resolved_tasks) > 0) {
    print(glue("Remaining resolved tasks: {length(it_resolved_tasks)-1}"))

    resolved_task <- it_resolved_tasks[[1]]

    period <- value(queue[[resolved_task]])

    if (period$count > cutoff) {
      queue <- append(queue, list(futureCall(get_period, list(period$start_offset - buffer, ceiling(period$duration / 2) + 2 * buffer, time_end)),
                      futureCall(get_period, list(period$start_offset + floor(period$duration / 2) - buffer, ceiling(period$duration / 2) + 2 * buffer, time_end))))
    } else {
      periods <- append(periods, list(period))
    }

    it_resolved_tasks <- it_resolved_tasks[-1]
  }

  queue <- queue[-resolved_tasks]
}



# url_5_base <- 'https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/5/card/5?parameters=[{"type":"date/all-options","value":"past200minutes"}]'
