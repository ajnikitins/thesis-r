library(tidyverse)
library(lubridate)
library(glue)
library(httr)
library(jsonlite)
library(future)

# source("taskq.R")

# Okay, relative time might not work reliably




# Problem is with the relative time — it shifts as the program runs
# Need to implement overlaps between periods to allow for changes in minutes

plan(sequential)

# Get list of types
url_cba_types <- "https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/params/d41494b/values"
types_req <- GET(url_cba_types)
types <- content(types_req, "parsed")$values

buffer <- 0
cutoff <- 2000

time_end <- Sys.time()
time_start <- as_datetime("2021-12-31")

min_diff <- \(time1, time2) floor(difftime(time1, time2, units = "mins"))

get_count <- \(start_offset, duration, type) {
  url_query <- '[{"type":"date/all-options","value":"past<<duration>>minutes-from-<<start_offset>>minutes","target":["dimension",["field",77,null]],"id":"b6f7e9ea"},{"type":"string/=","value":["<<type>>"],"target":["dimension",["field",78,null]],"id":"64daade5"}]'

  req <- GET("https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/1/card/1",
                   query = list(parameters = glue(url_query, .open = "<<", .close = ">>")))
  # url <- glue('https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/1/card/1?parameters=%5B%7B%22type%22%3A%22date%2Fall-options%22%2C%22value%22%3A%22past{duration}minutes-from-{start_offset}minutes%22%2C%22target%22%3A%5B%22dimension%22%2C%5B%22field%22%2C77%2Cnull%5D%5D%2C%22id%22%3A%22b6f7e9ea%22%7D%5D')
  # req <- GET(url)

  res <- content(req, "parsed")

  res$data$rows[[1]]
}

get_period <- \(start_offset, duration, time_begin, type) {
  time_exec <- Sys.time()
  dilation <- min_diff(time_exec, time_begin)

  cat(glue("type: {type}, start_offset: {start_offset}, dilation: {dilation}, duration: {duration}"))
  if (start_offset + dilation < 0) start_offset <- -dilation

  count <- tryCatch(
    {
      get_count(start_offset + dilation, duration, type)
    },
    error = \(cond) {
      message("Something went wrong, will retry this period")
      message("Original error:")
      message(cond)
      return(NULL)
    }
  )

  cat(glue(", count: {count} \n", .trim = FALSE))

  list(start_offset = start_offset, duration = duration, count = count, time_exec = time_exec, time_begin = time_begin, type = type)
}

periods_types <- types %>%
  map(\(type) {

    periods <- list()

    queue <- list(futureCall(get_period, list(0, min_diff(time_end, time_start), time_end, type)))

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

        if (is.null(period$count)) {
          queue <- append(queue, list(futureCall(get_period, list(period$start_offset, period$duration, time_end, period$type))))
        } else if (period$count > cutoff) {
          queue <- append(queue, list(futureCall(get_period, list(period$start_offset - buffer, ceiling(period$duration / 2) + 2 * buffer, time_end, period$type)),
                          futureCall(get_period, list(period$start_offset + floor(period$duration / 2) - buffer, ceiling(period$duration / 2) + 2 * buffer, time_end, period$type))))
        } else {
          periods <- append(periods, list(period))
        }

        it_resolved_tasks <- it_resolved_tasks[-1]
      }

      queue <- queue[-resolved_tasks]
    }
    periods
  })

# get_data_cba_minute <- \(base_periods = NULL, time_start = as_datetime("2021-12-31"), time_end = Sys.time(), cutoff = 2000, buffer = 0) {
#
#   verify_period <- \(system, period) {
#     if (period$count > cutoff) {
#       system$queue <- append(system$queue, list(futureCall(get_period, list(period$start_offset - buffer, ceiling(period$duration / 2) + 2 * buffer, time_end)),
#                                   futureCall(get_period, list(period$start_offset + floor(period$duration / 2) - buffer, ceiling(period$duration / 2) + 2 * buffer, time_end))))
#     } else {
#       system$periods <- append(system$periods, list(period))
#     }
#   }
#
#
#   if (is.null(periods)) {
#     base_periods <- get_period(0, min_diff(time_end, time_start), time_end)
#   }
#
#   system <- list(queue = list(), periods = list())
#
#   system <- base_periods %>%
#     map(\(period) list(start_offset = period$start_offset + min_diff(time_end, period$time_begin), duration = period$duration, count = period$count, time_exec = time_end, time_begin = time_end))
#     reduce(verify_period, .init = system)
#
#   while (length(system$queue) > 0) {
#     resolved_tasks <- which(resolved(system$queue))
#
#     if (length(resolved_tasks) == 0) {
#       print(glue("Waiting on {length(system$queue)} running tasks..."))
#       Sys.sleep(0.01)
#       next
#     }
#
#     system <- system$
#
#     it_resolved_tasks <- resolved_tasks
#     while (length(it_resolved_tasks) > 0) {
#       print(glue("Remaining resolved tasks: {length(it_resolved_tasks)-1}"))
#
#       resolved_task <- it_resolved_tasks[[1]]
#
#       period <- value(queue[[resolved_task]])
#
#       if (period$count > cutoff) {
#         queue <- append(queue, list(futureCall(get_period, list(period$start_offset - buffer, ceiling(period$duration / 2) + 2 * buffer, time_end)),
#                                     futureCall(get_period, list(period$start_offset + floor(period$duration / 2) - buffer, ceiling(period$duration / 2) + 2 * buffer, time_end))))
#       } else {
#         periods <- append(periods, list(period))
#       }
#
#       it_resolved_tasks <- it_resolved_tasks[-1]
#     }
#
#     queue <- queue[-resolved_tasks]
#   }
#
# }


# url_5_base <- 'https://report.comebackalive.in.ua/api/public/dashboard/e4f44bc7-05f4-459b-a10b-cc10e6637217/dashcard/5/card/5?parameters=[{"type":"date/all-options","value":"past200minutes"}]'
