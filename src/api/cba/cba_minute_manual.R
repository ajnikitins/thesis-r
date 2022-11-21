source("src/api/cba/cba_minute_utils.R")

count_dupes <- \(rows_coll, f_date, f_type, f_value_uah, f_comment, fuzzy_comment = FALSE) {
  rows_coll %>%
    filter(date == f_date, type == f_type, value_uah == f_value_uah, (str_detect(comment, fixed(f_comment)) & fuzzy_comment) | (comment == f_comment & !fuzzy_comment)) %>%
    nrow()
}

process_pot_dupes_coll <- \(rows_coll) {
  rows_coll_short <- rows_coll %>%
    mutate(date = as_date(date)) %>%
    ungroup()

  rows_coll_short %>%
    distinct(date, type, value_uah, comment) %>%
    rowwise() %>%
    partition(cl) %>%
    mutate(count_dupes = count_dupes(rows_coll_short, date, type, value_uah, comment, fuzzy_comment = TRUE)) %>%
    # mutate(count_data = count_dupes(rows_coll_short, date, type, value_uah, comment, fuzzy_comment = FALSE)) %>%
    # filter(count_dupes == count_data) %>%
    mutate(actual_dupes = make_cba_req(card = 1, date_start = date, type = type, value_between = c(value_uah, value_uah), string_filter = comment, case_sensitive = TRUE)[[1]][[1]]) %>%
    collect() %>%
    filter(actual_dupes > count_dupes)
}

add_dupes <- \(rows_coll, pot_dupes) {
  rows_dupes <- pot_dupes %>%
    rowwise() %>%
    partition(cl) %>%
    mutate(rows = list(transform_rows(compact(make_cba_req(5, date_start = date, type = type, value_between = c(value_uah, value_uah), string_filter = comment, case_sensitive = TRUE))))) %>%
    collect() %>%
    pull(rows) %>%
    bind_rows()

  collate_rows_by_text(list(rows_coll, rows_dupes))
}

{
  cl <- new_cluster(20)
  cluster_copy(cl, c("make_cba_req", "transform_rows", "find_max_value", "split_by_value", "get_rows_by_text", "alph", "collate_rows_by_text", "count_dupes"))
  cluster_library(cl, c("tidyverse", "lubridate", "glue", "httr", "jsonlite"))
  unlink("logs/cba/*.txt")
  cluster_send(cl, sink(file(glue("logs/cba/{Sys.getpid()}.txt"), open = "wt"), type = "message"))
  # cluster_send(cl, closeAllConnections())
}

data_raw <- readRDS("data/cba/rows/data_cba_rows_text_alph_raw.RDS")

data_base <- data_raw %>%
  rowwise() %>%
  partition(cl) %>%
  mutate(rows = list(map(compact(rows), transform_rows))) %>%
  mutate(rows_coll = list(collate_rows_by_text(rows))) %>%
  mutate(count_coll = nrow(rows_coll),
         count_missing = count - count_coll) %>%
  collect()

# Further work with one's where collation was underperforming
data_unsafe <- data_base %>%
  filter(count_missing > 0) %>%
  filter(date == "2022-02-22") %>%
  # First get possible duplicates
  # Duplicates are possible for the last row in each query
  # count_dupes how many duplicates each row has (not how many time's it's there)
  mutate(pot_dupes = map(rows_coll, process_pot_dupes_coll)) #%>%

rows_text_alph_unsafe_dupes <- rows_text_alph_unsafe %>%
  # Naivelly just request more of the dupes and hope for the best
  mutate(rows_coll = map2(rows_coll, pot_dupes, add_dupes))

