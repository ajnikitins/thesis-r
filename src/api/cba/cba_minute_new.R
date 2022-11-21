source("src/api/cba/cba_minute_utils.R")

cl <- new_cluster(20)
cluster_copy(cl, c("make_cba_req", "transform_rows", "find_max_value", "split_by_value", "get_rows_by_text", "alph", "collate_rows_by_text", "count_dupes"))
cluster_library(cl, c("tidyverse", "lubridate", "glue", "httr", "jsonlite"))
unlink("logs/cba/*.txt")
cluster_send(cl, sink(file(glue("logs/cba/{Sys.getpid()}.txt"), open = "wt"), type = "message"))
# cluster_send(cl, closeAllConnections())


# 1. Total number per day
# 2. Per type
# 3. Per amount
# 4. Text magics (brute force)?

# Period = {date, count, type, value_start, value_end, filter}

periods <- NULL

## 1. Filter based on total daily value
data_cba_count <- readRDS("data/cba/data_cba_count.RDS") %>%
  pivot_wider(names_from = type, values_from = count) %>%
  mutate(`Solidgate Card` = `Solidgate Foreign` + `Solidgate UKR`, .keep = "unused") %>%
  pivot_longer(-date, names_to = "type", values_to = "count")

data_cba_count_agg <- data_cba_count %>%
  group_by(date) %>%
  summarise(count = sum(count))

rows_agg <- data_cba_count_agg %>%
  filter(count <= 2000 & count > 0) %>%
  rowwise() %>%
  partition(cl) %>%
  mutate(rows = list(transform_rows(make_cba_req(card = 5, date_start = date)))) %>%
  collect() %>%
  pull(rows) %>%
  bind_rows()

saveRDS(rows_agg, "data/cba/rows/data_cba_rows_agg.RDS")

## 2. Filter based on total daily value per type
data_cba_count_type <- data_cba_count_agg %>%
  filter(count > 2000) %>%
  select(-count) %>%
  left_join(data_cba_count, by = "date")

rows_type <- data_cba_count_type %>%
  filter(count <= 2000 & count > 0) %>%
  rowwise() %>%
  partition(cl) %>%
  mutate(rows = list(transform_rows(make_cba_req(card = 5, date_start = date, type = type)))) %>%
  collect() %>%
  pull(rows) %>%
  bind_rows()

saveRDS(rows_type, "data/cba/rows/data_cba_rows_type.RDS")

## 3. Filter based on total daily value per type and with value
data_cba_count_value <- data_cba_count_type %>%
  filter(count > 2000) %>%
  rowwise() %>%
  # Find maximum value of all periods
  mutate(value_max = ceiling(find_max_value(date, type, count)))

saveRDS(data_cba_count_value, "data/cba/data_cba_count_value.RDS")
data_cba_count_value <- readRDS("data/cba/data_cba_count_value.RDS")

# This has some overlaps, but oh, well
data_cba_count_value_split <- data_cba_count_value %>%
  rowwise() %>%
  partition(cl) %>%
  # filter(date == "2022-02-28" & type == "Oschad UAH") %>%
  mutate(splits = list(split_by_value(date, type, count, value_max))) %>%
  collect() %>%
  unnest_longer(splits) %>%
  hoist(splits, "value_min", "value_max", "count") %>%
  select(-splits)

saveRDS(data_cba_count_value_split, "data/cba/data_cba_count_value_split.RDS")
data_cba_count_value_split <- readRDS("data/cba/data_cba_count_value_split.RDS")

# Get potential overlap duplicates
data_cba_count_value_split_verify <- data_cba_count_value_split %>%
  group_by(date, type) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  mutate(count_orig = data_cba_count_value$count, d = count_orig - count)

# Check if any overlaps have >2000 donations in individual sections (thankfully none)
data_cba_count_value_split_verify %>%
  rowwise() %>%
  mutate(max_period_count = (\(f_date, f_type) { data_cba_count_value_split %>% filter(date == f_date, type == f_type) %>% select(count) %>% max()})(date, type)) %>%
  filter(d < 0, max_period_count > 2000)

data_cba_count_value_split <- data_cba_count_value_split %>%
  left_join(select(data_cba_count_value_split_verify, date, type, d), by = c("date", "type"))

# Grab date-type-value combinations that don't have overlaps
data_cba_count_value_split_nooverlap <- data_cba_count_value_split %>%
  filter(d == 0 & count > 0)

# Grab date-type-value combinations that may overlap
data_cba_count_value_split_overlap <- data_cba_count_value_split %>%
  filter(d < 0 & count > 0)

# Get unoverlapped rows
rows_value_nooverlap <- data_cba_count_value_split_nooverlap %>%
  filter(count <= 2000) %>%
  rowwise() %>%
  partition(cl) %>%
  mutate(rows = list(transform_rows(make_cba_req(card = 5, date_start = date, type = type, value_between = c(value_min, value_max))))) %>%
  collect() %>%
  pull(rows) %>%
  bind_rows()

saveRDS(rows_value_nooverlap, "data/cba/rows/data_cba_rows_value_nooverlap.RDS")

# Get overlapped rows
rows_value_overlap_raw <- data_cba_count_value_split_overlap %>%
  rowwise() %>%
  partition(cl) %>%
  mutate(rows = list(transform_rows(make_cba_req(card = 5, date_start = date, type = type, value_between = c(value_min, value_max))))) %>%
  collect() %>%
  # Fix overlaps (actually works, goddamn)
  # Bring rows together for each day-type combination
  group_by(date, type) %>%
  arrange(value_min) %>%
  summarise(value_min = min(value_min), value_max = max(value_max), count = sum(count), d = mean(d), rows = list(rows), .groups = "drop") %>%
  rowwise() %>%
  # Compare each set of rows to next, eliminate duplicates between sets (not within sets)
  # count_new helps to check if all overlaps have been eliminated
  mutate(rows = list(reduce2(rows, lead(rows), \(acc, rows, rows_lead) {
    if (!is.null(rows_lead)) {
      bind_rows(acc, anti_join(rows, rows_lead, by = c("date", "type", "value_uah", "comment")))
    } else {
      bind_rows(acc, rows)
    }
  }, .init = NULL)),
         count_new = nrow(rows))

rows_value_overlap <- rows_value_overlap_raw %>%
  pull(rows) %>%
  bind_rows()

saveRDS(rows_value_overlap, "data/cba/rows/data_cba_rows_value_overlap.RDS")


## 4. Textual split...
data_cba_count_text <- data_cba_count_value_split %>%
  filter(count > 2000) %>%
  rowwise()

# Runs query for each date-type-value-text (from alph) combination, saved for my sanity
rows_text_alph_raw <- data_cba_count_text %>%
  # filter(date == "2022-02-22") %>%
  # mutate(split = list(split_by_text(date, type, count, value_min, value_max)))
  rowwise() %>%
  partition(cl) %>%
  mutate(rows = list(get_rows_by_text(date, type, value_min, value_max, text = alph, case_sensitive = TRUE))) %>%
  collect()

saveRDS(rows_text_alph_raw, "data/cba/rows/data_cba_rows_text_alph_raw.RDS")
rows_text_alph_raw <- readRDS("data/cba/rows/data_cba_rows_text_alph_raw.RDS")

# Collate date-type-value-text rows
rows_text_alph <- rows_text_alph_raw %>%
  rowwise() %>%
  partition(cl) %>%
  mutate(rows = list(map(compact(rows), transform_rows))) %>%
  mutate(rows_coll = list(collate_rows_by_text(rows))) %>%
  mutate(count_coll = nrow(rows_coll),
         count_missing = count - count_coll) %>%
  collect()

# Save the one's where naive collation results in all rows
rows_text_alph_safe <- rows_text_alph %>%
  filter(count_missing == 0) %>%
  pull(rows_coll) %>%
  bind_rows() %>%
  select(-id)

saveRDS(rows_text_alph_safe, "data/cba/rows/data_cba_rows_text_alph_safe.RDS")



# process_pot_dupes <- \(rows, rows_coll) {
#  ...
#
#   pot_dupes <- rows %>%
#     map(slice, n()) %>%
#     bind_rows() %>%
#     ungroup() %>%
#     mutate(date = as_date(date)) %>%
#     ...
# }



tmp <- rows_text_alph_unsafe %>%
  select(-rows, -rows_coll)


## Check progress of text splitting
repeat {
  message("Printing status")

files <- list.files("logs/cba/") %>%
  map(~ file(glue("logs/cba/{.}"), "r"))

files %>%
  map(readLines) %>%
  map(~ .[[length(.)]]) %>%
  map(~ str_extract(., "(?<=er: ).")) %>%
  map(~ str_locate(alph, .)[[1]] / nchar(alph)) %>%
  map(message)

  map(files, close)


  Sys.sleep(5)
}



## Find duplicates
tmp <- data_cba_count %>%
  filter(count > 0) %>%
  rowwise() %>%
  mutate(duplicates = find_duplicates(date, type))

find_duplicates <- \(date, type) {
  rows <- make_cba_req(card = 5, date_start = date, type = type) %>%
    transform_rows()
  rows_dedup <- rows %>% distinct(date, type, value_uah, comment)

  d <- nrow(rows) - nrow(rows_dedup)
  if (d > 0) message(glue("Found duplicates for date: {date}, type: {type}"))

  d
}


#
rows <- make_cba_req(card = 5, date_start = "2022-10-10", type = "Privat UAH") %>%
  transform_rows()
rows_dedup <- rows %>% distinct(date, type, value_uah, comment)

nrow(rows) - nrow(rows_dedup)

tmp_a <- slice(rows, c(1, 1, 2))
tmp_b <- slice(rows, 1)

tmp_c <- anti_join(tmp_a, tmp_b)
