library(readxl)
library(writexl)

data_aid_raw <- read_xlsx("data/data_bilateralaid_kiel.xlsx", sheet = 3)

data_aid <- data_aid_raw %>%
  select(1:8) %>%
  set_names(c("id", "country", "datetime", "type_general", "type_specific", "explanation", "currency", "amount")) %>%
  distinct() %>%
  filter(if_all(everything(), ~ !is.na(.))) %>%
  mutate(amount = as.numeric(amount)) %>%
  # mutate(datetime = as_datetime(datetime), value = as.numeric(value)) %>%
  filter(currency == "USD" | currency == "EUR") %>%
  group_by(datetime) %>%
  arrange(desc(amount)) %>%
  summarize(across(everything(), first)) %>%
  arrange(desc(amount))

write_xlsx(data_aid, "data/events_aid.xlsx")
