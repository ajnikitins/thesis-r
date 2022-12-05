library(tidyverse)
library(mongolite)
library(stopwords)

# Connect to tweet database & load tweets
db <- mongo(collection = "tweets", db = "thesis-data", url = Sys.getenv("MONGO_URL"))

it_tweets <- db$iterate(query = '{"lang": "en"}', fields = '{"text": 1}')

raw_tweets <- it_tweets$batch(100)

tweets <- raw_tweets %>%
  bind_rows()

## Process tweet text
tweets_proc <- tweets %>%
  # set all words to lowercase
  mutate(text = tolower(text)) %>%
  # remove mentions
  mutate(text = str_replace_all(text, "@.+?\\b", " ")) %>%
  # remove the hash tag sign (#) but not the actual tag as this may contain information
  mutate(text = str_replace_all(text, "#", "")) %>%
  # remove URLs
  mutate(text = str_replace_all(text, "https?.*?(?=( |$))", " ")) %>%
  # remove noise phrases
  mutate(text = str_replace_all(text, "(\\\n)|(&amp;)|(&gt;)|(&lt;)", " ")) %>%
  # replace incorrect apostrophes
  mutate(text = str_replace_all(text, "’", "'")) %>%
  # remove stopwords
  mutate(text = str_replace_all(text, paste0("\\b", paste(stopwords("en", simplify = FALSE), collapse = "\\b|\\b"), "\\b"), " ")) %>%
  # remove all punctuations, including the question and exclamation marks
  mutate(text = str_replace_all(text, "[:punct:]|\\|", " ")) %>%
  # remove digits
  mutate(text = str_replace_all(text, "[\\d]", " ")) %>%
  # remove non-ASCII characters (including emojis)
  mutate(text = str_replace_all(text, "[^[:ascii:]]", " ")) %>%
  # remove one length words
  mutate(text = str_remove_all(text, "\\b[:alpha:]\\b")) %>%
  # remove extra whitespace
  mutate(text = str_remove_all(text, "(^ +)|( +$)|( +(?= ))"))
