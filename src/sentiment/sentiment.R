library(tidyverse)
library(glue)
library(mongolite)
library(stopwords)
library(textstem)
library(multidplyr)

# Process raw text for sentiment analysis
process_text <- \(text) {
  text %>%
    # set all words to lowercase
    tolower() %>%
    # remove mentions
    str_replace_all("@.+?\\b", " ") %>%
    # remove the hash tag sign (#) but not the actual tag as this may contain information
    str_replace_all("#", "") %>%
    # remove URLs
    str_replace_all("https?.*?(?=( |$))", " ") %>%
    # remove noise phrases
    str_replace_all("(\\\n)|(&amp;)|(&gt;)|(&lt;)", " ") %>%
    # replace incorrect apostrophes
    str_replace_all("’", "'") %>%
    # remove stopwords
    str_replace_all(paste0("\\b", paste(stopwords("en", simplify = FALSE), collapse = "\\b|\\b"), "\\b"), " ") %>%
    # remove all punctuations, including the question and exclamation marks
    str_replace_all("[:punct:]|\\|", " ") %>%
    # remove digits
    str_replace_all("[\\d](st|nd|rd|th)*", " ") %>%
    # remove non-ASCII characters (including emojis)
    str_replace_all("[^[:ascii:]]", " ") %>%
    # remove one length words
    str_remove_all("\\b[:alpha:]\\b") %>%
    # remove extra whitespace
    str_remove_all("(^ +)|( +$)|( +(?= ))") %>%
    # split tokens
    str_split(fixed(" ")) %>%
    # lemmatize tokens
    lemmatize_words()
}

# Setup NRC emotion lexicon
get_nrc <-  \() {
  nrc <- read.table("data/sentiment/NRC-Emotion-Lexicon-Senselevel-v0.92.txt", sep = "\t", header = FALSE, col.names = c("lemma", "emotion", "val")) %>%
    separate_rows(lemma, sep = "--|, ") %>%
    group_by(lemma, emotion) %>%
    summarize(val = if_else(any(val == 1), 1, 0), .groups = "drop") %>%
    pivot_wider(names_from = emotion, values_from = val)
}

# Finds emotions for all tokens in a tweet from a given lexicon
get_emotions <- \(tokens, lexicon = get_nrc()) {
  lexicon %>%
    filter(lemma %in% tokens) %>%
    summarize(across(-lemma, sum)) %>%
    as.list()
}

# Get "main" emotion of a tweet by largest number of words representing each emotion
# Returns "mixed" if there are conflicts
get_main_emotion <- \(emot) {
  max_emot <- emot %>%
    unlist() %>%
    `[`(c("anger", "anticip", "disgust", "fear", "joy", "sadness", "surprise", "trust")) %>%
    `[`(which(. == max(.)))

  if (length(max_emot) > 1) {
    "mixed"
  } else {
    names(max_emot)[1]
  }
}

# Get "main" sentiment (positive/negative) of a tweet by largest number of words representing each sentiment
# Returns "mixed" if there are conflicts
get_main_sentiment <- \(emot) {
  max_sent <- emot %>%
    unlist() %>%
    `[`(c("positive", "negative")) %>%
    `[`(which(. == max(.)))

  if (length(max_sent) > 1) {
    "mixed"
  } else {
    names(max_sent)[1]
  }
}

parse_tweets <- \(raw_tweets) {
  message(glue("Processing {nrow(raw_tweets)} tweets..."))

  tweets <- raw_tweets %>%
    rename(tweet_id = `_id`) %>%
    partition(cl) %>%
    # Process tweet text
    mutate(processed_text = process_text(text)) %>%
    # Get emotions from text
    mutate(emotions = map(processed_text, get_emotions, nrc),
           main_emotion = map(emotions, get_main_emotion),
           main_sentiment = map(emotions, get_main_sentiment)) %>%
    collect() %>%
    select(tweet_id, processed_text, emotions, starts_with("main"))

  # Add emotions to new collection
  db_emotions$insert(tweets, auto_unbox = TRUE)
}

# Load NRC list
nrc <- get_nrc()

# Setup cluster
cl <- new_cluster(parallel::detectCores())
cluster_library(cl, c("tidyverse", "stopwords", "textstem"))
cluster_copy(cl, c("nrc", "process_text", "get_emotions", "get_main_emotion", "get_main_sentiment"))

# Connect to tweet database & load tweets
db <- mongo(collection = "tweets", db = "thesis-data", url = Sys.getenv("MONGO_URL"))
db_emotions <- mongo(collection = "emotions", db = "thesis-data", url = Sys.getenv("MONGO_URL"))

# Process tweet sentiment
db$find(query = '{"lang": "en"}', fields = '{"text": 1}', pagesize = 50000, handler = parse_tweets)

# Clean-up
db$disconnect()
db_emotions$disconnect()
