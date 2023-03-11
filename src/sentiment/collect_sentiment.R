library(tidyverse)
library(mongolite)
library(lubridate)

# Connect to db
db <- mongo(collection = "emotions", db = "thesis-data", url = Sys.getenv("MONGO_URL"))

# Get daily emotions
pipeline_emotions <- '[{"$lookup": {"from": "tweets","localField": "tweet_id","foreignField": "_id","pipeline": [{"$project": {"_id": 0,"created_at": 1}}],"as": "result"}}, {"$unwind": {"path": "$result"}}, {"$project": {"date": {"$dateFromString": {"dateString": "$result.created_at"}},"main_emotion": 1,"main_sentiment": 1}}, {"$group": {"_id": {"date": {"$dateTrunc": {"date": "$date","unit": "day"}},"emotion": "$main_emotion"},"count": {"$count": {}}}}, {"$group": {"_id": "$_id.date","emotions": {"$push": {"k": "$_id.emotion","v": "$count"}}}}, {"$project": {"_id": 0,"date": "$_id","emotions": {"$arrayToObject": "$emotions"}}}, {"$sort": {"date": 1}}]'
raw_emotions <- db$aggregate(pipeline = pipeline_emotions)

saveRDS(raw_emotions, "data/sentiment/data_emotions_raw.RDS")

# raw_emotions <- readRDS("data/sentiment/data_emotions_raw.RDS")

# Process daily emotions
emotions <- raw_emotions %>%
  unnest_wider(emotions) %>%
  mutate(date = as_date(date),
         across(-date, replace_na, 0, .names = "emot_count_{.col}"),
         .keep = "unused") %>%
  rowwise() %>%
  mutate(across(starts_with("emot"), ~ . / sum(c_across(starts_with("emot"))), .names = "emot_prop_{str_remove(.col, 'emot_count_')}")) %>%
  ungroup()

saveRDS(emotions, "data/sentiment/data_emotions.RDS")

# Get daily sentiments
pipeline_sentiment <- '[{"$lookup": {"from": "tweets", "localField": "tweet_id", "foreignField": "_id", "pipeline": [{"$project": {"_id": 0, "created_at": 1}}], "as": "result"}}, {"$unwind": {"path": "$result"}}, {"$project": {"date": {"$dateFromString": {"dateString": "$result.created_at"}}, "main_emotion": 1, "main_sentiment": 1}}, {"$group": {"_id": {"date": {"$dateTrunc": {"date": "$date", "unit": "day"}}, "sentiment": "$main_sentiment"}, "count": {"$count": {}}}}, {"$group": {"_id": "$_id.date", "sentiments": {"$push": {"k": "$_id.sentiment", "v": "$count"}}}}, {"$project": {"_id": 0, "date": "$_id", "sentiments": {"$arrayToObject": "$sentiments"}}}, {"$sort": {"date": 1}}]'
raw_sentiments <- db$aggregate(pipeline = pipeline_sentiment)

saveRDS(raw_sentiments, "data/sentiment/data_sentiments_raw.RDS")

# raw_sentiments <- readRDS("data/sentiment/data_sentiments_raw.RDS")

# Process daily sentiments
sentiments <- raw_sentiments %>%
  unnest_wider(sentiments) %>%
  mutate(date = as_date(date),
         across(-date, replace_na, 0, .names = "sent_count_{.col}"),
         .keep = "unused") %>%
  rowwise() %>%
  mutate(across(starts_with("sent"), ~ . / sum(c_across(starts_with("sent"))), .names = "sent_prop_{str_remove(.col, 'sent_count_')}")) %>%
  ungroup()

saveRDS(sentiments, "data/sentiment/data_sentiments.RDS")

db$disconnect()

## Machine learning emotions

# Connect to db
db_ml <- mongo(collection = "emotions_ml", db = "thesis-data", url = Sys.getenv("MONGO_URL"))

# Get daily emotions
pipeline_emotions_ml <- '[{"$lookup": {"from": "tweets", "localField": "_id", "foreignField": "_id", "pipeline": [{"$project": {"_id": 0, "created_at": 1, "urls": {"$eq": [{"$type": "$entities.urls"}, "array"]}}}], "as": "result"}}, {"$unwind": {"path": "$result"}}, {"$project": {"date": {"$dateFromString": {"dateString": "$result.created_at"}}, "hasURL": "$result.urls", "main_emotion": 1}}, {"$group": {"_id": {"date": {"$dateTrunc": {"date": "$date", "unit": "day"}}, "emotion": "$main_emotion", "hasURL": "$hasURL"}, "count": {"$count": {}}}}, {"$group": {"_id": {"date": "$_id.date", "hasURL": "$_id.hasURL"}, "emotions": {"$push": {"k": "$_id.emotion", "v": "$count"}}}}, {"$project": {"_id": 0, "date": "$_id.date", "hasURL": "$_id.hasURL", "emotions": {"$arrayToObject": "$emotions"}}}, {"$sort": {"date": 1}}]'
raw_emotions_ml <- db_ml$aggregate(pipeline = pipeline_emotions_ml)

saveRDS(raw_emotions_ml, "data/sentiment/data_emotions_ml_raw.RDS")

# raw_emotions_ml <- readRDS("data/sentiment/data_emotions_ml_raw.RDS")

# Process daily emotions
emotions_ml <- raw_emotions_ml %>%
  ### SELECT TWEETS WITH AND WITHOUT URLs
  # filter(hasURL == FALSE) %>%
  # select(-hasURL) %>%
  unnest_wider(emotions) %>%
  mutate(date = as_date(date),
         across(-c(date, hasURL), ~ replace_na(., 0), .names = "emot_count_{.col}"),
         .keep = "unused") %>%
  group_by(date) %>%
  summarize(across(starts_with("emot"), sum)) %>%
  rowwise() %>%
  mutate(across(starts_with("emot"), ~ . / sum(c_across(starts_with("emot"))), .names = "emot_prop_{str_remove(.col, 'emot_count_')}")) %>%
  ungroup()

saveRDS(emotions_ml, "data/sentiment/data_emotions_ml.RDS")

db_ml$disconnect()
