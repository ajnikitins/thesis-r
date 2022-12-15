library(tidyverse)
library(mongolite)
library(lubridate)

# Connect to db
db <- mongo(collection = "emotions", db = "thesis-data", url = Sys.getenv("MONGO_URL"))

# Get daily emotions
pipeline_emotions <- '[{"$lookup": {"from": "tweets","localField": "tweet_id","foreignField": "_id","pipeline": [{"$project": {"_id": 0,"created_at": 1}}],"as": "result"}}, {"$unwind": {"path": "$result"}}, {"$project": {"date": {"$dateFromString": {"dateString": "$result.created_at"}},"main_emotion": 1,"main_sentiment": 1}}, {"$group": {"_id": {"date": {"$dateTrunc": {"date": "$date","unit": "day"}},"emotion": "$main_emotion"},"count": {"$count": {}}}}, {"$group": {"_id": "$_id.date","emotions": {"$push": {"k": "$_id.emotion","v": "$count"}}}}, {"$project": {"_id": 0,"date": "$_id","emotions": {"$arrayToObject": "$emotions"}}}, {"$sort": {"date": 1}}]'
raw_emotions <- db$aggregate(pipeline = pipeline_emotions)

# Process daily emotions
emotions <- raw_emotions %>%
  unnest_wider(emotions) %>%
  mutate(date = as_date(date),
         across(-date, replace_na, 0))

# saveRDS(emotions, "data/sentiment/data_emotions.RDS")

# Get daily sentiments
pipeline_sentiment <- '[{"$lookup": {"from": "tweets", "localField": "tweet_id", "foreignField": "_id", "pipeline": [{"$project": {"_id": 0, "created_at": 1}}], "as": "result"}}, {"$unwind": {"path": "$result"}}, {"$project": {"date": {"$dateFromString": {"dateString": "$result.created_at"}}, "main_emotion": 1, "main_sentiment": 1}}, {"$group": {"_id": {"date": {"$dateTrunc": {"date": "$date", "unit": "day"}}, "sentiment": "$main_sentiment"}, "count": {"$count": {}}}}, {"$group": {"_id": "$_id.date", "sentiments": {"$push": {"k": "$_id.sentiment", "v": "$count"}}}}, {"$project": {"_id": 0, "date": "$_id", "sentiments": {"$arrayToObject": "$sentiments"}}}, {"$sort": {"date": 1}}]'
raw_sentiments <- db$aggregate(pipeline = pipeline_sentiment)

# Process daily sentiments
sentiments <- raw_sentiments %>%
  unnest_wider(sentiments) %>%
  mutate(date = as_date(date),
         across(-date, replace_na, 0))

# saveRDS(sentiments, "data/sentiment/data_sentiments.RDS")

db$disconnect()
