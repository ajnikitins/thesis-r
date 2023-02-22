import os
from dotenv import load_dotenv
from pymongo import MongoClient
from transformers import AutoTokenizer, AutoModelForSequenceClassification, Trainer
import pandas as pd
import numpy as np


# Create class for holding tokenized texts (compatible with Dataset for the trainer)
class SimpleDataset:
    def __init__(self, tokenized_texts):
        self.tokenized_texts = tokenized_texts

    def __len__(self):
        return len(self.tokenized_texts["input_ids"])

    def __getitem__(self, idx):
        return {k: v[idx] for k, v in self.tokenized_texts.items()}


# Load tokenizer, model, and set up the predicting trainer
model_name = "j-hartmann/emotion-english-distilroberta-base"
tokenizer = AutoTokenizer.from_pretrained(model_name)
model = AutoModelForSequenceClassification.from_pretrained(model_name)
trainer = Trainer(model=model)

# Setup Mongo
load_dotenv()

client = MongoClient(os.getenv('MONGO_URL'))
db = client['thesis-data']
col_tweets = db['tweets']
col_emotions = db['emotions_ml']


# From how many currently are processed to maximum number of English tweets
tweet_range = range(3992355, 4173424, 10000)

for skip in tweet_range:
    # Load tweets
    tweets = list(col_tweets.find({'lang': 'en'}, {'text': 1}, skip=skip, limit=10000))
    tweet_text = [d['text'] for d in tweets]
    # Tokenize tweets
    tokenized_texts = tokenizer(tweet_text, truncation=True, padding=True)
    pred_dataset = SimpleDataset(tokenized_texts)

    # Run predictions and format the results
    predictions = trainer.predict(pred_dataset)
    preds = predictions.predictions.argmax(-1)
    labels = pd.Series(preds).map(model.config.id2label)
    # scores = (np.exp(predictions[0])/np.exp(predictions[0]).sum(-1, keepdims=True)).max(1)
    scores = (np.exp(predictions[0]) / np.exp(predictions[0]).sum(-1, keepdims=True))
    labels_full = list(model.config.id2label.values())

    scores_df = pd.DataFrame(scores, columns=labels_full)
    scores_df.insert(0, '_id', [d['_id'] for d in tweets])
    scores_df['main_emotion'] = labels

    tweets_emot = scores_df.to_dict('records')
    col_emotions.insert_many(tweets_emot)
