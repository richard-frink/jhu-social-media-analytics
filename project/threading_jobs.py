import sys
import nltk
import os
import multiprocessing as mp
import threading_jobs as tj
# uncomment to download nltk library
#nltk.download()
os.environ["NLTK_DATA"] = "D:\nltk_data"

import pandas
import numpy as np

sentiment140dataset = "testdata.manual.2009.06.14.csv"

s140tweets = pandas.read_csv(sentiment140dataset, encoding='utf-8')

s140tweets.drop("id", axis=1, inplace=True)
s140tweets.drop("date", axis=1, inplace=True)
s140tweets.drop("topic", axis=1, inplace=True)
s140tweets.drop("account", axis=1, inplace=True)

final_tweet_df = s140tweets

def translate_dataframe_to_post_format(frame):
    posts = []
    for index, row in frame.iterrows():
        words = row['post']
        post = [e.lower() for e in words.split() if len(e) >= 3]
        posts.append((post, row['emotion']))
    return posts

def get_words_in_tweets(posts):
    all_words = []
    for (words, sentiment) in posts:
      all_words.extend(words)
    return all_words

def get_word_features(wordlist):
    wordlist = nltk.FreqDist(wordlist)
    word_features = wordlist.keys()
    return word_features

posts = translate_dataframe_to_post_format(final_tweet_df)

words = get_words_in_tweets(posts)
word_features = get_word_features(words)

def extract_features(document):
    document_words = set(document)
    features = {}
    for word in word_features:
        features['contains(%s)' % word] = (word in document_words)
    return features

training_set = nltk.classify.apply_features(extract_features, posts)
classifier = nltk.NaiveBayesClassifier.train(training_set)

def process_frame(frame, classifier):
    for i, row in frame.iterrows():
        emotion = 2
        try:
            emotion = classifier.classify(extract_features(str(frame.at[i, 'body']).split()))
        except:
            emotion = -1
        frame.at[i, 'emotion'] = emotion
    return frame
