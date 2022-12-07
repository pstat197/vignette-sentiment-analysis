library(tidyverse)
library(tidytext)
library(tidymodels)
library(tokenizers)
library(textstem)
library(stopwords)
library(textdata)

# loading in the dataset.
dataset <- read.csv("/Users/shannon/Documents/PSTAT197/vignette-sentiment-analysis/data/IMDB-raw.csv")

### Functions for Text Cleaning ###

# Creating several wrapper functions to clean up the data.

# This first function removes the "<br />" terms from the text, as these are not words.
clean_fn <- function(.text){
  str_replace_all(.text, "<br />", " ") %>% tolower()
}

# This function removes all punctuation. 
clean_fn2 <- function(.text){
  str_remove_all(.text, '[[:punct:]]') %>%
    tolower()
}

# Applying this function to all of the reviews in our dataset.
dataset_clean <- dataset %>%
  mutate(review = clean_fn(review)) %>%
  mutate(review = clean_fn2(review))

### Miscellaneous Cleaning ###

# Adding a column with id's for each observation.
dataset_clean <- tibble::rowid_to_column(dataset_clean, ".id")

write.csv(dataset_clean, file = "/Users/shannon/Documents/PSTAT197/vignette-sentiment-analysis/data/dataset_clean.csv")

# Adding a column with the lengths of each review for each observation.
dataset_clean <- dataset_clean %>%
  mutate(length = nchar(dataset$review, type = "chars", allowNA = FALSE, keepNA = NA))

### Predictors ###

#Adding a column with the lengths of each review for each observation.
dataset_clean <- dataset_clean %>%
  mutate(length = nchar(dataset$review, type = "chars", allowNA = FALSE, keepNA = NA))

# matches words in reviews with words in afinn library
# provides an afinn score for each individual word
dataset_tokens <- dataset_clean %>% 
  unnest_tokens(word, review) %>%
  anti_join(get_stopwords()) %>%
  inner_join(get_sentiments("afinn")) # to use another sentiment library, replace "afinn"

# calculates the mean of afinn score by id
afinn_scores <- dataset_tokens %>%
  group_by(.id) %>%
  summarize(afinn_score = mean(value))

# merge with main dataset
dataset_clean <- merge(dataset_clean, afinn_scores, by = ".id")

### Predicting Sentiment With Bing ###
# Uses the Bing library to categorize words into positive and negative categories
# Our Bing score is based on whether there are more positive or negative words in each observation
bing_tokens <- dataset_clean %>% 
  unnest_tokens(word, review) %>%
  anti_join(get_stopwords()) %>%
  inner_join(get_sentiments("bing"))

# Calculating Bing Sentiment
# Joining by id and deciding sentiment based on the higher amount of positive or negative scores
bing_scores <- bing_tokens %>%
  group_by(.id) %>%
  summarize(bing_score = max(sentiment))

# Merge with main dataset
dataset_clean <- merge(dataset_clean, bing_scores, by = ".id")

### Predicting Sentiment with Loughran ###
dataset_loughran_tokens <- dataset_clean %>% 
  unnest_tokens(word, review) %>%
  anti_join(get_stopwords()) %>%
  inner_join(get_sentiments("loughran"))

loughran_scores <- dataset_loughran_tokens %>%
  group_by(.id) %>%
  summarize(loughran_score = max(sentiment))

dataset_clean <- merge(dataset_clean, loughran_scores, by = ".id")

### Predicting sentiment with NRC ###

dataset_nrc_tokens <- dataset_clean %>% 
  unnest_tokens(word, review) %>%
  anti_join(get_stopwords()) %>%
  inner_join(get_sentiments("nrc"))

nrc_scores <- dataset_nrc_tokens %>%
  group_by(.id) %>%
  summarize(nrc_score = max(sentiment))

dataset_clean <- merge(dataset_clean, nrc_scores, by = ".id")
