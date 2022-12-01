library(tidyverse)
library(tidytext)
library(tidymodels)
library(tokenizers)
library(textstem)
library(stopwords)


#loading in the dataset.
dataset <- read.csv("/Users/vardan/Documents/Github/vignette-sentiment-analysis/data/IMDB-raw.csv")


### Functions for Text Cleaning ###

# Creating several wrapper functions to clean up the data.

#This first function removes the "<br />" terms from the text, as these are not words.
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

# Adding a column with the lengths of each review for each observation.
dataset_clean <- dataset_clean %>%
  mutate(length = nchar(dataset$review, type = "chars", allowNA = FALSE, keepNA = NA))

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
  summarize(bing_scores = max(sentiment))

# Merge with main dataset
merge(dataset_clean, bing_scores, by = ".id")

# Renaming Bing Column
names(dataset_clean)[names(dataset_clean) == 'sentiment'] <- 'Bing Score'