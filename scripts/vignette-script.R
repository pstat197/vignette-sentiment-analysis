library(tidyverse)
library(tidytext)
library(tidymodels)
library(tokenizers)
library(textstem)
library(stopwords)
library(textdata)
library(corrplot)
library(wordcloud)
library(reshape2)
library(ggpubr)
library(treemap)

# loading in the dataset
# make sure to set working directory to folder containing vignette
dataset <- read.csv("data/IMDB-raw.csv")

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

write.csv(dataset_clean, file = "data/IMDB-clean.csv")

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




#Building the Logistic Regression Model
#Setting the seed.
set.seed(69)

#turning sentiment into a factor so that we can actually predict it.
dataset_clean$sentiment <- as.factor(dataset_clean$sentiment)

#Reordering the factor so that 'Yes' is the first factor.
dataset_clean$sentiment <- relevel(dataset_clean$sentiment, 'positive')



#splitting the data.
dataset_split <- initial_split(dataset_clean, prop = 0.70,
                               strata = sentiment)

#splitting the data into a training set and a testing set.
dataset_train <- training(dataset_split)
dataset_test <- testing(dataset_split)

#creating the recipe.
dataset_recipe <- recipe(sentiment ~ length + afinn_score + bing_score
                         + loughran_score + nrc_score, data = dataset_train)


#creating the logistic regression object.
log_reg <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

#creating the workflow.
log_wkflow <- workflow() %>% 
  add_model(log_reg) %>% 
  add_recipe(dataset_recipe)

#fitting the workflow with the object.
log_fit <- fit(log_wkflow, dataset_train)

#storing the accuracy of the logistic model on the testing data.
log_reg_acc <- augment(log_fit, new_data = dataset_test) %>%
  accuracy(truth = sentiment, estimate = .pred_class)





### Exploratory Data Analysis

# In this section, we will explore our IMDB Review dataset with a plethora of graphs and other visualizations.

# Corrplot 

library('corrplot')
length <- dataset_clean$length
afinn_score <- dataset_clean$afinn_score
corrplot(cor(data.frame(length, afinn_score)), type = "lower", method = "shade")

# In this corrplot, we how the length of each review effects the AFINN score of the review. Although our variables have a slight negative correlation, our corrplot shows us that review length does not affect AFINN score as they are not strongly correlated. 

# Distribution of Sentiment Scores Among Reviews

# We will use barplots to show how the distribution of positive and negative scores between different sentiment analysis models varies. We will plot the distributions of our Bing and Loughran scores against the actual split of the data to see how well we were able to classify our movie reviews.

# To do so, we must first count the amount of positive and negative scores for each sentiment analysis model. We can do this by grouping each dataframe by their sentiment score column and then counting each group.

score_table = dataset_clean %>% group_by('sentiment') %>% count(sentiment, sort=TRUE)

bing_table = dataset_clean %>% group_by('bing_score') %>% count(sentiment, sort=TRUE)

loughran_table = dataset_clean %>% group_by('loughran_score') %>% count(sentiment, sort=TRUE)

# Bar Plot of Actual Distribution 

# We then use a faceted bar plot to compare the distributions in our models versus the real distribution.

real_bar <- ggplot(data=score_table, aes(x=sentiment, y=n)) +
  geom_bar(stat='identity', color="blue", fill="white", width=0.5) + ggtitle("Actual Data Split")

# Bing Distribution Bar Plot
bing_bar <- ggplot(data=bing_table, aes(x=sentiment, y=n)) +
  geom_bar(stat='identity', color="orange", fill="white", width=0.5) + ggtitle("Bing Data Split")

# Loughran Distribution Bar Plot

loughran_bar <- ggplot(data=loughran_table, aes(x=sentiment, y=n)) +
  geom_bar(stat='identity', color="green", fill="white", width=0.5) + ggtitle("Loughran Data Split")


# Facet All Plots Together
figure <- ggarrange(real_bar, bing_bar, loughran_bar,
                    ncol = 3, nrow = 1)
figure

# Looking at our bar plots, we can see that the real split between our sentiment score values is surprisingly 50.07% positive and 49.99% negative. At first, this split seemed too good to be true but after checking with the publisher of the original dataset we confirmed that the data is in fact split about evenly. This graph also shows us that our NRC and Bing sentiment analysis models were able to classify reviews with perfect accuracy. 

# You wouldn't be able to tell the differences in accuracy unless you looked at their real values in their accuracy tables which show a perfect classification rate.


# Distribution of Number of Words Per Review

# In this graph, we explore how the length of movie reviews is distributed in our dataset.

# We took all of movie review lengths in our dataset and counted them by adjusting our stat variable to count.

word_count_bar <- ggplot(data=dataset_clean, aes(x=length)) +
  geom_bar(stat='count', color="purple", fill="white", width=2) + ggtitle("Distribution of Number of Words Per Review") 

word_count_bar

# Our graph shows us that the majority of our movie reviews have an average word length under 1500.

# Most Common Words In a Sentiment Analysis Model

# We look at our NRC sentiment analysis dataframe to find the most common words in our movie reviews that are also found in our NRC library.

# To do so, we must first count every time a word appeared in a movie review for each sentiment analysis model. 

word_counts <- dataset_nrc_tokens %>% count(word)
word_counts <- word_counts[order(-word_counts$n),]


# Only use the top 20 most popular words
first_20 <- word_counts[1:20,]

first_20_bar <- ggplot(data=first_20, aes(x=reorder(word,-n), y=n)) +
  geom_bar(stat='identity', color="purple", fill="white", width=0.5) + ggtitle("Most Popular Words in Movie Reviews (NRC Sentiment Analysis)") 
first_20_bar

# Our plot shows us that Good and Bad are by far the most common words found in our movie reviews that were recognized by the NRC library. Good and Bad are also likely to be best indicators of whether a review is positive or negative.

# Tree Map of Most Popular Movie Review Words

# We can also use the 'treemap' package to visualize word popularity by size.

treemap(first_20, # dataframe
        index=("word"),  # categorical column
        vSize = "n",  # numerical column
        type="index", # Type sets the organization and color scheme of your treemap
        palette = "Greens",  # color palette from RColorBrewer preset. You can also make a vector of your own color values.
        title="Most Popular Movie Review Words", # title
        fontsize.title = 14 # font size
)

# Word Clouds

# Using each sentiment analysis model dataframe, we can count every instance of a word and create wordcloud of the most popular words recognized from every model's corresponding library.

# Bing Word Cloud
bing_tokens %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# NRC Word Cloud
dataset_nrc_tokens %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Loughran Word Cloud
dataset_loughran_tokens %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# AFINN Word Cloud
dataset_tokens %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# We can also classify word clouds into positive and negative sentiments by sorting every word by their sentiment value and using acast() which casts our dataframe into an array of count values "n". We then call comparison.cloud() to make a word cloud after comparing sentiment score frequencies. After, we can apply our favorite hexadecimal color values to represent positive and negative words.

# Bing Negative & Positive Word Cloud
bing_scores %>%
  inner_join(bing_tokens) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#FF6F61", "#34568B"),
                   max.words = 100)

# AFINN Negative & Positive Word Cloud
afinn_scores %>%
  inner_join(dataset_tokens) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#9B2335", "#55B4B0"),
                   max.words = 100)

# Loughran Negative & Positive Word Cloud
loughran_scores %>%
  inner_join(dataset_loughran_tokens) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#E15D44", "#7FCDCD"),
                   max.words = 100)
