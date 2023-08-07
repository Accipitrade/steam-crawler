# Loading necessary packages
library(tidyverse)
library(lubridate)
library(tm)

# Reading data
reviews <- read_csv("data/reviews.csv", col_names = c('game_id', 'useful_count', 'funny_count', 'username', 'games_owned', 'reviews_written', 'recommendation', 'hours_played', 'review_date', 'review_text'))

# Preprocess the text
corpus <- Corpus(VectorSource(reviews$review_text))

# Convert the text to lower case
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove punctuation (but not hyphens)
corpus <- tm_map(corpus, removePunctuation, ucp=TRUE)

# Remove special characters
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to='ASCII//TRANSLIT')))

# Remove corrupted data
corpus <- tm_map(corpus, removeNumbers)

# Remove stop words
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Add preprocessed reviews back into dataframe
reviews$review_text <- sapply(corpus, as.character)

# Removing reviews with no date
reviews <- reviews %>% filter(!is.na(review_date))

# Reading the Positive and Negative Word List file
word_list <- read_excel("data/Positive and Negative Word List.xlsx")

positive_words <- word_list$C
negative_words <- word_list$B

# Function to count positive and negative matches
count_matches <- function(review) {
  words <- unlist(strsplit(review, " "))
  pos_matches <- sum(words %in% positive_words)
  neg_matches <- sum(words %in% negative_words)
  abs(pos_matches - neg_matches)
}

# Applying the function to every review and creating the review ranking
reviews$review_ranking <- sapply(reviews$review_text, count_matches) * reviews$recommendation

# Writing the preprocessed data back to csv
write_csv(reviews, "data/preprocessed_reviews.csv")
