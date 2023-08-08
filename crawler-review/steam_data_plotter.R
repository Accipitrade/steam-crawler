#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("tm")
#install.packages("readxl")
#install.packages("data.table")

library(tidyverse)
library(lubridate)
library(tm)
library(readxl)
library(ggplot2)
library(data.table)

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

#remove numbers
corpus <- tm_map(corpus, removeNumbers)

# Remove stop words
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Add preprocessed reviews back into dataframe
reviews$review_text <- sapply(corpus, as.character)

# Removing reviews with no date
reviews <- reviews %>% filter(!is.na(review_date))

# Reading the Positive and Negative Word List file
word_list <- read_xlsx("data/Positive and Negative Word List.xlsx")

# Assign Headers
names(word_list) <- c("A", "B", "C")

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

# Get game ID's
games <- read.csv("data/games.csv", header=F)

# Create final merged data
final_data <- merge(games[c("V2", "V3")], reviews[c("game_id", "recommendation", "review_date", "review_ranking")], by.x = "V2", by.y = "game_id")

# Store frequencies of reviews
review_count <- data.frame(table(final_data$V3))

# Filter games by only the ones needed to analyze
gamesToKeep <- c("Destiny 2", "DOOM", "Farming Simulator 19", "Only Up!")

final_data <- filter(final_data, V3 %in% gamesToKeep)

# Change date format
final_data$date <- as.POSIXct(final_data$review_date, format = "%B %d, %Y")


#plotting data --------------------------------------------------------------

scatter_ranking <- ggplot(subset(final_data, review_ranking<250))+
  facet_wrap(~V3)+
  geom_point(aes(x = date, y = review_ranking))+
  labs(x = "Year of Review", y = "Review Ranking")

scatter_ranking

ggsave(plot=scatter_ranking, width = 11, height = 8, file="data/scatter_ranking.png")

regression_ranking <- scatter_ranking+
  geom_smooth(aes(x=date, y=review_ranking), method="lm")

regression_ranking

ggsave(plot=regression_ranking, width = 11, height = 8, file="data/regression_ranking.png")

doom_ranking <- ggplot(subset(final_data, V3 == "DOOM" & review_ranking<250))+
  geom_point(aes(x = date, y = review_ranking), color="red")+
  labs(x = "Year of Review", y = "Review Ranking", title = "DOOM")
ggsave(plot=doom_ranking, width = 11, height = 8, file="data/doom_ranking.png")

doom_ranking2 <- doom_ranking + geom_smooth(aes(x=date, y=review_ranking), method="lm")
ggsave(plot=doom_ranking2, width = 11, height = 8, file="data/doom_ranking2.png")


destiny_ranking <- ggplot(subset(final_data, V3 == "Destiny 2" & review_ranking<250))+
  geom_point(aes(x = date, y = review_ranking), color="green")+
  labs(x = "Year of Review", y = "Review Ranking", title = "Destiny 2")
ggsave(plot=destiny_ranking, width = 11, height = 8, file="data/destiny2_ranking.png")

destiny_ranking2 <- destiny_ranking + geom_smooth(aes(x=date, y=review_ranking), method="lm")
ggsave(plot=destiny_ranking2, width = 11, height = 8, file="data/destiny2_ranking2.png")



farmingsim_ranking <- ggplot(subset(final_data, V3 == "Farming Simulator 19" & review_ranking<250))+
  geom_point(aes(x = date, y = review_ranking), color="blue")+
  labs(x = "Year of Review", y = "Review Ranking", title = "Farming Simulator 19")
ggsave(plot=farmingsim_ranking, width = 11, height = 8, file="data/farmingsim.png")

farmingsim_ranking2 <- farmingsim_ranking + geom_smooth(aes(x=date, y=review_ranking), method="lm")
ggsave(plot=farmingsim_ranking2, width = 11, height = 8, file="data/farmingsim_ranking2.png")

onlyUp_ranking <- ggplot(subset(final_data, V3 == "Only Up!" & review_ranking<250))+
  geom_point(aes(x = date, y = review_ranking), color="pink")+
  labs(x = "Year of Review", y = "Review Ranking", title = "Only Up!")
ggsave(plot=onlyUp_ranking, width = 11, height = 8, file="data/onlyUp_ranking.png")

onlyUp_ranking2 <- onlyUp_ranking + geom_smooth(aes(x=date, y=review_ranking), method="lm")
ggsave(plot=onlyUp_ranking2, width = 11, height = 8, file="data/onlyUp2.png")





# Reorganize month data into new final data frame 
final_data2 <- data.table(final_data)
final_data2[, month:=month(date)]
final_data2[, year:=year(date)]

reviews_by_month <- final_data2[, .(count=.N), by=.(V3, month, year)]
reviews_by_month[, date:=as.IDate(paste0(year, "-", month, "-01"))]

bar_frequency <- ggplot(reviews_by_month)+
  facet_wrap(~V3)+
  geom_col(aes(x = date, y = count))+
  labs(x = "Year of Review", y = "Number of Reviews")
bar_frequency
ggsave(plot=bar_frequency, width = 11, height = 8, file="data/bar_frequency2.png")

reviews_by_month2 <- final_data2[, .(count=.N), by=.(V3, month, year, recommendation)]
reviews_by_month2[, date:=as.IDate(paste0(year, "-", month, "-01"))]

reviews_by_month2[, reviewType:=factor(recommendation, labels = c("Negative Review", "Positive Review"))]

bar_frequency2 <- ggplot(reviews_by_month2)+
  facet_wrap(~V3)+
  geom_col(aes(x = date, y = count, fill= reviewType), position = "dodge")+
  labs(x = "Year of Review", y = "Number of Reviews", fill="Review type")

bar_frequency2

ggsave(plot=bar_frequency2, width = 11, height = 8, file="data/bar_frequency.png")


doom_frequency2 <- ggplot(subset(reviews_by_month2, V3=="DOOM"))+
  geom_col(aes(x = date, y = count, fill= reviewType), position = "dodge")+
  labs(x = "Year of Review", y = "Number of Reviews", fill="Review type")
ggsave(plot=doom_frequency2, width = 11, height = 8, file="data/doom_frequency.png")

destiny_frequency2 <- ggplot(subset(reviews_by_month2, V3=="Destiny 2"))+
  geom_col(aes(x = date, y = count, fill= reviewType), position = "dodge")+
  labs(x = "Year of Review", y = "Number of Reviews", fill="Review type")
ggsave(plot=destiny_frequency2, width = 11, height = 8, file="data/destiny2_frequency.png")

farmingsim_frequency2 <- ggplot(subset(reviews_by_month2, V3=="Farming Simulator 19"))+
  geom_col(aes(x = date, y = count, fill= reviewType), position = "dodge")+
  labs(x = "Year of Review", y = "Number of Reviews", fill="Review type")
ggsave(plot=farmingsim_frequency2, width = 11, height = 8, file="data/farmingsim_frequency.png")

onlyUp_frequency2 <- ggplot(subset(reviews_by_month2, V3=="DOOM"))+
  geom_col(aes(x = date, y = count, fill= reviewType), position = "dodge")+
  labs(x = "Year of Review", y = "Number of Reviews", fill="Review type")
ggsave(plot=onlyUp_frequency2, width = 11, height = 8, file="data/onlyUp_frequency.png")
