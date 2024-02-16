library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(scales)
library(readxl)
library(cld2)
library(stopwords)
install.packages('textdata')
library(textdata)

############NOTE###################
# Some code was altered or copied from Assignment01-02. Additionally, some code
# was utilized from in-class examples.

#Read In Data----
tweets <- read_excel("C:/Users/watso/Box/PhD/HPMT 5336 - Introduction to NLP/Data/1557-random-tweets-18June2019.xlsx",
                     col_names = TRUE)

tidy_tweets <- tweets %>%
  unnest_tokens(word, text)


#NRC Lexicon----
##Join with nrc----
nrc_tweets <- tidy_tweets %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  group_by(id) %>%
  summarise(
    positive = sum(sentiment == "positive"),
    negative = sum(sentiment == "negative"),
    .groups = "drop"
  ) %>%
  mutate(
    Sentiment = positive - negative,
    Method = "NRC") %>%
  select(id, Method, Sentiment)

##Join sentiment back with original dataset----
tweets_with_sentiment_NRC <- nrc_tweets %>%
  left_join(tweets %>% distinct(id, text), by = "id")

##Clean up Environment----
rm(nrc_tweets)

#AFINN Lexicon----
##Join with AFINN----
afinn_tweets <- tidy_tweets %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarise(
    Sentiment = sum(value),
    .groups = "drop"
  ) %>%
  mutate(Method = "AFINN") %>%
  select(id, Method, Sentiment)

##Join sentiment back with original dataset----
tweets_with_sentiment_AFINN <- afinn_tweets %>%
  left_join(tweets %>% distinct(id, text), by = "id")

##Clean up Environment
rm(afinn_tweets)

#Bing et al Lexicon----
##Join with Bing----
bing_tweets <- tidy_tweets %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  group_by(id) %>%
  summarise(
    positive = sum(sentiment == "positive"),
    negative = sum(sentiment == "negative"),
    .groups = "drop"
  ) %>%
  mutate(
    Sentiment = positive - negative,
    Method = "Bing et al") %>%
  select(id, Method, Sentiment)

##Join sentiment back with original dataset----
tweets_with_sentiment_Bing <- bing_tweets %>%
  left_join(tweets %>% distinct(id, text), by = "id")

#Comparison of different Lexica----
combined <- as.data.frame(bind_rows(tweets_with_sentiment_NRC, 
           tweets_with_sentiment_Bing,
           tweets_with_sentiment_AFINN)) %>%
  ggplot(aes(id, Sentiment, fill = Method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Method, ncol = 1, scales = "free_y")


#Top 20 Positive and Negative Tweets----
## Get dataset----
top_tweets <- combined %>%
  group_by(Method) %>%
  mutate(SentimentType = ifelse(Sentiment >= 0, "Positive", "Negative")) %>%
  group_by(Method, SentimentType) %>%
  top_n(n = 20, wt = abs(Sentiment)) %>%
  ungroup()

##Plot----
ggplot(top_tweets, aes(x = reorder(id, Sentiment), y = Sentiment, fill = Method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Method + SentimentType, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(x = "Tweet ID",
       y = "Sentiment Score",
       title = "Top 20 Positive and Negative Tweet IDs by Sentiment")


   wide_tweets <- top_tweets %>%
     pivot_wider(
       names_from = Method,  # The column that has the variable names in the current long format
       values_from = Sentiment,     # The column that has the values associated with those variable names
       id_cols = c(id, text)      # The identifier columns
     )


   ## Get dataset----
   top_tweets_complete <- combined %>%
     group_by(Method) %>%
     filter(all(complete.cases(.))) %>%
     mutate(SentimentType = ifelse(Sentiment >= 0, "Positive", "Negative")) %>%
     group_by(Method, SentimentType) %>%
     top_n(n = 20, wt = abs(Sentiment)) %>%
     ungroup()
