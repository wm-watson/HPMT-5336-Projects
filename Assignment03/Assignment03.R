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
library()

############NOTE###################
# Some code was altered or copied from Assignment01-02. Additionally, some code
# was utilized from in-class examples.

#Read In Data----
tweets <- read_excel("C:/Users/1187507/Box/PhD/HPMT 5336 - Introduction to NLP/Data/1557-random-tweets-18June2019.xlsx",
                     col_names = TRUE)
tidy_tweets <- tweets %>%
  unnest_tokens(word, text)

##Lexica----
get_sentiments("nrc")


##Join with nrd----
nrc <- tidy_tweets %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(id) %>%
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "NRC")
