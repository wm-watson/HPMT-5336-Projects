library(dplyr)
library(tidytext)
library(ggplot2)

############NOTE###################
# Some code was altered or copied from Assignment01-03. Additionally, some code
# was utilized from in-class examples.

# Data----
dc <- read.csv("/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/HPMT 5336 - Introduction to NLP/Data/discharge.csv",
               header = TRUE)

rad <- read.csv("/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/HPMT 5336 - Introduction to NLP/Data/radiology.csv",
                header = TRUE)

#Part 1----
##Subset data----
set.seed(103)
d2k <- dc[sample(nrow(dc), 2000),]
r2k<- rad[sample(nrow(rad), 2000),]

rm(dc)
rm(rad)

##Tokenize----
###DC----
d2kwords <- d2k %>% 
  unnest_tokens(word, text) %>%
  count(word, sort =TRUE) %>%
  mutate(source = "dc")

###Rad----
r2kwords <- r2k %>% 
  unnest_tokens(word, text) %>%
  count(word, sort =TRUE) %>%
  mutate(source = "rad")

##Combine DFs----
rd2k_words <-  bind_rows(d2kwords, r2kwords)

## Total tokens grouped by source----
rd2k_totals <- rd2k_words %>%
  group_by(source) %>%
  summarize(total = sum(n))

## Add the per-category totals to our total tokens frame----
rd2k_note_words <- left_join(rd2k_words, rd2k_totals)

## Rank tokens by frequency, per note----
freq_by_rank <- rd2k_note_words %>% 
  group_by(source) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total) %>%
  ungroup()

## Get TFIDF----
rdnote_tf_idf <- rd2k_note_words %>%
  bind_tf_idf(word, source, n)

### Top 10 Rad and DC tf-idf----
tf_idf_top_10 <- rdnote_tf_idf %>%
  group_by(tf_idf) %>%
  arrange(desc(tf_idf)) %>%
  group_by(source) %>%
  slice_head(n = 10)

#Part 2----
##Tokenize----
###DC----
d2kwords <- d2k %>% 
  unnest_tokens(word, text) %>%
  count(note_id, word, sort =TRUE) %>%
  mutate(source = "dc",
         ID = note_id)

###Rad----
r2kwords <- r2k %>% 
  unnest_tokens(word, text) %>%
  count(note_id, word, sort =TRUE) %>%
  mutate(source = "rad",
         ID = note_id)

##Combine DFs----
rd2k_words <-  bind_rows(d2kwords, r2kwords)

## Total tokens grouped by note_id----
rd2k_totals <- rd2k_words %>%
  group_by(note_id) %>%
  summarize(total = sum(n))

## Add the per-category totals to our total tokens frame----
rd2k_note_words <- left_join(rd2k_words, rd2k_totals)

## Rank tokens by frequency, per note----
freq_by_rank <- rd2k_note_words %>% 
  group_by(note_id, source) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total) %>%
  ungroup()

## Get TFIDF----
rdnote_tf_idf <- rd2k_note_words %>%
  bind_tf_idf(word, note_id, n)

### Top 15 Rad and DC tf-idf----
tf_idf_top_15 <- rdnote_tf_idf %>%
  group_by(tf_idf) %>%
  arrange(desc(tf_idf)) %>%
  group_by(source) %>%
  slice_head(n = 15)

##Plot----
ggplot(tf_idf_top_15, aes(x = tf_idf, y = reorder(word, tf_idf), fill = source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source ,  scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(x = "TF-IDF",
       y = "Word",
       title = "Top 15 Words (TF-IDF Score) by Type of Note")

# Bonus----
library(stringr)


## Combine columns----
tf_idf_top_15 <- tf_idf_top_15 %>%
  mutate(Word_note = str_c(word, note_id, sep = " "))

## Plot----
##Plot----
ggplot(tf_idf_top_15, aes(x = tf_idf, y = reorder(Word_note, tf_idf), fill = source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source ,  scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(x = "TF-IDF",
       y = "Word",
       title = "Bonus: Top 15 Words (TF-IDF Score) by Type of Note")
