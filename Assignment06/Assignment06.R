install.packages("word2vec")
library(tidyverse)
library(readxl)
library(tidytext)
library(tidyr)
library(igraph)
library(tidygraph)
library(stringr)
library(widyr)
library(ggraph) 
library(word2vec)
library(textdata)
install.packages("Rtsne")
library(Rtsne)


############NOTE###################
# Some code was altered or copied from Assignment01-05. Additionally, some code
# was utilized from in-class examples.


#Get Glove2Vec----
embedding_glove6b(return_path = TRUE)


glove6b <- embedding_glove6b(dimensions = 100, return_path = TRUE, manual_download = TRUE)

#Part 1----

##Tweets----
tweets <-  read_excel("C:/Users/watso/Box/PhD/HPMT 5336 - Introduction to NLP/Data/1557-random-tweets-18June2019.xlsx",
                  col_names = TRUE)

##Train Embedding----
### Original word2vec model with 15-d embeddings using 20 iterations----
tweets_w2v <- tolower(tweets$text) %>%
  word2vec(dim = 15, iter = 20)

####Print the top 5 closest terms for several words----
predict(tweets_w2v, "lean", type = "nearest", top_n = 5)
predict(tweets_w2v, "codeine", type = "nearest", top_n = 5)
predict(tweets_w2v, "nicotine", type = "nearest", top_n = 5)
predict(tweets_w2v_50, "morphine", type = "nearest", top_n = 5)

### First New word2vec model with 15-d embeddings using 100 iterations----
tweets_w2v_100 <- tolower(tweets$text) %>%
  word2vec(dim = 15, iter = 100)

#### Print the top 5 closest terms for several words----
predict(tweets_w2v_100, "lean", type = "nearest", top_n = 5)
predict(tweets_w2v_100, "codeine", type = "nearest", top_n = 5)
predict(tweets_w2v_100, "nicotine", type = "nearest", top_n = 5)
predict(tweets_w2v_100, "morphine", type = "nearest", top_n = 5)

### Second New word2vec model with 15-d embeddings using 1000 iterations----
tweets_w2v_1000 <- tolower(tweets$text) %>%
  word2vec(dim = 15, iter = 1000)

#### Print the top 5 closest terms for several words----
predict(tweets_w2v_1000, "lean", type = "nearest", top_n = 5)
predict(tweets_w2v_1000, "codeine", type = "nearest", top_n = 5)
predict(tweets_w2v_1000, "nicotine", type = "nearest", top_n = 5)
predict(tweets_w2v_1000, "morphine", type = "nearest", top_n = 5)

##Part 2----
###Data----
dc <- read.csv("C:/Users/watso/Box/PhD/HPMT 5336 - Introduction to NLP/Data/discharge.csv",
               header = TRUE)

rad <- read.csv("C:/Users/watso/Box/PhD/HPMT 5336 - Introduction to NLP/Data/radiology.csv",
                header = TRUE)

##Subset data----
set.seed(103)
d2k <- dc[sample(nrow(dc), 2000),]
r2k<- rad[sample(nrow(rad), 2000),]

##Train Embedding----
###Dc----
d2k_w2v <- tolower(d2k$text ) %>%
  word2vec(dim = 100, iter = 20)

r2k_w2v <- tolower(r2k$text ) %>%
  word2vec(dim = 100, iter = 20)

##Predict Words----
predict(d2k_w2v, "oxycodone", type = "nearest", top_n = 5)
predict(d2k_w2v, "ankle", type = "nearest", top_n = 5)
predict(d2k_w2v, "fracture", type = "nearest", top_n = 5)
predict(d2k_w2v, "debridement", type = "nearest", top_n = 5)

predict(r2k_w2v, "ulcer", type = "nearest", top_n = 5)
predict(r2k_w2v, "ankle", type = "nearest", top_n = 5)
predict(r2k_w2v, "fracture", type = "nearest", top_n = 5)
predict(r2k_w2v, "debridement", type = "nearest", top_n = 5)

##My Words----

predict(d2k_w2v, "foot", type = "nearest", top_n = 5)
predict(d2k_w2v, "ulcer", type = "nearest", top_n = 5)
predict(d2k_w2v, "diabetes", type = "nearest", top_n = 5)
predict(d2k_w2v, "debridement", type = "nearest", top_n = 5)
predict(d2k_w2v, "retinopathy", type = "nearest", top_n = 5)

predict(r2k_w2v, "foot", type = "nearest", top_n = 5)
predict(r2k_w2v, "ulcer", type = "nearest", top_n = 5)
predict(r2k_w2v, "diabetes", type = "nearest", top_n = 5)
predict(r2k_w2v, "debridement", type = "nearest", top_n = 5)
predict(r2k_w2v, "retinopathy", type = "nearest", top_n = 5)


#Part 3: Bonus----

tidy_glove <- glove6b %>%
  pivot_longer(contains("d"),
               names_to = "dimension") %>%
  rename(item1 = token)

nearest_neighbors <- function(df, token) {
  df %>%
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) / 
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE,
      maximum_size = NULL
    )(item1, dimension, value) %>%
    select(-item2)
}

tidy_glove %>%
  nearest_neighbors("foot")

tidy_glove %>%
  nearest_neighbors("ulcer")

tidy_glove %>%
  nearest_neighbors("diabetes")

tidy_glove %>%
  nearest_neighbors("debridement")

tidy_glove %>%
  nearest_neighbors("retinopathy")

