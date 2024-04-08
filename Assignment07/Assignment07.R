library(dplyr)
library(tidyr)
library(tidytext)
library(reshape2)
library(ggplot2)
library(quanteda)
library(readxl)
require(topicmodels)

############NOTE###################
# Some code was altered or copied from Assignment01-06. Additionally, some code
# was utilized from in-class examples.

# Part 1----
## Data----

text_rad <- read.csv("/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/HPMT 5336 - Introduction to NLP/Data/radiology.csv",
                     header = TRUE)


text_dc <- read.csv("/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/HPMT 5336 - Introduction to NLP/Data/discharge.csv",
                     header = TRUE)
set.seed(123)
text_rad_rand <- sample_n(text_rad, 2500)
text_dc_rand <- sample_n(text_dc, 2500)

texts <- rbind(text_rad_rand, text_dc_rand)

## Create Corpus----

corpus <- corpus(texts$text, docnames = texts$note_id)

## Tokenize and create a document-term matrix----
corpus_tokens <- corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() #%>%
  #tokens_remove(pattern = stopwords('en'))

## Document-term Matrix----
DTM <- corpus_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = 0.01, docfreq_type = "prop")

## Examine
dim(DTM)

## Topic Model (LDA)----
# load package topicmodels
require(topicmodels)
# number of topics
K <- 10
# compute the LDA model, inference via n iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, seed = 1, verbose = 25))

tmResult <- posterior(topicModel)
beta <- tmResult$terms   # get beta from results
theta <- tmResult$topics 

# View the top ten terms for each topic in the model
terms(topicModel, 10)


top10termsPerTopic <- terms(topicModel, 10)
topicNames <- apply(top10termsPerTopic, 2, paste, collapse=" ")



## Visualize----

install.packages("LDAvis")
install.packages("tsne")

library(LDAvis)
library(tsne)


svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(
  phi = tmResult$terms, 
  theta = tmResult$topics, 
  doc.length = rowSums(DTM), 
  vocab = colnames(DTM), 
  term.frequency = colSums(DTM),
  mds.method = svd_tsne,
  plot.opts = list(xlab="", ylab="")
)
serVis(json)

# Part 2----
## Tokenize and create a document-term matrix----
corpus_tokens <- corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>%
tokens_remove(pattern = stopwords('en'))

## Document-term Matrix----
DTM <- corpus_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = 0.01, docfreq_type = "prop")

## Examine
dim(DTM)

## Topic Model (LDA)----
# load package topicmodels

### 10 Topics----
require(topicmodels)
# number of topics
K <- 10
# compute the LDA model, inference via n iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, seed = 1, verbose = 25))

tmResult <- posterior(topicModel)
beta <- tmResult$terms   # get beta from results
theta <- tmResult$topics 

# View the top ten terms for each topic in the model
terms(topicModel, 10)

### 10 topics----
svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(
  phi = tmResult$terms, 
  theta = tmResult$topics, 
  doc.length = rowSums(DTM), 
  vocab = colnames(DTM), 
  term.frequency = colSums(DTM),
  mds.method = svd_tsne,
  plot.opts = list(xlab="", ylab="")
)
serVis(json)

# Part 3----
terms(topicModel, 10)
topicNames <- apply(top5termsPerTopic, 1, paste, collapse=" ")

# Filtering to topics
topicNames

#Discharge Topic 4 = R Topic 6
#Radiology Topic 10 = R Topic 1

## Discharge Documents----

topicToFilter <-  6  # you can set this manually ...
# # ... or have it selected by a term in the topic name
# topicToFilter <- grep('mexico ', topicNames)[1] 

# Note: depending on the corpus/topics, you'll need to play around with this number
topicThreshold <- 0.2 # minimum share of content must be attributed to the selected topic
selectedDocumentIndexes <- (theta[, topicToFilter] >= topicThreshold)
filteredCorpus_dc <- corpus %>% corpus_subset(subset = selectedDocumentIndexes)

# show length of filtered corpus
filteredCorpus_dc

## Radiology----
topicToFilter <-  1  # you can set this manually ...
# # ... or have it selected by a term in the topic name
# topicToFilter <- grep('mexico ', topicNames)[1] 

# Note: depending on the corpus/topics, you'll need to play around with this number
topicThreshold <- 0.2 # minimum share of content must be attributed to the selected topic
selectedDocumentIndexes <- (theta[, topicToFilter] >= topicThreshold)
filteredCorpus_rad <- corpus %>% corpus_subset(subset = selectedDocumentIndexes)

# show length of filtered corpus
filteredCorpus_rad