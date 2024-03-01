library(tidyverse)
library(readxl)
library(tidytext)
library(tidyr)
library(igraph)
library(tidygraph)
library(stringr)
library(widyr)
install.packages("ggraph")
library(ggraph)

############NOTE###################
# Some code was altered or copied from Assignment01-04. Additionally, some code
# was utilized from in-class examples.

#Upload Abstracts----

covid <- read_excel("C:/Users/watso/Box/PhD/HPMT 5336 - Introduction to NLP/Data/All_Articles_Excel_Augustuntil9October2020 (1).xlsx",
                    col_names = TRUE)

#Count Bigrams----
##Count Bigrams function----
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, Abstract, token = "ngrams", n = 2) %>%
    filter(!is.na(bigram)) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

##Visualize Bigrams function----
visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

#Part1----
##Sample 2,000 Abstracts----
set.seed(103)
abs_raw <- covid[sample(nrow(covid), 2000),]

##Count Bigrams----
abs_bigrams <- count_bigrams(abs_raw)

##Correlation of bigrams----
word_cors <- count_bigrams(abs_raw) %>%
  group_by(word1) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word1, word2, sort = TRUE)

###Filter therapeutic----
therp <- word_cors %>%
  filter(item1=="therapeutic") %>%
  arrange(desc(correlation)) %>%
  slice_head(n = 50)

####Visualize therapeutic----
visualize_bigrams(therp)

###Filter death----
death <- word_cors %>%
  filter(item1=="death") %>%
  arrange(desc(correlation)) %>%
  slice_head(n = 50)

####Visualize therapeutic----
visualize_bigrams(death)


#Part2----
##Remove tuples with <2 chars----
abs_bigrams_filter <- word_cors %>%
  filter(nchar(item1) >= 2) %>%
  filter(nchar(item2) >= 2)

###Remove Tuples with numbers----

abs_bigrams_filter <- abs_bigrams_filter %>% mutate(
  item1_numeric = !is.na(suppressWarnings(as.numeric(as.character(item1)))),
  item2_numeric = !is.na(suppressWarnings(as.numeric(as.character(item2))))
)

abs_bigrams_filter <- abs_bigrams_filter %>%
filter(!(item1_numeric|item2_numeric))


###Filter therapeutic----
therp <- abs_bigrams_filter %>%
  filter(item1=="therapeutic") %>%
  arrange(desc(correlation)) %>%
  slice_head(n = 50)

####Visualize therapeutic----
visualize_bigrams(therp)

###Filter death----


death <- abs_bigrams_filter %>%
  filter(item1=="death") %>%
  arrange(desc(correlation)) %>%
  slice_head(n = 50)

####Visualize death----
visualize_bigrams(death)
