library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(scales)
library(readxl)

# Upload Data----
covid <- read_excel("/Users/williamwatson/Downloads/All_Articles_Excel_Dec2019July2020.xlsx", col_names = TRUE)

#### Stopwords: Unnest----
covid_tidy_stop <- covid %>%
  unnest_tokens(word, Abstract) %>%
  group_by(word) %>%
  summarise(total = n()) %>%
  ungroup()
#### Unnest----
covid_tidy <- covid %>%
  unnest_tokens(word, Abstract) %>%
  group_by(word) %>%
  summarise(total = n()) %>%
  ungroup()

#### Anti-join----
covid_tidy <- covid_tidy %>%
  anti_join(stop_words)