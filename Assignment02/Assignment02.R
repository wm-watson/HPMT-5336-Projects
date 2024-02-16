
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(scales)
library(readxl)
library(cld2)
library(stopwords)

############NOTE###################
# Some code was altered or copied from Assignment01. Additionally, some code
# was utilized from in-class examples.

# Part 1----
## Upload Data----
covid <- read_excel("C:/Users/watso/Box/PhD/HPMT 5336 - Introduction to NLP/Data/All_Articles_Excel_Augustuntil9October2020 (1).xlsx", col_names = TRUE)

## Detect Languages----
detect_language_mixed(covid$Abstract)

##Spanish Only----

span_covid <- covid %>%
  filter(Language == 'es')

### Tokenize----
covid_tidy_stop <- span_covid %>%
  unnest_tokens(word, Abstract) %>%
  group_by(word) %>%
  summarise(total = n()) %>%
  ungroup()


### Spanish/English Stopwords----

stop_es <- tibble(word=stopwords::stopwords('es'))

stop_en <- tibble(word=stopwords::stopwords('en'))

toks <- covid_tidy_stop %>%
  anti_join(stop_es) %>%
  anti_join(stop_en)

##Plot Top 20----

### Top 20 words----
toks_plot_20 <- toks %>%
  arrange(desc(total)) %>%
  slice_head(n = 20)


 toks_plot_20 %>%
  ggplot(aes( x = reorder(word, total), y = total)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Barplot of Spanish Words: No Stopwords",
       x = NULL,
       y = "Total Count") +
  scale_y_continuous(labels = scales::comma)

print(toks_plot_20)

rm(toks_plot_20)
rm(covid_tidy_stop)
rm(span_covid)
rm(stop_es)
rm(toks)
# Part 2----

## English Abstracts----
covid_eng <- covid %>%
  filter(Language == 'en')

##Tokenize----
covid_tidy_stop <- covid_eng %>%
  unnest_tokens(word, Abstract) %>%
  group_by(word) %>%
  summarise(total = n()) %>%
  ungroup()

##Remove English Stopwords----
toks <- covid_tidy_stop %>%
  anti_join(stop_en)

## China Words----
chin_toks <- toks %>%
  group_by(word) %>%
  mutate(cntry_count = cumsum(str_detect(word,regex("^[Cc]hin[ae][a-z]*",
                                                    ignore_case = TRUE))))%>%
    filter(cntry_count==1)%>%
    ungroup()

##Italy words----
ital_toks <- toks %>%
  group_by(word) %>%
  mutate(cntry_count = cumsum(str_detect(word,regex("^[Ii]tal[a-z]*",
                                                    ignore_case = TRUE))))%>%
  filter(cntry_count==1)%>%
  ungroup()

##Bind Together----
chin_ital <- rbind(ital_toks, chin_toks)

##Plot----
### Top 8 words----
toks_plot_8 <- chin_ital %>%
  arrange(desc(total)) %>%
  slice_head(n = 8)

toks_plot_8 %>%
  ggplot(aes( x = reorder(word, total), y = total)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Barplot of Top Eight Words Containing 'Ital', 'Chine', or 'China'",
       x = NULL,
       y = "Total Count") +
  scale_y_continuous(labels = scales::comma)

rm(chin_ital)
rm(chin_toks)
rm(ital_toks)
rm(toks)
rm(toks_plot_8)


#Part 3----

##Tokenize----
covid_bigrams <- covid_eng %>%
  unnest_tokens(word, Abstract, token = "ngrams", n = 2) %>%
  group_by(word) %>%
  summarise(total = n()) %>%
  ungroup()

stop_es <- tibble(word=stopwords::stopwords('es'))

stop_en <- tibble(word=stopwords::stopwords('en'))

##Remove English Stopwords----
toks <- covid_tidy_stop %>%
  anti_join(stop_en) %>%
  anti_join(stop_es)

## Count Bigrams----

NROW(toks)

##Plot w/Stopwords----
### Top 20 words----
toks_plot_20 <- toks %>%
  arrange(desc(total)) %>%
  slice_head(n = 20)

toks_plot_20 %>%
  ggplot(aes( x = reorder(word, total), y = total)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Barplot of Top 20 Bigrams Contained in English Abstracts",
       x = NULL,
       y = "Total Count") +
  scale_y_continuous(labels = scales::comma)

