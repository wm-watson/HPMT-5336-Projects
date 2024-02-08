
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
covid <- read_excel("C:/Users/watso/Downloads/All_Articles_Excel_Augustuntil9October2020 (1).xlsx", col_names = TRUE)

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
# Part2----

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
  mutate(china_count = cumsum(str_detect(word,regex("^[Cc]hin[ae][a-z]*",
                                                    ignore_case = TRUE))))%>%
    filter(china_count==1)%>%
    ungroup()

##Italy words----
ital_toks <- toks %>%
  group_by(word) %>%
  mutate(ital_count = cumsum(str_detect(word,regex("^[Ii]tal[a-z]*",
                                                    ignore_case = TRUE))))%>%
  filter(ital_count==1)%>%
  ungroup()
  
