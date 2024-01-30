###Portions of the below code were created in class or were altered from the Git repository
###associated with the book

library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(scales)
library(readxl)

# Upload Data----
covid <- read_excel("/Users/williamwatson/Downloads/All_Articles_Excel_Dec2019July2020.xlsx", col_names = TRUE)

#Prepare Data----
## Tokenization----
###Stopwords: Count most common words----

#### Stopwords: Unnest----
covid_tidy_stop <- covid %>%
  unnest_tokens(word, Abstract) %>%
  group_by(word) %>%
  summarise(total = n()) %>%
  ungroup()

#### Top Words----
top_words <- covid_tidy_stop %>%
  arrange(desc(total)) %>%
  slice_head(n = 20)



#### Plot All Words----
  top_words_plot <- top_words %>%
    ggplot(aes( x = reorder(word, total), y = total)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() + 
    theme_minimal() +
    labs(title = "Barplot of All Words: Stopwords Included",
         x = NULL,
         y = "Total Count") +
  scale_y_continuous(labels = scales::comma)

print(top_words_plot)

## Remove Stop Words----
### Stopwords data----
data(stop_words)


#### Unnest----
covid_tidy <- covid %>%
  unnest_tokens(word, Abstract) %>%
  group_by(word) %>%
  summarise(total = n()) %>%
  ungroup()

#### Anti-join----
covid_tidy <- covid_tidy %>%
  anti_join(stop_words)

###Summary Stats----
####Count most common words----
covid_tidy %>%
  count(word, sort = TRUE)

#### Top Words----
top_words_no_stop <- covid_tidy %>%
  arrange(desc(total)) %>%
  slice_head(n = 20)


#### Plot All Words----
top_words_plot_no_stop <- top_words_no_stop %>%
  ggplot(aes( x = reorder(word, total), y = total)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Barplot of All Words: No Stopwords ",
       x = NULL,
       y = "Total Count") +
  scale_y_continuous(labels = scales::comma)

print(top_words_plot_no_stop)
