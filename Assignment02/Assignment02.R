
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(scales)
library(readxl)
install.packages("cld2")
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

detect_language_mixed(covid_tidy_stop$Abstract)

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
