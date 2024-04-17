library(textrecipes)
library(recipes)
library(future)
plan(multisession)
library(ranger)
library(tidymodels)
library(tidyverse)
library(tidymodels)
library(LiblineaR)
library(rsample)

############NOTE###################
# Some code was altered or copied from Assignment01-07. Additionally, some code
# was utilized from in-class examples.

# Part 1----
## Movie Review Data----
review_raw <- read.csv(file = "/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/HPMT 5336 - Introduction to NLP/Data/movieReviewSnippets_GroundTruth.csv",
                       header = TRUE)

### Histogram----
hist(review_raw$sentiment)

## Training/Test----

review_split <- review_raw %>%
  initial_split()

review_train <- training(review_split)
review_test <- testing(review_split)

##Recipe----

review_rec <- recipe(sentiment ~ text, data = review_train) %>%
  textrecipes::step_tokenize(text) %>%
  textrecipes::step_tokenfilter(text, max_tokens = 1e3) %>%
  textrecipes::step_tfidf(text) %>%
  step_normalize(all_predictors())

review_prep <- prep(review_rec)
review_bake <- bake(review_prep, new_data = NULL)

review_wf <- workflow() %>%
  add_recipe(review_rec)

## specify the model----

svm_spec <- svm_linear() %>%
  set_mode("regression") %>%
  set_engine("LiblineaR") 

# add the model to our workflow, train----
svm_fit <- review_wf %>%
  add_model(svm_spec) %>%
  fit(data = review_train)

# examine model coefficients - positive
svm_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  arrange(-estimate)

# examine model coefficients - negative
svm_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  arrange(estimate)

# Part 2----
## K-cross folds----
set.seed(123)
review_folds <- vfold_cv(review_train, 10)

review_folds

## Train model on folds----
set.seed(123)
svm_rs <- fit_resamples(
  review_wf %>% add_model(svm_spec),
  review_folds,
  control = control_resamples(save_pred = TRUE)
)

svm_rs

collect_metrics(svm_rs)