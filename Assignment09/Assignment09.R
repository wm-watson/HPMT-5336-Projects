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

## Plot----
svm_rs %>%
  collect_predictions() %>%
  ggplot(aes(sentiment, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Truth",
    y = "Predicted sentiment",
    color = NULL,
    title = "Predicted and true sentiment for a corpus of Movie Reviews",
    subtitle = "Each cross-validation fold is shown in a different color"
  )

# Part 3----
## Null Model----
null_regression <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("regression")

null_rs <- fit_resamples(
  review_wf %>% add_model(null_regression),
  review_folds,
  metrics = metric_set(rmse)
)

null_rs
collect_metrics(null_rs)

## Remove Stopwords: Snowball----
library(SnowballC)

stopword_rec <- function(stopword_name) {
  recipe(sentiment ~ text, data = review_train) %>%
    step_tokenize(text) %>%
    step_stopwords(text, stopword_source = stopword_name) %>%
    step_tokenfilter(text, max_tokens = 1e3) %>%
    step_tfidf(text) %>%
    step_normalize(all_predictors())
}

stopword_rec("snowball")


svm_wf <- workflow() %>%
  add_model(svm_spec)

svm_wf

set.seed(123)
snowball_rs <- fit_resamples(
  svm_wf %>% add_recipe(stopword_rec("snowball")),
  review_folds
)

set.seed(234)
smart_rs <- fit_resamples(
  svm_wf %>% add_recipe(stopword_rec("smart")),
  review_folds
)

set.seed(345)
stopwords_iso_rs <- fit_resamples(
  svm_wf %>% add_recipe(stopword_rec("stopwords-iso")),
  review_folds
)

# After fitting models to each of the cross-validation folds, these sets of results contain metrics computed for removing that set of stop words.


collect_metrics(smart_rs)


word_counts <- tibble(name = c("snowball", "smart", "stopwords-iso")) %>%
  mutate(words = map_int(name, ~length(stopwords::stopwords(source = .))))

show_best <- function(results, metric) {
  results %>%
    collect_metrics() %>%
    filter(.metric == metric) %>%
    arrange(mean) %>%
    slice(1)
}

list(snowball = snowball_rs,
     smart = smart_rs,
     `stopwords-iso` = stopwords_iso_rs) %>%
  map_dfr(~show_best(., "rmse"), .id = "name") %>%
  left_join(word_counts, by = "name") %>%
  mutate(name = paste0(name, " (", words, " words)"),
         name = fct_reorder(name, words)) %>%
  ggplot(aes(name, mean, color = name)) +
  geom_crossbar(aes(ymin = mean - std_err, ymax = mean + std_err), alpha = 0.6) +
  geom_point(size = 3, alpha = 0.8) +
  theme(legend.position = "none") +
  labs(x = NULL, y = "RMSE",
       title = "Model performance for three stop word lexicons",
       subtitle = "For this data set, the Snowball lexicon performed best")

## Varying N-Grams----
ngram_rec <- function(ngram_options) {
  recipe(sentiment ~ text, data = review_train) %>%
    step_tokenize(text, token = "ngrams", options = ngram_options) %>%
    step_tokenfilter(text, max_tokens = 1e3) %>%
    step_tfidf(text) %>%
    step_normalize(all_predictors())
}

# Trigrams to Ungram
ngram_rec(list(n = 3, n_min = 1))

# Add to recipe
fit_ngram <- function(ngram_options) {
  fit_resamples(
    svm_wf %>% add_recipe(ngram_rec(ngram_options)),
    review_folds
  )
}

set.seed(123)
unigram_rs <- fit_ngram(list(n = 1))

set.seed(234)
bigram_rs <- fit_ngram(list(n = 2, n_min = 1))

set.seed(345)
trigram_rs <- fit_ngram(list(n = 3, n_min = 1))

#Get Metrics of Models
collect_metrics(unigram_rs)
collect_metrics(bigram_rs)
collect_metrics(trigram_rs)


## Lemmatization----
library(spacyr)

spacy_install()
spacyr::spacy_initialize(entity = FALSE)

lemma_rec <- recipe(sentiment ~ text, data = review_train) %>%
  step_tokenize(text, engine = "spacyr") %>%
  step_lemma(text, role = "predictor", trained = TRUE) %>%
  step_tokenfilter(text, max_tokens = 1e3) %>%
  step_tfidf(text) %>%
  step_normalize(all_predictors())

lemma_rec

#Combine with workflow
lemma_rs <- fit_resamples(
  svm_wf %>% add_recipe(lemma_rec),
  review_folds
)

#How did it perform?
collect_metrics(lemma_rs)

## Tuneing Parameteres----
final_rec <- recipe(sentiment ~ text, data = review_train) %>%
  step_tokenize(text, token = "ngrams", options = list(n = 2, n_min = 1)) %>%
  step_tokenfilter(text, max_tokens = tune()) %>%
  step_tfidf(text) %>%
  step_normalize(all_predictors())

final_rec

# Specify Model
svm_spec <- svm_linear() %>%
  set_mode("regression") %>%
  set_engine("LiblineaR")

svm_spec

#Tune workflow
tune_wf <- workflow() %>%
  add_recipe(final_rec) %>%
  add_model(svm_spec)

tune_wf

# Parameters
final_grid <- grid_regular(
  max_tokens(range = c(1e3, 6e3)),
  levels = 6
)

final_grid

#Final Model
final_rs <- tune_grid(
  tune_wf,
  scotus_folds,
  grid = final_grid,
  metrics = metric_set(rmse, mae, mape),
  control = control_resamples(save_pred = TRUE)
)

final_rs

#Graph Tuning
final_rs %>%
  collect_metrics() %>%
  ggplot(aes(max_tokens, mean, color = .metric)) +
  geom_line(size = 1.5, alpha = 0.5) +
  geom_point(size = 2, alpha = 0.9) +
  facet_wrap(~.metric, scales = "free_y", ncol = 1) +
  theme(legend.position = "none") +
  labs(
    x = "Number of tokens",
    title = "Linear SVM performance across number of tokens",
    subtitle = "Performance improves as we include more tokens"
  )

#Get Best model
chosen_mae <- final_rs %>%
  select_by_pct_loss(metric = "mae", max_tokens, limit = 3)

chosen_mae

#Add to workflow
final_wf <- finalize_workflow(tune_wf, chosen_mae)

final_wf

#Fit model on Training Data
final_fitted <- last_fit(final_wf, review_split)

collect_metrics(final_fitted)

# Words most important to predicting
review_fit <- extract_fit_parsnip(final_fitted$.workflow[[1]])

review_fit %>%
  tidy() %>%
  filter(term != "Bias") %>%
  mutate(
    sign = case_when(estimate > 0 ~ "Positive (0 = neutral)",
                     TRUE ~ "Negative (0 = neutral)"),
    estimate = abs(estimate),
    term = str_remove_all(term, "tfidf_text_")
  ) %>%
  group_by(sign) %>%
  top_n(20, estimate) %>%
  ungroup() %>%
  ggplot(aes(x = estimate,
             y = fct_reorder(term, estimate),
             fill = sign)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap(~sign, scales = "free") +
  labs(
    y = NULL,
    title = paste("Variable importance for predicting rating", 
                  " of Amazon movie reviews"),
    subtitle = paste("These features are the most importance",
                     "in predicting the rating of movie")
  )


#Plot prediciton
final_fitted %>%
  collect_predictions() %>%
  ggplot(aes(sentiment, .pred)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Truth",
    y = "Predicted (sentiment)",
    title = paste("Predicted and true sentiments for the testing set of",
                  "Amazon movie reviews"),
    subtitle = "For the testing set, predictions are more reliable after 1850"
  )
