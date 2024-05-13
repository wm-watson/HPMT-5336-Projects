library(topicmodels)
library(reshape2)
library(tidyverse)
library(tidytext)
library(fuzzyjoin)
library(sqldf)
library(SnowballC)
library(LDAvis)
library(tsne)

###########     Will Watson Code      ############

######Maternal CM NLP----
#DATA----
##CM Contact Data----
con_raw <- read.csv("C:/Users/wpwatson/OneDrive - arkbluecross.com/Documents/Watson/PhD/NLP/mem_contact.csv",
                    header = TRUE)

con_reduce <- con_raw %>%
  dplyr::select(c('ID', 'MVDID','FORMDATE', 'QMEMBERCONTACTEDTODAY', 'Q5CONTACTMETHOD', 'QMATERNITYMEMBER', 'QMATERNITYMOMDUEDATE', 
                  'QMATERNITYPROBLEMS', 'QUTERINECONTRACTIONS', 'QMATERNITYSYMPTOMS',
                  'QMATERNITYOTHER', 'QLASTDOCTORVISIT', 'QBABYGESTATIONAL', 
                  'QBABYBIRTHWEIGHT', 'QBABYCURENTWEIGHT', 'QCMCOMMENT', 
                  'QMATERNITYMOMCOMMENT', 'QMATERNITYBABYCOMMENT'))



##Complex Assessment----
complex_raw <- read.csv("C:/Users/wpwatson/OneDrive - arkbluecross.com/Documents/Watson/PhD/NLP/Maternal_Complex_Assessment.csv",
                        header = TRUE)


comp_reduce <- complex_raw %>%
  dplyr::select(c('ID', 'MVDID', 'FORMDATE', 'Q1QUESTIONS', 'Q2MEDCAREEMOTIONALCOND', 'Q4MEDCAREMEDCOND', 'Q16STREETDRUGS',
                  'Q17FAMILYHISTORY', 'Q18CONCERNSOTHER', 'Q34STRESSLEVEL', 'Q35PHYSICALLYDEMANDING', 'Q37', 'Q44CSECTION',
                  'Q47LABOR', 'Q50', 'Q56GESTATION', 'Q57PRETERM', 'Q57HOWMANYBABIES', 'Q58BIRTHWEIGHT', 'Q62BIRTHWEIGHT'))



##Get Unique MVDIDs Contact and Assessment----


unq_con <- as.data.frame(unique(comp_reduce$MVDID)) %>%
  mutate(MVDID = unique(comp_reduce$MVDID)) %>%
  dplyr::select("MVDID")

unq_contact <- left_join(unq_con, con_reduce, by = "MVDID")

## Get last assessment per MVDID----

unq_comp <- comp_reduce %>%
  group_by(MVDID) %>%
  filter(FORMDATE == max(FORMDATE))


##Filter Complex for Term/Preterm----
comp_filtered <- unq_comp %>%
  filter(grepl("^(Term|Preterm)", Q57PRETERM))

##Merge if within 9 months----
#Fix FORMDATE so they both match
con_reduce$FORMDATE <- substr(con_reduce$FORMDATE, start = 1, stop = 10)
comp_filtered$FORMDATE <- substr(comp_filtered$FORMDATE, start = 1, stop = 10)


#Convert formdate
con_reduce$FORMDATE <- ymd(con_reduce$FORMDATE)
comp_filtered$FORMDATE <- ymd(comp_filtered$FORMDATE)

#Calculate 9 months prior
comp_filtered <- comp_filtered %>%
  mutate(start_date = FORMDATE - months(9),
         end_date = FORMDATE)

#Couldnt get merge to work--went to SQL
merged_raw <- sqldf("
                    select a.*, b.*
                    from con_reduce a
                    join comp_filtered b
                    on a.MVDID = b.MVDID
                    where a.formdate between b.formdate and b.end_date
                    ")

merged_raw <- read.csv("R:/GraduateStudents/WatsonWilliamP/NLP/merged_raw.csv",
                       header = TRUE)

# TOPIC MODELING----

install.packages("C:/Users/1187507/Downloads/Matrix_1.5-3.tar.gz", repos = NULL,
                 type = "source")
library(Matrix)
library(dplyr)
library(tidyr)
library(tidytext)
library(reshape2)
library(ggplot2)
library(quanteda)
library(readxl)
library(corpus)
require(topicmodels)

############NOTE###################
# Some code was altered or copied from Assignment01-06. Additionally, some code
# was utilized from in-class examples.

# Part 1----
## Data----

set.seed(123)

#Drop duplicated columns
merged_raw <- merged_raw[,-c(19,20,21)]

#Random Sample
# texts <-  sample_n(merged_raw, 2500)
texts <- merged_raw
#Rename Columns
texts <- texts %>%
  rename(note_id = ID) %>%
  rename(text = QMATERNITYMOMCOMMENT)

#Get rid of missing text
texts <- texts[texts$text !="",]

# Get Unique Doc IDs----
corpus <- texts %>%
  distinct(note_id, .keep_all = TRUE)


#Change docnames
## Create Corpus----
corpus <- corpus(corpus$text, docnames = corpus$note_id)



## Tokenize and create a document-term matrix----
corpus_tokens <- corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>%
  tokens_remove(pattern = stopwords('en'))

#Custom word list
custom_words <- c("mbr", "member", "call", "states", "reports", "encouraged", "understanding", "can", "s", "f", "u", "day", "informed",
                  "discussed", "rn", "bsn", "cm", "email", "case", "w", "ccm", "mem")

## Document-term Matrix----
DTM <- corpus_tokens %>% 
  tokens_remove("") %>%
  #tokens_remove(custom_words) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 3, min_docfreq = 0.01, docfreq_type = "prop")
# dfm_trim(max_termfreq = 500, max_docfreq = 0.75)

#I have empty documents
empty_docs <- which(rowSums(DTM) == 0) %>%
  print()

#Remove Empty
DTM <- DTM[-empty_docs,]


#Find freqs
term_freq <- colSums(as.matrix(DTM))
sorted_term_freq <- as.data.frame(sort(term_freq, decreasing = TRUE))

## Examine
dim(DTM)
sorted_term_freq

## Topic Model (LDA)----
# load package topicmodels
require(topicmodels)
# number of topics
K <- 4
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



library(textrecipes)
library(recipes)
library(tidymodels)
library(text)
#Classification----

## Test/Train----
classification <- merged_raw %>%
  mutate(Delivery = factor(if_else(
    Q57PRETERM == "Term (delivered 37 weeks or greater)", "Term",
    if_else(Q57PRETERM == "Preterm (delivered 36 weeks or less)", "Preterm", NA_character_)
  ))) %>%
  rename(text = QMATERNITYMOMCOMMENT)

# Handling NAs if needed
classification <- drop_na(classification, Delivery)

# Splitting the dataset while ensuring both classes are present in splits
class_split <- initial_split(classification, strata = Delivery)

# Extracting training and testing sets
class_train <- training(class_split)
class_test <- testing(class_split)

# Checking the distribution of the target variable in both sets
table(class_train$Delivery)
table(class_test$Delivery)

##Pre Process Data----
#Custom word list
custom_words <- c("mbr", "member", "call", "states", "reports", "encouraged", "understanding", "can", "s", "f", "u", "day", "informed",
                  "discussed", "rn", "bsn", "cm", "email", "case", "w", "ccm", "mem")


recipe_obj <- recipe(Delivery ~ text, data = class_train) %>%
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens = 1e3) %>%
  step_tfidf(text)


#add to workflow
class_wf <- workflow() %>% 
  add_recipe(recipe_obj)

###Naive Bayes----
library(discrim)
library(naivebayes)
nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")

nb_spec

####Fit model----
nb_fit <- class_wf %>% 
  add_model(nb_spec) %>% 
  fit(data = class_train)


###Evaluate----
#10-fold cross-validation

set.seed(234)
class_folds <- vfold_cv(class_train)

class_folds

nb_wf <- workflow() %>%
  add_recipe(recipe_obj) %>%
  add_model(nb_spec)

nb_wf

#Fit model many times
nb_rs <- fit_resamples(
  nb_wf,
  class_folds,
  control = control_resamples(save_pred = TRUE)
)

#Metrics
nb_rs_metrics <- collect_metrics(nb_rs)
nb_rs_predictions <- collect_predictions(nb_rs)

nb_rs_metrics

#Plot ROC
nb_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = Delivery, .pred_Term) %>%
  autoplot() +
  labs(
    color = NULL,
    title = "ROC Curve for Maternal Case Management",
    subtitle = "Naive Bayes"
  )

conf_mat_resampled(nb_rs, tidy = FALSE) %>%
  autoplot(type = "heatmap")

####Compare to the Null Model----
null_classification <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("classification")

null_rs <- workflow() %>%
  add_recipe(recipe_obj) %>%
  add_model(null_classification) %>%
  fit_resamples(
    class_folds
  )

#Null Model Performance
null_rs %>%
  collect_metrics()

##Lasso Regression----
lasso_spec <- logistic_reg(penalty = 0.01, mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

lasso_spec

#Make Workflow
lasso_wf <- workflow() %>%
  add_recipe(recipe_obj) %>%
  add_model(lasso_spec)

lasso_wf

library(glmnet)


#Fit model
set.seed(2020)
lasso_rs <- fit_resamples(
  lasso_wf,
  class_folds,
  control = control_resamples(save_pred = TRUE)
)

#Performance Metrics
lasso_rs_metrics <- collect_metrics(lasso_rs)
lasso_rs_predictions <- collect_predictions(lasso_rs)

lasso_rs_metrics

#Lasso Plots
lasso_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = Delivery, .pred_Term) %>%
  autoplot() +
  labs(
    color = NULL,
    title = "ROC Curve for Maternal Clinical Case Management",
    subtitle = "Lasso Regression"
  )

conf_mat_resampled(lasso_rs, tidy = FALSE) %>%
  autoplot(type = "heatmap")

##Tunning Lasso hyperparameteres----
tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

tune_spec

#Create Grid of Values
lambda_grid <- grid_regular(penalty(), levels = 30)
lambda_grid

#Add Tune to the workflow
tune_wf <- workflow() %>%
  add_recipe(recipe_obj) %>%
  add_model(tune_spec)

set.seed(2020)
tune_rs <- tune_grid(
  tune_wf,
  class_folds,
  grid = lambda_grid,
  control = control_resamples(save_pred = TRUE)
)

tune_rs

collect_metrics(tune_rs)


#Plot
autoplot(tune_rs) +
  labs(
    title = "Lasso model performance across regularization penalties",
    subtitle = "Performance metrics can be used to identity the best penalty"
  )

#Get best penalty
tune_rs %>%
  show_best(metric = "roc_auc", n=5)

#Get best auc
chosen_auc <- tune_rs %>% 
  select_by_one_std_err(metric = "roc_auc", -penalty)

#Final Lasso----
final_lasso <- finalize_workflow(tune_wf, chosen_auc)
final_lasso

fitted_lasso <- fit(final_lasso, class_train)

#Results of final Lasso
fitted_lasso %>% 
  extract_fit_parsnip() %>% 
  tidy() %>% 
  arrange(-estimate)

fitted_lasso %>% 
  extract_fit_parsnip() %>% 
  tidy() %>% 
  arrange(estimate)

#Performance Metrics
fit_lasso_rs_metrics <- collect_metrics(tune_rs)
fit_lasso_rs_predictions <- collect_predictions(tune_rs)


fit_lasso_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = Delivery, .pred_Term) %>%
  autoplot() +
  labs(
    color = NULL,
    title = "ROC Curve for Maternal Clinical Case Management",
    subtitle = "Final Lasso"
  )

#Final Lasso metrics
fit_lasso_rs_metrics


##Sparse Encoding----
library(hardhat)
sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

#Add to workflow
sparse_wf <- workflow() %>%
  add_recipe(recipe_obj, blueprint = sparse_bp) %>%
  add_model(tune_spec)

sparse_wf

#Parameter Tuning
smaller_lambda <- grid_regular(penalty(range = c(-5, 0)), levels = 20)
smaller_lambda


set.seed(2020)
sparse_rs <- tune_grid(
  sparse_wf,
  class_folds,
  grid = smaller_lambda,
  control = control_resamples(save_pred = TRUE)
)

sparse_rs

#Best ROC
sparse_rs %>%
  show_best(metric = "roc_auc", n = 5)

#Collect Metrics
sparse_rs_metrics <- collect_metrics(sparse_rs)
sparse_rs_predictions <- collect_predictions(sparse_rs)

#Embed Plots
sparse_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = Delivery, .pred_Term) %>%
  autoplot() +
  labs(
    color = NULL,
    title = "ROC Curve for Maternal Clinical Case Management",
    subtitle = "Sparse Embedding"
  )

##Final Model: Penalties and Tokens----
#Final sparse and number of tokens
class_rec_v2 <-
  recipe(Delivery ~ text, data = class_train)

class_rec_v2 <- class_rec_v2 %>%
  step_tokenize(text) %>%
  step_tokenfilter(text,
                   max_tokens = tune(), min_times = 100) %>%
  step_tfidf(text)

#Specify the Model
sparse_wf_vs <- sparse_wf %>% 
  update_recipe(class_rec_v2, blueprint = sparse_bp)

sparse_wf_vs

###Tune the model----
#Different Params for Tokens and penalty
final_grid <- grid_regular(
  penalty(range = c(-4, 0)),
  max_tokens(range = c(1e2, 1e3)),
  levels = c(penalty = 20, max_tokens = 3)
)

final_grid

#Run the model
set.seed(2020)
tune_rs <- tune_grid(
  sparse_wf_vs,
  class_folds,
  grid = final_grid,
  metrics = metric_set(roc_auc, accuracy, sensitivity, specificity)
)


#Evaluate the Modeling
autoplot(tune_rs) +
  labs(
    color = "Number of tokens",
    title = "Model performance across regularization penalties and tokens"
  )

#Choose the most accurate
choose_roc <- tune_rs %>%
  select_best(metric = "roc_auc")

choose_roc

##Final Model----
#Update Final workflow
final_wf <- finalize_workflow(sparse_wf_vs, choose_roc)
final_wf

#Run final model on training and evaluate on testing
final_fitted <- last_fit(final_wf, class_split)

collect_metrics(final_fitted)

#Confusion Matrix
collect_predictions(final_fitted) %>%
  conf_mat(truth = Delivery, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

#Roc Curve
collect_predictions(final_fitted)  %>%
  roc_curve(truth = Delivery, .pred_Term) %>%
  autoplot() +
  labs(
    color = NULL,
    title = "ROC Curve for Maternal Clinical Case Management",
    subtitle = "With final tuned lasso regularized classifier on the test set"
  )

#Results of final Lasso
final_fitted %>% 
  extract_fit_parsnip() %>% 
  tidy() %>% 
  arrange(-estimate)

final_fitted %>% 
  extract_fit_parsnip() %>% 
  tidy() %>% 
  arrange(estimate)
