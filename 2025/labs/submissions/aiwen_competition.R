#############
### SETUP ###
#############

rm(list=ls())

# install.packages(c("tidyverse"))
library(tidyverse)
library(glmnet) # for multinomial logit
library(ranger) # random forest
library(xgboost) # boosting
library(recipes) 
library(rsample)
library(yardstick)
library(tidymodels) 

# set unique 5-digit seed
set.seed(03201)

####################
### SPOTIFY DATA ###
####################

# read in data
spotify_data = read_csv("../data/19_spotify-train.csv.gz")

# preview data
head(spotify_data)
names(spotify_data)

# keep only people with >= 10 songs
spotify_data <- spotify_data %>%
  add_count(`Added by`, name = "n_songs") %>%
  filter(n_songs >= 10) %>%
  select(-n_songs)

# clean & separate genres
spotify_data_long <- spotify_data %>%
  mutate(Genre = str_replace_all(Genre, ",",  ";")) %>% # in case there's commas instead of ;
  separate_rows(Genre, sep = ";\\s*") %>% 
  filter(Genre != "")

# pivot wider for genres
spotify_data_wide <- spotify_data_long %>% 
  mutate(val = 1) %>% 
  pivot_wider(
    names_from = Genre,
    values_from = val,
    names_prefix = "genre_",
    values_fill = list(val = 0), # put if a genre is missing
    values_fn = list(val = sum)  # combine duplicates
  )

####################
# prep data for train/test

split_obj <- initial_split(spotify_data_wide, prop = .80, strata = `Added by`)
train_tbl <- training(split_obj)
test_tbl <- testing(split_obj)

names(spotify_data_wide)

# recipe for pre-processing
rec <- recipe(`Added by` ~ ., data = train_tbl) %>% # added by is the outcome variable
  step_rm(`Track URI`, track, `Album Name`, `Artist Name(s)`, `Release Date`, 
          `Added At`, `Record Label`, Key, Mode, `Time Signature`) %>% # drop non-predictive text columns
  step_mutate(`Added by` = as.factor(`Added by`),
              Explicit = as.integer(Explicit)) %>% 
  step_center(all_of(c("Duration (ms)", "Popularity", "Danceability", "Loudness", "Energy", "Speechiness",
                       "Acousticness", "Instrumentalness", "Liveness", "Valence", "Tempo"))) %>% 
  step_scale (all_of(c("Duration (ms)", "Popularity", "Danceability", "Loudness", "Energy", "Speechiness",
                       "Acousticness", "Instrumentalness", "Liveness", "Valence", "Tempo")))

prep_rec <- prep(rec, training = train_tbl, retain = TRUE)

x_train <- bake(prep_rec, train_tbl, all_predictors())
x_test <- bake(prep_rec, test_tbl, all_predictors())

y_train <- bake(prep_rec, train_tbl, all_outcomes())$`Added by`
y_test <- bake(prep_rec, test_tbl, all_outcomes())$`Added by`

# assemble train & test df's
train_df <- x_train %>% mutate(Added_by = factor(y_train))
test_df <- x_test %>% mutate(Added_by = factor(y_test))

############################
# random forest

rf_spec <- 
  rand_forest(
    mtry  = floor(sqrt(ncol(x_train))),   # sqrt(p) 
    trees = 500,
    min_n = 5
  ) %>% 
  set_engine("ranger", probability = TRUE, importance = "impurity") %>% 
  set_mode("classification")

rf_fit <- fit(rf_spec, Added_by ~ ., data = train_df)
rf_prob <- predict(rf_fit, new_data = test_df, type = "prob") %>% 
  setNames(paste0(".pred_", names(.)))

############################
# boosting

boost_spec <- 
  boost_tree(
    trees = 1000,
    learn_rate = 0.05,
    tree_depth = 6,
    min_n = 2,
    sample_size = 1,
    mtry = 0.8
  ) %>% 
  set_engine("xgboost",
             objective = "multi:softprob",
             eval_metric = "mlogloss",
             nthread = parallel::detectCores(),
             counts = FALSE 
  ) %>% 
  set_mode("classification")

boost_fit <- fit(boost_spec, Added_by ~ ., data = train_df)
boost_prob <- predict(boost_fit, new_data = test_df, type = "prob") %>% 
  setNames(paste0(".pred_", names(.)))

############################
# multinomial logistic regression

cv_mlogit <- cv.glmnet(
  x = as.matrix(x_train),
  y = y_train,
  family = "multinomial",
  type.measure = "class",
  nfolds = 5,
  parallel = FALSE
)
mlogit_prob <- predict(cv_mlogit,
                       newx = as.matrix(x_test),
                       s = "lambda.min",
                       type = "response")[,,1]

mlogit_prob <- as_tibble(mlogit_prob) %>% 
  setNames(paste0(".pred_", names(.)))

############################
# compare log losses

rf_ll <- mn_log_loss(
  bind_cols(test_df %>% select(Added_by), rf_prob),
  truth = Added_by,
  !!!syms(names(rf_prob))
)$.estimate

boost_ll <- mn_log_loss(
  bind_cols(test_df %>% select(Added_by), boost_prob),
  truth = Added_by,
  !!!syms(names(boost_prob))
)$.estimate

mlogit_ll <- mn_log_loss(
  bind_cols(test_df %>% select(Added_by), mlogit_prob),
  truth = Added_by,
  !!!syms(names(mlogit_prob))
)$.estimate

logloss_tbl <- tibble(
  model = c("Multinomial Logit (glmnet)",
            "Random Forest (ranger)",
            "Gradient-Boosted Trees (xgboost)"),
  log_loss = c(mlogit_ll, rf_ll, boost_ll)
) %>% arrange(log_loss)

print(logloss_tbl)

##############################
# see the model at work
print("=== y_test (actual labels) ===")
print(y_test)

rf_pred <- predict(
  rf_fit,
  new_data = test_df,
  type = "class" 
)

print("=== rf_fit predictions (.pred_class) ===")
print(rf_pred$.pred_class)

############################
# find proportion of test set correctly classified
# random forest
rf_pred_class <- predict(rf_fit, new_data = test_df, type = "class")$.pred_class

# boosting 
boost_pred_class <- predict(boost_fit, new_data = test_df, type = "class")$.pred_class

# multinomial logit
mlogit_prob_mat <- predict(
  cv_mlogit,
  newx = as.matrix(x_test),
  s = "lambda.min",
  type = "response"
)[,,1]
mlogit_pred_class <- colnames(mlogit_prob_mat)[
  max.col(mlogit_prob_mat, ties.method = "first")
]

############################
# proportion correctly identified
acc_rf <- mean(rf_pred_class == y_test)
acc_boost <- mean(boost_pred_class == y_test)
acc_mlogit <- mean(mlogit_pred_class == y_test)

accuracy_tbl <- tibble(
  model = c("Random Forest (ranger)",
            "Gradient-Boosted Trees (xgboost)",
            "Multinomial Logit (glmnet)"),
  accuracy = c(acc_rf, acc_boost, acc_mlogit)
) %>%
  arrange(desc(accuracy))

print(accuracy_tbl)

# random forest predicts around 54% of songs correctly, not bad compared to random toss-up with 10 people
# best model is random forest 
final_model <- rf_fit 
