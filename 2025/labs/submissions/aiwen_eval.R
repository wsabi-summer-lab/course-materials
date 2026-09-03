# Aiwen's Spotify Prediction Model Evaluation
# Trains on 19_spotify-train.csv.gz and evaluates on 19_spotify-test.csv.gz

library(tidyverse)
library(glmnet)
library(ranger)
library(xgboost)
library(recipes)
library(rsample)
library(yardstick)
library(tidymodels)

# Set seed
set.seed(03201)

# Load training data
spotify_train = read_csv("../data/19_spotify-train.csv.gz")

# Keep only people with >= 10 songs
spotify_train <- spotify_train %>%
  add_count(`Added by`, name = "n_songs") %>%
  filter(n_songs >= 10) %>%
  select(-n_songs)

# Clean & separate genres
spotify_train_long <- spotify_train %>%
  mutate(Genre = str_replace_all(Genre, ",",  ";")) %>%
  separate_rows(Genre, sep = ";\\s*") %>% 
  filter(Genre != "")

# Pivot wider for genres
spotify_train_wide <- spotify_train_long %>% 
  mutate(val = 1) %>% 
  pivot_wider(
    names_from = Genre,
    values_from = val,
    names_prefix = "genre_",
    values_fill = list(val = 0),
    values_fn = list(val = sum)
  )

# Recipe for pre-processing
rec <- recipe(`Added by` ~ ., data = spotify_train_wide) %>%
  step_rm(`Track URI`, track, `Album Name`, `Artist Name(s)`, `Release Date`, 
          `Added At`, `Record Label`, Key, Mode, `Time Signature`) %>%
  step_mutate(`Added by` = as.factor(`Added by`),
              Explicit = as.integer(Explicit)) %>% 
  step_center(all_of(c("Duration (ms)", "Popularity", "Danceability", "Loudness", "Energy", "Speechiness",
                       "Acousticness", "Instrumentalness", "Liveness", "Valence", "Tempo"))) %>% 
  step_scale(all_of(c("Duration (ms)", "Popularity", "Danceability", "Loudness", "Energy", "Speechiness",
                       "Acousticness", "Instrumentalness", "Liveness", "Valence", "Tempo")))

prep_rec <- prep(rec, training = spotify_train_wide, retain = TRUE)

x_train <- bake(prep_rec, spotify_train_wide, all_predictors())
y_train <- bake(prep_rec, spotify_train_wide, all_outcomes())$`Added by`

# Assemble training df
train_df <- x_train %>% mutate(Added_by = factor(y_train))

# Random forest specification
rf_spec <- 
  rand_forest(
    mtry  = floor(sqrt(ncol(x_train))),
    trees = 500,
    min_n = 5
  ) %>% 
  set_engine("ranger", probability = TRUE, importance = "impurity") %>% 
  set_mode("classification")

# Train random forest
rf_fit <- fit(rf_spec, Added_by ~ ., data = train_df)

# Load test data
spotify_test = read_csv("../data/19_spotify-test.csv.gz")

# Process test data the same way
spotify_test_long <- spotify_test %>%
  mutate(Genre = str_replace_all(Genre, ",",  ";")) %>%
  separate_rows(Genre, sep = ";\\s*") %>% 
  filter(Genre != "")

spotify_test_wide <- spotify_test_long %>% 
  mutate(val = 1) %>% 
  pivot_wider(
    names_from = Genre,
    values_from = val,
    names_prefix = "genre_",
    values_fill = list(val = 0),
    values_fn = list(val = sum)
  )

# Ensure test data has all columns from training data
missing_cols <- setdiff(names(spotify_train_wide), names(spotify_test_wide))
if(length(missing_cols) > 0) {
  spotify_test_wide[missing_cols] <- 0
}

# Ensure columns are in same order
spotify_test_wide <- spotify_test_wide[, names(spotify_train_wide)]

# Apply recipe to test data
x_test <- bake(prep_rec, spotify_test_wide, all_predictors())
y_test <- bake(prep_rec, spotify_test_wide, all_outcomes())$`Added by`

# Assemble test df
test_df <- x_test %>% mutate(Added_by = factor(y_test))

# Ensure test data has same columns as training data
missing_cols_test <- setdiff(names(train_df), names(test_df))
if(length(missing_cols_test) > 0) {
  test_df[missing_cols_test] <- 0
}

test_df <- test_df[, names(train_df)]

# Predict on test data
rf_prob <- predict(rf_fit, new_data = test_df, type = "prob") %>% 
  setNames(paste0(".pred_", names(.)))

# Get true labels for test data
y_test_actual <- test_df$Added_by

# Calculate log loss
log_loss <- mn_log_loss(
  bind_cols(test_df %>% select(Added_by), rf_prob),
  truth = Added_by,
  !!!syms(names(rf_prob))
)$.estimate

cat("=== Aiwen's Random Forest Model Test Log Loss ===\n")
cat("Log Loss:", log_loss, "\n")
cat("Test rows processed:", nrow(test_df), "\n") 