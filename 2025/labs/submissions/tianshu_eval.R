# Tianshu's Spotify Prediction Model Evaluation
# Trains on 19_spotify-train.csv.gz and evaluates on 19_spotify-test.csv.gz

library(cluster)
library(factoextra)
library(flexclust)
library(ggdendro)
library(ggplot2)
library(mclust)
library(tidyverse)
library(xgboost)
library(yardstick)

# Set seed
set.seed(19104)

# Load training data
spotify_train = read_csv("../data/19_spotify-train.csv.gz")

# Process training data
spotify_train = spotify_train %>%
  mutate(`Genre List` = str_split(Genre, ";\\s*")) %>%
  mutate(`Artist List` = str_split(`Artist Name(s)`, ",\\s*"))

spotify_train_cleaned = spotify_train %>%
  select_if(is.numeric) %>%
  mutate(across(everything(), ~ scale(.) %>% as.vector()))

# Genre one-hot encoding
spotify_long = spotify_train %>%
  select(-Genre) %>%
  unnest(`Genre List`)

spotify_wide = spotify_long %>%
  mutate(has_genre = 1) %>%
  pivot_wider(
    id_cols = `Track URI`,
    names_from = `Genre List`,
    values_from = has_genre,
    values_fill = list(has_genre = 0),
    values_fn = list(has_genre = max)
  )

# Join spotify_wide to spotify_train_cleaned
spotify_train_cleaned = spotify_train_cleaned %>%
  mutate(`Track URI` = spotify_train$`Track URI`) %>%
  left_join(spotify_wide, by = "Track URI") %>%
  mutate(`Added by` = spotify_train$`Added by`) %>%
  group_by(`Added by`) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(Added_by_label = as.numeric(as.factor(`Added by`)) - 1)

# Artist one-hot encoding
spotify_long = spotify_train %>%
  select(-`Artist Name(s)`) %>%
  unnest(`Artist List`)

spotify_wide = spotify_long %>%
  mutate(has_artist = 1) %>%
  pivot_wider(
    id_cols = `Track URI`,
    names_from = `Artist List`,
    values_from = has_artist,
    values_fill = list(has_artist = 0),
    values_fn = list(has_artist = max)
  )

# Join spotify_wide to spotify_train_cleaned
spotify_train_cleaned = spotify_train_cleaned %>%
  mutate(`Track URI` = spotify_train$`Track URI`) %>%
  left_join(spotify_wide, by = "Track URI") %>%
  mutate(`Added by` = spotify_train$`Added by`) %>%
  group_by(`Added by`) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(Added_by_label = as.numeric(as.factor(`Added by`)) - 1)

# Prepare training data
X_train <- as.matrix(spotify_train_cleaned[, !names(spotify_train_cleaned) %in% c("Track URI", "Added by", "Added_by_label")])
y_train <- spotify_train_cleaned$Added_by_label

# Quick model to get feature importance
quick_model <- xgboost(data = X_train, label = y_train, nrounds = 20, objective = "multi:softprob", num_class = length(unique(y_train)), verbose = 0)

importance <- xgb.importance(model = quick_model)
top_features <- importance$Feature[1:30]
X_train_top <- X_train[, top_features]

# Train final model
params <- list(
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = length(unique(y_train)),
  max_depth = 6,
  eta = 0.3,
  subsample = 0.8,
  colsample_bytree = 0.8
)

dtrain <- xgb.DMatrix(data = X_train_top, label = y_train)
final_model <- xgb.train(params, dtrain, nrounds = 100, verbose = 0)

# Load test data
spotify_test = read_csv("../data/19_spotify-test.csv.gz")

# Process test data the same way
spotify_test = spotify_test %>%
  mutate(`Genre List` = str_split(Genre, ";\\s*")) %>%
  mutate(`Artist List` = str_split(`Artist Name(s)`, ",\\s*"))

spotify_test_cleaned = spotify_test %>%
  select_if(is.numeric) %>%
  mutate(across(everything(), ~ scale(.) %>% as.vector()))

# Genre one-hot encoding for test data
spotify_long_test = spotify_test %>%
  select(-Genre) %>%
  unnest(`Genre List`)

spotify_wide_test = spotify_long_test %>%
  mutate(has_genre = 1) %>%
  pivot_wider(
    id_cols = `Track URI`,
    names_from = `Genre List`,
    values_from = has_genre,
    values_fill = list(has_genre = 0),
    values_fn = list(has_genre = max)
  )

# Join spotify_wide_test to spotify_test_cleaned
spotify_test_cleaned = spotify_test_cleaned %>%
  mutate(`Track URI` = spotify_test$`Track URI`) %>%
  left_join(spotify_wide_test, by = "Track URI") %>%
  mutate(`Added by` = spotify_test$`Added by`)

# Artist one-hot encoding for test data
spotify_long_test = spotify_test %>%
  select(-`Artist Name(s)`) %>%
  unnest(`Artist List`)

spotify_wide_test = spotify_long_test %>%
  mutate(has_artist = 1) %>%
  pivot_wider(
    id_cols = `Track URI`,
    names_from = `Artist List`,
    values_from = has_artist,
    values_fill = list(has_artist = 0),
    values_fn = list(has_artist = max)
  )

# Join spotify_wide_test to spotify_test_cleaned
spotify_test_cleaned = spotify_test_cleaned %>%
  mutate(`Track URI` = spotify_test$`Track URI`) %>%
  left_join(spotify_wide_test, by = "Track URI") %>%
  mutate(`Added by` = spotify_test$`Added by`)

# Ensure test data has same columns as training data
missing_cols <- setdiff(names(spotify_train_cleaned), names(spotify_test_cleaned))
if(length(missing_cols) > 0) {
  spotify_test_cleaned[missing_cols] <- 0
}

# Ensure columns are in same order
spotify_test_cleaned <- spotify_test_cleaned[, names(spotify_train_cleaned)]

# Prepare test data
X_test <- as.matrix(spotify_test_cleaned[, !names(spotify_test_cleaned) %in% c("Track URI", "Added by", "Added_by_label")])

# Ensure test data has same features as training data
missing_features <- setdiff(colnames(X_train), colnames(X_test))
if(length(missing_features) > 0) {
  zero_mat <- matrix(0, nrow = nrow(X_test), ncol = length(missing_features))
  colnames(zero_mat) <- missing_features
  X_test <- cbind(X_test, zero_mat)
}

# Reorder columns to match training data
X_test <- X_test[, colnames(X_train)]

# Get top features for test data
X_test_top <- X_test[, top_features]

# Predict on test data
pred_probs <- predict(final_model, X_test_top)
pred_matrix <- matrix(pred_probs, ncol = length(unique(y_train)), byrow = TRUE)

# Get true labels for test data
y_test_actual <- spotify_test_cleaned$`Added by`

# Calculate log loss
prob_tibble <- as_tibble(pred_matrix)
colnames(prob_tibble) <- paste0("class_", 1:ncol(prob_tibble))

loss_tbl <- prob_tibble %>%
  mutate(.truth = factor(y_test_actual), .rowid = row_number()) %>%
  relocate(.truth, .rowid)

# Calculate log loss
log_loss <- mn_log_loss(loss_tbl, truth = .truth, !!!syms(colnames(prob_tibble)))

cat("=== Tianshu's XGBoost Model Test Log Loss ===\n")
cat("Log Loss:", log_loss$.estimate, "\n")
cat("Test rows processed:", nrow(spotify_test_cleaned), "\n") 