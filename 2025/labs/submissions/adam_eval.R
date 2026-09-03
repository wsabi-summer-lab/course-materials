# Adam's Spotify Prediction Model Evaluation
# Trains on 19_spotify-train.csv.gz and evaluates on 19_spotify-test.csv.gz

library(tidyverse)
library(caret)
library(glmnet)
library(ranger)
library(xgboost)
library(yardstick)

# Set seed
set.seed(70377)

# Load training data
spotify_train = read_csv("../data/19_spotify-train.csv.gz")

# Process training data - genre one-hot encoding
df_long <- spotify_train %>%
  select(track, Genre) %>%
  separate_rows(Genre, sep = ";") %>%
  mutate(Genre = str_trim(Genre)) %>%
  distinct(track, Genre) %>%
  mutate(value = 1)

df_wide <- df_long %>%
  pivot_wider(
    id_cols = track,
    names_from = Genre,
    values_from = value,
    values_fill = list(value = 0)
  )

# Merge genre indicators back into training data
spotify_train_merged <- spotify_train %>%
  left_join(df_wide, by = "track") %>%
  select(-Genre)

# Convert Added by to factor
spotify_train_merged$`Added by` <- as.factor(spotify_train_merged$`Added by`)

# Prepare training data
X_train <- spotify_train_merged %>% select(-track, -`Added by`)
y_train <- spotify_train_merged$`Added by`

# Create model matrix
X_train_matrix <- model.matrix(~ . - 1, data = X_train)

# Convert labels to numeric for XGBoost
y_train_num <- as.numeric(y_train) - 1

# Create DMatrix
dtrain <- xgb.DMatrix(data = X_train_matrix, label = y_train_num)

# Train XGBoost model
xgb_model <- xgb.train(
  params = list(
    objective = "multi:softprob",
    num_class = length(unique(y_train)),
    eval_metric = "mlogloss",
    max_depth = 4,
    eta = 0.1,
    lambda = 1
  ),
  data = dtrain,
  nrounds = 100,
  verbose = 0
)

# Load test data
spotify_test = read_csv("../data/19_spotify-test.csv.gz")

# Process test data the same way
df_long_test <- spotify_test %>%
  select(track, Genre) %>%
  separate_rows(Genre, sep = ";") %>%
  mutate(Genre = str_trim(Genre)) %>%
  distinct(track, Genre) %>%
  mutate(value = 1)

df_wide_test <- df_long_test %>%
  pivot_wider(
    id_cols = track,
    names_from = Genre,
    values_from = value,
    values_fill = list(value = 0)
  )

# Merge genre indicators back into test data
spotify_test_merged <- spotify_test %>%
  left_join(df_wide_test, by = "track") %>%
  select(-Genre)

# Ensure test data has all the same columns as training data
missing_cols <- setdiff(names(spotify_train_merged), names(spotify_test_merged))
if(length(missing_cols) > 0) {
  spotify_test_merged[missing_cols] <- 0
}

# Ensure columns are in the same order
spotify_test_merged <- spotify_test_merged[, names(spotify_train_merged)]

# Prepare test data for prediction
X_test <- spotify_test_merged %>% select(-track, -`Added by`)
X_test_matrix <- model.matrix(~ . - 1, data = X_test)

# Ensure test matrix has same columns as training matrix
missing_cols_matrix <- setdiff(colnames(X_train_matrix), colnames(X_test_matrix))
if(length(missing_cols_matrix) > 0) {
  zero_mat <- matrix(0, nrow = nrow(X_test_matrix), ncol = length(missing_cols_matrix))
  colnames(zero_mat) <- missing_cols_matrix
  X_test_matrix <- cbind(X_test_matrix, zero_mat)
}
X_test_matrix <- X_test_matrix[, colnames(X_train_matrix)]

# Predict on test data
pred_probs <- predict(xgb_model, newdata = X_test_matrix)

# Convert to probability matrix
pred_matrix <- matrix(pred_probs, ncol = length(unique(y_train)), byrow = TRUE)

# Get true labels for test data
y_test_actual <- spotify_test_merged$`Added by`

# Calculate log loss
prob_tibble <- as_tibble(pred_matrix)
colnames(prob_tibble) <- paste0("class_", 1:ncol(prob_tibble))

loss_tbl <- prob_tibble %>%
  mutate(.truth = factor(y_test_actual), .rowid = row_number()) %>%
  relocate(.truth, .rowid)

# Calculate log loss
log_loss <- mn_log_loss(loss_tbl, truth = .truth, !!!syms(colnames(prob_tibble)))

cat("=== Adam's XGBoost Model Test Log Loss ===\n")
cat("Log Loss:", log_loss$.estimate, "\n")
cat("Test rows processed:", nrow(spotify_test_merged), "\n") 