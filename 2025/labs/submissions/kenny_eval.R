# Kenny's Spotify Prediction Model Evaluation
# Trains on 19_spotify-train.csv.gz and evaluates on 19_spotify-test.csv.gz

library(MLmetrics)
library(tidyverse)
library(dplyr)
library(glmnet)
library(caret)
library(yardstick)

# Set seed
set.seed(80085)

# Load training data
spotify_train = read_csv("../data/19_spotify-train.csv.gz")

# Filter to users with at least 10 songs
spotify_train <- spotify_train %>%
  group_by(`Added by`) %>%
  filter(n() >= 10) %>%
  ungroup()

# Prepare training data
spotify_train$`Release Date` <- as.Date(spotify_train$`Release Date`, format = "%m/%d/%y")
spotify_train$Years_Since_Release <- as.numeric(difftime(Sys.Date(), 
                                    spotify_train$`Release Date`, 
                                    units = "days")) / 365.25

spotify_train$Explicit_binary <- ifelse(spotify_train$Explicit == "TRUE", 1, 0)

# Select numeric features
numeric_columns <- c("Duration (ms)", "Popularity", "Danceability", "Energy",
                     "Speechiness", "Acousticness", 
                     "Instrumentalness", "Valence", "Tempo",
                     "Explicit_binary", "Years_Since_Release")

X_train <- spotify_train[, numeric_columns]

# Handle missing values
X_train <- as.data.frame(lapply(X_train, as.numeric))
X_train_mat <- as.matrix(do.call(cbind, lapply(X_train, function(col) {
  col[is.na(col)] <- mean(col, na.rm = TRUE)
  col
})))

y_train <- as.factor(spotify_train$`Added by`)

# Train Lasso model
final_model <- cv.glmnet(
  X_train_mat, y_train, 
  alpha = 1,
  family = "multinomial"
)

# Load test data
spotify_test = read_csv("../data/19_spotify-test.csv.gz")

# Process test data the same way
spotify_test$`Release Date` <- as.Date(spotify_test$`Release Date`, format = "%m/%d/%y")
spotify_test$Years_Since_Release <- as.numeric(difftime(Sys.Date(), 
                                    spotify_test$`Release Date`, 
                                    units = "days")) / 365.25

spotify_test$Explicit_binary <- ifelse(spotify_test$Explicit == "TRUE", 1, 0)

# Select same numeric features for test data
X_test <- spotify_test[, numeric_columns]

# Handle missing values in test data
X_test <- as.data.frame(lapply(X_test, as.numeric))
X_test_mat <- as.matrix(do.call(cbind, lapply(X_test, function(col) {
  col[is.na(col)] <- mean(col, na.rm = TRUE)
  col
})))

# Ensure test matrix has same columns as training matrix
if(ncol(X_test_mat) != ncol(X_train_mat)) {
  missing_cols <- setdiff(colnames(X_train_mat), colnames(X_test_mat))
  if(length(missing_cols) > 0) {
    zero_mat <- matrix(0, nrow = nrow(X_test_mat), ncol = length(missing_cols))
    colnames(zero_mat) <- missing_cols
    X_test_mat <- cbind(X_test_mat, zero_mat)
  }
  X_test_mat <- X_test_mat[, colnames(X_train_mat)]
}

# Predict on test data
pred_probs <- predict(final_model, newx = X_test_mat, s = "lambda.min", type = "response")

# Convert to probability matrix
prob_matrix <- as.matrix(pred_probs[, , 1])
colnames(prob_matrix) <- levels(y_train)

# Get true labels for test data
y_test_actual <- spotify_test$`Added by`

# Calculate log loss
prob_tibble <- as_tibble(prob_matrix)
colnames(prob_tibble) <- paste0("class_", 1:ncol(prob_tibble))

loss_tbl <- prob_tibble %>%
  mutate(.truth = factor(y_test_actual), .rowid = row_number()) %>%
  relocate(.truth, .rowid)

# Calculate log loss
log_loss <- mn_log_loss(loss_tbl, truth = .truth, !!!syms(colnames(prob_tibble)))

cat("=== Kenny's Lasso Model Test Log Loss ===\n")
cat("Log Loss:", log_loss$.estimate, "\n")
cat("Test rows processed:", nrow(spotify_test), "\n") 