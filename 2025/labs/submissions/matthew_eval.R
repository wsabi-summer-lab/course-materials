# Matthew's Spotify Prediction Model Evaluation
# Trains on 19_spotify-train.csv.gz and evaluates on 19_spotify-test.csv.gz

library(tidyverse)
library(mclust)
library(ranger)
library(caret)
library(ggplot2)
library(rBayesianOptimization)
library(xgboost)
library(yardstick)

# Set seed
set.seed(19846)

# Load training data
spotify_train <- read_csv("../data/19_spotify-train.csv.gz")

# Filter to valid contributors (>= 10 songs)
valid_contributors <- spotify_train %>%
  count(`Added by`) %>%
  filter(n >= 10) %>%
  pull(`Added by`)

spotify_filtered <- spotify_train %>%
  filter(`Added by` %in% valid_contributors) %>%
  mutate(`Added by` = factor(`Added by`))

# Select features
features <- c("Danceability", "Energy", "Loudness", "Speechiness", "Acousticness",
              "Instrumentalness", "Liveness", "Valence", "Tempo", "Popularity", "Duration (ms)")

spotify_model_data <- spotify_filtered %>%
  select(`Added by`, all_of(features)) %>%
  rename(Duration = `Duration (ms)`) %>%
  drop_na()

# Add previous contributor feature
add_prev_contributor <- function(df) {
  df %>%
    arrange(row_number()) %>%
    mutate(PrevContributor = lag(`Added by`)) %>%
    mutate(PrevContributor = fct_explicit_na(PrevContributor, na_level = "None"))
}

# Prepare training data
train_data <- spotify_model_data %>% add_prev_contributor()

all_contributors <- levels(spotify_model_data$`Added by`)
prev_levels <- c("None", all_contributors)
train_data$PrevContributor <- factor(train_data$PrevContributor, levels = prev_levels)

# Prepare training matrices
train_rf <- train_data
y_train <- train_rf$`Added by`
class_levels <- levels(y_train)

train_rf_mat <- model.matrix(~ . - 1, data = train_rf %>% select(-`Added by`))

# Train Random Forest model
final_rf <- ranger(
  x = train_rf_mat,
  y = y_train,
  mtry = 4,
  min.node.size = 5,
  sample.fraction = 0.8,
  num.trees = 1000,
  probability = TRUE,
  classification = TRUE,
  seed = 19846
)

# Load test data
spotify_test <- read_csv("../data/19_spotify-test.csv.gz")

# Process test data the same way
spotify_test_filtered <- spotify_test %>%
  filter(`Added by` %in% valid_contributors) %>%
  mutate(`Added by` = factor(`Added by`, levels = levels(spotify_model_data$`Added by`)))

spotify_test_model_data <- spotify_test_filtered %>%
  select(`Added by`, all_of(features)) %>%
  rename(Duration = `Duration (ms)`) %>%
  drop_na()

# Add previous contributor feature to test data
test_data <- spotify_test_model_data %>% add_prev_contributor()
test_data$PrevContributor <- factor(test_data$PrevContributor, levels = prev_levels)

# Ensure test data has same columns as training data
missing_cols <- setdiff(names(train_data), names(test_data))
if(length(missing_cols) > 0) {
  test_data[missing_cols] <- 0
}

# Ensure columns are in same order
test_data <- test_data[, names(train_data)]

# Prepare test matrices
test_rf <- test_data
test_rf_mat <- model.matrix(~ . - 1, data = test_rf %>% select(-`Added by`))

# Predict on test data
rf_probs <- predict(final_rf, data = test_rf_mat)$predictions

# Get true labels for test data
y_test_actual <- test_rf$`Added by`

# Calculate log loss
prob_tibble <- as_tibble(rf_probs)
colnames(prob_tibble) <- paste0("class_", 1:ncol(prob_tibble))

loss_tbl <- prob_tibble %>%
  mutate(.truth = factor(y_test_actual), .rowid = row_number()) %>%
  relocate(.truth, .rowid)

# Calculate log loss
log_loss <- mn_log_loss(loss_tbl, truth = .truth, !!!syms(colnames(prob_tibble)))

cat("=== Matthew's Random Forest Model Test Log Loss ===\n")
cat("Log Loss:", log_loss$.estimate, "\n")
cat("Test rows processed:", nrow(test_data), "\n") 