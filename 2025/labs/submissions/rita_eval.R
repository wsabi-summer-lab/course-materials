# Rita's Spotify Prediction Model Evaluation
# Trains on 19_spotify-train.csv.gz and evaluates on 19_spotify-test.csv.gz

library(cluster)
library(ggplot2)
library(tidyverse)
library(stringr)
library(xgboost)
library(caret)
library(readr)
library(purrr)
library(yardstick)

# Set seed
set.seed(19)

# Load training data
spotify_train <- read_csv("../data/19_spotify-train.csv.gz") %>%
  mutate(
    release_year = `Release Date` %>% str_extract("\\d{2}$") %>% as.integer()
  ) %>%
  { 
    max_splits <- max(str_count(.$Genre, fixed(";")), na.rm = TRUE) + 1
    separate(., col = Genre,
             into  = paste0("genre_", seq_len(max_splits)),
             sep   = ";", fill = "right", extra = "drop", remove = FALSE)
  } %>%
  mutate(
    added_by = case_when(
      if_any(starts_with("genre_"), ~ .x == "EDM")             ~ "Aiwen",
      if_any(starts_with("genre_"), ~ .x == "afrobeats")       ~ "Drewskii",
      if_any(starts_with("genre_"), ~ .x == "soft rock")       ~ "Matt",
      if_any(starts_with("genre_"), ~ .x == "classic country") ~ "Matt",
      if_any(starts_with("genre_"), ~ .x == "french pop")      ~ "hisayswhy",
      if_any(starts_with("genre_"), ~ .x == "trap")            ~ "Drewskii",
      if_any(starts_with("genre_"), ~ .x == "k-pop")           ~ "victoria",
      if_any(starts_with("genre_"), ~ .x == "mandopop")        ~ "Audrey Kuan",
      if_any(starts_with("genre_"), ~ .x == "melodic rap")     ~ "Drewskii",
      if_any(starts_with("genre_"), ~ .x == "rally house")     ~ "landowatts",
      if_any(starts_with("genre_"), ~ .x == "reggaeton")       ~ "Adam Kuechler",
      if_any(starts_with("genre_"), ~ .x == "yacht rock")      ~ "Matt",
      TRUE                                                   ~ `Added by`
    )
  )

# Cluster genres into buckets
genre_profiles <- spotify_train %>%
  group_by(Genre) %>%
  summarise(across(c(Danceability, Energy, Loudness, Speechiness,
                     Acousticness, Instrumentalness, Liveness,
                     Valence, Tempo), mean, na.rm = TRUE)) %>%
  ungroup()

feat_mat <- scale(select(genre_profiles, -Genre))
hc       <- hclust(dist(feat_mat), method = "ward.D2")

k <- 7
genre_profiles <- genre_profiles %>% mutate(bucket = cutree(hc, k = k))

spotify_train <- spotify_train %>%
  left_join(genre_profiles %>% select(Genre, Gbucket = bucket), by = "Genre") %>%
  mutate(Gbucket = factor(Gbucket))

# Build XGBoost data
df_train <- spotify_train %>%
  select(added_by, Gbucket, release_year,
         Danceability, Energy, Loudness, Speechiness,
         Instrumentalness, Liveness, Acousticness,
         Valence, Tempo, Popularity, Explicit) %>%
  mutate(Explicit = as.integer(Explicit))

# Remove near zero variance columns
nzv_info  <- nearZeroVar(df_train, saveMetrics = TRUE)
keep_cols <- rownames(nzv_info)[!nzv_info$zeroVar]
df_train   <- df_train[, keep_cols]

# Create model matrix for training
train_mat <- model.matrix(~ . -1 - added_by, data = df_train)
users     <- levels(factor(df_train$added_by))
train_lab <- as.integer(factor(df_train$added_by, levels = users)) - 1

# Create DMatrix for training
dtrain <- xgb.DMatrix(data = train_mat, label = train_lab)

# Train XGBoost model
params <- list(
  objective        = "multi:softprob",
  num_class        = length(users),
  eta              = 0.1,
  max_depth        = 6,
  subsample        = 0.8,
  colsample_bytree = 0.8
)

final_model <- xgb.train(
  params                = params,
  data                  = dtrain,
  nrounds               = 200,
  eval_metric           = "mlogloss",
  verbose               = 0
)

# Load test data
spotify_test <- read_csv("../data/19_spotify-test.csv.gz") %>%
  mutate(
    release_year = `Release Date` %>% str_extract("\\d{2}$") %>% as.integer()
  ) %>%
  { 
    max_splits <- max(str_count(.$Genre, fixed(";")), na.rm = TRUE) + 1
    separate(., col = Genre,
             into  = paste0("genre_", seq_len(max_splits)),
             sep   = ";", fill = "right", extra = "drop", remove = FALSE)
  } %>%
  mutate(
    added_by = case_when(
      if_any(starts_with("genre_"), ~ .x == "EDM")             ~ "Aiwen",
      if_any(starts_with("genre_"), ~ .x == "afrobeats")       ~ "Drewskii",
      if_any(starts_with("genre_"), ~ .x == "soft rock")       ~ "Matt",
      if_any(starts_with("genre_"), ~ .x == "classic country") ~ "Matt",
      if_any(starts_with("genre_"), ~ .x == "french pop")      ~ "hisayswhy",
      if_any(starts_with("genre_"), ~ .x == "trap")            ~ "Drewskii",
      if_any(starts_with("genre_"), ~ .x == "k-pop")           ~ "victoria",
      if_any(starts_with("genre_"), ~ .x == "mandopop")        ~ "Audrey Kuan",
      if_any(starts_with("genre_"), ~ .x == "melodic rap")     ~ "Drewskii",
      if_any(starts_with("genre_"), ~ .x == "rally house")     ~ "landowatts",
      if_any(starts_with("genre_"), ~ .x == "reggaeton")       ~ "Adam Kuechler",
      if_any(starts_with("genre_"), ~ .x == "yacht rock")      ~ "Matt",
      TRUE                                                   ~ `Added by`
    )
  )

# Apply same genre clustering to test data
spotify_test <- spotify_test %>%
  left_join(genre_profiles %>% select(Genre, Gbucket = bucket), by = "Genre") %>%
  mutate(Gbucket = factor(Gbucket))

# Handle NA values in Gbucket (replace with most common bucket from training)
most_common_bucket <- names(sort(table(spotify_train$Gbucket), decreasing = TRUE))[1]
spotify_test$Gbucket[is.na(spotify_test$Gbucket)] <- most_common_bucket

# Build test data
df_test <- spotify_test %>%
  select(added_by, Gbucket, release_year,
         Danceability, Energy, Loudness, Speechiness,
         Instrumentalness, Liveness, Acousticness,
         Valence, Tempo, Popularity, Explicit) %>%
  mutate(Explicit = as.integer(Explicit))

# Ensure test data has same columns as training data
missing_cols <- setdiff(names(df_train), names(df_test))
if(length(missing_cols) > 0) {
  df_test[missing_cols] <- 0
}
df_test <- df_test[, names(df_train)]

# Handle NA values in added_by (replace with "Unknown")
df_test$added_by[is.na(df_test$added_by)] <- "Unknown"

# Ensure all factor levels are aligned
df_test$added_by <- factor(df_test$added_by, levels = levels(df_train$added_by))
df_test$Gbucket <- factor(df_test$Gbucket, levels = levels(df_train$Gbucket))

# Replace any remaining NA values with 0
df_test[is.na(df_test)] <- 0

# Create model matrix for test
test_mat <- model.matrix(~ . -1 - added_by, data = df_test)

# Ensure test matrix has same column names as training matrix
colnames(test_mat) <- colnames(train_mat)

# Predict on test data
pred_prob <- predict(final_model, test_mat)
pred_matrix <- matrix(pred_prob, ncol = length(users), byrow = TRUE)

# Get true labels for test data
y_test_actual <- df_test$added_by

# Calculate log loss
prob_tibble <- as_tibble(pred_matrix)
colnames(prob_tibble) <- paste0("class_", 1:ncol(prob_tibble))

loss_tbl <- prob_tibble %>%
  mutate(.truth = factor(y_test_actual), .rowid = row_number()) %>%
  relocate(.truth, .rowid)

# Calculate log loss
log_loss <- mn_log_loss(loss_tbl, truth = .truth, !!!syms(colnames(prob_tibble)))

cat("=== Rita's XGBoost Model Test Log Loss ===\n")
cat("Log Loss:", log_loss$.estimate, "\n")
cat("Test rows processed:", nrow(df_test), "\n") 