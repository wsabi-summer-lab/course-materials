#############
### SETUP ###
#############

# install.packages(c("cluster", "factoextra", "flexclust", "ggdendro", "ggplot2", "mclust", "tidyverse"))
library(cluster)      # for hierarchical clustering and silhouette analysis
library(ggdendro)     # for dendrogram visualization
library(ggplot2)      # for plotting
library(tidyverse)    # for data manipulation and visualization
library(stringr)
library(xgboost)      # for XGBoost)
library(caret)
library(readr)
library(purrr)

# set seed
set.seed(19)

####################
### SPOTIFY DATA ###
####################

#––– 1. READ & PREPARE ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
spotify_data <- read_csv("data/19_spotify-train.csv.gz") %>%
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

#––– 2. CLUSTER GENRES INTO BUCKETS –––––––––––––––––––––––––––––––––––––––––––––––
genre_profiles <- spotify_data %>%
  group_by(Genre) %>%
  summarise(across(c(Danceability, Energy, Loudness, Speechiness,
                     Acousticness, Instrumentalness, Liveness,
                     Valence, Tempo), mean, na.rm = TRUE)) %>%
  ungroup()

feat_mat <- scale(select(genre_profiles, -Genre))
hc       <- hclust(dist(feat_mat), method = "ward.D2")
plot(hc, main = "Genre dendrogram", cex = 0.6)

k <- 7
genre_profiles <- genre_profiles %>% mutate(bucket = cutree(hc, k = k))

spotify_data <- spotify_data %>%
  left_join(genre_profiles %>% select(Genre, Gbucket = bucket), by = "Genre") %>%
  mutate(Gbucket = factor(Gbucket))

#––– 3. BUILD XGBOOST DATA –––––––––––––––––––––––––––––––––––––––––––––––––––––––
df <- spotify_data %>%
  select(added_by, Gbucket, release_year,
         Danceability, Energy, Loudness, Speechiness,
         Instrumentalness, Liveness, Acousticness,
         Valence, Tempo, Popularity, Explicit) %>%
  mutate(Explicit = as.integer(Explicit))

#––– 4. TRAIN/TEST SPLIT –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
set.seed(42)
train_idx <- createDataPartition(df$added_by, p = 0.8, list = FALSE)
train     <- df[train_idx, ] %>% mutate(across(where(is.factor), droplevels))
test      <- df[-train_idx, ] %>% mutate(across(where(is.factor), droplevels))

nzv_info  <- nearZeroVar(train, saveMetrics = TRUE)
keep_cols <- rownames(nzv_info)[!nzv_info$zeroVar]
train     <- train[, keep_cols]
test      <- test[, keep_cols]

train_mat <- model.matrix(~ . -1 - added_by, data = train)
test_mat  <- model.matrix(~ . -1 - added_by, data = test)

users     <- levels(factor(train$added_by))
train_lab <- as.integer(factor(train$added_by, levels = users)) - 1
test_lab  <- as.integer(factor(test$added_by,  levels = users)) - 1

dtrain <- xgb.DMatrix(data = train_mat, label = train_lab)
dtest  <- xgb.DMatrix(data = test_mat,  label = test_lab)

#––– 5. XGBOOST PARAMS & FINAL TRAIN –––––––––––––––––––––––––––––––––––––––––––––
watchlist <- list(train = dtrain, valid = dtest)

params <- list(
  objective        = "multi:softprob",
  num_class        = length(users),
  eta              = 0.1,
  max_depth        = 6,
  subsample        = 0.8,
  colsample_bytree = 0.8
)

# save the trained booster into `final_model`
final_model <- xgb.train(
  params                = params,
  data                  = dtrain,
  nrounds               = 200,
  watchlist             = watchlist,
  eval_metric           = "mlogloss",
  early_stopping_rounds = 10,
  verbose               = 1
)

#––– 6. EXTRACT & PRINT VALIDATION LOG LOSS –––––––––––––––––––––––––––––––––––––––
eval_log         <- final_model$evaluation_log
best_iter        <- final_model$best_iteration
best_val_logloss <- eval_log$valid_mlogloss[best_iter]

print(eval_log)
cat("Best validation mlogloss (iter", best_iter, "):", best_val_logloss, "\n\n")

#––– 7. FEATURE IMPORTANCE –––––––––––––––––––––––––––––––––––––––––––––––––––––––––
imp <- xgb.importance(model = final_model)
xgb.plot.importance(imp)

#––– 8. PREDICT & CONFUSION –––––––––––––––––––––––––––––––––––––––––––––––––––––––
pred_prob   <- predict(final_model, dtest)
pred_matrix <- matrix(pred_prob, ncol = length(users), byrow = TRUE)
pred_labels <- users[max.col(pred_matrix)]

accuracy <- mean(pred_labels == test$added_by)
cat("Test accuracy:", round(accuracy*100, 2), "%\n\n")

cm <- confusionMatrix(
  factor(pred_labels, levels = users),
  factor(test$added_by,    levels = users)
)
print(cm)

