# ---- Load libraries ----


### hi JP! i used headers and stuff to navigate and tell u what to ignore. 
# much luv :) <3

### SETUP ###


# install.packages(c("tidyverse"))
library(tidyverse)

# set unique 5-digit seed
set.seed(70377)


### SPOTIFY DATA ###

# read in data
spotify_data = read_csv("../data/19_spotify-train.csv.gz")

# preview data
head(spotify_data)


### SETUP ###


# install.packages(c("tidyverse"))
library(tidyverse)
library(tidyr)
library(caret)
library(glmnet)
library(ranger)
library(xgboost)
library(dplyr)

# set unique 5-digit seed
set.seed(70377)


### SPOTIFY DATA ###


# read in data
spotify_data = read_csv("../data/19_spotify-train.csv.gz")

# preview data
head(spotify_data)

library(nnet)

# ---- set up data ----

df_long <- spotify_data %>%
  select(track, Genre) %>%
  separate_rows(Genre, sep = ";") %>%
  mutate(Genre = str_trim(Genre)) %>%    # trim extra spaces
  distinct(track, Genre) %>%             # remove duplicates
  mutate(value = 1)

df_wide <- df_long %>%
  pivot_wider(
    id_cols = track,
    names_from = Genre,
    values_from = value,
    values_fill = list(value = 0)
  )

# Merge genre indicators back into spotify_data by 'track'
spotify_data_merged <- spotify_data %>%
  left_join(df_wide, by = "track")

spotify_data_merged <- spotify_data_merged %>%
  select(-Genre)



# ---- more data set up ----
# Convert Added by to factor
spotify_data_merged$`Added by` <- as.factor(spotify_data_merged$`Added by`)

# Filter to users with enough samples to avoid extreme sparsity
user_counts <- table(spotify_data_merged$`Added by`)
top_users <- names(user_counts[user_counts >= 20])  # e.g. keep users with 20+ songs added

data_filtered <- spotify_data_merged 

# Create train/test split (stratified)

train_idx <- createDataPartition(data_filtered$`Added by`, p = 0.8, list = FALSE)
train_data <- data_filtered[train_idx, ]
test_data <- data_filtered[-train_idx, ]


# ---- shall we begin? ignore this according to notes ----
############## lots of training stuff and everything below is a mess. ignore until line 210
##############I marked the line below where I tune then marked where the final model is made
# Prepare matrices for glmnet and xgboost
# Remove non-predictive columns, e.g. track and outcome
predictors <- train_data %>% select(-`Added by`, -track)
outcome <- train_data$`Added by`

X_train <- model.matrix(~ . -1, data = predictors)
y_train <- outcome

X_test <- model.matrix(~ . -1, data = test_data %>% select(-`Added by`, -track))
y_test <- test_data$`Added by`


combined_data <- bind_rows(
  train_data %>% select(-`Added by`, -track),
  test_data %>% select(-`Added by`, -track)
)

combined_mm <- model.matrix(~ . -1, data = combined_data)

# Now split back
X_train <- combined_mm[1:nrow(train_data), ]
X_test <- combined_mm[(nrow(train_data) + 1):nrow(combined_mm), ]

#multinomial shiii

cv_fit <- cv.glmnet(X_train, y_train, family = "multinomial", type.measure = "class")

best_lambda <- cv_fit$lambda.min

# Predict classes on test set
pred_glmnet <- predict(cv_fit, newx = X_test, s = best_lambda, type = "class")

# Accuracy
acc_glmnet <- mean(pred_glmnet == y_test)
print(paste("Glmnet Multinomial Accuracy:", round(acc_glmnet, 4)))


####many a tree
rf_model <- ranger(
  dependent.variable.name = "Added by",
  data = train_data %>% select(-track),
  probability = FALSE,
  num.trees = 500,
  classification = TRUE
)

# Predict on test
pred_rf <- predict(rf_model, data = test_data)$predictions

# Accuracy
acc_rf <- mean(pred_rf == y_test)
print(paste("Random Forest Accuracy:", round(acc_rf, 4)))



label_map <- levels(y_train)  # get factor levels (user labels)

# Convert factor labels to numeric 0-based for XGBoost
y_train_num <- as.numeric(y_train) - 1
y_test_num  <- as.numeric(y_test) - 1

# Create model matrices for train and test predictors
X_train_matrix <- model.matrix(~ . - 1, data = as.data.frame(X_train))
X_test_matrix  <- X_test  # X_test is already a matrix

# Ensure test matrix has same column names as training matrix
colnames(X_test_matrix) <- colnames(X_train_matrix)

# Create DMatrix objects for train and test
dtrain <- xgb.DMatrix(data = X_train_matrix, label = y_train_num)
dtest  <- xgb.DMatrix(data = X_test_matrix, label = y_test_num)

# Define parameters
params <- list(
  objective = "multi:softmax",
  num_class = as.numeric(length(label_map)),
  eval_metric = "merror",
  max_depth = 6,
  eta = 0.1,
  verbosity = 0
)

# You can tune parameters later using grid or caret if you want

# Train model with early stopping on validation
watchlist <- list(train = dtrain, eval = dtest)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = watchlist,
  early_stopping_rounds = 10,
  verbose = 1
)

# Predict on test set (predictions are numeric labels 0-based)
pred_xgb <- predict(xgb_model, newdata = dtest)

# Calculate accuracy (compare to y_test_num)
acc_xgb <- mean(pred_xgb == y_test_num)
print(paste("XGBoost Accuracy:", round(acc_xgb, 4)))

# Now you can put this accuracy into your results table alongside others
results <- tibble(
  Model = c("Glmnet Multinomial", "Random Forest", "XGBoost"),
  Accuracy = c(acc_glmnet, acc_rf, acc_xgb)
)

print(results)





# ---- xGboost tuning ----
################ now tune some shiiii
y <- as.integer(as.factor(spotify_data_merged$`Added by`)) - 1  # must be 0-indexed for XGBoost

# X is all columns *except* target and possibly 'track' or 'Genre' (depending on what's there)
X <- spotify_data_merged %>% 
  select(-track, -`Added by`)  

X_matrix <- model.matrix(~ . - 1, data = X)  # no intercept



X_train <- X_matrix[train_idx, ]
y_train <- y[train_idx]

X_val <- X_matrix[-train_idx, ]
y_val <- y[-train_idx]

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dval   <- xgb.DMatrix(data = X_val, label = y_val)



grid <- expand.grid(
  max_depth = c(2, 3, 4),
  eta       = c(0.01, 0.05, 0.1),
  lambda    = c(0.5, 1, 2)
)

num_class <- length(unique(y_train))

results <- list()

for (i in 1:nrow(grid)) {
  params <- list(
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = num_class,
    max_depth = grid$max_depth[i],
    eta = grid$eta[i],
    lambda = grid$lambda[i]
  )
  
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 200,
    watchlist = list(eval = dval),
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  best_iter <- model$best_iteration
  val_preds <- predict(model, newdata = dval)
  val_pred_matrix <- matrix(val_preds, ncol = num_class, byrow = TRUE)
  
  actual <- matrix(0, nrow = length(y_val), ncol = num_class)
  actual[cbind(1:length(y_val), y_val + 1)] <- 1
  
  logloss_val <- -mean(rowSums(actual * log(val_pred_matrix + 1e-15)))
  
  results[[i]] <- tibble(
    max_depth = grid$max_depth[i],
    eta = grid$eta[i],
    lambda = grid$lambda[i],
    best_iter = best_iter,
    val_logloss = logloss_val
  )
}

results_df <- bind_rows(results) %>% arrange(val_logloss)
best_params <- results_df %>% dplyr::slice(1)


final_model <- xgb.train(
  params = list(
    objective = "multi:softprob",
    num_class = length(unique(y)),    # for multiclass classification
    eval_metric = "mlogloss",
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    lambda = best_params$lambda
  ),
  data = dtrain,
  nrounds = best_params$best_iter,
  verbose = 0
)

# Predict probabilities
pred_probs <- predict(final_model, newdata = dval)

# Convert to class predictions
pred_matrix <- matrix(pred_probs, ncol = length(unique(y)), byrow = TRUE)
pred_labels <- max.col(pred_matrix) - 1  # xgboost uses 0-based class indices

accuracy <- mean(pred_labels == y_val)
print(paste("Test Set Accuracy:", round(accuracy * 100, 2), "%"))

X_test_df <- as.data.frame(X_test)
X_test_matrix <- model.matrix(~ . - 1, data = X_test_df)

train_colnames <- colnames(dtrain)  # or the matrix used to train final_model
missing_cols <- setdiff(train_colnames, colnames(X_test_matrix))
if(length(missing_cols) > 0) {
  zero_mat <- matrix(0, nrow = nrow(X_test_matrix), ncol = length(missing_cols))
  colnames(zero_mat) <- missing_cols
  X_test_matrix <- cbind(X_test_matrix, zero_mat)
}
X_test_matrix <- X_test_matrix[, train_colnames]


# ---- final model ----
################### now I make final model using parameters defined above


label_map <- factor(y)
y_num <- as.numeric(label_map) - 1  # xgboost expects labels starting at 0

# Create DMatrix with all data
dtrain_full <- xgb.DMatrix(data = X_matrix, label = y_num)




final_final_model = xgb.train(
  params = list(
    objective = "multi:softprob",
    num_class = length(unique(y)),    # for multiclass classification
    eval_metric = "mlogloss",
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    lambda = best_params$lambda
  ),
  data = dtrain_full,
  nrounds = best_params$best_iter,
  verbose = 0
)
# ---- visualization removed to avoid Genre column issues ----

# ---- get values ----
### 
# Read in actual test data
spotify_test = read_csv("../data/19_spotify-test.csv.gz")

# Process test data the same way as training data
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
  left_join(df_wide_test, by = "track")

spotify_test_merged <- spotify_test_merged %>%
  select(-Genre)

# Ensure test data has all the same columns as training data
missing_cols <- setdiff(names(spotify_data_merged), names(spotify_test_merged))
if(length(missing_cols) > 0) {
  spotify_test_merged[missing_cols] <- 0
}

# Ensure columns are in the same order
spotify_test_merged <- spotify_test_merged[, names(spotify_data_merged)]

# Prepare test data for prediction
X_test_actual <- spotify_test_merged %>% 
  select(-track, -`Added by`)  

X_test_matrix_actual <- model.matrix(~ . - 1, data = X_test_actual)

# Ensure test matrix has same columns as training matrix
missing_cols_matrix <- setdiff(colnames(X_matrix), colnames(X_test_matrix_actual))
if(length(missing_cols_matrix) > 0) {
  zero_mat <- matrix(0, nrow = nrow(X_test_matrix_actual), ncol = length(missing_cols_matrix))
  colnames(zero_mat) <- missing_cols_matrix
  X_test_matrix_actual <- cbind(X_test_matrix_actual, zero_mat)
}
X_test_matrix_actual <- X_test_matrix_actual[, colnames(X_matrix)]

# Predict on actual test data
pred_probs <- predict(final_final_model, newdata = X_test_matrix_actual)

# Convert to probability matrix
pred_matrix <- matrix(pred_probs, ncol = length(unique(y)), byrow = TRUE)

# Get true labels for test data
y_test_actual <- spotify_test_merged$`Added by`

# Calculate log loss
library(yardstick)

# Create tibble for log loss calculation
prob_tibble <- as_tibble(pred_matrix)
# Use simple numeric column names to avoid issues
colnames(prob_tibble) <- paste0("class_", 1:ncol(prob_tibble))

loss_tbl <- prob_tibble %>%
  mutate(.truth = factor(y_test_actual), .rowid = row_number()) %>%
  relocate(.truth, .rowid)

# Calculate log loss
final_log_loss <- mn_log_loss(loss_tbl, truth = .truth, !!!syms(colnames(prob_tibble)))

cat("\n=== Adam's XGBoost Model Test Log Loss ===")
cat("\nLog Loss:", final_log_loss$.estimate)

