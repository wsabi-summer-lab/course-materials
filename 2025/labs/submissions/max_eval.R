# Max's Spotify Prediction Model Evaluation
# Trains on 19_spotify-train.csv.gz and evaluates on 19_spotify-test.csv.gz

library(tidyverse)
library(glmnet)
library(tidymodels)
library(ranger)
library(future)
library(yardstick)

# Set seed
set.seed(17761)

# Load training data
data <- read.csv("../data/19_spotify-train.csv.gz")

# One-hot encode all unique genre values
data_onehot <- data %>%
  mutate(.row = row_number()) %>%
  separate_rows(Genre, sep = ";") %>%
  mutate(Genre = str_trim(Genre)) %>%
  mutate(present = 1) %>%
  pivot_wider(
    id_cols        = .row,
    names_from     = Genre,
    values_from    = present,
    values_fill    = list(present = 0)
  ) %>%
  select(-.row)

# Remove unnecessary features
sub_data <- data %>% 
  select(-c(Track.URI,track,Album.Name,Artist.Name.s.,Release.Date,Added.At,Record.Label,Genre, Key))

# Combine one hot features and other features
full_data <- cbind(sub_data,data_onehot)

row.names(full_data) <- NULL

# Set variable natures
full_data$Explicit <- factor(full_data$Explicit)
full_data$Mode <- factor(full_data$Mode)
full_data$Time.Signature <- factor(full_data$Time.Signature)
full_data$Added.by <- factor(full_data$Added.by)

# Features to scale
scaling_features <- c("Duration..ms.", "Popularity", "Danceability", "Energy", "Loudness", "Speechiness", "Acousticness", "Instrumentalness", "Liveness", "Valence", "Tempo")

# Pull scaling features
numerics <- full_data[, scaling_features]

# Scale requisite features
numerics_scaled <- as.data.frame(scale(numerics))

# Pull unscaled features
full_sub_data <- full_data %>%
  select(-scaling_features)

# Put categorical, scaled, and one-hot features
cat_data <- cbind(full_sub_data,numerics_scaled)
row.names(cat_data) <- NULL

# Set up model matrix to tune multinomial ridge model
X <- model.matrix(Added.by ~ ., data = cat_data)[,-1]
y <- cat_data$Added.by

# Set for parallel processing
doParallel::registerDoParallel(cores = 6)

# Run grid search over lambda 
fit.lasso <- glmnet::cv.glmnet(X,y,alpha=0,nfolds = 6,family="multinomial", parallel = TRUE)

# Stop parallel
doParallel::stopImplicitCluster()

# Pull conservative 1SE lambda that minimizes average multinomial deviance across the kfolds
best_s_1 <- fit.lasso$lambda.1se

# Refit on optimal lambda
preds_prob <- predict(
  fit.lasso, 
  newx = X, 
  s    = best_s_1, 
  type = "response"
)

# Below lines get probs into a df form for later use in an ensemble "stack" 
prob_mat <- preds_prob[,,1] 
prob_df_1 <- as.data.frame(preds_prob)

k <- 2                    
names(prob_df_1) <- substr(names(prob_df_1), 1, nchar(names(prob_df_1)) - k)

# Now setting up random forest hyperparameter tuning
# Re specify data to be safe
X <- model.matrix(Added.by ~ ., cat_data)[ , -1]
y <- cat_data$Added.by

# Set data in a matrix ready for training
dtrain <- data.frame(
  Matrix::as.matrix(X),
  Added.by = factor(y)
)

# Specify functional form
rf_rec <- recipe(Added.by ~ ., data = dtrain)

# Check raw n features
n_feat <- ncol(X)

# Tuning number of trees, min leaf size, and num features to consider per split
rf_spec <- rand_forest(
  trees = tune(),         
  mtry  = tune(),         
  min_n = tune()         
) %>% 
  set_mode("classification") %>% 
  set_engine(
    "ranger",
    importance      = "impurity",
    sample.fraction = tune(),           
    num.threads     = parallel::detectCores()
  )

# Specify the tidy models workflow
rf_wf <- workflow() %>% 
  add_recipe(rf_rec) %>% 
  add_model(rf_spec)

# Setting conservative ranges for hyperparameters
rf_param <- extract_parameter_set_dials(rf_wf) %>% 
  update(
    trees = trees(c(500, 2000)),
    mtry  = mtry(c(
               max(2L, round(0.05 * n_feat)),   
               max(2L, round(0.30 * n_feat))    
             )),
    min_n = min_n(c(10L, 50L)),
    `sample.fraction` = sample_prop(c(0.50, 0.80))  
  )

# Running grid search over hyperparameters value
rf_grid <- grid_latin_hypercube(rf_param, size = 60)    

# Cross validate for tuning
cv_folds <- vfold_cv(dtrain, v = 10, strata = Added.by)

plan(multisession, workers = parallel::detectCores())

# Run tune
rf_res <- tune_grid(
  rf_wf,
  resamples = cv_folds,
  grid      = rf_grid,
  metrics   = metric_set(mn_log_loss),  
  control   = control_grid(save_pred = TRUE)
)

plan(sequential)

# Pull best model based on log loss
best_rf   <- select_best(rf_res)
final_rf  <- finalize_workflow(rf_wf, best_rf) %>% fit(dtrain)

# Predict on train, then make sure df is set correctly for stacking
preds2 <- predict(final_rf, dtrain, type="prob")

k <- 6                       
colnames(preds2) <- substring(names(preds2), k + 1)

prob_df_2 <- preds2

# Specify response for stacking model
y <- cat_data$Added.by

# Ensure col ordering for both sub model dfs is correct
colnames(prob_df_2)  <- levels(y)
colnames(prob_df_1) <- levels(y)

# Combine both predicted prob results into one df
stack_df <- data.frame(
  prob_df_2,
  prob_df_1    
)

# Set names for specificity, assumes 10 names
names(stack_df) <- paste0(rep(c("rf_", "glm_"), each = 10),
                          rep(head(levels(y), 10), 2))

# Fit final stacking model trained on sub model predictions
stack_fit <- cv.glmnet(
  x            = as.matrix(stack_df),
  y            = y,
  family       = "multinomial",
  alpha        = 0,
  type.measure = "deviance",
  nfolds       = 10
)

# Pull conservative 1SE lambda for stack later on test data
best_s <- stack_fit$lambda.1se

# Load test data
test_data <- read.csv("../data/19_spotify-test.csv.gz")

# One hot encode all values found in the Genre feature
test_onehot <- test_data %>%
  mutate(.row = row_number()) %>%
  separate_rows(Genre, sep = ";") %>%
  mutate(Genre = str_trim(Genre)) %>%
  mutate(present = 1) %>%
  pivot_wider(
    id_cols        = .row,
    names_from     = Genre,
    values_from    = present,
    values_fill    = list(present = 0)
  ) %>%
  select(-.row)

# Take out unnecessary features
sub_test <- test_data %>% 
  select(-c(Track.URI,track,Album.Name,Artist.Name.s.,Release.Date,Added.At,Record.Label,Genre, Key))

# Recombine one-hot genre features and required numeric features
full_test <- cbind(sub_test,test_onehot)

row.names(full_test) <- NULL

# Set a few features as factors
full_test$Explicit <- factor(full_test$Explicit)
full_test$Mode <- factor(full_test$Mode)
full_test$Time.Signature <- factor(full_test$Time.Signature)
full_test$Added.by <- factor(full_test$Added.by)

# Manually specify the numeric features to be normalized
scaling_features_test <- c("Duration..ms.", "Popularity", "Danceability", "Energy", "Loudness", "Speechiness", "Acousticness", "Instrumentalness", "Liveness", "Valence", "Tempo")

# Pull numeric features
numerics_test <- full_test[, scaling_features_test]

# Scale numeric features
numerics_scaled_test <- as.data.frame(scale(numerics_test))

# Remove unscaled features
full_sub_test <- full_test %>%
  select(-scaling_features_test)

# Put scaled, categorical, and one-hot features together
cat_data_test <- cbind(full_sub_test,numerics_scaled_test)
row.names(cat_data_test) <- NULL

# Ensure test data has all the same columns as training data
missing_cols <- setdiff(names(cat_data), names(cat_data_test))
if(length(missing_cols) > 0) {
  cat_data_test[missing_cols] <- 0
}

# Ensure columns are in the same order as training data
cat_data_test <- cat_data_test[, names(cat_data)]

# Setting test data up for prediction
X_m1 <- model.matrix(Added.by ~ ., data = cat_data_test)[,-1]
y_test <- cat_data_test$Added.by

# Create df for RF model
dtest <- data.frame(
  Matrix::as.matrix(X_m1),
  Added.by = factor(y_test)
)

# Predict using sub_model 1 (multinomial logistic)
preds_prob_test <- predict(
  fit.lasso, 
  newx = X_m1, 
  s    = best_s_1, 
  type = "response"
)

# Pull results and set up for stacking
prob_df_1_test <- as.data.frame(preds_prob_test)

k <- 2                    
names(prob_df_1_test) <- substr(names(prob_df_1_test), 1, nchar(names(prob_df_1_test)) - k)

# Use fitted RF model to predict on test data
preds2_test <- predict(final_rf, dtest, type="prob")

# Put rf results in proper form for stacking
k <- 6                       
colnames(preds2_test) <- substring(names(preds2_test), k + 1)

prob_df_2_test <- preds2_test
prob_df_2_test <- as.data.frame(prob_df_2_test)

# Setting up stacked model data
colnames(prob_df_2_test)  <- levels(y_test)
colnames(prob_df_1_test) <- levels(y_test)

stack_df_test <- data.frame(
  prob_df_2_test,
  prob_df_1_test    
)
names(stack_df_test) <- paste0(rep(c("rf_", "glm_"), each = 10),
                          rep(head(levels(y_test), 10), 2))

# Use stack model fit on training data to predict on the test data
final_model <- predict(
  stack_fit, 
  newx = as.matrix(stack_df_test), 
  s    = best_s, 
  type = "response"
)

# The final output, a n by 10 matrix giving the prob predicted for each response level for each observation in the test data
prob_mat_final_test <- final_model[,,1]

# Setting up df for computing log loss of model on test data
loss_tbl <- as_tibble(prob_mat_final_test) %>%
  mutate(.truth = y_test, .rowid = row_number()) %>%
  relocate(.truth, .rowid)

# Calculate log loss
log_loss <- mn_log_loss(loss_tbl, truth = .truth, !!!syms(colnames(prob_mat_final_test)))

cat("=== Max's Ensemble Model Test Log Loss ===\n")
cat("Log Loss:", log_loss$.estimate, "\n")
cat("Test rows processed:", nrow(cat_data_test), "\n") 