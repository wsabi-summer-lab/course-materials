###################
### SETUP #########
###################
library(tidyverse)
library(mclust)
library(ranger)
library(caret)
library(ggplot2)
library(rBayesianOptimization)
library(xgboost)
set.seed(19846)

##########################
### DATA PREPROCESSING ###
##########################

spotify_data <- read_csv("../data/19_spotify-train.csv.gz")

valid_contributors <- spotify_data %>%
  count(`Added by`) %>%
  filter(n >= 10) %>%
  pull(`Added by`)

spotify_filtered <- spotify_data %>%
  filter(`Added by` %in% valid_contributors) %>%
  mutate(`Added by` = factor(`Added by`))

features <- c("Danceability", "Energy", "Loudness", "Speechiness", "Acousticness",
              "Instrumentalness", "Liveness", "Valence", "Tempo", "Popularity", "Duration (ms)")

spotify_model_data <- spotify_filtered %>%
  select(`Added by`, all_of(features)) %>%
  rename(Duration = `Duration (ms)`) %>%
  drop_na()

add_prev_contributor <- function(df) {
  df %>%
    arrange(row_number()) %>%
    mutate(PrevContributor = lag(`Added by`)) %>%
    mutate(PrevContributor = fct_explicit_na(PrevContributor, na_level = "None"))
}

train_index <- createDataPartition(spotify_model_data$`Added by`, p = 0.8, list = FALSE)
train_data <- spotify_model_data[train_index, ] %>% add_prev_contributor()
test_data <- spotify_model_data[-train_index, ] %>% add_prev_contributor()

all_contributors <- levels(spotify_model_data$`Added by`)
prev_levels <- c("None", all_contributors)
train_data$PrevContributor <- factor(train_data$PrevContributor, levels = prev_levels)
test_data$PrevContributor <- factor(test_data$PrevContributor, levels = prev_levels)

train_rf <- train_data
test_rf <- test_data

y_train <- train_rf$`Added by`
class_levels <- levels(y_train)

train_rf_mat <- model.matrix(~ . - 1, data = train_rf %>% select(-`Added by`))
test_rf_mat <- model.matrix(~ . - 1, data = test_rf %>% select(-`Added by`))

test_label_num <- as.numeric(test_rf$`Added by`) - 1
train_label_num <- as.numeric(y_train) - 1

compute_log_loss <- function(probs, truth, class_levels) {
  eps <- 1e-15
  probs <- as.matrix(probs)
  probs <- pmin(pmax(probs, eps), 1 - eps)
  truth <- factor(truth, levels = class_levels)
  y_mat <- model.matrix(~ truth - 1)
  colnames(y_mat) <- sub("^truth", "", colnames(y_mat))
  missing_classes <- setdiff(class_levels, colnames(y_mat))
  for (cls in missing_classes) {
    y_mat <- cbind(y_mat, rep(0, nrow(y_mat)))
    colnames(y_mat)[ncol(y_mat)] <- cls
  }
  y_mat <- y_mat[, class_levels, drop = FALSE]
  -mean(rowSums(y_mat * log(probs)))
}

folds <- createFolds(y_train, k = 5, list = TRUE, returnTrain = TRUE)

##########################
### RF HYPERPARAM TUNING ###
##########################

rf_cv_bayes <- function(mtry, min_node_size, sample_fraction) {
  mtry <- floor(mtry)
  min_node_size <- floor(min_node_size)
  sample_fraction <- min(max(sample_fraction, 0.2), 1.0)
  
  losses <- sapply(folds, function(train_idx) {
    val_idx <- setdiff(seq_len(nrow(train_rf_mat)), train_idx)
    rf_fit <- ranger(
      x = train_rf_mat[train_idx, ],
      y = y_train[train_idx],
      mtry = mtry,
      min.node.size = min_node_size,
      sample.fraction = sample_fraction,
      num.trees = 500,
      probability = TRUE,
      classification = TRUE,
      seed = 19846
    )
    preds <- predict(rf_fit, data = train_rf_mat[val_idx, ])$predictions
    compute_log_loss(preds, y_train[val_idx], class_levels)
  })
  list(Score = -mean(losses), Pred = 0)
}

opt_rf <- BayesianOptimization(
  rf_cv_bayes,
  bounds = list(
    mtry = c(2L, ncol(train_rf_mat)),
    min_node_size = c(1L, 10L),
    sample_fraction = c(0.2, 1.0)
  ),
  init_points = 5, n_iter = 10, acq = "ucb", kappa = 2.576, verbose = TRUE
)

best_rf <- opt_rf$'Best_Par'

############################
### XGBOOST TUNING ########
############################

train_xgb <- xgb.DMatrix(data = train_rf_mat, label = train_label_num)
test_xgb <- xgb.DMatrix(data = test_rf_mat, label = test_label_num)

xgb_cv_bayes <- function(max_depth, eta, min_child_weight, subsample, colsample_bytree) {
  max_depth <- floor(max_depth)
  
  params <- list(
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = length(class_levels),
    max_depth = max_depth,
    eta = eta,
    min_child_weight = min_child_weight,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    seed = 19846
  )
  
  cv <- xgb.cv(
    params = params,
    data = train_xgb,
    nrounds = 100,
    nfold = 5,
    early_stopping_rounds = 20,
    verbose = FALSE
  )
  
  list(Score = -min(cv$evaluation_log$test_mlogloss_mean), Pred = 0)
}

opt_xgb <- BayesianOptimization(
  xgb_cv_bayes,
  bounds = list(
    max_depth = c(3L, 10L),
    eta = c(0.01, 0.3),
    min_child_weight = c(1, 10),
    subsample = c(0.5, 1),
    colsample_bytree = c(0.5, 1)
  ),
  init_points = 5, n_iter = 10, acq = "ucb", kappa = 2.576, verbose = TRUE
)

best_xgb <- opt_xgb$Best_Par

##########################
### FINAL MODELS ########
##########################

# 1. Define apply_boost function (same as before)
apply_boost <- function(probs, prev_c, boost_params, class_levels) {
  probs <- as.numeric(probs)
  names(probs) <- class_levels
  if (prev_c %in% names(boost_params)) {
    probs[prev_c] <- probs[prev_c] * (1 + boost_params[prev_c])
  }
  probs / sum(probs)
}

# 2. Precompute RF predictions on each fold's validation set
rf_preds_folds <- vector("list", length(folds))
prev_contrib_folds <- vector("list", length(folds))
truth_folds <- vector("list", length(folds))

for (i in seq_along(folds)) {
  train_idx <- folds[[i]]
  val_idx <- setdiff(seq_len(nrow(train_rf_mat)), train_idx)
  
  rf_fit <- ranger(
    x = train_rf_mat[train_idx, ],
    y = y_train[train_idx],
    mtry = floor(best_rf[["mtry"]]),
    min.node.size = floor(best_rf[["min_node_size"]]),
    sample.fraction = best_rf[["sample_fraction"]],
    num.trees = 500,
    probability = TRUE,
    classification = TRUE,
    seed = 19846
  )
  
  rf_preds_folds[[i]] <- predict(rf_fit, data = train_rf_mat[val_idx, ])$predictions
  prev_contrib_folds[[i]] <- as.character(train_rf$PrevContributor[val_idx])
  truth_folds[[i]] <- y_train[val_idx]
}

# 3. Define evaluate_boost using the precomputed preds
evaluate_boost <- function(...) {
  boosts <- list(...)
  boosts_vec <- unlist(boosts)
  names(boosts_vec) <- class_levels
  
  fold_losses <- numeric(length(folds))
  
  for (i in seq_along(folds)) {
    preds <- rf_preds_folds[[i]]
    prev_contrib_fold <- prev_contrib_folds[[i]]
    truth_val <- truth_folds[[i]]
    
    boosted_preds <- t(mapply(
      apply_boost,
      split(preds, row(preds)),
      prev_contrib_fold,
      MoreArgs = list(boost_params = boosts_vec, class_levels = class_levels)
    ))
    
    fold_losses[i] <- compute_log_loss(boosted_preds, truth_val, class_levels)
  }
  
  list(Score = -mean(fold_losses), Pred = 0)
}

# 4. Run Bayesian optimization with fewer iterations for speed
bounds_list <- setNames(rep(list(c(0, 2)), length(class_levels)), class_levels)

cat("Tuning boosting parameters per contributor...\n")
opt_boost <- BayesianOptimization(
  FUN = evaluate_boost,
  bounds = bounds_list,
  init_points = 3,
  n_iter = 5,
  acq = "ucb",
  kappa = 2.576,
  verbose = TRUE
)

# 5. Extract best boost params found
boost_params <- unlist(opt_boost$Best_Par)
names(boost_params) <- class_levels
cat("Learned boost parameters:\n")
print(boost_params)

# 6. Train final RF on full training data
final_rf <- ranger(
  x = train_rf_mat,
  y = y_train,
  mtry = floor(best_rf[["mtry"]]),
  min.node.size = floor(best_rf[["min_node_size"]]),
  sample.fraction = best_rf[["sample_fraction"]],
  num.trees = 1000,
  probability = TRUE,
  classification = TRUE,
  seed = 19846
)

rf_probs <- predict(final_rf, data = test_rf_mat)$predictions

# 7. Train final XGBoost model with best params (as before)
params_xgb <- list(
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = length(class_levels),
  max_depth = floor(best_xgb[["max_depth"]]),
  eta = best_xgb[["eta"]],
  min_child_weight = best_xgb[["min_child_weight"]],
  subsample = best_xgb[["subsample"]],
  colsample_bytree = best_xgb[["colsample_bytree"]],
  seed = 19846
)

final_xgb <- xgb.train(
  params = params_xgb,
  data = train_xgb,
  nrounds = 100,
  verbose = 0
)

xgb_probs <- matrix(
  predict(final_xgb, newdata = test_xgb),
  ncol = length(class_levels),
  byrow = TRUE
)

prev_contrib_test <- as.character(test_rf$PrevContributor)

# 8. Apply learned boosting params to test predictions
rf_probs_boosted <- t(mapply(
  apply_boost, 
  split(rf_probs, row(rf_probs)), 
  prev_contrib_test, 
  MoreArgs = list(boost_params = boost_params, class_levels = class_levels)
))

xgb_probs_boosted <- t(mapply(
  apply_boost, 
  split(xgb_probs, row(xgb_probs)), 
  prev_contrib_test, 
  MoreArgs = list(boost_params = boost_params, class_levels = class_levels)
))

# 9. Compute and print log loss for all four models
log_rf <- compute_log_loss(as.data.frame(rf_probs), test_rf$`Added by`, class_levels)
log_rf_boost <- compute_log_loss(as.data.frame(rf_probs_boosted), test_rf$`Added by`, class_levels)
log_xgb <- compute_log_loss(as.data.frame(xgb_probs), test_rf$`Added by`, class_levels)
log_xgb_boost <- compute_log_loss(as.data.frame(xgb_probs_boosted), test_rf$`Added by`, class_levels)

cat("\nLog Loss (Random Forest):", log_rf)
cat("\nLog Loss (Random Forest + Boost):", log_rf_boost)
cat("\nLog Loss (XGBoost):", log_xgb) 
cat("\nLog Loss (XGBoost + Boost):", log_xgb_boost)

library(tidyverse)

# Combine train + test features and labels
all_rf_mat <- rbind(train_rf_mat, test_rf_mat)
y_all <- factor(c(as.character(y_train), as.character(test_rf$`Added by`)), levels = class_levels)

# Predict on combined data
rf_probs_all <- predict(final_rf, data = all_rf_mat)$predictions

prev_contrib_all <- factor(c(as.character(train_rf$PrevContributor), as.character(test_rf$PrevContributor)), levels = levels(test_rf$PrevContributor))

# Apply learned boost params to combined RF predictions
rf_probs_boosted_all <- t(mapply(
  apply_boost, 
  split(rf_probs_all, row(rf_probs_all)), 
  as.character(prev_contrib_all), 
  MoreArgs = list(boost_params = boost_params, class_levels = class_levels)
))

# Calibration helper functions
get_pred_max_prob <- function(probs_df) {
  probs_df <- as.data.frame(probs_df)
  max_prob <- apply(probs_df, 1, max)
  pred_class <- colnames(probs_df)[apply(probs_df, 1, which.max)]
  data.frame(PredClass = pred_class, MaxProb = max_prob)
}

prepare_calibration <- function(probs_df, true_labels, n_bins = 10) {
  preds <- get_pred_max_prob(probs_df)
  preds$TrueClass <- as.character(true_labels)
  preds$Correct <- as.numeric(preds$PredClass == preds$TrueClass)
  
  preds %>%
    mutate(ProbBin = cut(MaxProb, breaks = seq(0,1,length.out = n_bins + 1), include.lowest = TRUE)) %>%
    group_by(ProbBin) %>%
    summarize(
      MeanPredProb = mean(MaxProb),
      ObservedAcc = mean(Correct),
      Count = n(),
      .groups = 'drop'
    )
}

# Prepare calibration data
calib_rf <- prepare_calibration(rf_probs_all, y_all) %>% mutate(Model = "Random Forest")
calib_rf_boost <- prepare_calibration(rf_probs_boosted_all, y_all) %>% mutate(Model = "RF + Boost")

calib_all <- bind_rows(calib_rf, calib_rf_boost)

# Plot calibration curves
ggplot(calib_all, aes(x = MeanPredProb, y = ObservedAcc, color = Model)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Calibration Curves for Random Forest Models",
    x = "Mean Predicted Probability",
    y = "Observed Accuracy"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# -- Helper functions --

# Apply boost to predicted probabilities
apply_boost <- function(probs, prev_c, boost_params, class_levels) {
  probs <- as.numeric(probs)
  names(probs) <- class_levels
  if (prev_c %in% names(boost_params)) {
    probs[prev_c] <- probs[prev_c] * (1 + boost_params[prev_c])
  }
  probs / sum(probs)
}

# Compute log loss function (assuming you already have it)
# compute_log_loss(probs_df, true_labels, class_levels)

# Prepare calibration data: get predicted class and max prob
get_pred_max_prob <- function(probs_df) {
  probs_df <- as.data.frame(probs_df)
  max_prob <- apply(probs_df, 1, max)
  pred_class <- colnames(probs_df)[apply(probs_df, 1, which.max)]
  data.frame(PredClass = pred_class, MaxProb = max_prob)
}

# Prepare calibration summary for plotting
prepare_calibration <- function(probs_df, true_labels, n_bins = 10) {
  preds <- get_pred_max_prob(probs_df)
  preds$TrueClass <- as.character(true_labels)
  preds$Correct <- as.numeric(preds$PredClass == preds$TrueClass)
  
  preds %>%
    mutate(ProbBin = cut(MaxProb, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)) %>%
    group_by(ProbBin) %>%
    summarize(
      MeanPredProb = mean(MaxProb),
      ObservedAcc = mean(Correct),
      Count = n(),
      .groups = 'drop'
    )
}

# Prepare data for Platt scaling (one-vs-rest)
prepare_calib_data <- function(class_name, probs_df, true_labels) {
  data.frame(
    PredProb = probs_df[[class_name]],
    Label = as.integer(true_labels == class_name)
  )
}

# Fit Platt scaling models per class (logistic regression on train data)
fit_platt_models <- function(probs_df, true_labels, class_levels) {
  eps <- 1e-15
  platt_models <- list()
  for (cls in class_levels) {
    p_clipped <- pmax(pmin(probs_df[[cls]], 1 - eps), eps)
    calib_data <- data.frame(
      PredProb = p_clipped,
      Label = as.integer(true_labels == cls)
    )
    platt_models[[cls]] <- glm(Label ~ PredProb, data = calib_data, family = binomial)
  }
  platt_models
}

# Predict calibrated probabilities on test data using fitted Platt models
platt_scale_predict <- function(platt_models, probs_df, class_levels) {
  calibrated_probs <- matrix(NA, nrow = nrow(probs_df), ncol = length(class_levels))
  colnames(calibrated_probs) <- class_levels
  
  for (cls in class_levels) {
    p <- probs_df[[cls]]
    calibrated_probs[, cls] <- plogis(predict(platt_models[[cls]], newdata = data.frame(PredProb = p)))
  }
  
  # Normalize rows to sum to 1
  calibrated_probs <- calibrated_probs / rowSums(calibrated_probs)
  as.data.frame(calibrated_probs)
}

# ----------------------------------------
# ----------- Main workflow --------------
# ----------------------------------------

# Assuming these are available from your earlier code:
# train_rf_mat, test_rf_mat, y_train, test_rf, best_rf, boost_params, class_levels, train_rf, rf_probs, rf_probs_boosted

# 1. Predict train probabilities with RF and boosted RF
rf_probs_train <- predict(final_rf, data = train_rf_mat)$predictions
prev_contrib_train <- as.character(train_rf$PrevContributor)

rf_probs_boosted_train <- t(mapply(
  apply_boost,
  split(rf_probs_train, row(rf_probs_train)),
  prev_contrib_train,
  MoreArgs = list(boost_params = boost_params, class_levels = class_levels)
))

# Convert to data frames with correct colnames
rf_probs_train_df <- as.data.frame(rf_probs_train)
colnames(rf_probs_train_df) <- class_levels

rf_probs_boosted_train_df <- as.data.frame(rf_probs_boosted_train)
colnames(rf_probs_boosted_train_df) <- class_levels

# 2. Fit Platt scaling models on train data
platt_rf_models <- fit_platt_models(rf_probs_train_df, y_train, class_levels)
platt_rf_boost_models <- fit_platt_models(rf_probs_boosted_train_df, y_train, class_levels)

# 3. Predict test probabilities (assumed from earlier steps)
rf_probs_test_df <- as.data.frame(rf_probs)
colnames(rf_probs_test_df) <- class_levels

rf_probs_boosted_test_df <- as.data.frame(rf_probs_boosted)
colnames(rf_probs_boosted_test_df) <- class_levels

# 4. Apply Platt scaling on test data
rf_probs_platt_test <- platt_scale_predict(platt_rf_models, rf_probs_test_df, class_levels)
rf_probs_boosted_platt_test <- platt_scale_predict(platt_rf_boost_models, rf_probs_boosted_test_df, class_levels)

# 5. Prepare calibration data for plotting
calib_rf <- prepare_calibration(rf_probs_test_df, test_rf$`Added by`) %>% mutate(Model = "RF Raw")
calib_rf_boost <- prepare_calibration(rf_probs_boosted_test_df, test_rf$`Added by`) %>% mutate(Model = "RF Boosted")
calib_rf_platt <- prepare_calibration(rf_probs_platt_test, test_rf$`Added by`) %>% mutate(Model = "RF Raw + Platt")
calib_rf_boost_platt <- prepare_calibration(rf_probs_boosted_platt_test, test_rf$`Added by`) %>% mutate(Model = "RF Boosted + Platt")

calib_all <- bind_rows(calib_rf, calib_rf_boost, calib_rf_platt, calib_rf_boost_platt)

# 6. Plot calibration curves
ggplot(calib_all, aes(x = MeanPredProb, y = ObservedAcc, color = Model)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Calibration Curves: Random Forest Models (Out-of-Sample)",
    x = "Mean Predicted Probability",
    y = "Observed Accuracy"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 7. Compute and print log loss for all four sets
log_rf <- compute_log_loss(rf_probs_test_df, test_rf$`Added by`, class_levels)
log_rf_boost <- compute_log_loss(rf_probs_boosted_test_df, test_rf$`Added by`, class_levels)
log_rf_platt <- compute_log_loss(rf_probs_platt_test, test_rf$`Added by`, class_levels)
log_rf_boost_platt <- compute_log_loss(rf_probs_boosted_platt_test, test_rf$`Added by`, class_levels)

cat("\nLog Loss (RF Raw):", log_rf)
cat("\nLog Loss (RF Boosted):", log_rf_boost)
cat("\nLog Loss (RF Raw + Platt):", log_rf_platt)
cat("\nLog Loss (RF Boosted + Platt):", log_rf_boost_platt)


library(splines)
library(dplyr)

# 1. Helper to prepare one-vs-rest training data for a class
prepare_calib_data <- function(class_name, probs_df, true_labels) {
  data.frame(
    PredProb = probs_df[[class_name]],
    Label = as.integer(true_labels == class_name)
  )
}

# 2. Fit spline models (natural cubic splines)
fit_spline_models <- function(probs_df, true_labels, class_levels, df_spline = 4) {
  models <- list()
  for (cls in class_levels) {
    calib_data <- prepare_calib_data(cls, probs_df, true_labels)
    models[[cls]] <- glm(Label ~ ns(PredProb, df = df_spline), data = calib_data, family = binomial)
  }
  models
}

# 3. Predict calibrated probabilities using spline models
predict_spline_calibrated <- function(probs_df, spline_models, class_levels) {
  calibrated_probs <- matrix(NA, nrow = nrow(probs_df), ncol = length(class_levels))
  colnames(calibrated_probs) <- class_levels
  
  for (cls in class_levels) {
    pred_probs <- probs_df[[cls]]
    calibrated_probs[, cls] <- plogis(predict(spline_models[[cls]], newdata = data.frame(PredProb = pred_probs)))
  }
  
  calibrated_probs <- calibrated_probs / rowSums(calibrated_probs)  # Normalize
  as.data.frame(calibrated_probs)
}
# Fit on train data
spline_rf_models <- fit_spline_models(rf_probs_train_df, y_train, class_levels)

# Apply to test data
rf_probs_spline_test <- predict_spline_calibrated(as.data.frame(rf_probs), spline_rf_models, class_levels)

# Evaluate
log_spline_rf <- compute_log_loss(rf_probs_spline_test, test_rf$`Added by`, class_levels)
cat("Log Loss (RF + Spline Calibrated):", log_spline_rf)


# Save final model to required variable name
final_model <- final_rf
# Save the final model to a file
save(final_model, file = "../data/final_model.RData")
