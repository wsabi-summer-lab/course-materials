#############
### SETUP ###
#############

# install.packages(c("ggplot2", "pdp", "ranger", "tidyverse", "vip", "xgboost"))
library(ggplot2)
library(pdp)
library(ranger)
library(tidyverse)
library(vip)
library(xgboost)
library(splitTools)
library(dials)

# set seed
set.seed(18)

###########################
### NFL WIN PROBABILITY ###
###########################

# read in data
nfl_data = read_csv("../data/18_nfl-wp.csv.gz")

# preview data
head(nfl_data)

train_data = nfl_data %>%
  filter(season <= 2019)

test_data = nfl_data %>%
  filter(season > 2019)

folds = create_folds(
  y = train_data$game_id,
  k = 5,
  type = "grouped",
  invert = TRUE
)

train_labels = train_data %>%
  select(label_win)

train_data = train_data %>%
  select(-season, -game_id, -label_win)

grid_size = 10
grid = grid_space_filling(
  finalize(mtry(), train_data),
  min_n(),
  tree_depth(),
  learn_rate(range = c(-1.5, -0.5), trans = log10_trans()),
  loss_reduction(),
  sample_size = sample_prop(),
  size = grid_size
) %>%
  mutate(
    mtry = mtry / length(train_data),
    monotone_constraints = "(1, 0, 0, 1, -1, -1, -1, -1)"
  ) %>%
  rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )

get_row <- function(row) {
  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("logloss"),
      eta = row$eta,
      gamma = row$gamma,
      subsample = row$subsample,
      colsample_bytree = row$colsample_bytree,
      max_depth = row$max_depth,
      min_child_weight = row$min_child_weight,
      monotone_constraints = row$monotone_constraints
    )
  
  # do the cross validation
  wp_cv_model <- xgboost::xgb.cv(
    data = as.matrix(train_data),
    label = train_labels$label_win,
    params = params,
    # this doesn't matter with early stopping in xgb.cv, just set a big number
    # the actual optimal rounds will be found in this tuning process
    nrounds = 15000,
    # created above
    folds = folds,
    metrics = list("logloss"),
    early_stopping_rounds = 50,
    print_every_n = 50
  )
  
  # bundle up the results together for returning
  output <- params
  output$iter <- wp_cv_model$best_iteration
  output$logloss <- wp_cv_model$evaluation_log[output$iter]$test_logloss_mean
  
  row_result <- bind_rows(output)
  
  return(row_result)
}

results <- purrr::map_df(1:nrow(grid), function(x) {
  get_row(grid %>% dplyr::slice(x))
})

results %>%
  dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  tidyr::pivot_longer(
    eta:min_child_weight,
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(value, logloss, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "logloss") +
  theme_minimal()

best_model <- results %>%
  dplyr::arrange(logloss) %>%
  dplyr::slice(1)

params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = best_model$eta,
    gamma = best_model$gamma,
    subsample = best_model$subsample,
    colsample_bytree = best_model$colsample_bytree,
    max_depth = best_model$max_depth,
    min_child_weight = best_model$min_child_weight,
    monotone_constraints = best_model$monotone_constraints
  )

nrounds <- best_model$iter

dtrain <- xgboost::xgb.DMatrix(
  data = as.matrix(train_data),
  label = train_labels$label_win
)
colnames(dtrain) <- colnames(train_data)
wp_model <- xgboost::xgboost(
  params = params,
  data = dtrain,
  label = train_labels$label_win,
  nrounds = nrounds,
  verbose = 2
)

importance <- xgboost::xgb.importance(
  feature_names = colnames(wp_model),
  model = wp_model
)
xgboost::xgb.ggplot.importance(importance_matrix = importance)


preds <- stats::predict(
  wp_model,
  # get rid of the things not needed for prediction here
  as.matrix(test_data %>% select(-label_win, -game_id, -season))
) %>%
  tibble::as_tibble() %>%
  dplyr::rename(wp = value) %>%
  dplyr::bind_cols(test_data)

MLmetrics::LogLoss(preds$wp, preds$label_win)

# Fit 100 bootstraped xgboost models
boot_models <- purrr::map(1:100, function(i) {
  boot_data <- test_data %>%
    dplyr::sample_n(nrow(test_data), replace = TRUE)
  
  dtrain <- xgboost::xgb.DMatrix(
    data = as.matrix(boot_data %>% select(-label_win, -game_id, -season)),
    label = boot_data$label_win
  )
  
  xgboost::xgboost(
    params = params,
    data = dtrain,
    label = boot_data$label_win,
    nrounds = nrounds,
    verbose = 0
  )
})

# Create 95% CI for each play
preds <- purrr::map_df(boot_models, function(model) {
  pred <- stats::predict(
    model,
    as.matrix(test_data %>% select(-label_win, -game_id, -season))
  )
  # Add predictions to a tibble along with all features
  tibble::tibble(wp = pred) %>%
    dplyr::bind_cols(test_data)
})
# Calculate mean and 95% CI for each play (group by all features)
preds <- preds %>%
  dplyr::group_by(game_id, game_seconds_remaining, score_differential, yardline_100) %>%
  dplyr::summarise(
    wp = mean(wp),
    lower_ci = stats::quantile(wp, 0.025),
    upper_ci = stats::quantile(wp, 0.975),
    .groups = "drop"
  )

# Line plot WP ~ game_seconds_remaining with CIs for some score_differentials (color)
# and yardline_100 (facet)
preds <- preds %>%
  mutate(
    score_differential_bin = ggplot2::cut_number(score_differential, n = 5, labels = FALSE),
    score_differential_bin = factor(
      score_differential_bin,
      labels = paste("Bin", 1:5)
    ),
    yardline_100_bin = ggplot2::cut_number(yardline_100, n = 5, labels = FALSE),
    yardline_100_bin = factor(
      yardline_100_bin,
      labels = paste("Yardline Bin", 1:5)
    )
  )

# Step 2: Plot with binned variables
ggplot(preds, aes(x = game_seconds_remaining, y = wp)) +
  geom_line(aes(color = score_differential_bin), size = 1) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = score_differential_bin), alpha = 0.2) +
  facet_wrap(~yardline_100_bin, scales = "free_y") +
  labs(
    x = "Game Seconds Remaining",
    y = "Win Probability",
    color = "Score Differential Bin",
    fill = "Score Differential Bin"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")