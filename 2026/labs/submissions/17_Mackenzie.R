#############
### SETUP ###
#############

# Install packages only if needed
needed_packages = c("nnet", "tidyverse", "xgboost")

for (pkg in needed_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(nnet)
library(tidyverse)
library(xgboost)

set.seed(17)

########################
### HELPER FUNCTIONS ###
########################

clip_probability = function(p, eps = 1e-6) {
  pmin(pmax(p, eps), 1 - eps)
}

log_loss = function(actual, predicted) {
  predicted = clip_probability(predicted)
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

accuracy = function(actual, predicted) {
  mean(as.integer(predicted >= 0.5) == actual)
}

evaluate_predictions = function(model, split_name, actual, predicted) {
  tibble(
    model = model,
    split = split_name,
    log_loss = log_loss(actual, predicted),
    accuracy = accuracy(actual, predicted)
  )
}

################
### LOAD DATA ##
################

# Main modeling data for Tasks 1-3
pa = read_csv(
  "/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/17_play-action-vs-run.csv",
  show_col_types = FALSE
) |>
  mutate(
    playActionPass = as.integer(playActionPass),
    split = factor(split, levels = c("train", "validation", "test")),
    quarter = factor(quarter),
    down = factor(down),
    offenseFormation = factor(offenseFormation),
    receiverAlignment = factor(receiverAlignment),
    anyPreSnapMotion = factor(
      anyPreSnapMotion,
      levels = c(0, 1),
      labels = c("no_motion_or_shift", "motion_or_shift")
    )
  )

# Snap-frame coordinate data for Task 4 only
snap_examples = read_csv(
  "/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/17_snap-examples.csv",
  show_col_types = FALSE
) |>
  mutate(
    playActionPass = as.integer(playActionPass),
    side = factor(side, levels = c("offense", "defense", "football"))
  )

train = pa |> filter(split == "train")
valid = pa |> filter(split == "validation")
test = pa |> filter(split == "test")
train_valid = pa |> filter(split %in% c("train", "validation"))

cat("Train / validation / test plays:",
    nrow(train), "/", nrow(valid), "/", nrow(test), "\n")

############################
### TASK 1: CLASS BALANCE ###
############################

class_balance = pa |>
  group_by(split) |>
  summarise(
    n = n(),
    runs = sum(playActionPass == 0),
    play_action_passes = sum(playActionPass == 1),
    play_action_rate = mean(playActionPass),
    .groups = "drop"
  )

print(class_balance)

######################
### MODEL FEATURES ###
######################

model_vars = c(
  "quarter",
  "down",
  "yardsToGo",
  "yardsToEndzone",
  "gameClockSeconds",
  "playClockAtSnap",
  "scoreDifferential",
  "expectedPoints",
  "offenseFormation",
  "receiverAlignment",
  "anyPreSnapMotion",
  "offenseWidth",
  "defenseWidth",
  "offenseDepth",
  "defenseDepth",
  "meanOffenseBackfieldDepth",
  "meanDefenderDepth",
  "meanOffenseSpeed",
  "meanDefenseSpeed",
  "boxDefenders",
  "meanNearestDefender",
  "minNearestDefender"
)

# Keep only the variables needed for modeling and remove incomplete rows
model_data = pa |>
  select(split, playActionPass, all_of(model_vars)) |>
  drop_na()

design_formula = as.formula(
  paste("~", paste(model_vars, collapse = " + "))
)

# Build one design matrix so train/validation/test have identical columns
design_all = model.matrix(design_formula, model_data)[, -1, drop = FALSE]

train_idx = model_data$split == "train"
valid_idx = model_data$split == "validation"
test_idx = model_data$split == "test"
train_valid_idx = model_data$split %in% c("train", "validation")

# Standardization uses only the training rows
x_center = colMeans(design_all[train_idx, , drop = FALSE])
x_scale = apply(design_all[train_idx, , drop = FALSE], 2, sd)
x_scale[x_scale == 0] = 1

standardize = function(x) {
  x |>
    sweep(2, x_center, "-") |>
    sweep(2, x_scale, "/")
}

x_all = standardize(design_all)

x_train = x_all[train_idx, , drop = FALSE]
x_valid = x_all[valid_idx, , drop = FALSE]
x_test = x_all[test_idx, , drop = FALSE]
x_train_valid = x_all[train_valid_idx, , drop = FALSE]

y_train = model_data$playActionPass[train_idx]
y_valid = model_data$playActionPass[valid_idx]
y_test = model_data$playActionPass[test_idx]
y_train_valid = model_data$playActionPass[train_valid_idx]

##################################################
### TASK 1: BASELINE 1 - TRAINING MEAN MODEL #####
##################################################

mean_pred = mean(y_train)

mean_valid = rep(mean_pred, length(y_valid))
mean_test = rep(mean_pred, length(y_test))

mean_valid_result = evaluate_predictions(
  "Training mean",
  "validation",
  y_valid,
  mean_valid
)

mean_test_result = evaluate_predictions(
  "Training mean",
  "test",
  y_test,
  mean_test
)

print(mean_valid_result)
print(mean_test_result)

#########################################################
### TASK 1: BASELINE 2 - FORMATION + ALIGNMENT MODEL ####
#########################################################

formation_rates = train |>
  group_by(offenseFormation, receiverAlignment) |>
  summarise(
    rate = mean(playActionPass),
    n = n(),
    .groups = "drop"
  )

predict_formation_rate = function(new_data, fallback) {
  new_data |>
    left_join(
      formation_rates,
      by = c("offenseFormation", "receiverAlignment")
    ) |>
    mutate(rate = coalesce(rate, fallback)) |>
    pull(rate)
}

formation_valid = predict_formation_rate(valid, mean_pred)
formation_test = predict_formation_rate(test, mean_pred)

formation_valid_result = evaluate_predictions(
  "Formation rate",
  "validation",
  valid$playActionPass,
  formation_valid
)

formation_test_result = evaluate_predictions(
  "Formation rate",
  "test",
  test$playActionPass,
  formation_test
)

print(formation_valid_result)
print(formation_test_result)

###########################################################
### TASK 1: TABLE AND PLOT OF FORMATION PLAY-ACTION RATE ##
###########################################################

formation_table = train |>
  group_by(offenseFormation, receiverAlignment) |>
  summarise(
    n = n(),
    play_action_rate = mean(playActionPass),
    .groups = "drop"
  ) |>
  arrange(desc(play_action_rate))

cat("\nHighest play-action rate cells:\n")
print(formation_table |> slice_head(n = 10))

cat("\nLowest play-action rate cells:\n")
print(formation_table |> arrange(play_action_rate) |> slice_head(n = 10))

formation_plot = ggplot(
  formation_table,
  aes(
    x = receiverAlignment,
    y = reorder(offenseFormation, play_action_rate),
    fill = play_action_rate
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(100 * play_action_rate, 1), "%")),
            size = 3) +
  labs(
    title = "Training Data Play-Action Rate by Formation and Receiver Alignment",
    x = "Receiver alignment",
    y = "Offensive formation",
    fill = "Play-action rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.title = element_text(face = "bold")
  )

print(formation_plot)

###################################
### TASK 2: LOGISTIC REGRESSION ###
###################################

logistic_formula = as.formula(
  paste("playActionPass ~", paste(model_vars, collapse = " + "))
)

# Fit on Weeks 1-6 and evaluate on Week 7
logistic_fit = glm(
  logistic_formula,
  data = train,
  family = binomial()
)

logistic_valid = predict(
  logistic_fit,
  newdata = valid,
  type = "response"
)

logistic_valid_log_loss = log_loss(valid$playActionPass, logistic_valid)

logistic_valid_result = evaluate_predictions(
  "Logistic regression",
  "validation",
  valid$playActionPass,
  logistic_valid
)

print(logistic_valid_result)

# Refit on Weeks 1-7 and evaluate on Weeks 8-9
final_logistic_fit = glm(
  logistic_formula,
  data = train_valid,
  family = binomial()
)

logistic_test = predict(
  final_logistic_fit,
  newdata = test,
  type = "response"
)

logistic_test_log_loss = log_loss(test$playActionPass, logistic_test)

logistic_test_result = evaluate_predictions(
  "Logistic regression",
  "test",
  test$playActionPass,
  logistic_test
)

print(logistic_test_result)

####################################
### TASK 2: FEEDFORWARD NETWORK ####
####################################

nnet_grid = expand_grid(
  size = c(2, 4, 6, 8),
  decay = c(0.0001, 0.001, 0.01, 0.1, 1)
)

fit_nnet_candidate = function(size, decay, x, y) {
  nnet(
    x = x,
    y = y,
    size = size,
    decay = decay,
    entropy = TRUE,
    maxit = 400,
    trace = FALSE,
    MaxNWts = 10000
  )
}

nnet_validation = nnet_grid
nnet_validation$valid_log_loss = NA_real_

nnet_fits = vector("list", nrow(nnet_validation))

for (i in seq_len(nrow(nnet_validation))) {
  cat("Fitting neural network candidate", i, "of", nrow(nnet_validation), "\n")
  
  nnet_fits[[i]] = fit_nnet_candidate(
    size = nnet_validation$size[i],
    decay = nnet_validation$decay[i],
    x = x_train,
    y = y_train
  )
  
  valid_prob = as.numeric(
    predict(nnet_fits[[i]], x_valid, type = "raw")
  )
  
  nnet_validation$valid_log_loss[i] = log_loss(y_valid, valid_prob)
}

nnet_validation = nnet_validation |>
  arrange(valid_log_loss)

cat("\nNeural network validation grid:\n")
print(nnet_validation)

best_nnet_row = nnet_validation |> slice(1)

cat("\nBest neural network tuning parameters:\n")
print(best_nnet_row)

# Refit chosen network on Weeks 1-7
final_nnet = fit_nnet_candidate(
  size = best_nnet_row$size,
  decay = best_nnet_row$decay,
  x = x_train_valid,
  y = y_train_valid
)

nnet_valid = as.numeric(
  predict(nnet_fits[[which.min(nnet_grid$size * 0 + nnet_validation$valid_log_loss)]],
          x_valid,
          type = "raw")
)

nnet_test = as.numeric(
  predict(final_nnet, x_test, type = "raw")
)

nnet_valid_log_loss = best_nnet_row$valid_log_loss
nnet_test_log_loss = log_loss(y_test, nnet_test)

nnet_test_result = evaluate_predictions(
  "Feedforward NN",
  "test",
  y_test,
  nnet_test
)

print(nnet_test_result)

##############################
### TASK 3: XGBOOST MODEL ####
##############################

dtrain = xgb.DMatrix(x_train, label = y_train)
dvalid = xgb.DMatrix(x_valid, label = y_valid)
dtest = xgb.DMatrix(x_test, label = y_test)

xgb_grid = expand_grid(
  max_depth = c(2, 4),
  eta = c(0.03, 0.1),
  min_child_weight = c(1, 5)
)

get_best_iteration = function(fit) {
  early_stop = attr(fit, "early_stop")
  
  if (!is.null(early_stop) && length(early_stop$best_iteration) > 0) {
    return(early_stop$best_iteration)
  }
  
  if (!is.null(fit$best_iteration)) {
    return(fit$best_iteration)
  }
  
  NA_integer_
}

xgb_validation = xgb_grid
xgb_validation$best_iteration = NA_integer_
xgb_validation$valid_log_loss = NA_real_

xgb_fits = vector("list", nrow(xgb_validation))

for (i in seq_len(nrow(xgb_validation))) {
  cat("Fitting XGBoost candidate", i, "of", nrow(xgb_validation), "\n")
  
  params = list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = xgb_validation$max_depth[i],
    eta = xgb_validation$eta[i],
    min_child_weight = xgb_validation$min_child_weight[i],
    subsample = 0.9,
    colsample_bytree = 0.9,
    nthread = 2,
    seed = 17
  )
  
  xgb_fits[[i]] = xgb.train(
    params = params,
    data = dtrain,
    nrounds = 300,
    watchlist = list(validation = dvalid),
    early_stopping_rounds = 20,
    verbose = 0
  )
  
  valid_prob = as.numeric(
    predict(xgb_fits[[i]], dvalid)
  )
  
  xgb_validation$valid_log_loss[i] = log_loss(y_valid, valid_prob)
  xgb_validation$best_iteration[i] = get_best_iteration(xgb_fits[[i]])
}

xgb_validation = xgb_validation |>
  arrange(valid_log_loss)

cat("\nXGBoost validation grid:\n")
print(xgb_validation)

best_xgb_row = xgb_validation |> slice(1)

cat("\nBest XGBoost tuning parameters:\n")
print(best_xgb_row)

# Refit selected XGBoost model on Weeks 1-7
dtrain_valid = xgb.DMatrix(x_train_valid, label = y_train_valid)

final_xgb_params = list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = best_xgb_row$max_depth,
  eta = best_xgb_row$eta,
  min_child_weight = best_xgb_row$min_child_weight,
  subsample = 0.9,
  colsample_bytree = 0.9,
  nthread = 2,
  seed = 17
)

final_xgb_nrounds = if_else(
  is.na(best_xgb_row$best_iteration),
  300L,
  as.integer(best_xgb_row$best_iteration)
)

final_xgb = xgb.train(
  params = final_xgb_params,
  data = dtrain_valid,
  nrounds = final_xgb_nrounds,
  verbose = 0
)

xgb_test = as.numeric(
  predict(final_xgb, dtest)
)

xgb_valid_log_loss = best_xgb_row$valid_log_loss
xgb_test_log_loss = log_loss(y_test, xgb_test)

xgb_test_result = evaluate_predictions(
  "XGBoost",
  "test",
  y_test,
  xgb_test
)

print(xgb_test_result)

################################
### TASK 3: MODEL COMPARISON ###
################################

comparison = tibble(
  model = c(
    "Training mean",
    "Formation rate",
    "Logistic regression",
    "Feedforward NN",
    "XGBoost"
  ),
  selected_tuning = c(
    "None",
    "Formation + receiver alignment cells",
    "None",
    paste0(
      "hidden units = ", best_nnet_row$size,
      ", decay = ", best_nnet_row$decay
    ),
    paste0(
      "max_depth = ", best_xgb_row$max_depth,
      ", eta = ", best_xgb_row$eta,
      ", min_child_weight = ", best_xgb_row$min_child_weight,
      ", rounds = ", final_xgb_nrounds
    )
  ),
  validation_log_loss = c(
    log_loss(y_valid, mean_valid),
    log_loss(valid$playActionPass, formation_valid),
    logistic_valid_log_loss,
    nnet_valid_log_loss,
    xgb_valid_log_loss
  ),
  test_log_loss = c(
    log_loss(y_test, mean_test),
    log_loss(test$playActionPass, formation_test),
    logistic_test_log_loss,
    nnet_test_log_loss,
    xgb_test_log_loss
  )
) |>
  arrange(test_log_loss)

cat("\nFinal model comparison:\n")
print(comparison)

best_model = comparison |> slice(1)

cat("\nBest test model:\n")
print(best_model)

comparison_plot = ggplot(
  comparison,
  aes(x = reorder(model, test_log_loss), y = test_log_loss)
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = round(test_log_loss, 3)),
    hjust = -0.1,
    size = 4
  ) +
  coord_flip() +
  labs(
    title = "Final Test Log Loss by Model",
    x = NULL,
    y = "Test log loss"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(comparison_plot)

cat("\nInterpretation for Task 3:\n")
cat(
  "The best model is the one with the lowest test log loss. ",
  "Lower log loss means the model gave better-calibrated probabilities to the actual outcomes. ",
  "If XGBoost is best, that makes sense because boosted trees can split on thresholds ",
  "such as yards to go, box defenders, spacing, down, and formation, while also building ",
  "interactions among those variables. That is useful here because play-action tendency ",
  "is probably not a simple linear function of the pre-snap variables.\n"
)

####################################
### TASK 4: FIELD TENSOR CHANNELS ##
####################################

make_field_grid = function(play_data, cell_size = 2) {
  play_data |>
    filter(side %in% c("offense", "defense")) |>
    mutate(
      x_cell = floor(xStd / cell_size) * cell_size + cell_size / 2,
      y_cell = floor(yStd / cell_size) * cell_size + cell_size / 2,
      channel = if_else(side == "offense", "Offense", "Defense")
    ) |>
    count(
      playActionPass,
      gameId,
      playId,
      channel,
      x_cell,
      y_cell,
      name = "value"
    )
}

# Choose one example play-action pass if available;
# otherwise use the first example play.
example_key = snap_examples |>
  distinct(gameId, playId, playActionPass) |>
  arrange(desc(playActionPass)) |>
  slice(1)

example_play = snap_examples |>
  semi_join(example_key, by = c("gameId", "playId", "playActionPass"))

example_grid = make_field_grid(example_play, cell_size = 2)

channel_plot = ggplot(
  example_grid,
  aes(x_cell, y_cell, fill = value)
) +
  geom_tile(width = 2, height = 2, color = "white") +
  facet_wrap(~ channel, nrow = 1) +
  coord_fixed(
    xlim = c(0, 120),
    ylim = c(0, 160 / 3),
    expand = FALSE
  ) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Offensive and Defensive Snap Channels",
    subtitle = paste0(
      "Game ID: ", example_key$gameId,
      ", Play ID: ", example_key$playId,
      ", Play action pass: ", example_key$playActionPass
    ),
    x = "Standardized field x",
    y = "Standardized field y",
    fill = "Players"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

print(channel_plot)

cat("\nTask 4 explanation:\n")
cat(
  "This grid representation preserves where offensive and defensive players are located ",
  "at the snap. It separates the offense and defense into different channels, similar to ",
  "how an image can have separate color channels. This preserves spatial structure such ",
  "as spacing, width, depth, box density, and defender alignment.\n\n"
)

cat(
  "However, the grid loses some information from the tabular row. For example, it may lose ",
  "game context such as down, distance, clock, score differential, expected points, motion, ",
  "and formation labels unless those are added as extra channels or separate model inputs. ",
  "The grid also coarsens exact player coordinates into cells, so precise locations are ",
  "rounded to the nearest grid square.\n\n"
)

cat(
  "A 3 x 3 CNN filter would look at small local neighborhoods of the field grid. ",
  "For example, it could learn to detect clusters of defenders near the box, offensive ",
  "line spacing, or local player density. Before training, the user chooses the grid size, ",
  "cell size, number of channels, filter size, number of filters, activation function, ",
  "and network architecture. The model learns the filter weights from data.\n"
)
