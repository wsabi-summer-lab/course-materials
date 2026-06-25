#######################
### Authors: JP, RB ###
#######################

#############
### SETUP ###
#############

# install.packages(c("nnet", "tidyverse", "xgboost"))
library(nnet)
library(tidyverse)
library(xgboost)

set.seed(17)

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

pa = read_csv("../data/17_play-action-vs-run.csv", show_col_types = FALSE) |>
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

snap_examples = read_csv("../data/17_snap-examples.csv", show_col_types = FALSE) |>
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
print(pa |> count(split, playActionPass))

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

model_data = pa |>
    select(split, playActionPass, all_of(model_vars)) |>
    drop_na()

design_formula = as.formula(
    paste("~", paste(model_vars, collapse = " + "))
)

# Build one design matrix so train/validation/test have identical columns.
# Standardization uses only the training rows.
design_all = model.matrix(design_formula, model_data)[, -1, drop = FALSE]
train_idx = model_data$split == "train"
valid_idx = model_data$split == "validation"
test_idx = model_data$split == "test"
train_valid_idx = model_data$split %in% c("train", "validation")

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

########################
### TASK 1: BASELINES ##
########################

# TODO: report class balance by split.

# TODO: fit the training-mean baseline.
mean_pred = mean(y_train)
mean_valid = rep(mean_pred, length(y_valid))
mean_test = rep(mean_pred, length(y_test))
evaluate_predictions("Training mean", "validation", y_valid, mean_valid)
evaluate_predictions("Training mean", "test", y_test, mean_test)

# TODO: fit a formation-and-alignment rate baseline using training data only.
formation_rates = train |>
    group_by(offenseFormation, receiverAlignment) |>
    summarise(rate = mean(playActionPass), n = n(), .groups = "drop")

predict_formation_rate = function(new_data, fallback) {
    new_data |>
        left_join(formation_rates, by = c("offenseFormation", "receiverAlignment")) |>
        mutate(rate = coalesce(rate, fallback)) |>
        pull(rate)
}

formation_valid = predict_formation_rate(valid, mean_pred)
formation_test = predict_formation_rate(test, mean_pred)

# Class balance by split
pa |> count(split, playActionPass) |>
  group_by(split) |>
  mutate(pct = n / sum(n)) |>
  print()

# Training mean baseline
mean_pred = mean(y_train)
mean_valid = rep(mean_pred, length(y_valid))
mean_test  = rep(mean_pred, length(y_test))
evaluate_predictions("Training mean", "validation", y_valid, mean_valid)
evaluate_predictions("Training mean", "test",       y_test,  mean_test)

# Formation-rate baseline
formation_rates = train |>
  group_by(offenseFormation, receiverAlignment) |>
  summarise(rate = mean(playActionPass), n = n(), .groups = "drop")

predict_formation_rate = function(new_data, fallback) {
  new_data |>
    left_join(formation_rates, by = c("offenseFormation", "receiverAlignment")) |>
    mutate(rate = coalesce(rate, fallback)) |>
    pull(rate)
}

formation_valid = predict_formation_rate(valid, mean_pred)
formation_test  = predict_formation_rate(test,  mean_pred)
evaluate_predictions("Formation rate", "validation", valid$playActionPass, formation_valid)
evaluate_predictions("Formation rate", "test",       test$playActionPass,  formation_test)

# Play-action rate heatmap by formation x receiver alignment (train only)
pa_rate_table = train |>
  group_by(offenseFormation, receiverAlignment) |>
  summarise(n = n(), pa_rate = mean(playActionPass), .groups = "drop")

ggplot(pa_rate_table, aes(x = receiverAlignment,
                          y = reorder(offenseFormation, pa_rate),
                          fill = pa_rate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(pa_rate * 100), "%\n(n=", n, ")")), size = 2.8) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                       midpoint = mean_pred, labels = scales::percent_format(1),
                       name = "PA rate") +
  labs(title = "Play-action rate by formation and receiver alignment (weeks 1–6)",
       x = "Receiver alignment", y = "Offensive formation") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#The cells with the highest play action rates are I formation 3x1 but that has to be mislabeling cause thats not possible, jumbo 2x0 has a high play action rate relative to the others. The ones that are the lowest are wildcat players.

###################################
### TASK 2: LOGISTIC REGRESSION ###
###################################

logistic_formula = as.formula(
    paste("playActionPass ~", paste(model_vars, collapse = " + "))
)

# TODO: fit logistic regression on the training weeks and evaluate validation
#log loss.
logistic_fit = glm(logistic_formula, data = train, family = binomial())
logistic_valid = predict(logistic_fit, valid, type = "response")
logistic_valid_log_loss = log_loss(valid$playActionPass, logistic_valid)

final_logistic_fit = glm(logistic_formula, data = train_valid, family = binomial())
logistic_test = predict(final_logistic_fit, test, type = "response")
logistic_test_log_loss = log_loss(test$playActionPass, logistic_test)

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

# TODO: fit every candidate in nnet_grid on x_train/y_train and choose by
# validation log loss.
nnet_validation = nnet_grid
nnet_validation$valid_log_loss = NA_real_
nnet_fits = vector("list", nrow(nnet_validation))

for (i in seq_len(nrow(nnet_validation))) {
    nnet_fits[[i]] = fit_nnet_candidate(
        size = nnet_validation$size[i],
        decay = nnet_validation$decay[i],
        x = x_train,
        y = y_train
    )
    valid_prob = as.numeric(predict(nnet_fits[[i]], x_valid, type = "raw"))
    nnet_validation$valid_log_loss[i] = log_loss(y_valid, valid_prob)
}

best_nnet_row = nnet_validation |> arrange(valid_log_loss) |> slice(1)
nnet_valid_log_loss = best_nnet_row$valid_log_loss
best_nnet_row

# TODO: after choosing size and decay, refit that neural network on the
# combined train+validation weeks and evaluate on the test weeks.
final_nnet = fit_nnet_candidate(
    size = best_nnet_row$size,
    decay = best_nnet_row$decay,
    x = x_train_valid,
    y = y_train_valid
)
nnet_test = as.numeric(predict(final_nnet, x_test, type = "raw"))
nnet_test_log_loss = log_loss(y_test, nnet_test)


logistic_valid_log_loss
logistic_test_log_loss
nnet_test_log_loss


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

xgb_validation = xgb_grid
xgb_validation$best_iteration = NA_integer_
xgb_validation$valid_log_loss = NA_real_
xgb_fits = vector("list", nrow(xgb_validation))

for (i in seq_len(nrow(xgb_validation))) {
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
    evals = list(validation = dvalid),
    early_stopping_rounds = 20,
    verbose = 0,
    callbacks = list(xgb.cb.evaluation.log())
  )
  
  valid_prob = as.numeric(predict(xgb_fits[[i]], dvalid))
  xgb_validation$valid_log_loss[i] = log_loss(y_valid, valid_prob)
  xgb_validation$best_iteration[i] = if (!is.null(xgb_fits[[i]]$best_iteration)) {
    xgb_fits[[i]]$best_iteration
  } else {
    300L
  }
}

best_xgb_row = xgb_validation |> arrange(valid_log_loss) |> slice(1)
best_xgb_row

# Refit on train+validation, evaluate on test
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

final_xgb = xgb.train(
  params = final_xgb_params,
  data = dtrain_valid,
  nrounds = if_else(is.na(best_xgb_row$best_iteration), 300L, best_xgb_row$best_iteration),
  verbose = 0
)
xgb_test = as.numeric(predict(final_xgb, dtest))
xgb_test_log_loss = log_loss(y_test, xgb_test)

################################
### TASK 3: MODEL COMPARISON ###
################################

comparison = tibble(
  model = c("Training mean", "Formation rate", "Logistic regression", "Feedforward NN", "XGBoost"),
  selected_tuning = c("none", "formation + alignment", "none",
                      paste0("size=", best_nnet_row$size, ", decay=", best_nnet_row$decay),
                      paste0("depth=", best_xgb_row$max_depth, ", eta=", best_xgb_row$eta, ", mcw=", best_xgb_row$min_child_weight)),
  validation_log_loss = c(log_loss(y_valid, mean_valid),
                          log_loss(valid$playActionPass, formation_valid),
                          logistic_valid_log_loss,
                          nnet_valid_log_loss,
                          best_xgb_row$valid_log_loss),
  test_log_loss = c(log_loss(y_test, mean_test),
                    log_loss(test$playActionPass, formation_test),
                    logistic_test_log_loss,
                    nnet_test_log_loss,
                    xgb_test_log_loss)
)

print(comparison)

ggplot(comparison, aes(reorder(model, test_log_loss), test_log_loss)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = round(test_log_loss, 3)), hjust = -0.1) +
  coord_flip() +
  labs(x = NULL, y = "Test log loss")

####################################
### TASK 4: FIELD TENSOR CHANNELS ##
####################################

####################################
### TASK 4: FIELD TENSOR CHANNELS ##
####################################

example_key = snap_examples |>
  distinct(gameId, playId, playActionPass) |>
  slice(1)

example_play = snap_examples |>
  semi_join(example_key, by = c("gameId", "playId"))

example_grid = make_field_grid(example_play, cell_size = 2)

ggplot(example_grid, aes(x_cell, y_cell, fill = value)) +
  geom_tile(width = 2, height = 2, color = "white") +
  facet_wrap(~ channel, nrow = 1) +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 160 / 3), expand = FALSE) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Standardized field x", y = "Standardized field y", fill = "Players")

#The preserved informaiton is the exact location but what is lost is the individual players identiy like their position and other facotrs like that. 
#A 3x3 filter would condense our data to be less specific, it woudl cluster our data into more chunks like the linemen could be clustered together a little more. 
#we define filter size, number of filters layers pooling and the classifier, the models learns from the data to see what spatial patterns actually matter to predictive play-action vs run.