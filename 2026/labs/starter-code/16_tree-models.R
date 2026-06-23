#######################
### Authors: JP, RB ###
#######################

#############
### SETUP ###
#############

# install.packages(c("ranger", "rpart", "tidyverse", "xgboost"))
library(ranger)
library(rpart)
library(splines)
library(tidyverse)
library(xgboost)

set.seed(16)

clip_probability = function(p, eps = 1e-6) {
    pmin(pmax(p, eps), 1 - eps)
}

log_loss = function(actual, predicted) {
    predicted = clip_probability(predicted)
    -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

brier_score = function(actual, predicted) {
    mean((actual - predicted)^2)
}

###################
### LOAD DATA #####
###################

wp = read_csv("../data/16_win-probability.csv", show_col_types = FALSE) |>
    mutate(
        down = factor(down),
        qtr = factor(qtr),
        half = factor(half),
        posteam_type = factor(posteam_type),
        posteam_win_factor = factor(posteam_win, levels = c(0, 1)),
        field_pos = 100 - yardline_100
    )

required_seasons = 2021:2025
missing_seasons = setdiff(required_seasons, sort(unique(wp$season)))
if (length(missing_seasons) > 0) {
    warning(
        "The win-probability data are missing season(s): ",
        paste(missing_seasons, collapse = ", "),
        ". Update ../data/16_win-probability.csv before completing this lab."
    )
}

train = wp |> filter(season %in% 2021:2023)
valid = wp |> filter(season == 2024)
test = wp |> filter(season == 2025)
train_valid = wp |> filter(season %in% 2021:2024)

cat("Train / validation / test plays:",
    nrow(train), "/", nrow(valid), "/", nrow(test), "\n")

split_counts = c(
    train = nrow(train),
    validation = nrow(valid),
    test = nrow(test)
)
if (any(split_counts == 0)) {
    stop(
        "One or more season splits is empty. This lab expects ",
        "../data/16_win-probability.csv to contain seasons 2021-2025."
    )
}

tree_formula =
    posteam_win_factor ~ score_differential + yardline_100 + down + ydstogo +
    qtr + half_seconds_remaining + game_seconds_remaining +
    posteam_timeouts_remaining + defteam_timeouts_remaining +
    posteam_spread + posteam_type

##########################
### LOGISTIC MODEL #######
##########################

logistic_formula =
    posteam_win ~ score_differential * game_seconds_remaining +
    yardline_100 * down + ydstogo + qtr +
    posteam_timeouts_remaining + defteam_timeouts_remaining +
    posteam_spread + posteam_type

# TODO: fit this one logistic formula on train and evaluate validation log loss.
# logistic_fit = glm(logistic_formula, data = train, family = binomial())
# logistic_valid_prob = predict(logistic_fit, valid, type = "response")
# logistic_valid_log_loss = log_loss(valid$posteam_win, logistic_valid_prob)
#
# final_logistic_fit = glm(logistic_formula, data = train_valid, family = binomial())
# logistic_test_log_loss = log_loss(
#     test$posteam_win,
#     predict(final_logistic_fit, test, type = "response")
# )

###########################
### SINGLE TREE TUNING ####
###########################

tree_grid = expand_grid(
    maxdepth = c(2, 3, 4, 5, 6, 8, 10),
    minbucket = c(100, 300, 600)
)

fit_tree = function(data, maxdepth, minbucket) {
    rpart(
        tree_formula,
        data = data,
        method = "class",
        control = rpart.control(
            cp = 0,
            maxdepth = maxdepth,
            minsplit = 2 * minbucket,
            minbucket = minbucket,
            xval = 0
        )
    )
}

# TODO: fit each tree candidate on train and choose by validation log loss.
# tree_validation = tree_grid
# tree_validation$valid_log_loss = NA_real_
# for (i in seq_len(nrow(tree_validation))) {
#     candidate = fit_tree(
#         train,
#         maxdepth = tree_validation$maxdepth[i],
#         minbucket = tree_validation$minbucket[i]
#     )
#     tree_validation$valid_log_loss[i] = log_loss(
#         valid$posteam_win,
#         predict(candidate, valid, type = "prob")[, "1"]
#     )
# }
# best_tree = tree_validation |> arrange(valid_log_loss) |> slice(1)
# tree_fit = fit_tree(train_valid, best_tree$maxdepth, best_tree$minbucket)
# tree_test_log_loss = log_loss(
#     test$posteam_win,
#     predict(tree_fit, test, type = "prob")[, "1"]
# )

##############################
### RANDOM FOREST TUNING #####
##############################

rf_grid = expand_grid(
    mtry = c(2, 3, 4, 5),
    min_node_size = c(100, 250, 500, 1000)
)

fit_rf = function(data, mtry, min_node_size, num_trees = 300, importance = "none") {
    ranger(
        tree_formula,
        data = data,
        num.trees = num_trees,
        mtry = mtry,
        min.node.size = min_node_size,
        probability = TRUE,
        importance = importance,
        seed = 16,
        num.threads = 4
    )
}

# TODO: tune mtry and min_node_size on validation log loss.
# rf_validation = rf_grid
# rf_validation$valid_log_loss = NA_real_
# for (i in seq_len(nrow(rf_validation))) {
#     candidate = fit_rf(
#         train,
#         mtry = rf_validation$mtry[i],
#         min_node_size = rf_validation$min_node_size[i]
#     )
#     rf_validation$valid_log_loss[i] = log_loss(
#         valid$posteam_win,
#         predict(candidate, valid)$predictions[, "1"]
#     )
# }
# best_rf = rf_validation |> arrange(valid_log_loss) |> slice(1)
# rf_fit = fit_rf(
#     train_valid,
#     mtry = best_rf$mtry,
#     min_node_size = best_rf$min_node_size,
#     num_trees = 600,
#     importance = "impurity"
# )
# rf_test_log_loss = log_loss(
#     test$posteam_win,
#     predict(rf_fit, test)$predictions[, "1"]
# )

########################
### XGBOOST TUNING #####
########################

xgb_formula =
    ~ score_differential + yardline_100 + down + ydstogo + qtr +
    half_seconds_remaining + game_seconds_remaining +
    posteam_timeouts_remaining + defteam_timeouts_remaining +
    posteam_spread + posteam_type - 1

make_xgb_matrix = function(data) {
    model.matrix(xgb_formula, data)
}

x_train = make_xgb_matrix(train)
x_valid = make_xgb_matrix(valid)
x_train_valid = make_xgb_matrix(train_valid)
x_test = make_xgb_matrix(test)

dtrain = xgb.DMatrix(x_train, label = train$posteam_win)
dvalid = xgb.DMatrix(x_valid, label = valid$posteam_win)
dtrain_valid = xgb.DMatrix(x_train_valid, label = train_valid$posteam_win)
dtest = xgb.DMatrix(x_test, label = test$posteam_win)

xgb_grid = expand_grid(
    eta = c(0.03, 0.06),
    max_depth = c(2, 3),
    min_child_weight = c(50, 100),
    subsample = c(0.80, 1.00),
    colsample_bytree = c(0.80, 1.00)
)

base_xgb_params = list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    nthread = 4,
    seed = 16
)

# TODO: tune XGBoost. Use early stopping on the validation set.
# xgb_validation = xgb_grid
# xgb_validation$best_nrounds = NA_integer_
# xgb_validation$valid_log_loss = NA_real_
# for (i in seq_len(nrow(xgb_validation))) {
#     params = c(base_xgb_params, as.list(xgb_validation[i, names(xgb_grid)]))
#     candidate = xgb.train(
#         params = params,
#         data = dtrain,
#         nrounds = 1000,
#         watchlist = list(valid = dvalid),
#         early_stopping_rounds = 25,
#         verbose = 0
#     )
#     xgb_validation$best_nrounds[i] = candidate$best_iteration
#     xgb_validation$valid_log_loss[i] = candidate$best_score
# }
# best_xgb = xgb_validation |> arrange(valid_log_loss) |> slice(1)
# final_xgb_params = c(base_xgb_params, as.list(best_xgb[names(xgb_grid)]))
# xgb_fit = xgb.train(
#     params = final_xgb_params,
#     data = dtrain_valid,
#     nrounds = best_xgb$best_nrounds,
#     verbose = 0
# )
# xgb_test_log_loss = log_loss(test$posteam_win, predict(xgb_fit, dtest))

############################
### TEST COMPARISON TABLE ##
############################

# TODO: create a model comparison table after fitting all four final models.
# comparison = tibble(
#     model = c("Logistic model", "Classification tree", "Random forest", "XGBoost"),
#     validation_log_loss = c(...),
#     test_log_loss = c(logistic_test_log_loss, tree_test_log_loss, rf_test_log_loss, xgb_test_log_loss)
# )
#
# ggplot(comparison, aes(reorder(model, test_log_loss), test_log_loss)) +
#     geom_col() +
#     coord_flip() +
#     labs(x = NULL, y = "Test log loss")

#####################################
### PARTIAL DEPENDENCE-STYLE GRIDS ##
#####################################

quarter_grid = tibble(
    qtr = factor(1:4, levels = levels(wp$qtr)),
    game_seconds_remaining = c(3150, 2250, 1350, 450),
    half_seconds_remaining = c(1350, 450, 1350, 450)
)

score_grid = expand_grid(
    score_differential = -21:21,
    qtr = factor(1:4, levels = levels(wp$qtr))
) |>
    left_join(quarter_grid, by = "qtr") |>
    mutate(
        yardline_100 = 50,
        field_pos = 50,
        down = factor(1, levels = levels(wp$down)),
        ydstogo = 10,
        posteam_timeouts_remaining = 3,
        defteam_timeouts_remaining = 3,
        posteam_spread = 0,
        posteam_type = factor("home", levels = levels(wp$posteam_type))
    )

heatmap_grid = expand_grid(
    score_differential = -21:21,
    game_seconds_remaining = seq(0, 3600, by = 120)
) |>
    mutate(
        qtr = factor(
            case_when(
                game_seconds_remaining > 2700 ~ 1,
                game_seconds_remaining > 1800 ~ 2,
                game_seconds_remaining > 900 ~ 3,
                TRUE ~ 4
            ),
            levels = levels(wp$qtr)
        ),
        half_seconds_remaining = if_else(
            game_seconds_remaining > 1800,
            game_seconds_remaining - 1800,
            game_seconds_remaining
        ),
        yardline_100 = 50,
        field_pos = 50,
        down = factor(1, levels = levels(wp$down)),
        ydstogo = 10,
        posteam_timeouts_remaining = 3,
        defteam_timeouts_remaining = 3,
        posteam_spread = 0,
        posteam_type = factor("home", levels = levels(wp$posteam_type))
    )

field_grid = expand_grid(
    field_pos = 1:99,
    score_differential = c(-7, 0, 7),
    down = factor(1:4, levels = levels(wp$down))
) |>
    mutate(
        yardline_100 = 100 - field_pos,
        ydstogo = 10,
        qtr = factor(4, levels = levels(wp$qtr)),
        half_seconds_remaining = 300,
        game_seconds_remaining = 300,
        posteam_timeouts_remaining = 3,
        defteam_timeouts_remaining = 3,
        posteam_spread = 0,
        posteam_type = factor("home", levels = levels(wp$posteam_type))
    )

#########################
### BLOCK BOOTSTRAP #####
#########################

sample_games_with_replacement = function(data) {
    games = unique(data$game_id)
    sampled_games = tibble(
        boot_id = seq_along(games),
        game_id = sample(games, size = length(games), replace = TRUE)
    )

    sampled_games |>
        left_join(data, by = "game_id")
}

B = 100

# TODO: set best_model to the model class with the lowest final test log loss.
# Keep its selected tuning parameters fixed during the bootstrap.
# best_model = "TODO"

# TODO: predict on each plotting grid for every bootstrap refit.
# boot_score_predictions = matrix(NA_real_, nrow = nrow(score_grid), ncol = B)
# boot_heatmap_predictions = matrix(NA_real_, nrow = nrow(heatmap_grid), ncol = B)
# boot_field_predictions = matrix(NA_real_, nrow = nrow(field_grid), ncol = B)
#
# for (b in seq_len(B)) {
#     boot_data = sample_games_with_replacement(train_valid)
#
#     # TODO: fit the best model class to boot_data with fixed tuning parameters.
#     # TODO: store predictions in boot_score_predictions[, b]
#     # TODO: store predictions in boot_heatmap_predictions[, b]
#     # TODO: store predictions in boot_field_predictions[, b]
# }

# TODO: compute pointwise 95% intervals.
# score_intervals = score_grid |>
#     mutate(
#         wp_hat = rowMeans(boot_score_predictions, na.rm = TRUE),
#         wp_lo = apply(boot_score_predictions, 1, quantile, probs = 0.025, na.rm = TRUE),
#         wp_hi = apply(boot_score_predictions, 1, quantile, probs = 0.975, na.rm = TRUE)
#     )
#
# heatmap_intervals = heatmap_grid |>
#     mutate(
#         wp_hat = rowMeans(boot_heatmap_predictions, na.rm = TRUE),
#         wp_lo = apply(boot_heatmap_predictions, 1, quantile, probs = 0.025, na.rm = TRUE),
#         wp_hi = apply(boot_heatmap_predictions, 1, quantile, probs = 0.975, na.rm = TRUE),
#         interval_width = wp_hi - wp_lo
#     )
#
# field_intervals = field_grid |>
#     mutate(
#         wp_hat = rowMeans(boot_field_predictions, na.rm = TRUE),
#         wp_lo = apply(boot_field_predictions, 1, quantile, probs = 0.025, na.rm = TRUE),
#         wp_hi = apply(boot_field_predictions, 1, quantile, probs = 0.975, na.rm = TRUE)
#     )

####################################
### PLOTS WITH BOOTSTRAP INTERVALS #
####################################

# TODO: write one prediction helper per fitted model class, then bind predictions
# into long data frames for plotting. Add the bootstrap intervals to the rows for
# the best-performing model so the final plots only need to be made once.
# best_model_name = "TODO"
#
# score_predictions = bind_rows(
#     score_grid |> mutate(model = "Logistic model", wp_hat = predict(final_logistic_fit, score_grid, type = "response")),
#     score_grid |> mutate(model = "Classification tree", wp_hat = predict(tree_fit, score_grid, type = "prob")[, "1"]),
#     score_grid |> mutate(model = "Random forest", wp_hat = predict(rf_fit, score_grid)$predictions[, "1"]),
#     score_grid |> mutate(model = "XGBoost", wp_hat = predict(xgb_fit, xgb.DMatrix(make_xgb_matrix(score_grid))))
# ) |>
#     left_join(
#         score_intervals |> select(score_differential, qtr, wp_lo, wp_hi),
#         by = c("score_differential", "qtr")
#     ) |>
#     mutate(
#         wp_lo = if_else(model == best_model_name, wp_lo, NA_real_),
#         wp_hi = if_else(model == best_model_name, wp_hi, NA_real_)
#     )
#
# ggplot(score_predictions, aes(score_differential, wp_hat, color = qtr)) +
#     geom_ribbon(
#         aes(ymin = wp_lo, ymax = wp_hi, fill = qtr),
#         alpha = 0.15,
#         color = NA
#     ) +
#     geom_line(linewidth = 1) +
#     facet_wrap(~ model) +
#     geom_hline(yintercept = 0.5, color = "gray60", linetype = "dashed") +
#     labs(
#         x = "Possession-team score differential",
#         y = "Predicted win probability",
#         color = "Quarter",
#         fill = "Quarter"
#     )

# TODO: make the heatmap for the best model and include interval_width from
# heatmap_intervals as a second heatmap, contour layer, or facet.
# TODO: make the field-position plot and add ribbons from field_intervals for
# the best-performing model.
