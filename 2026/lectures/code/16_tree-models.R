#######################
### Authors: JP, RB ###
#######################

########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "ranger", "readr", "rpart", "scales", "tidyr", "xgboost"))

library(dplyr)
library(ggplot2)
library(ranger)
library(readr)
library(rpart)
library(scales)
library(tidyr)
library(xgboost)

################
### SETTINGS ###
################

set.seed(16)

data_path = "2026/lectures/data/16_expected-points.csv"
figure_dir = "2026/lectures/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

train_seasons = 2021:2023
valid_season = 2024
test_season = 2025

theme_week = theme_minimal(base_size = 12) +
    theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot"
    )

rmse = function(actual, predicted) sqrt(mean((actual - predicted)^2))

#######################
### SCHEMATIC FIGURE ##
#######################

make_tree_icon = function(center_x, center_y, scale = 1, label = "") {
    nodes = tibble(
        local_id = c("a", "b", "c", "d", "e"),
        x = center_x + scale * c(0, -0.42, 0.42, -0.65, -0.20),
        y = center_y + scale * c(0.52, 0.05, 0.05, -0.42, -0.42),
        label = c(label, "", "", "", "")
    )
    edges = tibble(
        from = c("a", "a", "b", "b"),
        to = c("b", "c", "d", "e")
    ) |>
        left_join(nodes |> select(from = local_id, x_from = x, y_from = y), by = "from") |>
        left_join(nodes |> select(to = local_id, x_to = x, y_to = y), by = "to")
    list(nodes = nodes, edges = edges)
}

forest_icons = lapply(seq_len(5), \(i) make_tree_icon(i, 2.55, 0.75, paste0("Tree ", i)))
forest_nodes = bind_rows(lapply(forest_icons, `[[`, "nodes"))
forest_edges = bind_rows(lapply(forest_icons, `[[`, "edges"))

forest_arrows = tibble(
    x = 1:5,
    y = 1.85,
    xend = 3,
    yend = 0.85
)

forest_diagram = ggplot() +
    geom_segment(
        data = forest_edges,
        aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
        linewidth = 0.7,
        color = "gray40"
    ) +
    geom_point(
        data = forest_nodes,
        aes(x, y),
        size = 3.2,
        color = "#1F78B4"
    ) +
    geom_text(
        data = forest_nodes |> filter(label != ""),
        aes(x, y + 0.35, label = label),
        size = 3.4,
        color = "gray20"
    ) +
    geom_segment(
        data = forest_arrows,
        aes(x = x, y = y, xend = xend, yend = yend),
        linewidth = 0.55,
        color = "gray55",
        arrow = grid::arrow(length = grid::unit(0.13, "inches"), type = "closed")
    ) +
    geom_label(
        data = tibble(
            x = 3,
            y = 0.5,
            label = "Average tree predictions\nRF expected points"
        ),
        aes(x, y, label = label),
        linewidth = 0.35,
        fill = "#E5F4E3",
        size = 4.0,
        lineheight = 0.95
    ) +
    coord_cartesian(xlim = c(0.25, 5.75), ylim = c(0.05, 3.35), clip = "off") +
    labs(
        title = "Random Forest Prediction"
    ) +
    theme_void(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(8, 8, 8, 8)
    )

ggsave(
    file.path(figure_dir, "16_random-forest-diagram.png"),
    forest_diagram,
    width = 8.2,
    height = 4.2,
    dpi = 300,
    bg = "white"
)

################
### LOAD DATA ##
################

ep = read_csv(data_path, show_col_types = FALSE) |>
    mutate(
        down = factor(down),
        half = factor(half),
        field_pos = 100 - yardline_100,
        next_score_class = factor(pts_next_score, levels = c(-7, -3, -2, 0, 2, 3, 7))
    )

model_formula =
    pts_next_score ~ yardline_100 + down + ydstogo + half +
    half_seconds_remaining + posteam_timeouts_remaining +
    defteam_timeouts_remaining + posteam_spread

class_formula =
    next_score_class ~ yardline_100 + down + ydstogo + half +
    half_seconds_remaining + posteam_timeouts_remaining +
    defteam_timeouts_remaining + posteam_spread

train = ep |> filter(season %in% train_seasons)
valid = ep |> filter(season == valid_season)
test = ep |> filter(season == test_season)
train_valid = ep |> filter(season %in% c(train_seasons, valid_season))

#########################
### SINGLE TREE TUNING ##
#########################

fit_tree = function(data, depth) {
    rpart(
        model_formula,
        data = data,
        method = "anova",
        control = rpart.control(
            cp = 0,
            maxdepth = depth,
            minsplit = 800,
            minbucket = 300,
            xval = 0
        )
    )
}

depth_results = tibble(depth = 1:10) |>
    mutate(
        fit = lapply(depth, \(d) fit_tree(train, d)),
        train_rmse = vapply(fit, \(m) rmse(train$pts_next_score, predict(m, train)), numeric(1)),
        valid_rmse = vapply(fit, \(m) rmse(valid$pts_next_score, predict(m, valid)), numeric(1))
    )

best_depth = depth_results$depth[which.min(depth_results$valid_rmse)]
tree_fit = fit_tree(train_valid, best_depth)

display_tree_fit = fit_tree(train_valid, depth = 3)

pretty_var = function(x) {
    recode(
        x,
        yardline_100 = "Yards to end zone",
        down = "Down",
        ydstogo = "Yards to go",
        half = "Half",
        half_seconds_remaining = "Half seconds remaining",
        posteam_timeouts_remaining = "Offense timeouts",
        defteam_timeouts_remaining = "Defense timeouts",
        posteam_spread = "Possession-team spread",
        .default = x
    )
}

pretty_condition = function(x) {
    x = gsub("yardline_100", "yds to end", x, fixed = TRUE)
    x = gsub("posteam_spread", "spread", x, fixed = TRUE)
    x = gsub("posteam_timeouts_remaining", "off TO", x, fixed = TRUE)
    x = gsub("defteam_timeouts_remaining", "def TO", x, fixed = TRUE)
    x = gsub("half_seconds_remaining", "half sec", x, fixed = TRUE)
    x = gsub("ydstogo", "yds to go", x, fixed = TRUE)
    x = gsub(">=", " >= ", x, fixed = TRUE)
    x = gsub("<", " < ", x, fixed = TRUE)
    x = gsub("(?<![<>])=", " = ", x, perl = TRUE)
    x = gsub("\\s+", " ", x)
    trimws(x)
}

tree_frame = display_tree_fit$frame |>
    mutate(
        node = as.integer(row.names(display_tree_fit$frame)),
        depth = floor(log2(node)),
        x = 2 * ((node - 2^depth + 0.5) / 2^depth) - 1,
        y = -depth,
        type = ifelse(var == "<leaf>", "leaf", "split"),
        split_question = vapply(
            seq_along(node),
            \(i) {
                if (type[i] == "split") {
                    node_id = node[i]
                    pretty_condition(tail(path.rpart(display_tree_fit, nodes = 2 * node_id, print.it = FALSE)[[as.character(2 * node_id)]], 1))
                } else {
                    NA_character_
                }
            },
            character(1)
        ),
        label = ifelse(
            type == "leaf",
            paste0("Leaf\nEP = ", number(yval, accuracy = 0.01), "\nn = ", comma(n)),
            paste0("If ", split_question, "?\nmean EP = ", number(yval, accuracy = 0.01), "\nn = ", comma(n))
        )
    )

learned_tree_edges = tree_frame |>
    filter(node != 1) |>
    mutate(parent = node %/% 2) |>
    left_join(tree_frame |> select(parent = node, x_from = x, y_from = y), by = "parent") |>
    mutate(
        x_mid = (x_from + x) / 2,
        y_mid = (y_from + y) / 2 + 0.08,
        edge_label = ifelse(node %% 2 == 0, "yes", "no")
    )

learned_tree_diagram = ggplot() +
    geom_segment(
        data = learned_tree_edges,
        aes(x = x_from, y = y_from - 0.10, xend = x, yend = y + 0.10),
        linewidth = 0.65,
        color = "gray48"
    ) +
    geom_label(
        data = learned_tree_edges,
        aes(x_mid, y_mid, label = edge_label),
        linewidth = 0,
        fill = "white",
        size = 2.7,
        color = "gray25",
        label.padding = grid::unit(0.10, "lines")
    ) +
    geom_label(
        data = tree_frame,
        aes(x, y, label = label, fill = type),
        linewidth = 0.35,
        label.padding = grid::unit(0.20, "lines"),
        size = 2.9,
        lineheight = 0.95,
        color = "gray10"
    ) +
    scale_fill_manual(values = c("split" = "#DCEBFA", "leaf" = "#E5F4E3")) +
    coord_cartesian(xlim = c(-1.08, 1.08), ylim = c(-3.35, 0.35), clip = "off") +
    labs(
        title = "Regression Tree for Expected Points"
    ) +
    theme_void(base_size = 12) +
    theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(8, 8, 8, 8)
    )

ggsave(
    file.path(figure_dir, "16_regression-tree-diagram.png"),
    learned_tree_diagram,
    width = 8.8,
    height = 5.1,
    dpi = 300,
    bg = "white"
)

############################
### CLASSIFICATION TREES ###
############################

class_log_loss = function(actual, probability_matrix) {
    probability_matrix = pmax(probability_matrix, 1e-6)
    class_index = match(as.character(actual), colnames(probability_matrix))
    -mean(log(probability_matrix[cbind(seq_along(class_index), class_index)]))
}

fit_class_tree = function(data, depth) {
    rpart(
        class_formula,
        data = data,
        method = "class",
        parms = list(split = "gini"),
        control = rpart.control(
            cp = 0,
            maxdepth = depth,
            minsplit = 800,
            minbucket = 300,
            xval = 0
        )
    )
}

class_depth_results = tibble(depth = 1:10) |>
    mutate(
        fit = lapply(depth, \(d) fit_class_tree(train, d)),
        train_log_loss = vapply(
            fit,
            \(m) class_log_loss(train$next_score_class, predict(m, train, type = "prob")),
            numeric(1)
        ),
        valid_log_loss = vapply(
            fit,
            \(m) class_log_loss(valid$next_score_class, predict(m, valid, type = "prob")),
            numeric(1)
        )
    )

best_class_depth = class_depth_results$depth[which.min(class_depth_results$valid_log_loss)]
class_tree_fit = fit_class_tree(train_valid, best_class_depth)

complexity_plot = depth_results |>
    select(depth, train_rmse, valid_rmse) |>
    pivot_longer(
        c(train_rmse, valid_rmse),
        names_to = "sample",
        values_to = "rmse"
    ) |>
    mutate(
        sample = recode(sample, train_rmse = "Training", valid_rmse = "Validation")
    ) |>
    ggplot(aes(depth, rmse, color = sample)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_vline(xintercept = best_depth, color = "gray55", linetype = "dashed") +
    scale_x_continuous(breaks = depth_results$depth) +
    scale_color_manual(values = c("Training" = "#4D4D4D", "Validation" = "#1F78B4")) +
    labs(
        title = "Training and Validation RMSE by Tree Depth",
        x = "Maximum tree depth",
        y = "RMSE in next-score points",
        color = NULL
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "16_tree-depth-validation.png"),
    complexity_plot,
    width = 7.6,
    height = 4.5,
    dpi = 300
)

##########################
### EXPECTED POINT CURVE #
##########################

curve_grid = expand_grid(
    field_pos = 1:99,
    down = factor(1:4, levels = levels(ep$down))
) |>
    mutate(
        yardline_100 = 100 - field_pos,
        ydstogo = 10,
        half = factor(1, levels = levels(ep$half)),
        half_seconds_remaining = 900,
        posteam_timeouts_remaining = 3,
        defteam_timeouts_remaining = 3,
        posteam_spread = 0
    )

curve_subtitle = "10 yards to go, first half, 15:00 left, 3 timeouts each, possession team spread 0"

curve_grid = curve_grid |>
    mutate(tree_ep = predict(tree_fit, curve_grid))

class_probability_grid = bind_cols(
    curve_grid |> select(field_pos, down),
    predict(class_tree_fit, curve_grid, type = "prob") |>
        as_tibble(.name_repair = "minimal")
) |>
    pivot_longer(
        cols = -c(field_pos, down),
        names_to = "next_score",
        values_to = "probability"
    ) |>
    mutate(
        next_score = factor(
            next_score,
            levels = c("-7", "-3", "-2", "0", "2", "3", "7"),
            labels = c("Opp TD (-7)", "Opp FG (-3)", "Opp safety (-2)", "No score (0)", "Safety (2)", "FG (3)", "TD (7)")
        )
    )

class_probability_plot = ggplot(class_probability_grid, aes(field_pos, probability, fill = next_score)) +
    geom_area(color = "white", linewidth = 0.12, alpha = 0.95) +
    facet_wrap(~ down, ncol = 2, labeller = labeller(down = \(x) paste("Down", x))) +
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
    scale_fill_manual(
        values = c(
            "Opp TD (-7)" = "#8B1A1A",
            "Opp FG (-3)" = "#D95F02",
            "Opp safety (-2)" = "#E78AC3",
            "No score (0)" = "#BDBDBD",
            "Safety (2)" = "#80B1D3",
            "FG (3)" = "#33A02C",
            "TD (7)" = "#1F78B4"
        )
    ) +
    labs(
        title = "Next Score Probabilities by Field Position and Down",
        x = "Offense field position: yards from own goal line",
        y = "Predicted probability",
        fill = "Next score"
    ) +
    theme_week +
    theme(
        legend.position = "right",
        strip.text = element_text(face = "bold")
    )

ggsave(
    file.path(figure_dir, "16_next-score-class-probabilities.png"),
    class_probability_plot,
    width = 8.4,
    height = 5.6,
    dpi = 300
)

bin_data = train_valid |>
    mutate(field_bin = pmax(0, pmin(95, 5 * floor(field_pos / 5)))) |>
    summarise(
        n = n(),
        observed_ep = mean(pts_next_score),
        field_pos = mean(field_pos),
        .by = c(field_bin, down)
    ) |>
    filter(n >= 150)

tree_curve_plot = ggplot() +
    geom_point(
        data = bin_data,
        aes(field_pos, observed_ep, color = down),
        alpha = 0.5,
        size = 1.7
    ) +
    geom_step(
        data = curve_grid,
        aes(field_pos, tree_ep, color = down),
        linewidth = 1
    ) +
    geom_hline(yintercept = 0, color = "gray55", linetype = "dashed") +
    scale_color_manual(
        values = c("1" = "#1F78B4", "2" = "#33A02C", "3" = "#E31A1C", "4" = "#6A3D9A"),
        labels = paste("Down", 1:4)
    ) +
    labs(
        title = "Expected Points by Field Position and Down",
        subtitle = curve_subtitle,
        x = "Offense field position: yards from own goal line",
        y = "Expected next score",
        color = NULL
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "16_expected-points-tree.png"),
    tree_curve_plot,
    width = 8.2,
    height = 5,
    dpi = 300
)

########################
### MODEL COMPARISON ###
########################

mean_pred = mean(train_valid$pts_next_score)

linear_fit = lm(
    pts_next_score ~ yardline_100 * down + ydstogo + half +
        half_seconds_remaining + posteam_timeouts_remaining +
        defteam_timeouts_remaining + posteam_spread,
    data = train_valid
)

fit_rf = function(data, mtry, min_node_size, num_trees = 300, importance = "none") {
    ranger(
        model_formula,
        data = data,
        num.trees = num_trees,
        mtry = mtry,
        min.node.size = min_node_size,
        importance = importance,
        seed = 16,
        num.threads = 4
    )
}

rf_grid = expand_grid(
    mtry = c(2, 3, 4),
    min_node_size = c(100, 250, 500, 1000)
)

rf_validation = rf_grid
rf_validation$valid_rmse = NA_real_
for (i in seq_len(nrow(rf_validation))) {
    rf_candidate = fit_rf(
        train,
        mtry = rf_validation$mtry[i],
        min_node_size = rf_validation$min_node_size[i]
    )
    rf_validation$valid_rmse[i] = rmse(
        valid$pts_next_score,
        predict(rf_candidate, valid)$predictions
    )
}

best_rf = rf_validation |>
    arrange(valid_rmse) |>
    slice(1)

rf_fit = fit_rf(
    train_valid,
    mtry = best_rf$mtry,
    min_node_size = best_rf$min_node_size,
    num_trees = 600,
    importance = "impurity"
)

curve_grid = curve_grid |>
    mutate(random_forest_ep = predict(rf_fit, curve_grid)$predictions)

random_forest_curve_plot = ggplot(curve_grid, aes(field_pos, random_forest_ep, color = down)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, color = "gray55", linetype = "dashed") +
    scale_color_manual(
        values = c("1" = "#1F78B4", "2" = "#33A02C", "3" = "#E31A1C", "4" = "#6A3D9A"),
        labels = paste("Down", 1:4)
    ) +
    labs(
        title = "Random Forest Expected Points by Field Position and Down",
        subtitle = curve_subtitle,
        x = "Offense field position: yards from own goal line",
        y = "Predicted expected next score",
        color = NULL
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "16_random-forest-expected-points.png"),
    random_forest_curve_plot,
    width = 8.2,
    height = 5,
    dpi = 300
)

xgb_formula =
    ~ yardline_100 + down + ydstogo + half + half_seconds_remaining +
    posteam_timeouts_remaining + defteam_timeouts_remaining + posteam_spread - 1

x_train = model.matrix(xgb_formula, train)
x_valid = model.matrix(xgb_formula, valid)
x_train_valid = model.matrix(xgb_formula, train_valid)
x_test = model.matrix(xgb_formula, test)

dtrain = xgb.DMatrix(x_train, label = train$pts_next_score)
dvalid = xgb.DMatrix(x_valid, label = valid$pts_next_score)
dtrain_valid = xgb.DMatrix(x_train_valid, label = train_valid$pts_next_score)
dtest = xgb.DMatrix(x_test, label = test$pts_next_score)

xgb_base_params = list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    nthread = 4,
    seed = 16
)

xgb_grid = expand_grid(
    eta = c(0.03, 0.06),
    max_depth = c(2, 3),
    min_child_weight = c(50, 100),
    subsample = c(0.80, 1.00),
    colsample_bytree = c(0.80, 1.00)
)

extract_xgb_best = function(fit) {
    evaluation_log = attr(fit, "evaluation_log")
    early_stop = attr(fit, "early_stop")
    best_iteration = early_stop$best_iteration
    if (length(best_iteration) == 0 || is.null(best_iteration) || is.na(best_iteration)) {
        best_iteration = evaluation_log$iter[which.min(evaluation_log$valid_rmse)]
    }
    tibble(
        best_nrounds = best_iteration,
        valid_rmse = min(evaluation_log$valid_rmse)
    )
}

xgb_validation = xgb_grid
xgb_validation$best_nrounds = NA_integer_
xgb_validation$valid_rmse = NA_real_
for (i in seq_len(nrow(xgb_validation))) {
    candidate_params = c(
        xgb_base_params,
        as.list(xgb_grid[i, ])
    )
    xgb_candidate = xgb.train(
        params = candidate_params,
        data = dtrain,
        nrounds = 700,
        evals = list(train = dtrain, valid = dvalid),
        early_stopping_rounds = 40,
        verbose = 0
    )
    candidate_best = extract_xgb_best(xgb_candidate)
    xgb_validation$best_nrounds[i] = candidate_best$best_nrounds
    xgb_validation$valid_rmse[i] = candidate_best$valid_rmse
}

best_xgb = xgb_validation |>
    arrange(valid_rmse) |>
    slice(1)

xgb_params = c(
    xgb_base_params,
    as.list(best_xgb |> select(eta, max_depth, min_child_weight, subsample, colsample_bytree))
)
best_nrounds = best_xgb$best_nrounds

xgb_fit = xgb.train(
    params = xgb_params,
    data = dtrain_valid,
    nrounds = best_nrounds,
    verbose = 0
)

test_rmse = c(
    "Mean only" = rmse(test$pts_next_score, rep(mean_pred, nrow(test))),
    "Linear model" = rmse(test$pts_next_score, predict(linear_fit, test)),
    "Single tree" = rmse(test$pts_next_score, predict(tree_fit, test)),
    "Random forest" = rmse(test$pts_next_score, predict(rf_fit, test)$predictions),
    "Boosted trees" = rmse(test$pts_next_score, predict(xgb_fit, dtest))
)

performance = tibble(
    model = factor(names(test_rmse), levels = names(test_rmse)),
    rmse = as.numeric(test_rmse)
)

performance_plot = ggplot(performance, aes(rmse, model, fill = model)) +
    geom_col(width = 0.58) +
    geom_text(
        aes(label = number(rmse, accuracy = 0.001)),
        hjust = -0.12,
        size = 3.2,
        show.legend = FALSE
    ) +
    scale_fill_manual(
        values = c(
            "Mean only" = "#737373",
            "Linear model" = "#4D4D4D",
            "Single tree" = "#1F78B4",
            "Random forest" = "#33A02C",
            "Boosted trees" = "#E31A1C"
        )
    ) +
    labs(
        title = "Test RMSE by Expected Points Model",
        x = "Test RMSE in next-score points (lower is better)",
        y = NULL,
        fill = NULL
    ) +
    coord_cartesian(
        xlim = c(0, max(performance$rmse) + 0.25),
        clip = "off"
    ) +
    theme_week +
    theme(
        legend.position = "none",
        plot.margin = margin(5.5, 36, 5.5, 5.5)
    )

ggsave(
    file.path(figure_dir, "16_model-comparison.png"),
    performance_plot,
    width = 8.4,
    height = 4.2,
    dpi = 300
)

##############################
### BOOSTED TREE EP CURVES ###
##############################

curve_grid = curve_grid |>
    mutate(boosted_ep = predict(xgb_fit, xgb.DMatrix(model.matrix(xgb_formula, curve_grid))))

boosted_curve_plot = ggplot(curve_grid, aes(field_pos, boosted_ep, color = down)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, color = "gray55", linetype = "dashed") +
    scale_color_manual(
        values = c("1" = "#1F78B4", "2" = "#33A02C", "3" = "#E31A1C", "4" = "#6A3D9A"),
        labels = paste("Down", 1:4)
    ) +
    labs(
        title = "Boosted Tree Expected Points by Field Position and Down",
        subtitle = curve_subtitle,
        x = "Offense field position: yards from own goal line",
        y = "Predicted expected next score",
        color = NULL
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "16_boosted-expected-points.png"),
    boosted_curve_plot,
    width = 8.2,
    height = 5,
    dpi = 300
)

print(depth_results |> select(depth, train_rmse, valid_rmse))
print(class_depth_results |> select(depth, train_log_loss, valid_log_loss))
print(rf_validation |> arrange(valid_rmse) |> head(8))
print(xgb_validation |> arrange(valid_rmse) |> head(8))
print(performance)
