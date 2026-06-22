#######################
### Authors: JP, RB ###
#######################

########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "glmnet", "Matrix", "scales", "tidyr"))

library(dplyr)
library(ggplot2)
library(glmnet)
library(Matrix)
library(scales)
library(tidyr)

################
### SETTINGS ###
################

set.seed(15)

data_path = "2026/labs/data/15_nba-lineups.rds"
figure_dir = "2026/lectures/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

theme_week = theme_minimal(base_size = 12) +
    theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot"
    )

selected_players = c(
    "Shai Gilgeous-Alexander",
    "Nikola Jokic",
    "Anthony Edwards",
    "Jalen Brunson",
    "Luka Doncic",
    "Victor Wembanyama",
    "Stephen Curry",
    "Domantas Sabonis",
    "Mikal Bridges",
    "Klay Thompson",
    "Austin Reaves",
    "Kyle Kuzma"
)

################
### LOAD DATA ##
################

nba = readRDS(data_path) |>
    filter(!is.na(lineup_team), !is.na(lineup_opp), !is.na(pts_poss)) |>
    arrange(game_id, period, poss_num_team)

split_lineup = function(x) strsplit(x, ", ", fixed = TRUE)
player_name = function(x) sub("^[0-9]+\\s+", "", x)

off_lineups = split_lineup(nba$lineup_team)
def_lineups = split_lineup(nba$lineup_opp)

complete_lineups = lengths(off_lineups) == 5 & lengths(def_lineups) == 5
nba = nba[complete_lineups, ]
off_lineups = off_lineups[complete_lineups]
def_lineups = def_lineups[complete_lineups]

#######################
### DESIGN MATRIX #####
#######################

player_tokens = unique(unlist(c(off_lineups, def_lineups), use.names = FALSE))
player_labels = player_name(player_tokens)
duplicate_names = names(which(table(player_labels) > 1))
player_labels = ifelse(player_labels %in% duplicate_names, player_tokens, player_labels)
player_index = setNames(seq_along(player_tokens), player_tokens)

n = nrow(nba)
off_tokens = unlist(off_lineups, use.names = FALSE)
def_tokens = unlist(def_lineups, use.names = FALSE)

X = sparseMatrix(
    i = c(rep(seq_len(n), lengths(off_lineups)), rep(seq_len(n), lengths(def_lineups))),
    j = unname(player_index[c(off_tokens, def_tokens)]),
    x = c(rep(1, length(off_tokens)), rep(-1, length(def_tokens))),
    dims = c(n, length(player_tokens))
)
colnames(X) = player_labels

games = sort(unique(nba$game_id))
n_games = length(games)
train_games = games[seq_len(floor(0.70 * n_games))]
valid_games = games[(floor(0.70 * n_games) + 1):floor(0.85 * n_games)]
test_games = games[(floor(0.85 * n_games) + 1):n_games]

idx_train = which(nba$game_id %in% train_games)
idx_valid = which(nba$game_id %in% valid_games)
idx_test = which(nba$game_id %in% test_games)

y = nba$pts_poss
train_mean = mean(y[idx_train])
y_centered = y - train_mean

player_possessions = as.numeric(colSums(abs(X[idx_train, ])))

center_coef = function(beta) {
    beta - weighted.mean(beta, player_possessions)
}

######################
### FIT ESTIMATORS ###
######################

# Raw plus-minus: average signed possession outcome while a player is on court.
pm_coef = as.numeric(crossprod(X[idx_train, ], y_centered[idx_train])) /
    pmax(player_possessions, 1)
pm_coef[player_possessions == 0] = 0
pm_coef = center_coef(pm_coef)

# APM: least-squares fit using the same plus-minus design matrix.
apm_fit = glmnet(
    X[idx_train, ], y_centered[idx_train],
    alpha = 0,
    lambda = 0,
    intercept = FALSE,
    standardize = FALSE
)
apm_coef = center_coef(as.numeric(coef(apm_fit))[-1])

# RAPM: ridge APM with lambda selected on later regular-season games.
lambdas = 10^seq(-4, 2.5, length.out = 50)
ridge_fit = glmnet(
    X[idx_train, ], y_centered[idx_train],
    alpha = 0,
    lambda = lambdas,
    intercept = FALSE,
    standardize = FALSE
)

valid_pred = predict(ridge_fit, newx = X[idx_valid, ])
valid_rmse = sqrt(colMeans(
    (matrix(y_centered[idx_valid], nrow = length(idx_valid), ncol = ncol(valid_pred)) -
        valid_pred)^2
))
best_lambda = ridge_fit$lambda[which.min(valid_rmse)]
rapm_coef = center_coef(as.numeric(coef(ridge_fit, s = best_lambda))[-1])

######################
### ESTIMATE PLOT ####
######################

estimates = tibble(
    player = player_labels,
    possessions = player_possessions,
    `P/M` = 100 * pm_coef,
    APM = 100 * apm_coef,
    RAPM = 100 * rapm_coef
)

estimate_plot_data = estimates |>
    filter(player %in% selected_players) |>
    arrange(RAPM) |>
    mutate(player = factor(player, levels = player)) |>
    pivot_longer(c(`P/M`, APM, RAPM), names_to = "estimator", values_to = "estimate")

estimate_ranges = estimate_plot_data |>
    summarise(
        xmin = min(estimate),
        xmax = max(estimate),
        .by = player
    )

estimate_plot = ggplot(estimate_plot_data, aes(estimate, player, color = estimator)) +
    geom_vline(xintercept = 0, color = "gray55", linetype = "dashed") +
    geom_segment(
        data = estimate_ranges,
        aes(x = xmin, xend = xmax, y = player, yend = player),
        inherit.aes = FALSE,
        color = "gray78",
        linewidth = 0.8
    ) +
    geom_point(size = 2.5) +
    scale_color_manual(values = c("P/M" = "#4D4D4D", "APM" = "#B22222", "RAPM" = "#1F78B4")) +
    labs(
        title = "Three Plus-Minus Estimates",
        subtitle = "Same 2023-24 possession data; estimates shown in points per 100 possessions",
        x = "Estimated player impact",
        y = NULL,
        color = "Estimator"
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "15_pm-apm-rapm-estimates.png"),
    estimate_plot,
    width = 8.4,
    height = 5.4,
    dpi = 300
)

#########################
### HELD-OUT RMSE PLOT ##
#########################

predict_rmse = function(beta, idx) {
    prediction = train_mean + as.numeric(X[idx, ] %*% beta)
    sqrt(mean((y[idx] - prediction)^2))
}

test_rmse = c(
    "Mean only" = sqrt(mean((y[idx_test] - train_mean)^2)),
    "P/M" = predict_rmse(pm_coef, idx_test),
    "APM" = predict_rmse(apm_coef, idx_test),
    "RAPM" = predict_rmse(rapm_coef, idx_test)
)

performance = tibble(
    estimator = factor(names(test_rmse), levels = names(test_rmse)),
    rmse = as.numeric(test_rmse),
    rmse_change = 100 * (rmse - test_rmse["Mean only"])
)

performance_plot = ggplot(performance, aes(rmse_change, estimator, fill = estimator)) +
    geom_vline(xintercept = 0, color = "gray55", linetype = "dashed") +
    geom_col(width = 0.58) +
    geom_text(
        aes(
            label = number(rmse_change, accuracy = 0.01, style_positive = "plus"),
            hjust = ifelse(rmse_change < 0, 1.15, -0.15)
        ),
        size = 3.2,
        show.legend = FALSE
    ) +
    scale_fill_manual(values = c("Mean only" = "#737373", "P/M" = "#4D4D4D", "APM" = "#B22222", "RAPM" = "#1F78B4")) +
    labs(
        title = "Final Holdout Prediction Error",
        subtitle = paste0("Change in test RMSE vs. mean-only baseline; validation selected lambda = ", number(best_lambda, accuracy = 0.001)),
        x = "RMSE change in points per 100 possessions (lower is better)",
        y = NULL,
        fill = "Estimator"
    ) +
    coord_cartesian(
        xlim = c(min(performance$rmse_change) - 0.08, max(performance$rmse_change) + 0.22),
        clip = "off"
    ) +
    theme_week +
    theme(
        legend.position = "none",
        plot.margin = margin(5.5, 28, 5.5, 5.5)
    )

ggsave(
    file.path(figure_dir, "15_holdout-rmse.png"),
    performance_plot,
    width = 8.4,
    height = 3.8,
    dpi = 300
)
