#######################
### Authors: RB, JP ###
### Solution script ###
#######################

#############
### SETUP ###
#############

# install.packages(c("ggplot2", "glmnet", "Matrix", "tidyverse"))
library(ggplot2)
library(glmnet)
library(Matrix)
library(tidyverse)

set.seed(15)

####################
### NBA LINEUPS ###
####################

nba_lineups = readRDS("~/Downloads/15_nba-lineups.rds") |>
    filter(!is.na(lineup_team), !is.na(lineup_opp), !is.na(pts_poss)) |>
    arrange(game_id, period, poss_num_team)

split_lineup = function(x) strsplit(x, ", ", fixed = TRUE)
player_name = function(x) sub("^[0-9]+\\s+", "", x)

off_lineups = split_lineup(nba_lineups$lineup_team)
def_lineups = split_lineup(nba_lineups$lineup_opp)

complete_lineups = lengths(off_lineups) == 5 & lengths(def_lineups) == 5
nba_lineups = nba_lineups[complete_lineups, ]
off_lineups = off_lineups[complete_lineups]
def_lineups = def_lineups[complete_lineups]

player_tokens = unique(unlist(c(off_lineups, def_lineups), use.names = FALSE))
player_labels = player_name(player_tokens)

# If two players share the same name, keep the NBA ID attached.
duplicate_names = names(which(table(player_labels) > 1))
player_labels = ifelse(player_labels %in% duplicate_names, player_tokens, player_labels)
player_index = setNames(seq_along(player_tokens), player_tokens)

#######################
### DESIGN MATRICES ###
#######################

n = nrow(nba_lineups)
off_tokens = unlist(off_lineups, use.names = FALSE)
def_tokens = unlist(def_lineups, use.names = FALSE)

# Combined plus-minus design:
#   +1 when the player is on offense
#   -1 when the player is on defense
#    0 otherwise
X_combined = sparseMatrix(
    i = c(
        rep(seq_len(n), lengths(off_lineups)),
        rep(seq_len(n), lengths(def_lineups))
    ),
    j = unname(player_index[c(off_tokens, def_tokens)]),
    x = c(rep(1, length(off_tokens)), rep(-1, length(def_tokens))),
    dims = c(n, length(player_tokens))
)
colnames(X_combined) = player_labels

# Offense-defense split design:
#   offense columns get +1
#   defense columns get -1, so larger defensive coefficients are better
X_split = sparseMatrix(
    i = c(
        rep(seq_len(n), lengths(off_lineups)),
        rep(seq_len(n), lengths(def_lineups))
    ),
    j = c(
        unname(player_index[off_tokens]),
        length(player_tokens) + unname(player_index[def_tokens])
    ),
    x = c(rep(1, length(off_tokens)), rep(-1, length(def_tokens))),
    dims = c(n, 2 * length(player_tokens))
)
colnames(X_split) = c(
    paste0(player_labels, "_off"),
    paste0(player_labels, "_def")
)

player_lookup = tibble(
    player_token = player_tokens,
    player = player_labels,
    combined_col = seq_along(player_tokens),
    off_col = seq_along(player_tokens),
    def_col = length(player_tokens) + seq_along(player_tokens)
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
# Edit selected_players to choose who appears in your coefficient plots.

####################
### DATA SPLITS ####
####################

games = sort(unique(nba_lineups$game_id))
n_games = length(games)

train_games = games[seq_len(floor(0.70 * n_games))]
valid_games = games[(floor(0.70 * n_games) + 1):floor(0.85 * n_games)]
test_games = games[(floor(0.85 * n_games) + 1):n_games]

idx_train = which(nba_lineups$game_id %in% train_games)
idx_valid = which(nba_lineups$game_id %in% valid_games)
idx_test = which(nba_lineups$game_id %in% test_games)

y = nba_lineups$pts_poss
train_mean = mean(y[idx_train])
y_centered = y - train_mean
lambdas = 10^seq(-4, 2.5, length.out = 50)

Xc_train = X_combined[idx_train, ]
Xc_valid = X_combined[idx_valid, ]
Xc_test = X_combined[idx_test, ]

Xs_train = X_split[idx_train, ]
Xs_valid = X_split[idx_valid, ]
Xs_test = X_split[idx_test, ]

y_train = y[idx_train]
y_valid = y[idx_valid]
y_test = y[idx_test]
y_train_centered = y_centered[idx_train]
y_valid_centered = y_centered[idx_valid]
y_test_centered = y_centered[idx_test]

player_possessions = as.numeric(colSums(abs(Xc_train)))

cat("Prepared", nrow(nba_lineups), "possessions from", n_games, "games and",
    length(player_tokens), "players.\n")
cat("Train / validation / test possessions:",
    length(idx_train), "/", length(idx_valid), "/", length(idx_test), "\n")

rmse = function(truth, prediction) {
    sqrt(mean((truth - prediction)^2))
}

fit_ridge_path = function(X_train, y_train_centered, X_valid, y_valid_centered, lambdas) {
    ridge_fit = glmnet(
        x = X_train,
        y = y_train_centered,
        alpha = 0,
        family = "gaussian",
        lambda = lambdas,
        intercept = FALSE,
        standardize = FALSE
    )

    valid_pred = predict(ridge_fit, newx = X_valid)
    valid_rmse = sqrt(colMeans(
        (as.matrix(valid_pred) - y_valid_centered)^2
    ))

    list(
        fit = ridge_fit,
        validation = tibble(
            lambda = ridge_fit$lambda,
            valid_rmse = as.numeric(valid_rmse)
        ),
        best_lambda = ridge_fit$lambda[which.min(valid_rmse)],
        best_valid_rmse = min(valid_rmse)
    )
}

predict_centered_model = function(fit, X_new, lambda = NULL) {
    if (is.null(lambda)) {
        train_mean + as.numeric(predict(fit, newx = X_new))
    } else {
        train_mean + as.numeric(predict(fit, newx = X_new, s = lambda))
    }
}

######################
### COMBINED MODEL ###
######################

## --- 1. Mean-only baseline ---------------------------------------------
mean_only_valid_pred = rep(train_mean, length(idx_valid))
mean_only_valid_rmse = rmse(y_valid, mean_only_valid_pred)
cat("Mean-only validation RMSE:", mean_only_valid_rmse, "\n")

## --- 2. Raw plus-minus --------------------------------------------------
# Per-possession raw plus-minus: average point differential attributable to
# the player while they were on the floor (no adjustment for teammates).
raw_pm_combined = as.numeric(
    Matrix::crossprod(Xc_train, y_train_centered)
) / player_possessions
raw_pm_combined[player_possessions == 0] = 0
names(raw_pm_combined) = player_labels

raw_pm_valid_pred = train_mean + as.numeric(Xc_valid %*% raw_pm_combined)
raw_pm_valid_rmse = rmse(y_valid, raw_pm_valid_pred)
cat("Raw plus-minus validation RMSE:", raw_pm_valid_rmse, "\n")

## --- 3. Unregularized APM (OLS via glmnet with lambda -> 0) -------------
apm_combined_fit = glmnet(
    x = Xc_train,
    y = y_train_centered,
    alpha = 0,
    family = "gaussian",
    lambda = c(1e-2, 0),
    intercept = FALSE,
    standardize = FALSE
)
apm_combined_coef = as.numeric(coef(apm_combined_fit, s = 0))[-1]
names(apm_combined_coef) = player_labels

apm_valid_pred = predict_centered_model(apm_combined_fit, Xc_valid, lambda = 0)
apm_valid_rmse = rmse(y_valid, apm_valid_pred)
cat("Unregularized combined APM validation RMSE:", apm_valid_rmse, "\n")

## --- 4. Ridge RAPM over the lambda grid ---------------------------------
combined_ridge = fit_ridge_path(
    X_train = Xc_train,
    y_train_centered = y_train_centered,
    X_valid = Xc_valid,
    y_valid_centered = y_valid_centered,
    lambdas = lambdas
)
cat("Combined RAPM best lambda:", combined_ridge$best_lambda, "\n")
cat("Combined RAPM best validation RMSE:", combined_ridge$best_valid_rmse, "\n")

combined_valid_curve = combined_ridge$validation
print(
    ggplot(combined_valid_curve, aes(lambda, valid_rmse)) +
        geom_line() +
        geom_point(
            data = combined_valid_curve |> filter(lambda == combined_ridge$best_lambda),
            color = "red", size = 2
        ) +
        scale_x_log10() +
        labs(
            title = "Combined model: validation RMSE vs. lambda",
            x = "lambda (log scale)", y = "Validation RMSE"
        ) +
        theme_minimal()
)

combined_rapm_coef = as.numeric(
    coef(combined_ridge$fit, s = combined_ridge$best_lambda)
)[-1]
names(combined_rapm_coef) = player_labels

combined_test_pred = predict_centered_model(
    combined_ridge$fit,
    X_new = Xc_test,
    lambda = combined_ridge$best_lambda
)
combined_rapm_test_rmse = rmse(y_test, combined_test_pred)
cat("Combined RAPM test RMSE:", combined_rapm_test_rmse, "\n")

## --- Coefficient plot: raw PM vs. APM vs. RAPM (per 100 possessions) ----
shrinkage_df = tibble(
    player = player_labels,
    raw_pm = raw_pm_combined * 100,
    apm = apm_combined_coef * 100,
    rapm = combined_rapm_coef * 100
) |>
    filter(player %in% selected_players) |>
    pivot_longer(c(raw_pm, apm, rapm), names_to = "model", values_to = "estimate") |>
    mutate(
        model = factor(model, levels = c("raw_pm", "apm", "rapm"),
                        labels = c("Raw +/-", "APM", "RAPM"))
    )

player_order = shrinkage_df |>
    filter(model == "RAPM") |>
    arrange(estimate) |>
    pull(player)

shrinkage_df = shrinkage_df |> mutate(player = factor(player, levels = player_order))

print(
    ggplot(shrinkage_df, aes(x = estimate, y = player, color = model)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
        geom_point(size = 2.5) +
        labs(
            title = "Combined model: raw +/- vs. APM vs. RAPM",
            subtitle = "Points per 100 possessions, sorted by RAPM",
            x = "Estimate (pts / 100 poss)", y = NULL, color = "Model"
        ) +
        theme_minimal()
)

# Players whose estimates move the most under ridge shrinkage
shrinkage_movement = tibble(
    player = player_labels,
    possessions = player_possessions,
    raw_pm = raw_pm_combined * 100,
    apm = apm_combined_coef * 100,
    rapm = combined_rapm_coef * 100,
    apm_to_rapm_shift = abs(apm - rapm)
) |>
    arrange(desc(apm_to_rapm_shift))

cat("\nPlayers whose APM estimate moves most after ridge shrinkage:\n")
print(head(shrinkage_movement, 10))
cat(
    "These tend to be players with relatively few possessions and/or who",
    "share almost all of their floor time with the same teammates,",
    "so OLS cannot separate their individual effect from those teammates;",
    "ridge shrinks the resulting extreme, high-variance estimates toward zero.\n"
)

#############################
### OFFENSE/DEFENSE MODEL ###
#############################

split_ridge = fit_ridge_path(
    X_train = Xs_train,
    y_train_centered = y_train_centered,
    X_valid = Xs_valid,
    y_valid_centered = y_valid_centered,
    lambdas = lambdas
)
cat("\nSplit RAPM best lambda:", split_ridge$best_lambda, "\n")
cat("Split RAPM best validation RMSE:", split_ridge$best_valid_rmse, "\n")

split_test_pred = predict_centered_model(
    split_ridge$fit,
    X_new = Xs_test,
    lambda = split_ridge$best_lambda
)
split_rapm_test_rmse = rmse(y_test, split_test_pred)
cat("Split RAPM test RMSE:", split_rapm_test_rmse, "\n")

split_rapm_coef = as.numeric(coef(split_ridge$fit, s = split_ridge$best_lambda))[-1]
off_coef = split_rapm_coef[seq_along(player_labels)]
def_coef = split_rapm_coef[length(player_labels) + seq_along(player_labels)]
names(off_coef) = player_labels
names(def_coef) = player_labels

## --- Offense/defense coefficient plot -----------------------------------
split_df = tibble(
    player = player_labels,
    Offense = off_coef * 100,
    Defense = def_coef * 100
) |>
    filter(player %in% selected_players) |>
    pivot_longer(c(Offense, Defense), names_to = "side", values_to = "estimate")

split_order = split_df |>
    group_by(player) |>
    summarize(total = sum(estimate)) |>
    arrange(total) |>
    pull(player)

split_df = split_df |> mutate(player = factor(player, levels = split_order))

print(
    ggplot(split_df, aes(x = estimate, y = player, color = side)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
        geom_point(size = 2.5) +
        labs(
            title = "Offense/defense RAPM estimates",
            subtitle = "Points per 100 possessions",
            x = "Estimate (pts / 100 poss)", y = NULL, color = NULL
        ) +
        theme_minimal()
)

# Identify a player whose combined estimate hides an offense/defense split
od_gap = tibble(
    player = player_labels,
    combined_rapm = combined_rapm_coef * 100,
    off = off_coef * 100,
    def = def_coef * 100
) |>
    mutate(off_def_gap = abs(off - def)) |>
    arrange(desc(off_def_gap))

cat("\nPlayers with the largest offense/defense gap (hidden by the combined model):\n")
print(head(od_gap, 10))

########################
### COMPARISON NOTES ###
########################

mean_only_test_pred = rep(train_mean, length(idx_test))
mean_only_test_rmse = rmse(y_test, mean_only_test_pred)

raw_pm_test_pred = train_mean + as.numeric(Xc_test %*% raw_pm_combined)
raw_pm_test_rmse = rmse(y_test, raw_pm_test_pred)

apm_test_pred = predict_centered_model(apm_combined_fit, Xc_test, lambda = 0)
apm_test_rmse = rmse(y_test, apm_test_pred)

test_results = tibble(
    model = c("Mean-only", "Raw plus-minus", "Combined APM", "Combined RAPM", "Split RAPM"),
    test_rmse = c(
        mean_only_test_rmse,
        raw_pm_test_rmse,
        apm_test_rmse,
        combined_rapm_test_rmse,
        split_rapm_test_rmse
    )
) |>
    arrange(test_rmse)

cat("\n=== Final test RMSE comparison ===\n")
print(test_results)

cat(
    "\nThe split offense/defense RAPM model and the combined RAPM model both",
    "regularize away the wild, unstable estimates of raw plus-minus and",
    "unregularized APM, so they achieve the lowest test RMSE. The split model",
    "reveals that a player's overall (combined) impact can mask very different",
    "offensive and defensive contributions -- e.g. the players atop the",
    "offense/defense gap table above look comparable in the combined model but",
    "are clearly stronger on one end of the floor than the other once the",
    "effects are separated. Whether this extra flexibility helps prediction",
    "depends on how close split_rapm_test_rmse is to combined_rapm_test_rmse:",
    "if they're nearly identical, the added offense/defense split mainly aids",
    "interpretation rather than predictive accuracy; if split is meaningfully",
    "lower, the extra parameters are earning their keep.\n"
)
