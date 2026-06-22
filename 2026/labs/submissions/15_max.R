#######################
### Authors: RB, JP ###
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

nba_lineups = readRDS("../data/15_nba-lineups.rds") |>
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

#Set each predicted value to the average over all train values. 
mean_pred = rep(train_mean, length(y_test))

# TODO: Compute raw plus-minus using training possessions.

raw_pm = as.numeric(t(Xc_train) %*% y_train_centered) / player_possessions

# TODO: Fit an unregularized combined APM model on the training set.
apm_fit = glmnet(
  x = Xc_train,
  y = y_train,
  alpha = 0,
  lambda = 0,
  intercept = TRUE,
  standardize = FALSE
)
# If you use y_train_centered, set intercept = FALSE and add train_mean back
# when making validation or test predictions.



# TODO: Fit combined ridge models and choose lambda using validation RMSE.
combined_ridge = fit_ridge_path(
  X_train = Xc_train, 
  y_train_centered = y_train_centered, 
  X_valid = Xc_valid, 
  y_valid_centered = y_valid_centered, 
  lambdas = lambdas)

combined_ridge$best_lambda
combined_ridge$best_valid_rmse

combined_valid_curve = combined_ridge$validation
plot(combined_valid_curve$lambda, combined_valid_curve$valid_rmse, log = "x")

combined_test_pred = predict_centered_model(
  combined_ridge$fit,
  X_new = Xc_test,
  lambda = combined_ridge$best_lambda)

rmse(y_test, combined_test_pred)


names(raw_pm) = colnames(Xc_train)

apm_coef = as.numeric(coef(apm_fit))[-1]
names(apm_coef) = colnames(Xc_train)

rapm_coef = as.numeric(coef(combined_ridge$fit, s = combined_ridge$best_lambda))[-1]
names(rapm_coef) = colnames(Xc_train)

plot_df = tibble(
  player = selected_players,
  `Raw P/M` = 100 * raw_pm[selected_players],
  `APM`     = 100 * apm_coef[selected_players],
  `RAPM`    = 100 * rapm_coef[selected_players]
) |>
  pivot_longer(-player, names_to = "method", values_to = "estimate")

rapm_order = plot_df |>
  filter(method == "RAPM") |>
  arrange(estimate) |>
  pull(player)

plot_df = plot_df |>
  mutate(player = factor(player, levels = rapm_order))

ggplot(plot_df, aes(x = estimate, y = player, color = method)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    x = "Estimate (points per 100 possessions)",
    y = NULL, color = NULL,
    title = "Combined model: Raw P/M vs APM vs RAPM"
  ) +
  theme_minimal()

movement = tibble(
  player = selected_players,
  raw   = 100 * raw_pm[selected_players],
  rapm  = 100 * rapm_coef[selected_players],
  shift = abs(100 * raw_pm[selected_players] - 100 * rapm_coef[selected_players]),
  possessions = player_possessions[match(selected_players, colnames(Xc_train))]
) |>
  arrange(desc(shift))
print(movement)
#############################
### OFFENSE/DEFENSE MODEL ###
#############################

# TODO: Fit split ridge models and choose lambda using validation RMSE.
split_ridge = fit_ridge_path(
    X_train = Xs_train,
    y_train_centered = y_train_centered,
    X_valid = Xs_valid,
    y_valid_centered = y_valid_centered,
    lambdas = lambdas
)
split_ridge$best_lambda
split_ridge$best_valid_rmse

split_test_pred = predict_centered_model(
    split_ridge$fit,
    X_new = Xs_test,
    lambda = split_ridge$best_lambda
)
rmse(y_test, split_test_pred)

# TODO: Make a selected-player coefficient plot showing offense and defense
#       estimates separately, in points per 100 possessions.

# --- Offense/defense coefficient plot -------------------------------------

# Pull the chosen split model's coefficients (length 2m), drop intercept slot
split_coef = as.numeric(coef(split_ridge$fit, s = split_ridge$best_lambda))[-1]
names(split_coef) = colnames(X_split)        # "<player>_off" and "<player>_def"

# Selected players, offense and defense, in points per 100 possessions
split_df = tibble(
  player  = selected_players,
  Offense = 100 * split_coef[paste0(selected_players, "_off")],
  Defense = 100 * split_coef[paste0(selected_players, "_def")]
) |>
  pivot_longer(-player, names_to = "side", values_to = "estimate")

# Order the y-axis by offensive estimate (swap to Defense or combined if you prefer)
off_order = split_df |>
  filter(side == "Offense") |>
  arrange(estimate) |>
  pull(player)
split_df = split_df |>
  mutate(player = factor(player, levels = off_order))

# Plot: offense vs defense per player, with a zero line
ggplot(split_df, aes(x = estimate, y = player, color = side)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(aes(group = player), color = "grey80") +
  geom_point(size = 3) +
  labs(
    x = "Points per 100 possessions",
    y = NULL, color = NULL,
    title = "Offense/Defense RAPM (combined split model)"
  ) +
  theme_minimal()
########################
### COMPARISON NOTES ###
########################

#Evaluations:
#RMSE mean only: 1.192
mean_only_rmse = rmse(y_test, mean_pred)
mean_only_rmse
#RMSE raw_pm: 1.206
raw_pm[!is.finite(raw_pm)] = 0
raw_pm_test_pred = train_mean + as.numeric(Xc_test %*% raw_pm)
raw_pm_test_rmse = rmse(y_test, raw_pm_test_pred)
raw_pm_test_rmse
#RMSE APM: 1.204
apm_test_pred = as.numeric(predict(apm_fit, newx = Xc_test))
apm_test_rmse = rmse(y_test, apm_test_pred)
apm_test_rmse
#RMSE RAPM combined: 1.191
rmse(y_test, combined_test_pred)
#RMSE RAPM split: 1.191
rmse(y_test, split_test_pred)