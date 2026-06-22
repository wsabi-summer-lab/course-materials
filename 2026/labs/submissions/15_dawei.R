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

nba_lineups = readRDS("C:/Users/sundw/Downloads/15_nba-lineups.rds") |>
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
    "LeBron James"
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

## --- Mean-only validation RMSE (baseline) ---
mean_only_valid_pred = rep(train_mean, length(idx_valid))
mean_only_valid_rmse = rmse(y_valid, mean_only_valid_pred)
cat("Mean-only validation RMSE:", round(mean_only_valid_rmse, 4), "\n")

## --- Raw plus-minus (training possessions only) ---
# crossprod(Xc_train, y_train) gives, for each player, the signed point total
# accumulated while they were on the floor (+ on offense, - on defense).
# Dividing by player_possessions converts this into a per-possession rate;
# multiplying by 100 expresses it in points per 100 possessions.
raw_pm_per_poss = as.numeric(crossprod(Xc_train, y_train)) / player_possessions
raw_pm_per100 = raw_pm_per_poss * 100
names(raw_pm_per100) = player_labels

## --- Unregularized combined APM model ---
combined_apm_fit = glmnet(
  x = Xc_train,
  y = y_train_centered,
  alpha = 0,
  family = "gaussian",
  lambda = 0,
  intercept = FALSE,
  standardize = FALSE
)
combined_apm_coef = as.numeric(combined_apm_fit$beta[, 1])

combined_apm_valid_pred = predict_centered_model(combined_apm_fit, X_new = Xc_valid, lambda = 0)
combined_apm_test_pred  = predict_centered_model(combined_apm_fit, X_new = Xc_test,  lambda = 0)
combined_apm_valid_rmse = rmse(y_valid, combined_apm_valid_pred)
combined_apm_test_rmse  = rmse(y_test,  combined_apm_test_pred)
cat("Combined APM (unregularized) validation RMSE:", round(combined_apm_valid_rmse, 4), "\n")
cat("Combined APM (unregularized) test RMSE:      ", round(combined_apm_test_rmse, 4), "\n")

## --- Combined ridge models (RAPM), choose lambda via validation RMSE ---
combined_ridge = fit_ridge_path(
  X_train = Xc_train,
  y_train_centered = y_train_centered,
  X_valid = Xc_valid,
  y_valid_centered = y_valid_centered,
  lambdas = lambdas
)
combined_ridge$best_lambda
combined_ridge$best_valid_rmse

combined_valid_curve = combined_ridge$validation
plot(combined_valid_curve$lambda, combined_valid_curve$valid_rmse, log = "x",
     xlab = "lambda", ylab = "Validation RMSE", main = "Combined RAPM validation curve")

## --- Evaluate selected combined RAPM model on test set ---
combined_test_pred = predict_centered_model(
  combined_ridge$fit,
  X_new = Xc_test,
  lambda = combined_ridge$best_lambda
)
combined_rapm_test_rmse = rmse(y_test, combined_test_pred)
cat("Combined RAPM test RMSE:", round(combined_rapm_test_rmse, 4), "\n")

combined_rapm_coef = as.numeric(coef(combined_ridge$fit, s = combined_ridge$best_lambda))[-1]
names(combined_rapm_coef) = player_labels

## --- Selected-player coefficient plot: raw P/M vs APM vs RAPM ---
combined_compare = player_lookup |>
  filter(player %in% selected_players) |>
  transmute(
    player,
    `Raw +/-` = raw_pm_per100[combined_col],
    APM  = combined_apm_coef[combined_col] * 100,
    RAPM = combined_rapm_coef[combined_col] * 100
  )

player_order = combined_compare$player[order(combined_compare$RAPM)]

combined_compare_long = combined_compare |>
  pivot_longer(-player, names_to = "method", values_to = "value") |>
  mutate(
    player = factor(player, levels = player_order),
    method = factor(method, levels = c("Raw +/-", "APM", "RAPM"))
  )

ggplot(combined_compare_long, aes(x = player, y = value, fill = method)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  coord_flip() +
  labs(
    title = "Combined model: Raw +/- vs. APM vs. RAPM",
    x = NULL,
    y = "Points per 100 possessions",
    fill = NULL
  ) +
  theme_minimal()

#############################
### OFFENSE/DEFENSE MODEL ###
#############################

## --- Split ridge models (RAPM), choose lambda via validation RMSE ---
split_ridge = fit_ridge_path(
  X_train = Xs_train,
  y_train_centered = y_train_centered,
  X_valid = Xs_valid,
  y_valid_centered = y_valid_centered,
  lambdas = lambdas
)
split_ridge$best_lambda
split_ridge$best_valid_rmse

split_valid_curve = split_ridge$validation
plot(split_valid_curve$lambda, split_valid_curve$valid_rmse, log = "x",
     xlab = "lambda", ylab = "Validation RMSE", main = "Split RAPM validation curve")

## --- Evaluate selected offense/defense RAPM model on test set ---
split_test_pred = predict_centered_model(
  split_ridge$fit,
  X_new = Xs_test,
  lambda = split_ridge$best_lambda
)
split_rapm_test_rmse = rmse(y_test, split_test_pred)
cat("Split RAPM test RMSE:", round(split_rapm_test_rmse, 4), "\n")

split_rapm_coef = as.numeric(coef(split_ridge$fit, s = split_ridge$best_lambda))[-1]

## --- Selected-player coefficient plot: offense vs defense ---
# Note: in X_split, defense columns are coded -1, so larger defensive
# coefficients mean BETTER defense (consistent with the comment in the
# design-matrix section) -- no sign flip needed here.
split_compare = player_lookup |>
  filter(player %in% selected_players) |>
  transmute(
    player,
    Offense = split_rapm_coef[off_col] * 100,
    Defense = split_rapm_coef[def_col] * 100
  )

player_order2 = split_compare$player[order(split_compare$Offense + split_compare$Defense)]

split_compare_long = split_compare |>
  pivot_longer(-player, names_to = "side", values_to = "value") |>
  mutate(
    player = factor(player, levels = player_order2),
    side = factor(side, levels = c("Offense", "Defense"))
  )

ggplot(split_compare_long, aes(x = player, y = value, fill = side)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  labs(
    title = "Offense/defense RAPM",
    x = NULL,
    y = "Points per 100 possessions",
    fill = NULL
  ) +
  theme_minimal()

########################
### COMPARISON NOTES ###
########################

cat("Combined RAPM test RMSE:", round(combined_rapm_test_rmse, 4), "\n")
cat("Split RAPM test RMSE:   ", round(split_rapm_test_rmse, 4), "\n")

# The split model allows us to see a player's contribution on both offense and defense, as opposed
# to a combined number
