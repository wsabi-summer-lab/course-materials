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

# Mean-only baseline
mean_only_valid_rmse = rmse(y_valid, train_mean)
mean_only_valid_rmse

# Raw plus-minus (points per 100 possessions) from training possessions
raw_pm = as.numeric(as.matrix(crossprod(Xc_train, y_train))) / player_possessions
raw_pm_per100 = raw_pm * 100

# Unregularized combined APM
combined_apm_fit = glmnet(
  x = Xc_train,
  y = y_train_centered,
  alpha = 0,
  family = "gaussian",
  lambda = 0,
  intercept = FALSE,
  standardize = FALSE
)
apm_coefs = as.numeric(coef(combined_apm_fit))[-1]
apm_per100 = apm_coefs * 100

# Fit combined ridge models and choose lambda using validation RMSE.
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
plot(combined_valid_curve$lambda, combined_valid_curve$valid_rmse, log = "x")

combined_test_pred = predict_centered_model(
  combined_ridge$fit,
  X_new = Xc_test,
  lambda = combined_ridge$best_lambda
)
combined_rapm_test_rmse = rmse(y_test, combined_test_pred)
combined_rapm_test_rmse

rapm_coefs = as.numeric(coef(combined_ridge$fit, s = combined_ridge$best_lambda))[-1]
rapm_per100 = rapm_coefs * 100

# Selected-player coefficient plot: raw P/M vs APM vs RAPM, pts/100 poss
combined_coef_table = tibble(
  player = player_labels,
  `Raw +/-` = raw_pm_per100,
  APM = apm_per100,
  RAPM = rapm_per100
) |>
  filter(player %in% selected_players) |>
  pivot_longer(-player, names_to = "method", values_to = "value")

ggplot(combined_coef_table, aes(x = player, y = value, fill = method)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = NULL, y = "Points per 100 possessions", fill = NULL,
       title = "Raw +/- vs APM vs RAPM")

#############################
### OFFENSE/DEFENSE MODEL ###
#############################

# Fit split ridge models and choose lambda using validation RMSE.
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
split_rapm_test_rmse = rmse(y_test, split_test_pred)
split_rapm_test_rmse

split_coefs = as.numeric(coef(split_ridge$fit, s = split_ridge$best_lambda))[-1]
off_per100 = split_coefs[seq_along(player_tokens)] * 100
def_per100 = split_coefs[length(player_tokens) + seq_along(player_tokens)] * 100

# Selected-player coefficient plot: offense vs defense RAPM, pts/100 poss
split_coef_table = tibble(
  player = player_labels,
  Offense = off_per100,
  Defense = def_per100
) |>
  filter(player %in% selected_players) |>
  pivot_longer(-player, names_to = "side", values_to = "value")

ggplot(split_coef_table, aes(x = player, y = value, fill = side)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = NULL, y = "Points per 100 possessions", fill = NULL,
       title = "Offense/Defense RAPM")

########################
### HORSESHOE MODEL ###
########################

# install.packages("Mhorseshoe")
# The original "horseshoe" package was archived by CRAN on 2025-10-10, so
# this uses Mhorseshoe instead, which implements the same horseshoe prior
# via the scalable algorithm of Johndrow et al. (2020) and is built to
# handle larger n/p than the old package.
library(Mhorseshoe)

# The sampler needs a dense design; this is the same combined matrix
# used for the ridge/APM fits above.
Xc_train_dense = as.matrix(Xc_train)
Xc_valid_dense = as.matrix(Xc_valid)
Xc_test_dense = as.matrix(Xc_test)

hs_fit = approx_horseshoe(
  y = y_train_centered,
  X = Xc_train_dense,
  burn = 200,
  iter = 500,
  auto.threshold = TRUE
)

hs_coefs = as.numeric(hs_fit$BetaHat)
hs_per100 = hs_coefs * 100

hs_valid_pred = train_mean + as.numeric(Xc_valid_dense %*% hs_coefs)
hs_valid_rmse = rmse(y_valid, hs_valid_pred)
hs_valid_rmse

hs_test_pred = train_mean + as.numeric(Xc_test_dense %*% hs_coefs)
hs_test_rmse = rmse(y_test, hs_test_pred)
hs_test_rmse

# Add the horseshoe estimates to the combined coefficient plot
horseshoe_coef_table = tibble(
  player = player_labels,
  `Raw +/-` = raw_pm_per100,
  APM = apm_per100,
  RAPM = rapm_per100,
  Horseshoe = hs_per100
) |>
  filter(player %in% selected_players) |>
  pivot_longer(-player, names_to = "method", values_to = "value")

ggplot(horseshoe_coef_table, aes(x = player, y = value, fill = method)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = NULL, y = "Points per 100 possessions", fill = NULL,
       title = "Raw +/- vs APM vs RAPM vs Horseshoe")

########################
### COMPARISON NOTES ###
########################

# Compare test RMSE for the combined and split RAPM models, plus horseshoe.
tibble(
  model = c("Combined ridge RAPM", "Split ridge RAPM", "Combined horseshoe RAPM"),
  test_rmse = c(combined_rapm_test_rmse, split_rapm_test_rmse, hs_test_rmse)
)

# The split model reveals offense/defense tradeoffs that the combined model
# hides: a player can be a strong offensive, weak defensive player (or vice
# versa) and the combined model will blend the two into one middling
# plus-minus number, while the split model reports them separately.
#
# The horseshoe prior is a different way of regularizing than ridge: ridge
# shrinks every player's coefficient toward zero by roughly the same
# proportion, while the horseshoe prior's heavy tails let high-possession,
# high-signal players keep estimates close to their unregularized value
# while shrinking low-possession, low-signal players much harder toward
# zero. RAPM and horseshoe RAPM should therefore agree closely for
# high-minute players and diverge most for bench players with few
# possessions.