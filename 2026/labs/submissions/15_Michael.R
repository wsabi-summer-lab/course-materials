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

#Task 1


# --- Mean-only baseline ---
mean_only_pred_valid = rep(train_mean, length(idx_valid))
cat("Mean-only validation RMSE:", rmse(y_valid, mean_only_pred_valid), "\n")

# --- Raw Plus-Minus ---
pm_num = as.numeric(t(Xc_train) %*% y_train_centered)
pm_den = player_possessions
raw_pm = pm_num / pm_den

raw_pm_pred_valid = as.numeric(Xc_valid %*% raw_pm) + train_mean
cat("Raw P/M validation RMSE:", rmse(y_valid, raw_pm_pred_valid), "\n")

# --- Unregularized APM ---
apm_fit = glmnet(
  x           = Xc_train,
  y           = y_train_centered,
  alpha       = 0,
  lambda      = 1e-10,
  family      = "gaussian",
  intercept   = FALSE,
  standardize = FALSE
)
apm_pred_valid = train_mean + as.numeric(predict(apm_fit, newx = Xc_valid))
cat("APM validation RMSE:", rmse(y_valid, apm_pred_valid), "\n")

# --- Ridge RAPM over lambda grid ---
combined_ridge = fit_ridge_path(
  X_train          = Xc_train,
  y_train_centered = y_train_centered,
  X_valid          = Xc_valid,
  y_valid_centered = y_valid_centered,
  lambdas          = lambdas
)
cat("Best lambda (combined):", combined_ridge$best_lambda, "\n")
cat("Best RAPM validation RMSE:", combined_ridge$best_valid_rmse, "\n")

# Validation curve
combined_valid_curve = combined_ridge$validation
plot(combined_valid_curve$lambda, combined_valid_curve$valid_rmse,
     log  = "x", type = "b", pch = 16,
     xlab = "Lambda", ylab = "Validation RMSE",
     main = "Combined Ridge RAPM: Validation RMSE vs Lambda")
abline(v = combined_ridge$best_lambda, col = "red", lty = 2)

####################
### TASK 2: VISUALIZE SHRINKAGE ###
####################

# Extract coefficients at best lambda (per 100 possessions)
rapm_coefs = as.numeric(coef(combined_ridge$fit, s = combined_ridge$best_lambda))[-1]
apm_coefs  = as.numeric(coef(apm_fit))[-1]

raw_pm_100  = raw_pm * 100
apm_100     = apm_coefs * 100
rapm_100    = rapm_coefs * 100

# Build plot dataframe for selected players
plot_df = player_lookup |>
  filter(player %in% selected_players) |>
  mutate(
    raw_pm = raw_pm_100[combined_col],
    apm    = apm_100[combined_col],
    rapm   = rapm_100[combined_col]
  ) |>
  arrange(rapm) |>
  mutate(player = factor(player, levels = player))

plot_long = plot_df |>
  pivot_longer(cols = c(raw_pm, apm, rapm),
               names_to  = "model",
               values_to = "estimate") |>
  mutate(model = factor(model,
                        levels = c("raw_pm", "apm", "rapm"),
                        labels = c("Raw P/M", "APM", "RAPM")))

ggplot(plot_long, aes(x = estimate, y = player, color = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, alpha = 0.85) +
  scale_color_manual(values = c("Raw P/M" = "#888888", "APM" = "#E07B39", "RAPM" = "#2C7BB6")) +
  labs(
    title    = "Combined Plus-Minus Estimates: Raw P/M vs APM vs RAPM",
    subtitle = "Sorted by RAPM | Units: points per 100 possessions",
    x        = "Estimate (pts / 100 possessions)",
    y        = NULL,
    color    = "Model",
    shape    = "Model"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# --- Shrinkage magnitude + possession counts ---
shrinkage_df = plot_df |>
  mutate(apm_to_rapm = abs(apm - rapm)) |>
  arrange(desc(apm_to_rapm)) |>
  select(player, raw_pm, apm, rapm, apm_to_rapm)

print(shrinkage_df)

player_lookup |>
  filter(player %in% selected_players) |>
  mutate(possessions = player_possessions[combined_col]) |>
  arrange(possessions) |>
  select(player, possessions) |>
  print()

#The players estimate move most under ridge regulation are players like Wemby and Jokic, which can be due to colinearity and they are typically in the game and the time they are not in the game is such a small sample realitive whcih adds a lot of variance or false credit.

#############################
### TASK 3: OFFENSE/DEFENSE RAPM ###
#############################

# --- Fit split ridge models ---
split_ridge = fit_ridge_path(
  X_train          = Xs_train,
  y_train_centered = y_train_centered,
  X_valid          = Xs_valid,
  y_valid_centered = y_valid_centered,
  lambdas          = lambdas
)
cat("Best lambda (split):", split_ridge$best_lambda, "\n")
cat("Best split RAPM validation RMSE:", split_ridge$best_valid_rmse, "\n")

# --- Extract off/def coefficients at best lambda ---
split_coefs = as.numeric(coef(split_ridge$fit, s = split_ridge$best_lambda))[-1]

n_players = length(player_tokens)
off_coefs_100 = split_coefs[1:n_players] * 100
def_coefs_100 = split_coefs[(n_players + 1):(2 * n_players)] * 100

# --- Build plot dataframe using direct name matching ---
split_plot_df = player_lookup |>
  filter(player %in% selected_players) |>
  mutate(
    offense = off_coefs_100[match(player_token, player_tokens)],
    defense = def_coefs_100[match(player_token, player_tokens)],
    total   = offense + defense
  ) |>
  arrange(total) |>
  mutate(player = factor(player, levels = player))

print(split_plot_df |> select(player, offense, defense, total))

split_long = split_plot_df |>
  pivot_longer(cols = c(offense, defense),
               names_to  = "side",
               values_to = "estimate") |>
  mutate(side = factor(side, levels = c("offense", "defense"),
                       labels = c("Offense", "Defense")))

ggplot(split_long, aes(x = estimate, y = player, color = side, shape = side)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, alpha = 0.85) +
  scale_color_manual(values = c("Offense" = "#E07B39", "Defense" = "#2C7BB6")) +
  scale_shape_manual(values = c("Offense" = 16, "Defense" = 17)) +
  labs(
    title    = "Split RAPM: Offensive and Defensive Estimates",
    subtitle = "Sorted by total impact | Units: points per 100 possessions",
    x        = "Estimate (pts / 100 possessions)",
    y        = NULL,
    color    = "Side",
    shape    = "Side"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

#Stephen Curry's difference in offensive and defensive is not captured by his RAPM, but is made clearer for his split RAPM.

####################
### TASK 4: FINAL TEST EVALUATION ###
####################

# --- Mean-only ---
mean_only_test_pred = rep(train_mean, length(idx_test))
cat("Mean-only test RMSE:      ", rmse(y_test, mean_only_test_pred), "\n")

# --- Raw Plus-Minus ---
raw_pm_test_pred = as.numeric(Xc_test %*% raw_pm) + train_mean
cat("Raw P/M test RMSE:        ", rmse(y_test, raw_pm_test_pred), "\n")

# --- APM ---
apm_test_pred = train_mean + as.numeric(predict(apm_fit, newx = Xc_test))
cat("APM test RMSE:            ", rmse(y_test, apm_test_pred), "\n")

# --- Combined RAPM ---
combined_test_pred = predict_centered_model(
  combined_ridge$fit,
  X_new  = Xc_test,
  lambda = combined_ridge$best_lambda
)
cat("Combined RAPM test RMSE:  ", rmse(y_test, combined_test_pred), "\n")

# --- Split RAPM ---
split_test_pred = predict_centered_model(
  split_ridge$fit,
  X_new  = Xs_test,
  lambda = split_ridge$best_lambda
)
cat("Split RAPM test RMSE:     ", rmse(y_test, split_test_pred), "\n")


#Split RAPM is the best or tied for best with combined RAPM, split RAPM and combine RAPM each achieve the lowest test RMSE of 1.1907. The split model reveals a deeper diff between the players, Wemby great at defense eh on offense, JOkic is fine at defense great on offense which is a lot more insighful than just their overall RAMP
