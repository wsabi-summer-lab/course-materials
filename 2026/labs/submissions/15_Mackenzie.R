#############
### SETUP ###
#############

library(ggplot2)
library(glmnet)
library(Matrix)
library(tidyverse)

set.seed(15)

####################
### NBA LINEUPS ###
####################

nba_lineups = readRDS("/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/15_nba-lineups.rds") |>
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

duplicate_names = names(which(table(player_labels) > 1))
player_labels = ifelse(player_labels %in% duplicate_names, player_tokens, player_labels)
player_index = setNames(seq_along(player_tokens), player_tokens)

#######################
### DESIGN MATRICES ###
#######################

n = nrow(nba_lineups)
off_tokens = unlist(off_lineups, use.names = FALSE)
def_tokens = unlist(def_lineups, use.names = FALSE)

X_combined = sparseMatrix(
  i = c(rep(seq_len(n), lengths(off_lineups)),
        rep(seq_len(n), lengths(def_lineups))),
  j = unname(player_index[c(off_tokens, def_tokens)]),
  x = c(rep(1, length(off_tokens)), rep(-1, length(def_tokens))),
  dims = c(n, length(player_tokens))
)
colnames(X_combined) = player_labels

X_split = sparseMatrix(
  i = c(rep(seq_len(n), lengths(off_lineups)),
        rep(seq_len(n), lengths(def_lineups))),
  j = c(unname(player_index[off_tokens]),
        length(player_tokens) + unname(player_index[def_tokens])),
  x = c(rep(1, length(off_tokens)), rep(-1, length(def_tokens))),
  dims = c(n, 2 * length(player_tokens))
)
colnames(X_split) = c(paste0(player_labels, "_off"),
                      paste0(player_labels, "_def"))

selected_players = c(
  "Shai Gilgeous-Alexander","Nikola Jokic","Anthony Edwards","Jalen Brunson",
  "Luka Doncic","Victor Wembanyama","Stephen Curry","Domantas Sabonis",
  "Mikal Bridges","Klay Thompson","Austin Reaves","Kyle Kuzma"
)

####################
### DATA SPLITS ####
####################

games = sort(unique(nba_lineups$game_id))
n_games = length(games)

train_games = games[seq_len(floor(0.70 * n_games))]
valid_games = games[(floor(0.70 * n_games) + 1):floor(0.85 * n_games)]
test_games  = games[(floor(0.85 * n_games) + 1):n_games]

idx_train = which(nba_lineups$game_id %in% train_games)
idx_valid = which(nba_lineups$game_id %in% valid_games)
idx_test  = which(nba_lineups$game_id %in% test_games)

y = nba_lineups$pts_poss
train_mean = mean(y[idx_train])
y_centered = y - train_mean

lambdas = 10^seq(-4, 2.5, length.out = 50)

Xc_train = X_combined[idx_train, ]
Xc_valid = X_combined[idx_valid, ]
Xc_test  = X_combined[idx_test, ]

Xs_train = X_split[idx_train, ]
Xs_valid = X_split[idx_valid, ]
Xs_test  = X_split[idx_test, ]

y_train = y[idx_train]
y_valid = y[idx_valid]
y_test  = y[idx_test]

y_train_centered = y_centered[idx_train]
y_valid_centered = y_centered[idx_valid]
y_test_centered  = y_centered[idx_test]

player_possessions = as.numeric(colSums(abs(Xc_train)))

rmse = function(truth, pred) sqrt(mean((truth - pred)^2))

fit_ridge_path = function(X_train, y_train_centered, X_valid, y_valid_centered, lambdas) {
  ridge_fit = glmnet(
    x = X_train, y = y_train_centered,
    alpha = 0, family = "gaussian",
    lambda = lambdas, intercept = FALSE, standardize = FALSE
  )
  valid_pred = predict(ridge_fit, newx = X_valid)
  valid_rmse = sqrt(colMeans((as.matrix(valid_pred) - y_valid_centered)^2))
  list(
    fit = ridge_fit,
    validation = tibble(lambda = ridge_fit$lambda, valid_rmse = as.numeric(valid_rmse)),
    best_lambda = ridge_fit$lambda[which.min(valid_rmse)],
    best_valid_rmse = min(valid_rmse)
  )
}

predict_centered_model = function(fit, X_new, lambda = NULL) {
  if (is.null(lambda)) train_mean + as.numeric(predict(fit, newx = X_new))
  else train_mean + as.numeric(predict(fit, newx = X_new, s = lambda))
}

######################
### COMBINED MODEL ###
######################

valid_rmse_mean = rmse(y_valid, rep(train_mean, length(y_valid)))
test_rmse_mean  = rmse(y_test,  rep(train_mean, length(y_test)))

raw_pm_coef = colSums(Xc_train * y_train_centered) / player_possessions
valid_raw_pred = train_mean + as.numeric(Xc_valid %*% raw_pm_coef)
test_raw_pred  = train_mean + as.numeric(Xc_test  %*% raw_pm_coef)

valid_rmse_raw = rmse(y_valid, valid_raw_pred)
test_rmse_raw  = rmse(y_test,  test_raw_pred)

### APM via glmnet (lambda = 0)
apm_fit = glmnet(
  x = Xc_train,
  y = y_train_centered,
  alpha = 0,
  lambda = 0,
  intercept = FALSE,
  standardize = FALSE
)

apm_coef = as.numeric(coef(apm_fit))[-1]
names(apm_coef) = colnames(X_combined)

valid_apm_pred = train_mean + as.numeric(Xc_valid %*% apm_coef)
test_apm_pred  = train_mean + as.numeric(Xc_test  %*% apm_coef)

valid_rmse_apm = rmse(y_valid, valid_apm_pred)
test_rmse_apm  = rmse(y_test,  test_apm_pred)

### Ridge RAPM
combined_ridge = fit_ridge_path(
  X_train = Xc_train,
  y_train_centered = y_train_centered,
  X_valid = Xc_valid,
  y_valid_centered = y_valid_centered,
  lambdas = lambdas
)

combined_test_pred = predict_centered_model(
  combined_ridge$fit, X_new = Xc_test, lambda = combined_ridge$best_lambda
)
test_rmse_combined_ridge = rmse(y_test, combined_test_pred)

### RAPM coefficients (drop intercept)
coef_rapm_100 = 100 * as.numeric(coef(combined_ridge$fit, s = combined_ridge$best_lambda))[-1]

coef_raw_100  = 100 * raw_pm_coef
coef_apm_100  = 100 * apm_coef

coef_table_combined = tibble(
  player = colnames(X_combined),
  raw_pm = coef_raw_100,
  apm = coef_apm_100,
  rapm = coef_rapm_100
)

plot_players_combined = coef_table_combined |>
  filter(player %in% selected_players) |>
  pivot_longer(cols = c(raw_pm, apm, rapm),
               names_to = "model", values_to = "pm_100") |>
  group_by(player) |>
  mutate(order = unique(pm_100[model == "rapm"])) |>
  ungroup() |>
  arrange(desc(order)) |>
  mutate(player = factor(player, levels = unique(player)))

ggplot(plot_players_combined,
       aes(x = player, y = pm_100, color = model, group = model)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() + geom_line() +
  coord_flip() +
  labs(title = "Combined PM: Raw vs APM vs RAPM (per 100 poss)",
       x = "Player", y = "Points per 100 possessions")

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

split_test_pred = predict_centered_model(
  split_ridge$fit, X_new = Xs_test, lambda = split_ridge$best_lambda
)
test_rmse_split_ridge = rmse(y_test, split_test_pred)

split_coef = as.numeric(coef(split_ridge$fit, s = split_ridge$best_lambda))
names(split_coef) = colnames(X_split)

off_coef_100 = 100 * split_coef[grep("_off$", names(split_coef))]
def_coef_100 = 100 * split_coef[grep("_def$", names(split_coef))]

split_coef_table = tibble(
  player = player_labels,
  off_100 = off_coef_100,
  def_100 = def_coef_100
)

### FIXED SORTING — compute total BEFORE pivot_longer
plot_players_split = split_coef_table |>
  filter(player %in% selected_players) |>
  mutate(total = off_100 + def_100) |>
  arrange(desc(total)) |>
  pivot_longer(cols = c(off_100, def_100),
               names_to = "side", values_to = "pm_100") |>
  mutate(side = recode(side, off_100 = "Offense", def_100 = "Defense")) |>
  mutate(player = factor(player, levels = unique(player)))

ggplot(plot_players_split,
       aes(x = player, y = pm_100, fill = side)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Offense vs Defense RAPM (per 100 poss)",
       x = "Player", y = "Points per 100 possessions")

########################
### FINAL COMPARISON ###
########################

cat("\n===== FINAL TEST RMSE SUMMARY =====\n")
cat("Mean-only:", test_rmse_mean, "\n")
cat("Raw PM:", test_rmse_raw, "\n")
cat("APM:", test_rmse_apm, "\n")
cat("Combined RAPM:", test_rmse_combined_ridge, "\n")
cat("Split RAPM:", test_rmse_split_ridge, "\n")
