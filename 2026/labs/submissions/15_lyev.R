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

# TODO: Report mean-only validation RMSE.

baseline_valid_pred <- rep(train_mean, length(idx_valid))
baseline_valid_rmse <- rmse(y_valid, baseline_valid_pred)   #1.19



# TODO: Compute raw plus-minus using training possessions.

coef_raw_pm <- as.numeric(t(Xc_train) %*% y_train_centered / player_possessions)



# TODO: Fit an unregularized combined APM model on the training set.
# If you use y_train_centered, set intercept = FALSE and add train_mean back
# when making validation or test predictions.

apm_fit <- glmnet(
  x = Xc_train,
  y = y_train_centered,
  alpha = 0,
  family = "gaussian",
  lambda = 0,            # no regularization
  intercept = FALSE,
  standardize = FALSE
)

# Validation predictions (add train_mean back)
apm_valid_pred <- train_mean + as.numeric(
  predict(apm_fit, newx = Xc_valid, s = 0)
)
apm_valid_rmse <- rmse(y_valid, apm_valid_pred) #1.197

# Test predictions (also add train_mean back)
apm_test_pred <- train_mean + as.numeric(
  predict(apm_fit, newx = Xc_test, s = 0)
)
apm_test_rmse <- rmse(y_test, apm_test_pred) # 1.203



# TODO: Fit combined ridge models and choose lambda using validation RMSE.
combined_ridge = fit_ridge_path(
    X_train = Xc_train,
    y_train_centered = y_train_centered,
    X_valid = Xc_valid,
    y_valid_centered = y_valid_centered,
    lambdas = lambdas
)
combined_ridge$best_lambda # 0.0244
combined_ridge$best_valid_rmse # 1.189

combined_valid_curve = combined_ridge$validation
plot(combined_valid_curve$lambda, combined_valid_curve$valid_rmse, log = "x")
# TODO: Evaluate the selected combined RAPM model on the test set.

combined_test_pred = predict_centered_model(
    combined_ridge$fit,
    X_new = Xc_test,
    lambda = combined_ridge$best_lambda
)
rmse(y_test, combined_test_pred) #1.1907



# TODO: Make a selected-player coefficient plot comparing P/M, APM, and RAPM
#       in points per 100 possessions.

## Raw plus-minus (already computed earlier)
coef_raw_pm_100 <- 100 * coef_raw_pm
names(coef_raw_pm_100) <- player_labels

## Unregularized APM
coef_apm <- as.numeric(coef(apm_fit, s = 0))[-1]   # drop intercept
names(coef_apm) <- player_labels
coef_apm_100 <- 100 * coef_apm

## Ridge RAPM (best lambda)
coef_ridge <- as.numeric(coef(combined_ridge$fit, s = combined_ridge$best_lambda))[-1]
names(coef_ridge) <- player_labels
coef_ridge_100 <- 100 * coef_ridge

# --- Build tidy coefficient table ---

coef_tbl <- tibble(
  player = rep(player_labels, 3),
  model = rep(c("Raw P/M", "APM", "RAPM"), each = length(player_labels)),
  value = c(coef_raw_pm_100, coef_apm_100, coef_ridge_100)
) |>
  filter(player %in% selected_players)

# --- Plot ---

ggplot(coef_tbl, aes(x = player, y = value, fill = model)) +
  geom_col(position = position_dodge(width = 0.75)) +
  coord_flip() +
  labs(
    title = "Player Impact Estimates (Points per 100 Possessions)",
    subtitle = "Comparing Raw Plus/Minus, Unregularized APM, and Ridge RAPM",
    x = "",
    y = "Impact (per 100 possessions)",
    fill = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 12)
  )

#Jokic and Wemby's RAPM seems to be right in the middle of their two estimates
#Jokic's raw p/m is good because he plays for a good team in a good lineup
#but his APM is likely bad because a small sample size interaction with another player greatly affects the result
#Wemby's raw p/m is bad because this season he played on a bad team
#and his APM is inflatedly good because he likely had favorable small sample size interactions

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
split_ridge$best_lambda # 0.0244
split_ridge$best_valid_rmse # 1.188


# TODO: Evaluate the selected offense/defense RAPM model on the test set.

split_test_pred = predict_centered_model(
    split_ridge$fit,
    X_new = Xs_test,
    lambda = split_ridge$best_lambda
)
rmse(y_test, split_test_pred) # 1.191

# TODO: Make a selected-player coefficient plot showing offense and defense
#       estimates separately, in points per 100 possessions.


# Extract coefficients at best lambda
coef_split <- as.numeric(coef(split_ridge$fit, s = split_ridge$best_lambda))[-1]
names(coef_split) <- colnames(X_split)

# Separate offense and defense
coef_off <- coef_split[player_lookup$off_col]
coef_def <- coef_split[player_lookup$def_col]

names(coef_off) <- player_labels
names(coef_def) <- player_labels

# Convert to per 100 possessions
coef_off_100 <- 100 * coef_off
coef_def_100 <- 100 * coef_def

# Build tidy table
coef_split_tbl <- tibble(
  player = rep(player_labels, 2),
  type = rep(c("Offense", "Defense"), each = length(player_labels)),
  value = c(coef_off_100, coef_def_100)
) |>
  filter(player %in% selected_players)

# Plot
ggplot(coef_split_tbl, aes(x = player, y = value, fill = type)) +
  geom_col(position = position_dodge(width = 0.75)) +
  coord_flip() +
  labs(
    title = "Offense and Defense RAPM (Points per 100 Possessions)",
    subtitle = paste0("λ = ", signif(split_ridge$best_lambda, 3)),
    x = "",
    y = "Impact (per 100 possessions)",
    fill = "Component"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 12)
  )

#In this year, Wemby's overall score hides a clear difference in Offense and Defense
#His defense is excellent, but offense below average



########################
### COMPARISON NOTES ###
########################

# TODO: Compare test RMSE for the combined and split RAPM models.


# 1. Mean-only baseline
test_pred_baseline <- rep(train_mean, length(idx_test))
test_rmse_baseline <- rmse(y_test, test_pred_baseline) # 1.192

# 2. Raw plus-minus
test_pred_raw_pm <- train_mean + as.numeric(Xc_test %*% coef_raw_pm)
test_rmse_raw_pm <- rmse(y_test, test_pred_raw_pm)

# 3. Combined APM (unregularized)
test_pred_apm <- train_mean + as.numeric(
  predict(apm_fit, newx = Xc_test, s = 0)
)
test_rmse_apm <- rmse(y_test, test_pred_apm) # 1.204

# 4. Combined RAPM (best lambda)
test_pred_ridge <- predict_centered_model(
  combined_ridge$fit,
  X_new = Xc_test,
  lambda = combined_ridge$best_lambda
)
test_rmse_ridge <- rmse(y_test, test_pred_ridge) # 1.19067

# 5. Split RAPM (offense + defense)
test_pred_split <- predict_centered_model(
  split_ridge$fit,
  X_new = Xs_test,
  lambda = split_ridge$best_lambda
)
test_rmse_split <- rmse(y_test, test_pred_split) # 1.1907


# TODO: Briefly explain what the split model reveals that the combined model hides.

#The split model reveals the different facets of players' performance, and a slight detriment to accuracy

