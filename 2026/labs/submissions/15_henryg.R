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
# TODO: Compute raw plus-minus using training possessions.
# TODO: Fit an unregularized combined APM model on the training set.
# If you use y_train_centered, set intercept = FALSE and add train_mean back
# when making validation or test predictions.

# TODO: Fit combined ridge models and choose lambda using validation RMSE.
# --- Task 1: combined plus-minus models, compared by validation RMSE ---

# (1) Mean-only baseline: predict the training mean for every possession.
mean_valid_rmse = rmse(y_valid, train_mean)

# (2) Raw (unadjusted) plus-minus per player, from TRAINING possessions only:
#     per possession, raw_pm_j = mean pts while j is on offense (+1)
#                                - mean pts while j is on defense (-1).
off_ind = 1 * (Xc_train > 0)                        # 1 on offense possessions
def_ind = 1 * (Xc_train < 0)                        # 1 on defense possessions
off_n   = colSums(off_ind)
def_n   = colSums(def_ind)
off_pts = as.numeric(crossprod(off_ind, y_train))   # total pts while on offense
def_pts = as.numeric(crossprod(def_ind, y_train))   # total pts while on defense
raw_pm  = off_pts / off_n - def_pts / def_n
raw_pm[!is.finite(raw_pm)] = 0                      # players with no off OR no def poss
names(raw_pm) = colnames(X_combined)

#     As a predictor, sum each lineup's player effects: y_hat = mean + X %*% raw_pm.
#     This double-counts the credit shared among teammates -- the very flaw that
#     adjustment fixes -- so expect it to predict worse than the mean baseline.
raw_valid_pred = train_mean + as.numeric(Xc_valid %*% raw_pm)
raw_valid_rmse = rmse(y_valid, raw_valid_pred)

# (3) Unregularized combined APM = ridge with lambda = 0.
#     Each row has equal +1 and -1 entries, so X %*% 1 = 0: OLS is identified
#     only up to an additive constant on the coefficients, but predictions (and
#     therefore RMSE) are unaffected. intercept = FALSE because y is centered.
apm_fit = glmnet(
  x = Xc_train, y = y_train_centered, alpha = 0, family = "gaussian",
  lambda = 0, intercept = FALSE, standardize = FALSE
)
apm_valid_pred = predict_centered_model(apm_fit, X_new = Xc_valid)
apm_valid_rmse = rmse(y_valid, apm_valid_pred)

# (4) Ridge RAPM over the lambda grid is fit just below (combined_ridge).



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
# --- Task 1 reporting: validation RMSE at each lambda, and the chosen one ---
print(combined_ridge$validation, n = Inf)        # validation RMSE for every lambda

task1_validation = tibble(
  model      = c("mean-only", "raw plus-minus", "APM (lambda = 0)",
                 "RAPM ridge (best lambda)"),
  valid_rmse = c(mean_valid_rmse, raw_valid_rmse, apm_valid_rmse,
                 combined_ridge$best_valid_rmse)
) |> arrange(valid_rmse)
print(task1_validation)

cat(sprintf(
  "\nChosen lambda (minimizes validation RMSE): %.5g  ->  validation RMSE %.5f\n",
  combined_ridge$best_lambda, combined_ridge$best_valid_rmse))

combined_test_pred = predict_centered_model(
    combined_ridge$fit,
    X_new = Xc_test,
    lambda = combined_ridge$best_lambda
)
rmse(y_test, combined_test_pred)

# TODO: Evaluate the selected combined RAPM model on the test set.
# TODO: Make a selected-player coefficient plot comparing P/M, APM, and RAPM
#       in points per 100 possessions.










#############################
### OFFENSE/DEFENSE MODEL ###
#############################

# TODO: Fit split ridge models and choose lambda using validation RMSE.
# split_ridge = fit_ridge_path(
#     X_train = Xs_train,
#     y_train_centered = y_train_centered,
#     X_valid = Xs_valid,
#     y_valid_centered = y_valid_centered,
#     lambdas = lambdas
# )
# split_ridge$best_lambda
# split_ridge$best_valid_rmse
#
# split_test_pred = predict_centered_model(
#     split_ridge$fit,
#     X_new = Xs_test,
#     lambda = split_ridge$best_lambda
# )
# rmse(y_test, split_test_pred)

# TODO: Evaluate the selected offense/defense RAPM model on the test set.
# TODO: Make a selected-player coefficient plot showing offense and defense
#       estimates separately, in points per 100 possessions.




# ===== TASK 2: shrinkage — Raw P/M vs APM vs RAPM (points per 100 poss) =====
combined_estimates = tibble(
  player      = colnames(X_combined),
  raw         = 100 * as.numeric(raw_pm),
  apm         = 100 * as.numeric(coef(apm_fit))[-1],
  rapm        = 100 * as.numeric(coef(combined_ridge$fit, s = combined_ridge$best_lambda))[-1],
  possessions = player_possessions
)

plot_combined = combined_estimates |>
  filter(player %in% selected_players) |>
  mutate(player = fct_reorder(player, rapm)) |>            # sort plot by RAPM
  pivot_longer(c(raw, apm, rapm), names_to = "method", values_to = "per100") |>
  mutate(method = factor(method, levels = c("raw", "apm", "rapm"),
                         labels = c("Raw P/M", "APM", "RAPM")))

ggplot(plot_combined, aes(per100, player, color = method, shape = method)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +   # zero line
  geom_point(size = 3, alpha = 0.9) +
  labs(x = "Points per 100 possessions", y = NULL, color = NULL, shape = NULL,
       title = "Combined plus-minus: Raw vs APM vs RAPM (sorted by RAPM)") +
  theme_minimal()

# Which players move most under ridge (APM -> RAPM), and why:
shrinkage = combined_estimates |>
  mutate(shift = apm - rapm, abs_shift = abs(shift)) |>
  arrange(desc(abs_shift))
cat("\nLargest APM -> RAPM moves (all players):\n")
print(head(select(shrinkage, player, apm, rapm, shift, possessions), 10))
cat(sprintf("cor(|APM - RAPM|, log possessions) = %.3f\n",
            cor(shrinkage$abs_shift, log(shrinkage$possessions))))






# ===== TASK 3: split offense/defense RAPM =====
split_ridge = fit_ridge_path(Xs_train, y_train_centered, Xs_valid, y_valid_centered, lambdas)
split_ridge$best_lambda
split_ridge$best_valid_rmse

m = length(player_tokens)
split_coef = 100 * as.numeric(coef(split_ridge$fit, s = split_ridge$best_lambda))[-1]
split_estimates = tibble(
  player  = player_labels,
  offense = split_coef[1:m],               # first m cols are *_off
  defense = split_coef[(m + 1):(2 * m)]    # next m cols are *_def (already signed so higher = better D)
)

plot_split = split_estimates |>
  filter(player %in% selected_players) |>
  mutate(total = offense + defense, player = fct_reorder(player, total)) |>
  pivot_longer(c(offense, defense), names_to = "side", values_to = "per100") |>
  mutate(side = factor(side, levels = c("offense", "defense"),
                       labels = c("Offense (scoring)", "Defense (prevention)")))

ggplot(plot_split, aes(per100, player, color = side, shape = side)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 3) +
  labs(x = "Points per 100 possessions", y = NULL, color = NULL, shape = NULL,
       title = "Offense/Defense RAPM (higher = better on both axes)") +
  theme_minimal()

# A player whose single combined number hides an offense/defense split:
split_vs_total = split_estimates |>
  left_join(select(combined_estimates, player, rapm), by = "player") |>
  filter(player %in% selected_players) |>
  mutate(off_def_gap = offense - defense) |>
  arrange(desc(abs(off_def_gap)))
cat("\nSelected players by offense-defense asymmetry:\n")
print(head(select(split_vs_total, player, rapm, offense, defense, off_def_gap), 5))
top = split_vs_total[1, ]
cat(sprintf("%s: combined RAPM %.1f/100 hides offense %.1f vs defense %.1f (gap %.1f).\n",
            top$player, top$rapm, top$offense, top$defense, top$off_def_gap))





# ===== TASK 4: final test RMSE for all five models =====
mean_test_pred  = rep(train_mean, length(y_test))
raw_test_pred   = train_mean + as.numeric(Xc_test %*% raw_pm)
apm_test_pred   = predict_centered_model(apm_fit, X_new = Xc_test)
rapm_test_pred  = predict_centered_model(combined_ridge$fit, X_new = Xc_test,
                                         lambda = combined_ridge$best_lambda)
split_test_pred = predict_centered_model(split_ridge$fit, X_new = Xs_test,
                                         lambda = split_ridge$best_lambda)

task4_test = tibble(
  model = c("mean-only", "raw plus-minus", "combined APM", "combined RAPM", "split RAPM"),
  test_rmse = c(rmse(y_test, mean_test_pred), rmse(y_test, raw_test_pred),
                rmse(y_test, apm_test_pred),  rmse(y_test, rapm_test_pred),
                rmse(y_test, split_test_pred))
) |> arrange(test_rmse)
print(task4_test)




########################
### COMPARISON NOTES ###
########################

# TODO: Compare test RMSE for the combined and split RAPM models.
# TODO: Briefly explain what the split model reveals that the combined model hides.
