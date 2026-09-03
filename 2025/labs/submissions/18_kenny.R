library(ggplot2)
library(pdp)
library(ranger)
library(tidyverse)
library(vip)
library(xgboost)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

set.seed(18)

nfl_data = read_csv("18_nfl-wp.csv.gz")

head(nfl_data)

nfl_game_state <- nfl_data %>%
  select(-game_id, -season)

nfl_game_state$label_win <- as.numeric(nfl_game_state$label_win)

X <- as.matrix(nfl_game_state %>% select(-label_win))
y <- nfl_game_state$label_win

train_idx <- sample(1:nrow(X), 0.8 * nrow(X))
X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test <- X[-train_idx, ]
y_test <- y[-train_idx]

xgb_model <- xgboost(
  data = X_train,
  label = y_train,
  objective = "binary:logistic",
  nrounds = 100,
  max_depth = 5,
  eta = 0.1,
  verbose = 0
)

pred_probs <- predict(xgb_model, X_test)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

mean(pred_labels == y_test)

# Partial dependence plots

base_state <- data.frame(
  yardline_100 = 50,
  down = 1,
  ydstogo = 10,
  score_differential = 0,
  game_seconds_remaining = 1800,
  posteam_timeouts_remaining = 3,
  defteam_timeouts_remaining = 3,
  posteam_spread = 0
)

yard_vals <- seq(1, 99, by = 1)
score_diffs <- c(-14, -7, -3, 0, 3, 7, 14)
time_bins <- c(3600, 3000, 2400, 1800, 1200, 600)

pdp_grid <- expand.grid(
  yardline_100 = yard_vals,
  score_differential = score_diffs,
  game_seconds_remaining = time_bins
)

pdp_grid$down <- base_state$down
pdp_grid$ydstogo <- base_state$ydstogo
pdp_grid$posteam_timeouts_remaining <- base_state$posteam_timeouts_remaining
pdp_grid$defteam_timeouts_remaining <- base_state$defteam_timeouts_remaining
pdp_grid$posteam_spread <- base_state$posteam_spread

train_cols <- colnames(X)
pdp_matrix <- pdp_grid[, train_cols]
dmat_pdp <- xgb.DMatrix(data = as.matrix(pdp_matrix))

pdp_grid$wp <- predict(xgb_model, dmat_pdp)

ggplot(pdp_grid, aes(x = yardline_100, y = wp, color = as.factor(score_differential))) +
  geom_line() +
  facet_wrap(~game_seconds_remaining) +
  labs(
    title = "Partial Dependence of Win Probability on Yard Line",
    x = "Yardline (100 = own goal line)",
    y = "Estimated Win Probability",
    color = "Score Differential"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "RdYlBu", direction = -1)

# Heat map

score_diffs <- seq(-21, 21, by = 3)
time_remaining <- seq(0, 3600, by = 100)

heatmap_grid <- expand.grid(
  score_differential = score_diffs,
  game_seconds_remaining = time_remaining
)

heatmap_grid$yardline_100 <- base_state$yardline_100
heatmap_grid$down <- base_state$down
heatmap_grid$ydstogo <- base_state$ydstogo
heatmap_grid$posteam_timeouts_remaining <- base_state$posteam_timeouts_remaining
heatmap_grid$defteam_timeouts_remaining <- base_state$defteam_timeouts_remaining
heatmap_grid$posteam_spread <- base_state$posteam_spread

heatmap_matrix <- heatmap_grid[, train_cols]
dmat_heatmap <- xgb.DMatrix(as.matrix(heatmap_matrix))

heatmap_grid$wp <- predict(xgb_model, dmat_heatmap)

ggplot(heatmap_grid, aes(x = score_differential, y = game_seconds_remaining, fill = wp)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Win Probability", option = "C") +
  scale_y_reverse() +  # so time moves top to bottom like a football clock
  labs(
    title = "Win Probability by Score Differential and Time Remaining",
    x = "Score Differential (posteam)",
    y = "Time Remaining (seconds)"
  ) +
  theme_minimal()

# Underdog partial dependence plot

yard_vals <- seq(1, 99, by = 1)
spreads <- c(-14, -3, 0, 3, 14)
time_bins <- c(3600, 1800, 300)

pdp_spread_grid <- expand.grid(
  yardline_100 = yard_vals,
  posteam_spread = spreads,
  game_seconds_remaining = time_bins
)

pdp_spread_grid$down <- base_state$down
pdp_spread_grid$ydstogo <- base_state$ydstogo
pdp_spread_grid$score_differential <- base_state$score_differential
pdp_spread_grid$posteam_timeouts_remaining <- base_state$posteam_timeouts_remaining
pdp_spread_grid$defteam_timeouts_remaining <- base_state$defteam_timeouts_remaining

pdp_spread_grid$time_bin <- factor(pdp_spread_grid$game_seconds_remaining,
                                   labels = c("Late (300s)", "Mid (1800s)", "Early (3600s)"))

pdp_matrix <- pdp_spread_grid[, train_cols]
dmat_pdp <- xgb.DMatrix(as.matrix(pdp_matrix))

pdp_spread_grid$wp <- predict(xgb_model, dmat_pdp)

ggplot(pdp_spread_grid, aes(x = yardline_100, y = wp, color = as.factor(posteam_spread))) +
  geom_line() +
  facet_wrap(~time_bin) +
  labs(
    title = "Partial Dependence of Win Probability on Yard Line",
    subtitle = "Faceted by Time Remaining, Colored by Point Spread",
    x = "Yardline (100 = own goal line)",
    y = "Estimated Win Probability",
    color = "Point Spread"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "RdBu", direction = 1)

# Bootstraping

B <- 100

game_ids <- unique(nfl_data$game_id)

feature_cols <- c(
  "yardline_100", "down", "ydstogo", "score_differential",
  "game_seconds_remaining", "posteam_timeouts_remaining",
  "defteam_timeouts_remaining", "posteam_spread"
)

prediction_grid <- expand.grid(
  yardline_100 = seq(1, 99, by = 10),
  down = 1,
  ydstogo = 10,
  score_differential = 0,
  game_seconds_remaining = 1800,
  posteam_timeouts_remaining = 3,
  defteam_timeouts_remaining = 3,
  posteam_spread = 0
)

N_pred <- nrow(prediction_grid)

bootstrap_preds <- matrix(NA, nrow = N_pred, ncol = B)

for (b in 1:B) {
  
  sampled_games <- sample(game_ids, replace = TRUE)
  
  df_boot <- nfl_data %>% dplyr::filter(game_id %in% sampled_games)
  
  df_boot <- df_boot %>% semi_join(data.frame(game_id = sampled_games), by = "game_id")
  
  X_boot <- as.matrix(df_boot[, feature_cols])
  y_boot <- df_boot$label_win

  xgb_boot <- xgboost(
    data = X_boot,
    label = y_boot,
    objective = "binary:logistic",
    nrounds = 50,
    max_depth = 5,
    eta = 0.1,
    verbose = 0
  )
  
  X_pred <- as.matrix(prediction_grid[, feature_cols])
  dmat_pred <- xgb.DMatrix(X_pred)
  bootstrap_preds[, b] <- predict(xgb_boot, dmat_pred)
}

mean_wp <- rowMeans(bootstrap_preds)
lower_ci <- apply(bootstrap_preds, 1, quantile, probs = 0.025)
upper_ci <- apply(bootstrap_preds, 1, quantile, probs = 0.975)

prediction_grid$wp_mean <- mean_wp
prediction_grid$wp_lower <- lower_ci
prediction_grid$wp_upper <- upper_ci

ggplot(prediction_grid, aes(x = yardline_100, y = wp_mean)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = wp_lower, ymax = wp_upper), alpha = 0.2, fill = "blue") +
  labs(
    title = "Estimated Win Probability with 95% Confidence Intervals",
    x = "Yardline (100 = own goal line)",
    y = "Win Probability"
  ) +
  theme_minimal()

# Confidence intervals for each play

N <- nrow(nfl_data)
bootstrap_preds_all <- matrix(NA, nrow = N, ncol = B)

game_ids <- unique(nfl_data$game_id)

for (b in 1:B) {
  sampled_games <- sample(game_ids, replace = TRUE)
  
  df_boot <- dplyr::filter(nfl_data, game_id %in% sampled_games)
  X_boot <- as.matrix(df_boot[, feature_cols])
  y_boot <- df_boot$label_win
  
  xgb_boot <- xgboost(
    data = X_boot,
    label = y_boot,
    objective = "binary:logistic",
    nrounds = 50,
    max_depth = 5,
    eta = 0.1,
    verbose = 0
  )

  X_all <- as.matrix(nfl_data[, feature_cols])
  dmat_all <- xgb.DMatrix(X_all)
  bootstrap_preds_all[, b] <- predict(xgb_boot, dmat_all)
}

nfl_data$wp_mean <- rowMeans(bootstrap_preds_all)
nfl_data$wp_lower <- apply(bootstrap_preds_all, 1, quantile, probs = 0.025)
nfl_data$wp_upper <- apply(bootstrap_preds_all, 1, quantile, probs = 0.975)
nfl_data$wp_width <- nfl_data$wp_upper - nfl_data$wp_lower

ggplot(nfl_data, aes(x = wp_width)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of 95% Confidence Interval Widths",
    x = "CI Width (Upper - Lower)",
    y = "Number of Plays"
  ) +
  theme_minimal()

ggplot(nfl_data, aes(x = as.factor(down), y = wp_width)) +
  geom_boxplot(fill = "orange") +
  labs(
    title = "Win Probability CI Width by Down",
    x = "Down",
    y = "CI Width"
  ) +
  theme_minimal()

# Plot of wp against time remaining

time_seq <- seq(0, 3600, by = 60)
score_diffs <- c(-14, -7, 0, 7, 14)
yardlines <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)

wp_grid <- expand.grid(
  game_seconds_remaining = time_seq,
  score_differential = score_diffs,
  yardline_100 = yardlines
)

wp_grid$down <- base_state$down
wp_grid$ydstogo <- base_state$ydstogo
wp_grid$posteam_timeouts_remaining <- base_state$posteam_timeouts_remaining
wp_grid$defteam_timeouts_remaining <- base_state$defteam_timeouts_remaining
wp_grid$posteam_spread <- base_state$posteam_spread

wp_grid$yardline_label <- paste0("Yardline: ", wp_grid$yardline_100)

N_grid <- nrow(wp_grid)
B <- 100
bootstrap_preds_wp <- matrix(NA, nrow = N_grid, ncol = B)

game_ids <- unique(nfl_data$game_id)

for (b in 1:B) {
  sampled_games <- sample(game_ids, replace = TRUE)
  boot_data <- dplyr::filter(nfl_data, game_id %in% sampled_games)
  
  X_boot <- as.matrix(boot_data[, feature_cols])
  y_boot <- boot_data$label_win
  
  xgb_boot <- xgboost(
    data = X_boot,
    label = y_boot,
    objective = "binary:logistic",
    nrounds = 50,
    max_depth = 5,
    eta = 0.1,
    verbose = 0
  )
  
  X_grid <- as.matrix(wp_grid[, feature_cols])
  dmat_grid <- xgb.DMatrix(X_grid)
  bootstrap_preds_wp[, b] <- predict(xgb_boot, dmat_grid)
}

wp_grid$wp_mean <- rowMeans(bootstrap_preds_wp)
wp_grid$wp_lower <- apply(bootstrap_preds_wp, 1, quantile, probs = 0.025)
wp_grid$wp_upper <- apply(bootstrap_preds_wp, 1, quantile, probs = 0.975)
wp_grid$wp_width <- wp_grid$wp_upper - wp_grid$wp_lower

ggplot(wp_grid, aes(x = game_seconds_remaining, y = wp_mean,
                    color = as.factor(score_differential),
                    fill = as.factor(score_differential))) +
  geom_line() +
  geom_ribbon(aes(ymin = wp_lower, ymax = wp_upper), alpha = 0.2, color = NA) +
  facet_wrap(~yardline_label) +
  scale_x_reverse() +
  labs(
    title = "Win Probability vs Time Remaining",
    subtitle = "Shaded 95% Confidence Intervals from Bootstrapped XGBoost",
    x = "Time Remaining (seconds)",
    y = "Win Probability",
    color = "Score Diff",
    fill = "Score Diff"
  ) +
  theme_minimal()




