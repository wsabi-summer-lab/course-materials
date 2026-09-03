#############
### SETUP ###
#############
getwd()
# install.packages(c("ggplot2", "pdp", "ranger", "tidyverse", "vip", "xgboost"))
library(ggplot2)
library(pdp)
library(ranger)
library(tidyverse)
library(vip)
library(xgboost)
#install.packages(c("progress", "crayon"))
library(progress)
library(crayon)
library(dplyr)
library(tidyr)
# set seed for reproducibility
set.seed(18)

###########################
### NFL WIN PROBABILITY ###
###########################

# ------------------ LOAD DATA ------------------
nfl_data <- read_csv("../data/18_nfl-wp.csv.gz")

# ------------------ PREVIEW DATA ------------------
head(nfl_data)
colnames(nfl_data)
summary(nfl_data)

# ------------------ FEATURE ENGINEERING ------------------
features <- c(
  "score_differential", "game_seconds_remaining", 
  "posteam_spread", "posteam_timeouts_remaining", 
  "defteam_timeouts_remaining", "yardline_100", 
  "down", "ydstogo"
)

# Remove rows with any missing values in used columns
nfl_data <- nfl_data %>% drop_na(all_of(c("label_win", features)))

# ------------------ TRAIN/TEST SPLIT ------------------
n <- nrow(nfl_data)
train_idx <- sample(seq_len(n), size = floor(0.8 * n))  # 80/20 split

train_data <- nfl_data[train_idx, ]
test_data <- nfl_data[-train_idx, ]

# ------------------ K-FOLD CV SETUP ------------------
k <- 5
folds <- sample(rep(1:k, length.out = nrow(train_data)))

# ------------------ GRID SETUP ------------------
grid <- expand.grid(
  eta = c(0.05, 0.1, 0.2),            # 3 values instead of 5
  max_depth = c(4, 6, 8),             # 3 values instead of 5
  subsample = c(0.7, 0.9),            # 2 values instead of 3
  colsample_bytree = c(0.7, 0.9),     # 2 values instead of 3
  nrounds = c(75, 125, 175)            # 3 values instead of 4
)



# ------------------ RUN GRID SEARCH WITH K-FOLD CV ------------------
cv_results <- list()

# Initialize progress bar
pb <- progress_bar$new(
  format = paste0(
    blue(":current"), "/", blue(":total"), " [:bar] ", green(":percent"), " ETA: :eta"
  ),
  total = nrow(grid),
  clear = FALSE, width = 60
)

for (i in 1:nrow(grid)) {
  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = grid$eta[i],
    max_depth = grid$max_depth[i],
    subsample = grid$subsample[i],
    colsample_bytree = grid$colsample_bytree[i]
  )
  
  fold_logloss <- c()
  
  for (j in 1:k) {
    train_fold <- train_data[folds != j, ]
    val_fold <- train_data[folds == j, ]
    
    dtrain_fold <- xgb.DMatrix(data = as.matrix(train_fold[, features]), label = train_fold$label_win)
    dval_fold <- xgb.DMatrix(data = as.matrix(val_fold[, features]), label = val_fold$label_win)
    
    model_cv <- xgb.train(
      params = params,
      data = dtrain_fold,
      nrounds = grid$nrounds[i],
      verbose = 0
    )
    
    preds <- predict(model_cv, dval_fold)
    logloss <- -mean(val_fold$label_win * log(preds) + (1 - val_fold$label_win) * log(1 - preds))
    fold_logloss <- c(fold_logloss, logloss)
  }
  
  avg_logloss <- mean(fold_logloss)
  cv_results[[i]] <- list(params = params, logloss = avg_logloss)
  
  pb$tick()  # update progress bar
  
  # Print a colorful message after each grid iteration
  cat(green$bold(sprintf(" Completed grid %d/%d | avg_logloss=%.5f \n", i, nrow(grid), avg_logloss)))
}

# ------------------ SELECT BEST PARAMETERS ------------------
loglosses <- sapply(cv_results, function(x) x$logloss)
best_index <- which.min(loglosses)
best_params <- cv_results[[best_index]]$params

print("Best params found via CV:")
print(best_params)

# ------------------ TRAIN FINAL MODEL ON FULL TRAINING DATA ------------------
dtrain_full <- xgb.DMatrix(data = as.matrix(train_data[, features]), label = train_data$label_win)

final_model <- xgb.train(
  params = best_params,
  data = dtrain_full,
  verbose = 1,
  nrounds= 175,
)

# ------------------ PREDICT ON TEST SET ------------------
X_test <- as.matrix(test_data[, features])
dtest <- xgb.DMatrix(data = X_test)
test_preds <- predict(final_model, dtest)
predicted_win <- as.numeric(test_preds > 0.5)

# ------------------ EVALUATION ------------------
accuracy <- mean(predicted_win == test_data$label_win)
cat(sprintf("Test set accuracy: %.4f\n", accuracy))

# ------------------ FEATURE IMPORTANCE ------------------
importance <- xgb.importance(model = final_model, feature_names = features)
xgb.plot.importance(importance)
# Create bins of predicted probabilities
calib_df <- data.frame(
  predicted_prob = test_preds,
  actual = test_data$label_win
) %>%
  mutate(bin = cut(predicted_prob, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(
    bin_center = mean(predicted_prob, na.rm = TRUE),
    observed = mean(actual),
    count = n(),
    .groups = "drop"
  ) %>%
  filter(count >= 10)  # Filter bins with at least 10 observations to reduce noise
dev.off()
# Plot
#install.packages("binom")  # for confidence intervals
library(binom)  # for confidence intervals

calib_df <- data.frame(
  predicted_prob = test_preds,
  actual = test_data$label_win
) %>%
  mutate(bin = cut(predicted_prob, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(
    bin_center = mean(predicted_prob, na.rm = TRUE),
    observed = mean(actual),
    count = n(),
    .groups = "drop"
  ) %>%
  filter(count >= 10) %>%
  rowwise() %>%
  mutate(
    ci = list(binom.confint(observed * count, count, methods = "wilson")[, c("lower", "upper")])
  ) %>%
  tidyr::unnest_wider(ci)

ggplot(calib_df, aes(x = bin_center, y = observed)) +
  geom_point(aes(size = count), color = "darkblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.015, color = "darkblue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_size_area(max_size = 10) +  # scale size by count with max dot size 10
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  labs(
    title = "Calibration Plot with Varying Dot Size by Bin Count",
    x = "Predicted Win Probability",
    y = "Observed Win Rate",
    size = "Bin Count"
  ) +
  theme_minimal(base_size = 14)
# Features used in the model
features <- c(
  "score_differential", "game_seconds_remaining", 
  "posteam_spread", "posteam_timeouts_remaining", 
  "defteam_timeouts_remaining", "yardline_100", 
  "down", "ydstogo"
)

# Fixed values (median of features NOT varied in any PDP grid)
fixed_vals_all <- train_data %>%
  summarise(across(all_of(setdiff(features, c(
    "yardline_100", "score_differential", "game_seconds_remaining", "posteam_spread"
  ))), ~median(.x, na.rm = TRUE)))

# Prediction helper function
get_wp_preds <- function(grid, varying_vars) {
  missing_cols <- setdiff(features, colnames(grid))
  fixed_cols_to_add <- intersect(missing_cols, colnames(fixed_vals_all))
  if (length(fixed_cols_to_add) > 0) {
    grid <- bind_cols(grid, fixed_vals_all[rep(1, nrow(grid)), fixed_cols_to_add, drop = FALSE])
  }
  missing_still <- setdiff(features, colnames(grid))
  if (length(missing_still) > 0) {
    for (col in missing_still) {
      grid[[col]] <- NA_real_
    }
  }
  grid <- grid[, features]
  dmat <- xgb.DMatrix(data = as.matrix(grid))
  predict(final_model, dmat)
}

# ------------------ PDP 1 ------------------
varying_vars_1 <- c("yardline_100", "score_differential", "game_seconds_remaining")

pdp1 <- expand.grid(
  yardline_100 = seq(1, 99, by = 1),
  score_differential = c(-14, -7, 0, 7, 14),
  game_seconds_remaining = c(1800, 900, 300)
)
pdp1$wp <- get_wp_preds(pdp1, varying_vars_1)

plot1 <- ggplot(pdp1, aes(x = yardline_100, y = wp, color = factor(score_differential))) +
  geom_line(size = 1) +
  facet_wrap(~ game_seconds_remaining, labeller = label_both) +
  labs(
    title = "Win Probability vs Yardline by Score Differential",
    x = "Yardline (Distance to Endzone)",
    y = "Estimated Win Probability",
    color = "Score Differential"
  ) +
  theme_minimal()

# ------------------ PDP 2 ------------------
varying_vars_2 <- c("score_differential", "game_seconds_remaining")

pdp2 <- expand.grid(
  score_differential = seq(-21, 21, by = 1),
  game_seconds_remaining = seq(0, 3600, by = 60)
)
pdp2$wp <- get_wp_preds(pdp2, varying_vars_2)

plot2 <- ggplot(pdp2, aes(x = score_differential, y = game_seconds_remaining, fill = wp)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Heatmap of Win Probability by Score Differential and Time Remaining",
    x = "Score Differential",
    y = "Game Seconds Remaining",
    fill = "Win Probability"
  ) +
  theme_minimal()

# ------------------ PDP 3 ------------------
varying_vars_3 <- c("yardline_100", "posteam_spread", "game_seconds_remaining")

pdp3 <- expand.grid(
  yardline_100 = seq(1, 99, by = 1),
  posteam_spread = c(-7, 0, 7, 14),
  game_seconds_remaining = c(1800, 900, 300)
)
pdp3$wp <- get_wp_preds(pdp3, varying_vars_3)

plot3 <- ggplot(pdp3, aes(x = yardline_100, y = wp, color = factor(posteam_spread))) +
  geom_line(size = 1) +
  facet_wrap(~ game_seconds_remaining, labeller = label_both) +
  labs(
    title = "Win Probability vs Yardline by Point Spread",
    x = "Yardline (Distance to Endzone)",
    y = "Estimated Win Probability",
    color = "Point Spread"
  ) +
  theme_minimal()

# ------------------ Print plots ------------------
print(plot1)
print(plot2)
print(plot3)

# ---------------------------
# Use my tuned hyperparameters here
best_params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.1,            # example, replace with your best
  max_depth = 6,        # example, replace with your best
  subsample = 0.9,      # example, replace with your best
  colsample_bytree = 0.7 # example, replace with your best
)
nrounds <- 175  # fixed number of boosting rounds from tuning

# ---------------------------
B <- 20  # number of bootstrap samples

# Make sure your train_data has a game identifier column; replace 'game_id' below as needed
game_ids <- unique(train_data$game_id)

# Matrix to store WP predictions for each bootstrap model, for all plays in train_data
wp_preds_mat <- matrix(NA, nrow = nrow(train_data), ncol = B)

for (b in 1:B) {
  # Sample games with replacement (block bootstrap)
  boot_games <- sample(game_ids, size = length(game_ids), replace = TRUE)
  
  # Build bootstrap sample by binding all plays from sampled games (including duplicates)
  boot_data <- do.call(rbind, lapply(boot_games, function(g) train_data[train_data$game_id == g, ]))
  
  # Prepare DMatrix
  dtrain_boot <- xgb.DMatrix(data = as.matrix(boot_data[, features]), label = boot_data$label_win)
  
  # Train bootstrap model with fixed params and nrounds
  boot_model <- xgb.train(
    params = best_params,
    data = dtrain_boot,
    nrounds = nrounds,
    verbose = 0
  )
  
  # Predict WP on entire train_data
  dtrain_full <- xgb.DMatrix(data = as.matrix(train_data[, features]))
  wp_preds_mat[, b] <- predict(boot_model, dtrain_full)
  
  cat(sprintf("Completed bootstrap %d/%d\n", b, B))
}

# ---------------------------
# Calculate 90% CIs and CI widths per play
ci_df <- train_data %>%
  select(game_id, score_differential, game_seconds_remaining, yardline_100, down) %>%
  mutate(
    wp_mean = rowMeans(wp_preds_mat),
    wp_lower = apply(wp_preds_mat, 1, quantile, probs = 0.05),
    wp_upper = apply(wp_preds_mat, 1, quantile, probs = 0.95),
    ci_width = wp_upper - wp_lower
  )

# Plot distribution of CI widths
ggplot(ci_df, aes(x = ci_width)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue", color = "black") +
  labs(title = "Distribution of 95% CI Widths for Win Probability",
       x = "CI Width", y = "Count") +
  theme_minimal()

# Plot CI width vs time remaining
ggplot(ci_df, aes(x = game_seconds_remaining, y = ci_width)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "CI Width vs Time Remaining",
       x = "Game Seconds Remaining",
       y = "CI Width") +
  theme_minimal()

# Plot CI width by down
ggplot(ci_df, aes(x = factor(down), y = ci_width)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "CI Width by Down",
       x = "Down",
       y = "CI Width") +
  theme_minimal()

# ---------------------------
# Step 5: WP vs Time Remaining plot with 95% CI ribbons for select score differentials and yardlines

fixed_vals <- train_data %>%
  summarise(across(all_of(setdiff(features, c("score_differential", "game_seconds_remaining", "yardline_100"))), ~median(.x, na.rm = TRUE)))

score_vals <- c(-14, 0, 14)
yard_vals <- c(10, 50, 90)
time_vals <- seq(0, 3600, by = 60)

plot_grid <- expand.grid(
  score_differential = score_vals,
  yardline_100 = yard_vals,
  game_seconds_remaining = time_vals
) %>%
  bind_cols(fixed_vals[rep(1, nrow(.)), ]) %>%
  select(all_of(features))

# Predict WP mean and CIs on plot_grid from bootstrap preds:
# For speed here, use final_model to predict mean WP, but if you want full bootstrap uncertainty, you’d need to predict from each bootstrap model.
# For demonstration, let's predict mean WP from final_model:

dmat_plot <- xgb.DMatrix(data = as.matrix(plot_grid))
plot_grid$wp_mean <- predict(final_model, dmat_plot)

# For CI ribbons using bootstrap, we'd need to predict plot_grid for each bootstrap model similarly — but that can be slow.
# As a quick approach: approximate CI width by average CI width from train_data

avg_ci_width <- mean(ci_df$ci_width, na.rm = TRUE)
plot_grid <- plot_grid %>%
  mutate(
    wp_lower = pmax(0, wp_mean - avg_ci_width / 2),
    wp_upper = pmin(1, wp_mean + avg_ci_width / 2)
  )

ggplot(plot_grid, aes(x = game_seconds_remaining, y = wp_mean, color = factor(score_differential))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = wp_lower, ymax = wp_upper, fill = factor(score_differential)), alpha = 0.2, color = NA) +
  facet_wrap(~ yardline_100) +
  scale_x_reverse() +  # Time counts down to 0
  labs(
    title = "Win Probability vs Time Remaining\nwith Approximate 95% Confidence Intervals",
    x = "Game Seconds Remaining",
    y = "Estimated Win Probability",
    color = "Score Differential",
    fill = "Score Differential"
  ) +
  theme_minimal()