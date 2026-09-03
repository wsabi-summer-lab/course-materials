#############
### SETUP ###
#############

# install.packages(c("ggplot2", "pdp", "ranger", "tidyverse", "vip", "xgboost"))
library(ggplot2)
library(pdp)
library(ranger)
library(tidyverse)
library(vip)
library(xgboost)

# set seed
set.seed(18)

###########################
### NFL WIN PROBABILITY ###
###########################

# read in data
nfl_data = read_csv("../data/18_nfl-wp.csv.gz")

# preview data
head(nfl_data)
names(nfl_data)


##fit a win probability model with random forest using ranger
nfl = nfl_data %>%
  mutate(
    down = factor(down),
    ydstogo = as.numeric(ydstogo))

train = nfl %>% filter(season <= 2018)
valid = nfl %>% filter(season == 2019)
test = nfl %>% filter(season >= 2020)


rf_model = ranger(
  formula = label_win ~ yardline_100
  + score_differential
  + game_seconds_remaining
  + posteam_spread
  + posteam_timeouts_remaining
  + defteam_timeouts_remaining
  + down   
  + ydstogo,  
  data        = train,
  num.trees   = 500,
  mtry        = 5,
  min.node.size = 5,
  probability = TRUE)


#line plot of WP vs yardline
pred_fun <- function(object, newdata) {
  out <- predict(object, data = newdata)$predictions
  if (is.matrix(out)) {
    # classification‐with‐probabilities: take the 2nd column (Pr[class = 1])
    return(out[, 2])
  } else {
    # regression or single‐vector output
    return(as.numeric(out))
  }
}

# 2) Define medians / slices
med_spread     <- median(train$posteam_spread)
med_score_diff <- median(train$score_differential)
med_yardline   <- median(train$yardline_100)
med_ydstogo    <- median(train$ydstogo)
med_off_to     <- median(train$posteam_timeouts_remaining)
med_def_to     <- median(train$defteam_timeouts_remaining)
first_down     <- factor(1, levels = levels(train$down))

score_vals <- c(-14, -7, 0, 7, 14)
spread_vals <- c(-7, 0, 7)
time_vals   <- c(3600, 1800, 600)   # seconds remaining
yard_seq    <- seq(min(train$yardline_100), max(train$yardline_100), length.out = 100)

# — 2a) WP vs. yardline, by score_diff & time_remaining —
grid_a <- expand.grid(
  yardline_100              = yard_seq,
  score_differential        = score_vals,
  game_seconds_remaining    = time_vals,
  posteam_spread            = med_spread,
  posteam_timeouts_remaining= med_off_to,
  defteam_timeouts_remaining= med_def_to,
  down                      = first_down,
  ydstogo                   = med_ydstogo,
  stringsAsFactors          = FALSE
)
grid_a$WP <- pred_fun(rf_model, grid_a)

ggplot(grid_a, aes(x = yardline_100, y = WP, color = factor(score_differential))) +
  geom_line(size = 1) +
  facet_wrap(~ game_seconds_remaining, ncol = 1,
             labeller = labeller(game_seconds_remaining = 
                                   c(`3600`="60 min","1800"="30 min","600"="10 min"))) +
  labs(
    x     = "Yardline (100 = opp. goal line)",
    y     = "Win Probability",
    color = "Score Diff",
    title = "WP vs. Yardline by Score Differential & Time Remaining"
  ) +
  theme_minimal()

# — 2b) Heatmap of WP over (score_diff × time_remaining) —
grid_b <- expand.grid(
  score_differential        = seq(min(train$score_differential), max(train$score_differential), by = 1),
  game_seconds_remaining    = seq(max(train$game_seconds_remaining), 0, length.out = 100),
  yardline_100              = med_yardline,
  posteam_spread            = med_spread,
  posteam_timeouts_remaining= med_off_to,
  defteam_timeouts_remaining= med_def_to,
  down                      = first_down,
  ydstogo                   = med_ydstogo,
  stringsAsFactors          = FALSE
)
grid_b$WP <- pred_fun(rf_model, grid_b)

ggplot(grid_b, aes(x = score_differential,
                   y = game_seconds_remaining,
                   fill = WP)) +
  geom_tile() +
  scale_y_reverse(breaks = time_vals,
                  labels = c("60 min","30 min","10 min")) +
  labs(
    x     = "Score Differential",
    y     = "Time Remaining",
    fill  = "Win Prob.",
    title = "Heatmap of Win Prob. (Score Diff × Time Remaining)"
  ) +
  theme_minimal()

# — 2c) WP vs. yardline, by point spread & time_remaining —
grid_c <- expand.grid(
  yardline_100              = yard_seq,
  posteam_spread            = spread_vals,
  game_seconds_remaining    = time_vals,
  score_differential        = med_score_diff,
  posteam_timeouts_remaining= med_off_to,
  defteam_timeouts_remaining= med_def_to,
  down                      = first_down,
  ydstogo                   = med_ydstogo,
  stringsAsFactors          = FALSE
)
grid_c$WP <- pred_fun(rf_model, grid_c)

ggplot(grid_c, aes(x = yardline_100, y = WP, color = factor(posteam_spread))) +
  geom_line(size = 1) +
  facet_wrap(~ game_seconds_remaining, nrow = 1,
             labeller = labeller(game_seconds_remaining = 
                                   c(`3600`="60 min","1800"="30 min","600"="10 min"))) +
  labs(
    x     = "Yardline",
    y     = "Win Probability",
    color = "Point Spread",
    title = "WP vs. Yardline by Point Spread & Time Remaining"
  ) +
  theme_minimal()

#bootstrapping
# 1. Combine train+valid to form the pool we’ll bootstrap from
train_all <- bind_rows(train, valid)

# 2. Block‐bootstrap parameters
B        <- 100
game_ids <- unique(train_all$game_id)
n_play   <- nrow(nfl)

# 3. Storage for B replicate predictions of P(win=1) on each play
WP_boot <- matrix(NA_real_, nrow = n_play, ncol = B)

# 4. Loop: sample games → refit RF → predict all plays
for (b in seq_len(B)) {
  # 4a) draw a bootstrap sample of game_ids
  sampled_games <- sample(game_ids, length(game_ids), replace = TRUE)
  
  # 4b) stack up all plays from those games
  boot_df <- do.call(rbind, lapply(sampled_games, function(g) {
    train_all[train_all$game_id == g, ]
  }))
  
  # 4c) fit RF on this bootstrap draw
  rf_b <- ranger(
    formula       = label_win ~ yardline_100
    + score_differential
    + game_seconds_remaining
    + posteam_spread
    + posteam_timeouts_remaining
    + defteam_timeouts_remaining
    + down
    + ydstogo,
    data          = boot_df,
    num.trees     = 500,
    mtry          = 5,
    min.node.size = 5,
    probability   = TRUE,
    seed          = 18,
    verbose       = FALSE
  )
  
  # 4d) predict P(win=1) on every play in `nfl`
  preds_mat      <- predict(rf_b, data = nfl)$predictions
  WP_boot[, b]   <- if (is.matrix(preds_mat)) preds_mat[, 2] else preds_mat
}

# 5. Compute 95% CIs for each play
ci_bounds <- t(apply(WP_boot, 1, quantile, probs = c(0.025, 0.975)))
colnames(ci_bounds) <- c("WP_lower", "WP_upper")

# 6. Attach to nfl and compute CI width
nfl <- nfl %>%
  bind_cols(as.data.frame(ci_bounds)) %>%
  mutate(CI_width = WP_upper - WP_lower)

# Now nfl has columns:
#   WP_lower, WP_upper, CI_width
ggplot(nfl, aes(x = CI_width)) +
  geom_histogram(bins = 50, color = "black", fill = "steelblue") +
  labs(
    title = "Distribution of 95% CI Widths",
    x     = "CI Width",
    y     = "Count"
  ) +
  theme_minimal()

# 2) CI width by down (boxplot)
ggplot(nfl, aes(x = down, y = CI_width)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(
    title = "CI Width by Down",
    x     = "Down",
    y     = "CI Width"
  ) +
  theme_minimal()

# 3) CI width vs. time remaining (scatter + smoother)
ggplot(nfl, aes(x = game_seconds_remaining, y = CI_width)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") +
  scale_x_reverse(breaks = seq(0, 3600, by = 600),
                  labels = c("0m","10m","20m","30m","40m","50m","60m")) +
  labs(
    title = "CI Width vs. Time Remaining",
    x     = "Time Remaining",
    y     = "CI Width"
  ) +
  theme_minimal()
