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
nfl_data <- read_csv("../data/18_nfl-wp.csv.gz")

# preview data
head(nfl_data)

# renaming columns
nfl <- nfl_data %>%
  rename(
    game_id     = game_id,
    yardline    = yardline_100,
    down        = down,
    ydstogo     = ydstogo,
    score_diff  = score_differential,
    seconds_rem = game_seconds_remaining,
    to_home     = posteam_timeouts_remaining,
    to_away     = defteam_timeouts_remaining,
    spread      = posteam_spread,
    win         = label_win
  )

# define feature matrix & label
features <- c("yardline","down","ydstogo","score_diff","seconds_rem","to_home","to_away","spread")
X_all <- as.matrix(nfl[, features])
y_all <- nfl$win

#====================================================================

# -----------------------------------------------------------------------------
# Task 1: TRAINING & TUNING THE XGBOOST MODEL
# -----------------------------------------------------------------------------

# 1.1 Blocked train/validation split (80% games / 20% games)
games     <- unique(nfl$game_id)
train_g   <- sample(games, floor(0.8 * length(games)), replace = FALSE)
train_idx <- which(nfl$game_id %in% train_g)
valid_idx <- setdiff(seq_len(nrow(nfl)), train_idx)

dtrain <- xgb.DMatrix(X_all[train_idx, ], label = y_all[train_idx])
dvalid <- xgb.DMatrix(X_all[valid_idx, ], label = y_all[valid_idx])

# 1.2 Grid search over logloss
grid <- expand.grid(
  max_depth = c(3,5,7),
  eta       = c(0.01,0.1,0.3),
  subsample = c(0.7,1),
  colsample = c(0.7,1)
)

best_ll     <- Inf
best_params <- list()
best_nrounds <- NULL

for(i in seq_len(nrow(grid))) {
  params <- list(
    objective         = "binary:logistic",
    eval_metric       = "logloss",
    max_depth         = grid$max_depth[i],
    eta               = grid$eta[i],
    subsample         = grid$subsample[i],
    colsample_bytree  = grid$colsample[i]
  )
  bst <- xgb.train(
    params,
    dtrain,
    nrounds               = 200,
    watchlist             = list(val = dvalid),
    early_stopping_rounds = 10,
    verbose               = 0
  )
  ll <- bst$evaluation_log[bst$best_iteration]$val_logloss
  if(ll < best_ll) {
    best_ll      <- ll
    best_params  <- params
    best_nrounds <- bst$best_iteration
  }
}

# 1.3 Train final model on all data
dall <- xgb.DMatrix(X_all, label = y_all)
final_model <- xgb.train(
  best_params,
  dall,
  nrounds = best_nrounds,
  verbose = 0
)

# -----------------------------------------------------------------------------
# Task 2: PARTIAL DEPENDENCE PLOTS (PDPs)
# -----------------------------------------------------------------------------

## 2a) ŴP vs yardline (color = score_diff, facet = seconds_rem)
pd_a <- expand.grid(
  yardline    = 1:100,
  score_diff  = c(-14, -7, 0, 7, 14),
  seconds_rem = c(3600, 1800, 600),
  down        = 2,
  ydstogo     = 5,
  to_home     = 2,
  to_away     = 2,
  spread      = 0
)
pd_a$wp_hat <- predict(final_model, xgb.DMatrix(as.matrix(pd_a[,features])))

ggplot(pd_a, aes(x = yardline, y = wp_hat, color = factor(score_diff))) +
  geom_line(size = 1) +
  facet_wrap(~ seconds_rem,
             labeller = labeller(seconds_rem = c(
               `3600` = "60 min", `1800` = "30 min", `600` = "10 min"
             ))) +
  labs(
    title = "2a) PDP: Win Prob vs Yardline",
    x     = "Yardline to Goal",
    y     = "ŴP(x)",
    color = "Score Diff"
  ) +
  theme_minimal()

## 2b) Heatmap: ŴP vs score_diff & seconds_rem
pd_b <- expand.grid(
  score_diff  = seq(-21, 21, by = 3),
  seconds_rem = seq(0, 3600, length.out = 50),
  yardline    = 50,
  down        = 2,
  ydstogo     = 5,
  to_home     = 2,
  to_away     = 2,
  spread      = 0
)
pd_b$wp_hat <- predict(final_model, xgb.DMatrix(as.matrix(pd_b[,features])))

ggplot(pd_b, aes(x = score_diff, y = seconds_rem, fill = wp_hat)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "2b) PDP Heatmap: ŴP ∼ Score Diff & Time",
    x     = "Score Diff",
    y     = "Seconds Remaining",
    fill  = "ŴP"
  ) +
  theme_minimal()

## 2c) ŴP vs yardline (color = spread, facet = seconds_rem)
pd_c <- expand.grid(
  yardline    = 1:100,
  spread      = c(-10, -3, 0, 3, 10),
  seconds_rem = c(3600, 1800, 600),
  down        = 2,
  ydstogo     = 5,
  to_home     = 2,
  to_away     = 2,
  score_diff  = 0
)
pd_c$wp_hat <- predict(final_model, xgb.DMatrix(as.matrix(pd_c[,features])))

ggplot(pd_c, aes(x = yardline, y = wp_hat, color = factor(spread))) +
  geom_line(size = 1) +
  facet_wrap(~ seconds_rem,
             labeller = labeller(seconds_rem = c(
               `3600` = "60 min", `1800` = "30 min", `600` = "10 min"
             ))) +
  labs(
    title = "2c) PDP: Win Prob vs Yardline by Spread",
    x     = "Yardline",
    y     = "ŴP(x)",
    color = "Spread"
  ) +
  theme_minimal()

# -----------------------------------------------------------------------------
# Task 3: BLOCK BOOTSTRAP (B = 100) — quantify uncertainty
# -----------------------------------------------------------------------------
B        <- 100
games    <- unique(nfl$game_id)
n_games  <- length(games)
pred_mat <- matrix(NA, nrow = B, ncol = nrow(nfl))

for(b in seq_len(B)) {
  sampled_games <- sample(games, n_games, replace = TRUE)
  dfb <- bind_rows(lapply(sampled_games,
                          function(g) filter(nfl, game_id == g)))
  Xb <- as.matrix(dfb[,features])
  db <- xgb.DMatrix(Xb, label = dfb$win)
  mb <- xgb.train(best_params, db, nrounds = best_nrounds, verbose = 0)
  pred_mat[b, ] <- predict(mb, dall)
}

# -----------------------------------------------------------------------------
# Task 4: COMPUTE 95% CIs & EXPLORE CI WIDTHS
# -----------------------------------------------------------------------------
cis <- apply(pred_mat, 2, quantile, probs = c(0.025, 0.975))

nfl <- nfl %>%
  mutate(
    wp_hat   = predict(final_model, dall),
    ci_lo    = cis[1, ],
    ci_hi    = cis[2, ],
    ci_width = ci_hi - ci_lo
  )

# 4a) Distribution of CI widths
summary(nfl$ci_width)
ggplot(nfl, aes(x = ci_width)) +
  geom_histogram(binwidth = 0.02) +
  labs(
    title = "4a) Distribution of 95% CI Widths",
    x     = "CI Width"
  ) +
  theme_minimal()

# 4b) CI width by down
ggplot(nfl, aes(x = factor(down), y = ci_width)) +
  geom_boxplot() +
  labs(
    title = "4b) CI Width by Down",
    x     = "Down",
    y     = "CI Width"
  ) +
  theme_minimal()

# 4c) CI width vs seconds remaining
ggplot(nfl, aes(x = seconds_rem, y = ci_width)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  labs(
    title = "4c) CI Width vs Seconds Remaining",
    x     = "Seconds Remaining",
    y     = "CI Width"
  ) +
  theme_minimal()

# -----------------------------------------------------------------------------
# Task 5: ŴP vs TIME REMAINING WITH 95% CI RIBBON
# -----------------------------------------------------------------------------

# 5.1) build small grid
grid5 <- expand.grid(
  seconds_rem      = seq(0, 3600, length.out = 100),
  score_diff       = c(-7, 0, 7),
  yardline         = c(20, 50, 80),
  down             = 2,
  ydstogo          = 5,
  to_home          = 2,
  to_away          = 2,
  spread           = 0
)
X5 <- as.matrix(grid5[,features])
grid5$wp_hat <- predict(final_model, xgb.DMatrix(X5))

# 5.2) bootstrap CIs on grid5
pred5_mat <- matrix(NA, nrow = B, ncol = nrow(grid5))
for(b in seq_len(B)) {
  sampled_games <- sample(games, n_games, replace = TRUE)
  dfb <- bind_rows(lapply(sampled_games,
                          function(g) filter(nfl, game_id == g)))
  mb <- xgb.train(best_params,
                  xgb.DMatrix(as.matrix(dfb[,features]), label = dfb$win),
                  nrounds = best_nrounds, verbose = 0)
  pred5_mat[b, ] <- predict(mb, xgb.DMatrix(X5))
}
ci5 <- apply(pred5_mat, 2, quantile, probs = c(0.025, 0.975))
grid5$ci_lo <- ci5[1, ]
grid5$ci_hi <- ci5[2, ]

# 5.3) final ribbon plot
ggplot(grid5, aes(x = seconds_rem, y = wp_hat, color = factor(score_diff))) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = factor(score_diff)),
              alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  facet_wrap(~ yardline,
             labeller = labeller(yardline = c(
               `20` = "20 yd", `50` = "50 yd", `80` = "80 yd"
             ))) +
  labs(
    title = "5) ŴP vs Time Remaining with 95% CI Ribbon",
    x     = "Seconds Remaining",
    y     = "ŴP(x)",
    color = "Score Diff",
    fill  = "Score Diff"
  ) +
  theme_minimal()
