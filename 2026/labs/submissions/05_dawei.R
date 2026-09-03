#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "splines", "tidyverse"))
library(ggplot2)
library(nnet)
library(splines)
library(tidyverse)
library(tidyr)
library(dplyr)

# set seed
set.seed(6)

#######################
### EXPECTED POINTS ###
#######################

# load data
nfl_data = read_csv("C:/Users/sundw/Downloads/05_expected-points.csv")

# Just to see:
# model_linear <- lm(pts_next_score ~ yardline_100, data = nfl_data)

# yardline_seq <- data.frame(yardline_100 = 1:99)
# 
# plot_dat <- data.frame(
#   yardline_100 = 1:99,
#   expected_score = predict(model_linear, newdata = yardline_seq)
# )
# 
# ggplot(plot_dat, aes(x = yardline_100, y = expected_score)) +
#   geom_line() +
#   labs(x = "Yardline (distance to end zone)", y = "Expected Points",
#        title = "Expected Points by Yardline (Linear Model)")

model_mlr <- multinom(pts_next_score ~ yardline_100, data = nfl_data)

#Plot 1

score_vals <- as.numeric(levels(factor(nfl_data$pts_next_score)))

probs <- predict(model_mlr, newdata = yardline_seq, type = "probs")

expected_score <- probs %*% score_vals

plot_dat <- data.frame(yardline_100 = 1:99, expected_score = as.numeric(expected_score))

ggplot(plot_dat, aes(x = yardline_100, y = expected_score)) +
  geom_line() +
  labs(x = "Yardline (distance to end zone)", y = "Expected Points",
       title = "Expected Points by Yardline")

# There's only one predictor, so the model doesn't take into account other factors. The extreme values
# (for example, when at the 1-yard line) should probably curve up/down more.

model_spline_mlr <- multinom(pts_next_score ~ bs(yardline_100), data = nfl_data)

probs_spline <- predict(model_spline_mlr, newdata = yardline_seq, type = "probs")

expected_score_spline <- probs_spline %*% score_vals

plot_dat_spline_mlr <- data.frame(yardline_100 = 1:99, expected_score = as.numeric(expected_score_spline))

ggplot(plot_dat_spline_mlr, aes(x = yardline_100, y = expected_score)) +
  geom_line() +
  labs(x = "Yardline (distance to end zone)", y = "Expected Points",
       title = "Expected Points by Yardline (Multinomial Spline Model)")

# The spline improves upon the previous model because its extremities are greater.

nfl_data$down <- factor(nfl_data$down)

model_down <- multinom(pts_next_score ~ bs(yardline_100) + down, data = nfl_data)

pred_grid <- expand.grid(yardline_100 = 1:99, down = factor(1:4))

probs_down <- predict(model_down, newdata = pred_grid, type = "probs")

pred_grid$expected_score <- as.numeric(probs_down %*% score_vals)

ggplot(pred_grid, aes(x = yardline_100, y = expected_score, color = down)) +
  geom_line() +
  labs(x = "Yardline (distance to end zone)", y = "Expected Points",
       color = "Down", title = "Expected Points by Yardline and Down")

# Here, downs are encoded categorically, since they're not really numerical/continuous in the same sense
# that yardline would be. Downs are situational, so there really isn't a linear relationship from the
# first to the fourth down.

model_ydstogo <- multinom(pts_next_score ~ bs(yardline_100) + down + bs(ydstogo), data = nfl_data)

pred_grid2 <- expand.grid(
  yardline_100 = 1:99,
  down = factor(1:4),
  ydstogo = c(1, 5, 10, 15, 20)
)

probs_ydstogo <- predict(model_ydstogo, newdata = pred_grid2, type = "probs")

pred_grid2$expected_score <- as.numeric(probs_ydstogo %*% score_vals)

ggplot(pred_grid2, aes(x = yardline_100, y = expected_score, color = factor(ydstogo))) +
  geom_line() +
  facet_wrap(~ down) +
  labs(x = "Yardline (distance to end zone)", y = "Expected Points",
       color = "Yards to Go", title = "Expected Points by Yardline, Down, and Yards to Go")

model_ydstogo_log <- multinom(pts_next_score ~ bs(yardline_100) + down + log(ydstogo), data = nfl_data)

probs_ydstogo_log <- predict(model_ydstogo_log, newdata = pred_grid2, type = "probs")

pred_grid2$expected_score <- as.numeric(probs_ydstogo_log %*% score_vals)

ggplot(pred_grid2, aes(x = yardline_100, y = expected_score, color = factor(ydstogo))) +
  geom_line() +
  facet_wrap(~ down) +
  labs(x = "Yardline (distance to end zone)", y = "Expected Points",
       color = "Yards to Go", title = "Expected Points by Yardline, Down, and Yards to Go (Log)")

# Yards to go seems to affect expected points less and less as the down number goes up.

model_time <- multinom(pts_next_score ~ bs(yardline_100) + down + log(ydstogo) + half_seconds_remaining, data = nfl_data)

pred_grid3 <- expand.grid(
  yardline_100 = 1:99,
  down = factor(1),
  ydstogo = 10,
  half_seconds_remaining = c(60, 300, 600, 900, 1200, 1800)
)

probs_time <- predict(model_time, newdata = pred_grid3, type = "probs")

pred_grid3$expected_score <- as.numeric(probs_time %*% score_vals)

ggplot(pred_grid3, aes(x = yardline_100, y = expected_score, color = factor(half_seconds_remaining))) +
  geom_line() +
  labs(x = "Yardline (distance to end zone)", y = "Expected Points",
       color = "Seconds Remaining", title = "Expected Points by Yardline and Time Remaining (1st & 10)")

model_time_spline <- multinom(pts_next_score ~ bs(yardline_100) + down + log(ydstogo) + bs(half_seconds_remaining), data = nfl_data)

probs_time_spline <- predict(model_time_spline, newdata = pred_grid3, type = "probs")

pred_grid3$expected_score <- as.numeric(probs_time_spline %*% score_vals)

ggplot(pred_grid3, aes(x = yardline_100, y = expected_score, color = factor(half_seconds_remaining))) +
  geom_line() +
  labs(x = "Yardline (distance to end zone)", y = "Expected Points",
       color = "Seconds Remaining", title = "Expected Points by Yardline and Time Remaining (1st & 10)")

# Task 2

model_spread2 <- multinom(pts_next_score ~ bs(yardline_100) + down + log(ydstogo) + posteam_spread, data = nfl_data)

pred_grid5 <- expand.grid(
  yardline_100 = 1:99,
  down = factor(1),
  ydstogo = 10,
  posteam_spread = 0
)

probs_no_spread <- predict(model_ydstogo_log, newdata = pred_grid5, type = "probs")
pred_grid5$expected_score <- as.numeric(probs_no_spread %*% score_vals)
pred_grid5$model <- "Without Spread"

pred_grid6 <- pred_grid5 %>% mutate(model = "With Spread")
probs_with_spread <- predict(model_spread2, newdata = pred_grid6, type = "probs")
pred_grid6$expected_score <- as.numeric(probs_with_spread %*% score_vals)

plot_dat_compare2 <- bind_rows(pred_grid5, pred_grid6)

ggplot(plot_dat_compare2, aes(x = yardline_100, y = expected_score, linetype = model)) +
  geom_line() +
  labs(x = "Yardline (distance to end zone)", y = "Expected Points",
       linetype = "Model", title = "Expected Points: With vs. Without Spread (1st & 10, Spread = 0)")

# These models are similar, but not the exact same. The model without spread includes games with any
# spread, so there is likely more variance.

# Discussion: These are not the same. A player who is better at shooting 3's will likely take more 3's
# than a player who isn't, so when calculating the make% of all attempts in a season, players who shoot
# well will be overrepresented. On the other hand, the average "true" 3-point shooting percentage of
# all players in the league will include players who almost never shoot 3s because they aren't good at
# it, who will be weighed equally compared to high-volume shooters, so it will likely be lower.