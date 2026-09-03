#############
### SETUP ###
#############

rm(list=ls())

# install.packages(c("ggplot2", "pdp", "ranger", "tidyverse", "vip", "xgboost"))
library(ggplot2)
library(pdp)
library(ranger)
library(tidyverse)
library(vip)
library(xgboost)
library(tidymodels)
library(furrr)
# install.packages("tidymodels")

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

nfl_data <- nfl_data %>% mutate(label_win = factor(label_win))

# Use only 1st & 10 plays
nfl_data <- nfl_data %>%
  filter(down == 1, ydstogo == 10)

# train-test split by game
nfl_split <- initial_split(nfl_data, prop = 0.8, strata = label_win)
nfl_train <- training(nfl_split)
nfl_test  <- testing(nfl_split)

# remove id columns
nfl_recipe <- recipe(label_win ~ ., data = nfl_train) %>%
  update_role(game_id, season, new_role = "ID")

###################
# random forest
rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 5) %>% # just 5 trees to make it run faster
  set_engine("ranger", importance = "impurity", probability = TRUE) %>%
  set_mode("classification")

rf_workflow <- workflow() %>%
  add_recipe(nfl_recipe) %>%
  add_model(rf_spec)

rf_grid <- grid_regular(mtry(range = c(2, 6)), min_n(range = c(5, 15)), levels = 3)

rf_tuned <- tune_grid(
  rf_workflow,
  resamples = vfold_cv(nfl_train, v = 5),
  grid = rf_grid,
  metrics = metric_set(roc_auc, mn_log_loss)
)

best_rf <- select_best(rf_tuned, metric = "mn_log_loss")
final_rf <- finalize_workflow(rf_workflow, best_rf)

rf_fit <- fit(final_rf, data = nfl_train)

###################
# PDPs

baseline <- nfl_train %>% # training tibble
  summarise(
    posteam_timeouts_remaining = median(posteam_timeouts_remaining, na.rm = TRUE),
    defteam_timeouts_remaining = median(defteam_timeouts_remaining, na.rm = TRUE),
    posteam_spread             = median(posteam_spread,             na.rm = TRUE),
    down                       = 1,
    ydstogo                    = 10
  )

## part a
score_vals <- c(-14, 0, 7)
time_vals  <- c(1800, 900, 120) # seconds left in half
yard_grid  <- seq(1, 99, by = 1)

grid_2a <- expand.grid(
  yardline_100           = yard_grid,
  score_differential     = score_vals,
  game_seconds_remaining = time_vals
) %>% bind_cols(baseline[rep(1, nrow(.)), ])

grid_2a <- grid_2a %>%
  mutate(
    game_id = "fake_game",
    season = 2023
  )

pred_2a <- predict(rf_fit, new_data = grid_2a, type = "prob") %>%
  bind_cols(grid_2a) %>%
  rename(wp = .pred_1)

ggplot(pred_2a,
       aes(x = yardline_100, y = wp,
           colour = factor(score_differential))) +
  geom_line(size = 1) +
  facet_wrap(~game_seconds_remaining,
             labeller = labeller(game_seconds_remaining = function(x)
               paste(x, "sec"))) +
  labs(title  = "PDP: Win Probability vs Yard Line",
       x      = "Yard Line (1 = Own Goal-line, 99 = Opp Goal-line)",
       y      = "Estimated WP",
       colour = "Score Diff") +
  theme_minimal()

## part b
score_vals2 <- seq(-21, 21, by = 3)
time_vals2  <- seq(0, 1800, by = 60)

grid_2b <- expand.grid(
  score_differential     = score_vals2,
  game_seconds_remaining = time_vals2
) %>%
  mutate(yardline_100 = 60) %>%          # fix field position dead-centre
  bind_cols(baseline[rep(1, nrow(.)), ])

grid_2b <- grid_2b %>%
  mutate(
    game_id = "fake_game",
    season = 2023
  )

pred_2b <- predict(rf_fit, new_data = grid_2b, type = "prob") %>%
  bind_cols(grid_2b) %>%
  rename(wp = .pred_1)

ggplot(pred_2b,
       aes(x = score_differential, y = game_seconds_remaining, fill = wp)) +
  geom_tile() +
  scale_y_reverse() +                    # top = kickoff, bottom = end of half
  scale_fill_viridis_c(name = "WP") +
  labs(title = "PDP Heat-map: Score Diff & Time Remaining",
       x = "Score Differential",
       y = "Seconds Remaining (Half)") +
  theme_minimal()

## part c
spread_vals <- c(-10, 0, 10)
time_vals3  <- c(1800, 300)

grid_2c <- expand.grid(
  yardline_100           = yard_grid,
  posteam_spread         = spread_vals,
  game_seconds_remaining = time_vals3
) %>%
  mutate(score_differential = 0) %>%     # hold game state even
  bind_cols(baseline[rep(1, nrow(.)), ] %>% select(-posteam_spread))

grid_2c <- grid_2c %>%
  mutate(
    game_id = "fake_game",
    season = 2023
  )

pred_2c <- predict(rf_fit, new_data = grid_2c, type = "prob") %>%
  bind_cols(grid_2c) %>%
  rename(wp = .pred_1)

ggplot(pred_2c,
       aes(x = yardline_100, y = wp,
           colour = factor(posteam_spread))) +
  geom_line(size = 1) +
  facet_wrap(~game_seconds_remaining,
             labeller = labeller(game_seconds_remaining = function(x)
               paste(x, "sec"))) +
  labs(title  = "PDP: Win Probability vs Yard Line (by Point Spread)",
       x      = "Yard Line",
       y      = "Estimated WP",
       colour = "Spread") +
  theme_minimal()

#####################
# bootstrapping

game_ids <- unique(nfl_data$game_id)
B <- 10 # to make it run faster

bootstrap_preds <- function(b) {
  boot_games <- sample(game_ids, replace = TRUE)
  boot_data <- nfl_data %>% filter(game_id %in% boot_games)
  
  boot_fit <- fit(final_rf, data = boot_data)
  pred <- predict(boot_fit, new_data = nfl_data, type = "prob")$.pred_1
  return(pred)
}

boot_mat <- future_map_dfc(1:B, bootstrap_preds, .options = furrr_options(seed = TRUE))
colnames(boot_mat) <- paste0("boot_", 1:B)
nfl_boot <- bind_cols(nfl_data, boot_mat)

#########################
# confidence intervals

nfl_boot <- nfl_boot %>%
  mutate(
    lower = apply(select(., starts_with("boot_")), 1, quantile, probs = 0.025),
    upper = apply(select(., starts_with("boot_")), 1, quantile, probs = 0.975),
    width = upper - lower
  )

# Histogram of CI widths
ggplot(nfl_boot, aes(width)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  labs(title = "CI Widths (95%)", x = "Width", y = "Count")

# CI width vs time
ggplot(nfl_boot, aes(game_seconds_remaining, width)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  labs(title = "CI Width vs Game Time", x = "Seconds Left", y = "CI Width")

#########################
# ribbon plot
yard_vals <- c(20, 50, 80)
score_vals <- c(-7, 0, 7)

ribbon_df <- expand.grid(
  yardline_100 = yard_vals,
  score_differential = score_vals,
  game_seconds_remaining = seq(0, 1800, by = 60),
  posteam_spread = 0,
  posteam_timeouts_remaining = 3,
  defteam_timeouts_remaining = 3,
  down = 1,
  ydstogo = 10,
  season = 2023,
  game_id = "test_id"
)

ribbon_pred <- predict(rf_fit, new_data = ribbon_df, type = "prob") %>%
  bind_cols(ribbon_df) %>%
  rename(pred_wp = .pred_1)

ggplot(ribbon_pred,
       aes(game_seconds_remaining, pred_wp,
           color = factor(score_differential),
           fill = factor(score_differential))) +
  geom_line(size = 1) +
  facet_wrap(~yardline_100, labeller = label_both) +
  scale_x_reverse() +
  labs(title = "Win Probabilities with Confidence Over Time",
       x = "Seconds Remaining", y = "Win Probability") +
  theme_minimal()
