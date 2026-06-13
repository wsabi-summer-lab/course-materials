#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "readr", "splines", "tidyverse"))
library(ggplot2)
library(nnet)
library(readr)
library(splines)
library(tidyverse)

# set seed
set.seed(9)

##############################
### PART 1: EXPECTED POINTS ###
##############################

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
nfl_data = read_csv("../data/09_expected-points.csv", show_col_types = FALSE)

nfl_model_data = nfl_data |>
  mutate(
    pts_next_score_factor = factor(pts_next_score)
  )

# Task 1:
# - Refit your preferred expected-points model from Lab 5
# - A concrete default is:
#   pts_next_score ~ bs(yardline_100, df = 6) +
#                    factor(down) +
#                    log(ydstogo) +
#                    bs(half_seconds_remaining, df = 5)
# - Convert fitted class probabilities into expected points

# You will probably want helper objects like these:
score_values = sort(unique(nfl_data$pts_next_score))
game_ids = unique(nfl_data$game_id)

# Default model scaffold. You may modify this if you choose a different EP model.
ep_formula = pts_next_score_factor ~
  bs(yardline_100, df = 6) +
  factor(down) +
  log(ydstogo) +
  bs(half_seconds_remaining, df = 5)

# Target state for the bootstrap study:
target_state = tibble(
  yardline_100 = 35,
  down = 1,
  ydstogo = 10,
  half_seconds_remaining = 1800
)

# If your preferred model also uses posteam_spread, add:
# target_state = target_state |>
#     mutate(posteam_spread = 0)

# Optional helper:
# - Write a function that takes a matrix/data frame of predicted class probabilities
#   and converts it to expected points using score_values
expected_points_from_probs = function(prob_matrix, score_values) {
  prob_df = as_tibble(prob_matrix)
  
  # TODO: make sure the probability columns line up with score_values.
  # Hint: columns from predict(..., type = "probs") are class labels.
  
  # TODO: return the expected points for each row:
  # rowSums(probabilities * score_values)
}

# Task 2:
# - Decide which bootstrap variation is most appropriate here:
#   * observation bootstrap
#   * cluster bootstrap
#   * parametric bootstrap
#   * residual bootstrap
# - State your choice in comments
# - Explain why it matches the dependence structure of this dataset

# The grouping variable that matters here is:
# game_id

# Task 3:
# - Implement your chosen bootstrap with at least B = 200 resamples
# - For each resample:
#   * create a bootstrap dataset
#   * refit the EP model
#   * recompute expected points at target_state

B = 200

bootstrap_ep = rep(NA_real_, B)

# If you use a cluster bootstrap, you will likely want to:
# - sample game_ids with replacement
# - rebuild the bootstrap dataset by binding together all rows
#   from each sampled game

for (b in seq_len(B)) {
  # TODO: sample games or rows, depending on your bootstrap choice.
  # sampled_game_ids = sample(game_ids, size = length(game_ids), replace = TRUE)
  
  # TODO: build the bootstrap dataset.
  # boot_data = ...
  
  # TODO: fit the model on boot_data.
  # boot_model = multinom(ep_formula, data = boot_data, trace = FALSE)
  
  # TODO: predict class probabilities at target_state.
  # boot_probs = predict(boot_model, newdata = target_state, type = "probs")
  
  # TODO: convert probabilities to expected points and store the result.
  # bootstrap_ep[b] = expected_points_from_probs(boot_probs, score_values)
}

# Task 4:
# - Store the bootstrap estimates in a vector
# - Make a plot of the bootstrap distribution

# Task 5:
# - Compute:
#   * the original fitted expected-points estimate
#   * the bootstrap standard error
#   * the 95% percentile interval

# Task 6:
# - In comments, explain why a naive row-by-row observation bootstrap
#   is less appropriate for this dataset

################################
### PART 2: NBA FREE THROWS ####
################################

nba_players = read_delim(
  "../data/09_nba-free-throws.csv",
  delim = ";",
  show_col_types = FALSE
)

# Task 1:
# - Recreate the player-level free-throw dataset from Lab 8
# - Include:
#   * Player
#   * FT_total = approximate total free throws made across the season
#   * FTA_total = approximate total free throws attempted across the season
#   * FT_percent
# - Basketball Reference reports FT and FTA as per-game values in this table,
#   so convert them to approximate totals using G before treating them as counts
# - Filter to players with at least 25 approximate total free-throw attempts

nba_free_throws = nba_players |>
  mutate(
    FT_total = round(FT * G),
    FTA_total = round(FTA * G),
    FT_percent = FT_total / FTA_total
  )

# TODO: decide how to handle multi-team players.
# Option 1: keep only rows where Tm == "TOT" for players who changed teams.
# Option 2: aggregate team rows yourself.
#
# nba_player_level = nba_free_throws |>
#     ... |>
#     filter(FTA_total >= 25)

# Task 2:
# - For each player, construct a 95% bootstrap confidence interval
#   for free-throw percentage
# - Overlay the bootstrap intervals on your Lab 8 player plot
# - Compare bootstrap, Wald, and Agresti-Coull intervals

# You may want helper functions for:
# - one bootstrap resample of a player's free throws
# - a percentile interval from bootstrap draws

bootstrap_ft_percent = function(ft_made, ft_attempted, B = 1000) {
  # TODO: create B bootstrap resamples for one player.
  # Hint: represent the season as a vector with ft_made ones and
  # ft_attempted - ft_made zeros, then sample with replacement.
}

percentile_interval = function(bootstrap_draws, level = 0.95) {
  alpha = 1 - level
  quantile(
    bootstrap_draws,
    probs = c(alpha / 2, 1 - alpha / 2),
    na.rm = TRUE
  )
}

# Task 3:
# - Revisit the Lab 8 simulation study
# - Add bootstrap confidence intervals to the same coverage framework
# - Plot bootstrap coverage probability against p
# - Compare to Wald and Agresti-Coull

# Useful objects from Lab 8:
sample_sizes = c(10, 50, 100, 250, 500, 1000)
p_grid = seq(0, 1, length.out = 1000)
M = 100
z_975 = qnorm(0.975)

bootstrap_coverage = tibble()

# TODO: extend your Lab 8 simulation loop.
# For each simulated Binomial(n, p) count, compute:
# - Wald interval
# - Agresti-Coull interval
# - bootstrap percentile interval
# Then summarize coverage by method, n, and p.
