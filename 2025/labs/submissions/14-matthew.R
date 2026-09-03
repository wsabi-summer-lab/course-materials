#############
### SETUP ###
#############

# Load required libraries
library(cmdstanr)
library(tidyverse)
library(ggplot2)

# Set seed for reproducibility
set.seed(14)

##############################
### LOAD & PREPARE DATA ###
##############################

# Read NFL game data
nfl_data <- read_csv("../data/14_nfl-games.csv.gz")

# Create a unique sorted list of teams
teams <- sort(unique(c(nfl_data$home_team, nfl_data$away_team)))
team_indices <- tibble(team = teams, id = 1:length(teams))

# Add numeric indices for modeling
nfl_data <- nfl_data %>%
  mutate(season_index = season - min(season) + 1) %>%
  left_join(team_indices, by = c("home_team" = "team")) %>%
  rename(home_index = id) %>%
  left_join(team_indices, by = c("away_team" = "team")) %>%
  rename(away_index = id)

# Format data list for Stan
data_train <- list(
  N_games = nrow(nfl_data),
  N_teams = length(teams),
  N_seasons = max(nfl_data$season_index),
  y = nfl_data$pts_H_minus_A,
  H = nfl_data$home_index,
  A = nfl_data$away_index,
  S = nfl_data$season_index
)

###########################
### COMPILE & FIT MODEL ###
###########################

# Compile the Stan model
model <- cmdstan_model("../submissions/spivey.stan")

# Fit model using MCMC
fit <- model$sample(
  data = data_train,
  seed = 12345,
  chains = 1,
  iter_sampling = 1000,
  iter_warmup = 500
)

#########################
### INSPECT RESULTS ###
#########################

# Print summary of key parameters
fit$summary(variables = c("beta_0", "sigma_games", "sigma_teams", "sigma_seasons", "gamma"))

# Extract posterior draws for alpha (team HFA vector)
alpha_draws <- fit$draws("alpha")  # dimensions: iterations x chains x N_teams

# Convert to a matrix: iterations x N_teams (combine chains if needed)
library(posterior)
alpha_mat <- as_draws_matrix(alpha_draws)

# Summarize: get mean HFA per team
alpha_means <- colMeans(alpha_mat)

# Put into a tibble with team names
team_hfa <- tibble(
  team = teams,
  hfa = alpha_means
) %>%
  arrange(desc(hfa))  # highest HFA at top

print(team_hfa)

library(tibble)
library(dplyr)

# Extract posterior draws for alpha (team-specific HFA vector)
alpha_draws <- fit$draws("alpha")  # dimensions: iterations x chains x N_teams

# Convert to matrix: iterations x N_teams (flatten chains)
alpha_mat <- as_draws_matrix(alpha_draws)  # combines chains

# Calculate mean HFA per team
alpha_means <- colMeans(alpha_mat)

# Create tibble with team names and HFA estimates
team_hfa <- tibble(
  team = teams,
  home_field_advantage = alpha_means
) %>%
  arrange(desc(home_field_advantage))  # sort descending: best HFA at top

print(team_hfa)
