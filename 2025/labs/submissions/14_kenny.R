library(ggplot2)
library(rstan)
library(tidyverse)
library(dplyr)

set.seed(14)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

nfl_data = read_csv("14_nfl-games.csv.gz")

head(nfl_data)

nfl_data <- nfl_data %>%
  mutate(season = as.numeric(substr(game_id, 1, 4)))

all_teams <- sort(unique(c(nfl_data$home_team, nfl_data$away_team)))

team_lookup <- tibble(
  team = all_teams,
  team_index = 1:length(all_teams)
)

nfl_data <- nfl_data %>%
  left_join(team_lookup, by = c("home_team" = "team")) %>%
  rename(H_index = team_index) %>%
  left_join(team_lookup, by = c("away_team" = "team")) %>%
  rename(A_index = team_index)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/submissions")

model = stan_model(file = "14_kenny.stan")
model

season_levels <- sort(unique(nfl_data$season))
season_index <- match(nfl_data$season, season_levels)

data_train = list(
  N_games = nrow(nfl_data) ,
  N_teams = length(unique(nfl_data$H_index)),
  N_seasons = length(season_levels),
  y = nfl_data$pts_H_minus_A, 
  H_index = nfl_data$H_index, 
  A_index = nfl_data$A_index, 
  season = season_index
)

fit = sampling(model, data = data_train , iter = 1500, chains = 1, seed = 12345)

fit

posterior_samples <- rstan::extract(fit)

n_teams <- dim(posterior_samples$alpha_team)[2]

summary_df <- data.frame(
  team_index = 1:n_teams,
  mean = apply(posterior_samples$alpha_team, 2, mean),
  lower = apply(posterior_samples$alpha_team, 2, quantile, 0.025),
  upper = apply(posterior_samples$alpha_team, 2, quantile, 0.975)
)

summary_df <- summary_df %>%
  left_join(team_lookup, by = "team_index")

ggplot(summary_df, aes(x = reorder(team, mean), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(
    title = "Posterior Means and 95% Credible Intervals for Team-Specific Home Field Advantage",
    x = "Team",
    y = "Home Field Advantage (alpha_team)"
  ) +
  theme_minimal()

