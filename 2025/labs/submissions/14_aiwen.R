#############
### SETUP ###
#############

rm(list=ls())

# install.packages(c("ggplot2", "rstan", "tidyverse"))
library(ggplot2)
library(rstan)
library(tidyverse)
# library(Rtools)
# install.packages("Rtools")

# set seed
set.seed(14)

#################
### NFL GAMES ###
#################

# load NFL games data from 2018-2023
nfl_data = read_csv("../data/14_nfl-games.csv.gz")
# preview data
head(nfl_data)
names(nfl_data)

# make new column for season, where season 1 is 2018, etc.
nfl_data <- nfl_data %>%
  mutate(season_num = season - 2017)

# team indices
teams <- sort(unique(c(nfl_data$home_team, nfl_data$away_team)))
team_indices <- setNames(1:length(teams), teams)

# numeric indices for teams and outcome y
nfl_data <- nfl_data %>%
  mutate(
    H = team_indices[home_team],
    A = team_indices[away_team],
    y = pts_H_minus_A
  )

# stan data
stan_data <- list(
  N_games = nrow(nfl_data),
  N_teams = length(teams),
  N_seasons = max(nfl_data$season_num),
  y = nfl_data$y,
  home = nfl_data$H,
  away = nfl_data$A,
  season = nfl_data$season_num
)

model <- stan_model(file = "../submissions/14_aiwen.stan")

fit <- sampling(
  model,
  data = stan_data,
  iter = 2000,
  chains = 4,
  seed = 12345
)

fit

#########################
# part 2
posterior_samples <- rstan::extract(fit)

alpha_team_means <- apply(posterior_samples$alpha_team, 2, mean)
alpha_team_lower <- apply(posterior_samples$alpha_team, 2, quantile, probs = 0.025)
alpha_team_upper <- apply(posterior_samples$alpha_team, 2, quantile, probs = 0.975)

hfa_df <- data.frame(
  team = teams,
  mean = alpha_team_means,
  lower = alpha_team_lower,
  upper = alpha_team_upper
)

hfa_df <- hfa_df %>% arrange(mean) %>% mutate(team = factor(team, levels = team))

ggplot(hfa_df, aes(y = team, x = mean)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  labs(title = "Posterior Means and 95% Credible Intervals for Home Field Advantage",
       x = "Home Field Advantage", y = "Team") +
  theme_minimal()

#########################
# part 3

team_strength_means <- apply(posterior_samples$team_strength, 2, mean)
team_strength_lower <- apply(posterior_samples$team_strength, 2, quantile, probs = 0.025)
team_strength_upper <- apply(posterior_samples$team_strength, 2, quantile, probs = 0.975)

strength_df <- data.frame(
  team = teams,
  mean = team_strength_means,
  lower = team_strength_lower,
  upper = team_strength_upper
)

strength_df <- strength_df %>% arrange(mean) %>% mutate(team = factor(team, levels = team))

ggplot(strength_df, aes(y = team, x = mean)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  labs(title = "Posterior Means and 95% Credible Intervals for Team Strength",
       x = "Team Strength", y = "Team") +
  theme_minimal()
