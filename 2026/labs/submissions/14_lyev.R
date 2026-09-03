#######################
### Authors: RB, JP ###
#######################

#############
### SETUP ###
#############

# install.packages(c("bayesplot", "ggplot2", "rstan", "tidyverse"))
library(bayesplot)
library(ggplot2)
library(rstan)
library(tidyverse)

set.seed(14)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#################
### NFL GAMES ###
#################

nfl = read_csv("../data/14_nfl-games.csv.gz") |>
  filter(season_type == "REG", season == 2023)

teams = sort(unique(c(nfl$home_team, nfl$away_team)))
team_index = tibble(team = teams, index = seq_along(teams))

nfl = nfl |>
  left_join(team_index, by = c("home_team" = "team")) |>
  rename(H_index = index) |>
  left_join(team_index, by = c("away_team" = "team")) |>
  rename(A_index = index)

stan_data = list(
  N_games = nrow(nfl),
  N_teams = length(teams),
  y = nfl$pts_H_minus_A,
  H = nfl$H_index,
  A = nfl$A_index
)

model = stan_model("14_nfl-model.stan")


#Task 1:


fit = sampling(model, data = stan_data, chains = 4, iter = 2000, seed = 14)
rstan::check_hmc_diagnostics(fit) #no issues in diagnostics
#0 divergent transitions
#No warnings from stan


#Key paramters
#HFA: 2.58, n_eff = 4600
#sigma_team: 4.85, n_eff = 1307
#sigma_game: 12.85, n_eff = 3475

mcmc_trace(as.array(fit), pars = "sigma_game")
mcmc_trace(as.array(fit), pars = "sigma_team")



#Task 2:

# TODO: extract posterior draws. Each row of draws$strength is one plausible
# football world after seeing the data.
draws = rstan::extract(fit)

dal = which(teams == "DAL")
phi = which(teams == "PHI")

# TODO: estimate P(beta_DAL > beta_PHI | data) by counting posterior draws.
mean(draws$strength[, dal] > draws$strength[, phi])
#0.96575 odds for Dallas

# TODO: compute each team's probability of ranking in the top five.
ranks = t(apply(draws$strength, 1, function(x) rank(-x, ties.method = "random")))
top_five_prob = colMeans(ranks <= 5)

#Highest posterior mean
posterior_means = colMeans(draws$strength)
best_mean_team = teams[which.max(posterior_means)]
best_mean_team # Baltimore

#Probs of being 1st
first_place_prob = colMeans(ranks == 1)
best_first_place_team = teams[which.max(first_place_prob)]
best_first_place_team #Also Baltimore


#Task 3:

# TODO: simulate a neutral-site Dallas vs Philadelphia game from each draw.
y_future = rnorm(
    n = length(draws$sigma_game),
    mean = draws$strength[, dal] - draws$strength[, phi],
    sd = draws$sigma_game
)

expected_diff = mean(y_future)
expected_diff #6.57 in favor of Dallas

p_dal_win = mean(y_future > 0)
p_dal_win #0.685

predictive_interval = quantile(y_future, probs = c(0.025, 0.975))
predictive_interval #-19.25 to 33.64

strength_diff = draws$strength[, dal] - draws$strength[, phi]
credible_interval = quantile(strength_diff, probs = c(0.025, 0.975))
credible_interval #-0.431 to 13.948

#The predictive interval is much wider because variance is larger for a single prediction


#Task 4:

# Posterior draws
strength = draws$strength
sigma_game = draws$sigma_game

# Number of posterior draws
S = length(sigma_game)

# Replicated score differentials for each game
y_rep = matrix(NA, nrow = S, ncol = nrow(nfl))

for (s in 1:S) {
  mu_s = strength[s, nfl$H_index] - strength[s, nfl$A_index]
  y_rep[s, ] = rnorm(nrow(nfl), mean = mu_s, sd = sigma_game[s])
}
# y_rep


obs = nfl$pts_H_minus_A
rep_mean = colMeans(y_rep)

df_plot = tibble(
  observed = obs,
  replicated = rep_mean
)

ggplot(df_plot, aes(x = observed)) +
  geom_histogram(fill = "blue", alpha = 0.5, bins = 30) +
  geom_histogram(aes(x = replicated), fill = "red", alpha = 0.5, bins = 30)


obs_blowout_rate = mean(abs(obs) >= 21) #0.165
rep_blowout_rate = mean(abs(y_rep) >= 21) #0.147
rep_blowout_rate_draws = rowMeans(abs(y_rep) >= 21)
quantile(rep_blowout_rate_draws, c(0.025, 0.5, 0.975))

#The model consistently predicts more blowouts than there actually ends up being

#Football variable 1: teams try less hard once they're up by a lot, meaning blowouts are less likely to happen
#Football variable 2: schedules are designed for good teams to play more good teams, meaning these blowouts are less likely to happen in real life


#Task 5:


#Make sigma team prior substantially tighter, let's say N(0,2)

#This increases shrinkage across the league and makes the teams in the leaderboard a lot closer together

#A dynamic model would account for team evolution across seasons, and give each team its own prior going into a season


