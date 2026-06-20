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

nfl = read_csv("C:/Users/sundw/downloads/14_nfl-games.csv") |>
    filter(season_type == "REG", season == 2023)

teams = sort(unique(c(nfl$home_team, nfl$away_team)))
team_index = tibble(team = teams, index = seq_along(teams))

nfl = nfl |>
    left_join(team_index, by = c("home_team" = "team")) |>
    rename(H = index) |>
    left_join(team_index, by = c("away_team" = "team")) |>
    rename(A = index)

stan_data = list(
    N_games = nrow(nfl),
    N_teams = length(teams),
    y = nfl$pts_H_minus_A,
    H = nfl$H,
    A = nfl$A
)

model = stan_model("C:/Users/sundw/Downloads/14_nfl-model.stan")

# TODO: fit with at least four chains and inspect diagnostics.
fit = sampling(model, data = stan_data, chains = 4, iter = 2000, seed = 14)
rstan::check_hmc_diagnostics(fit)

# There are 0 of 4000 iterations with divergences, 0 of 4000 iterations with max tree depth of 10,
# and no pathological behavior.

# TODO: extract posterior draws. Each row of draws$strength is one plausible
# football world after seeing the data.
draws = rstan::extract(fit)

dal = which(teams == "DAL")
phi = which(teams == "PHI")

# TODO: estimate P(beta_DAL > beta_PHI | data) by counting posterior draws.
mean(draws$strength[, dal] > draws$strength[, phi])

# mean = 0.97375

# TODO: compute each team's probability of ranking in the top five.
ranks = t(apply(draws$strength, 1, function(x) rank(-x, ties.method = "random")))
top_five_prob = colMeans(ranks <= 5)

# TODO: simulate a neutral-site Dallas vs Philadelphia game from each draw.
y_future = rnorm(
    n = length(draws$sigma_game),
    mean = draws$strength[, dal] - draws$strength[, phi],
    sd = draws$sigma_game
)

# TODO: compare mean(y_future > 0) with mean(draws$strength[, dal] > draws$strength[, phi]).
mean(y_future > 0) # 0.68925
mean(draws$strength[, dal] > draws$strength[, phi]) # 0.97375


# TODO: use draws$y_rep for posterior predictive checks.
# TODO: find a neutral-site spread with posterior predictive cover probability >= 0.55.

y_obs = nfl$pts_H_minus_A
y_rep = draws$y_rep  # matrix: n_draws x N_games

# ---- 1. Score-differential distributions ----
# Overlay observed density against a sample of replicated datasets
ppc_dens_overlay(y_obs, y_rep[sample(nrow(y_rep), 50), ])

# ---- 2. Home win rates ----
home_win_obs = mean(y_obs > 0)
home_win_rep = apply(y_rep, 1, function(y) mean(y > 0))

home_win_obs
mean(home_win_rep)
ggplot(tibble(x = home_win_rep), aes(x)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = home_win_obs, color = "red", linewidth = 1) +
  labs(title = "Posterior predictive home win rate", x = "Home win rate")

# Bayesian p-value: how extreme is the observed value relative to replicates?
mean(home_win_rep >= home_win_obs)

# ---- 3. Rate of blowouts (|margin| >= 21) ----
blowout_obs = mean(abs(y_obs) >= 21)
blowout_rep = apply(y_rep, 1, function(y) mean(abs(y) >= 21))

blowout_obs
mean(blowout_rep)
ggplot(tibble(x = blowout_rep), aes(x)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = blowout_obs, color = "red", linewidth = 1) +
  labs(title = "Posterior predictive blowout rate", x = "P(|margin| >= 21)")

mean(blowout_rep >= blowout_obs)

y_future = rnorm(
  n = length(draws$sigma_game),
  mean = draws$strength[, dal] - draws$strength[, phi],
  sd = draws$sigma_game
)

cover_prob = function(s) mean(y_future > s)

s_grid = seq(-10, 10, by = 0.1)
cover_at_s = sapply(s_grid, cover_prob)

# largest s with cover probability >= 0.55
s_max = max(s_grid[cover_at_s >= 0.55])
s_max

ggplot(tibble(s = s_grid, p = cover_at_s), aes(s, p)) +
  geom_line() +
  geom_hline(yintercept = 0.55, linetype = "dashed", color = "red") +
  geom_vline(xintercept = s_max, linetype = "dashed", color = "blue") +
  labs(x = "Spread s", y = "P(Team A covers)",
       title = "Posterior predictive cover probability vs. spread")
