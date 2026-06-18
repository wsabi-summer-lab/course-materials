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

nfl = read_csv("/Users/maxgotuaco/Documents/GitHub/lab-materials/2026/labs/data/14_nfl-games.csv") |>
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

model = stan_model("/Users/maxgotuaco/Documents/GitHub/lab-materials/2026/labs/starter-code/14_nfl-model.stan")
#warning from stan: hash mismaths so recompiling; make sure Stan code ends with a blank line

# Fit with four chains
fit = sampling(model, data = stan_data, chains = 4, iter = 2000, seed = 14)

check_hmc_diagnostics(fit)         
get_num_divergent(fit)          

print(fit, pars = c("home_field", "sigma_game", "sigma_team"))

mcmc_trace(as.array(fit), pars = c("home_field", "sigma_game", "sigma_team"))
#Rhat -


# Posterior draws.
draws = rstan::extract(fit)

# neutral-site matchup
A = which(teams == "DAL")
B = which(teams == "PHI")

diff_strength = draws$strength[, A] - draws$strength[, B]

n_draws  = length(draws$sigma_game)
y_future = rnorm(n_draws, mean = diff_strength, sd = draws$sigma_game)

# Expected score differential
mean(y_future)

#Predictive win probability for A
mean(y_future > 0)

#95% posterior predictive interval for the future game
quantile(y_future, c(0.025, 0.975))

# Compare with the 95% credible interval for the difference in team strengths.
# This interval is much narrower: it omits single-game noise (sigma_game).
quantile(diff_strength, c(0.025, 0.975))

# Sportsbook spread: A is favored by s points and covers when y_future > s.
# P(cover) = P(y_future > s) decreases as s grows, so the largest s with
# P(cover) >= 0.55 is the 45th percentile of y_future.
s_max = quantile(y_future, 0.45)
s_max
mean(y_future > s_max)   # sanity check: should be ~0.55


y_obs = stan_data$y       # observed score differentials
y_rep = draws$y_rep       # rows = draws, cols = games


ppc_dens_overlay(y_obs, y_rep[1:100, ])

home_win = function(y) mean(y > 0)
ppc_stat(y_obs, y_rep, stat = "home_win")

blowout = function(y) mean(abs(y) >= 21)
ppc_stat(y_obs, y_rep, stat = "blowout")

rep_home_win = apply(y_rep, 1, home_win)
rep_blowout  = apply(y_rep, 1, blowout)

c(observed = home_win(y_obs), rep_mean = mean(rep_home_win))
c(observed = blowout(y_obs),  rep_mean = mean(rep_blowout))

mean(rep_home_win >= home_win(y_obs))
mean(rep_blowout  >= blowout(y_obs))
# - Systematic feature missed: the Normal likelihood has thin tails, so it
#   under-produces blowouts
# - omitted variables such as quarterback availability / rest

tight_code = "
data {
    int<lower=1> N_games;
    int<lower=2> N_teams;
    vector[N_games] y;
    array[N_games] int<lower=1, upper=N_teams> H;
    array[N_games] int<lower=1, upper=N_teams> A;
}
parameters {
    real home_field;
    vector[N_teams] strength_raw;
    real<lower=0> sigma_team;
    real<lower=0> sigma_game;
}
transformed parameters {
    vector[N_teams] strength;
    strength = sigma_team * (strength_raw - mean(strength_raw));
}
model {
    home_field ~ normal(0, 5);
    strength_raw ~ std_normal();
    sigma_team ~ normal(0, 2);   // TIGHTER than original normal(0, 7)
    sigma_game ~ normal(0, 20);
    y ~ normal(home_field + strength[H] - strength[A], sigma_game);
}
"

model_tight = stan_model(model_code = tight_code)
fit_tight   = sampling(model_tight, data = stan_data, chains = 4, iter = 2000, seed = 14)
draws_tight = rstan::extract(fit_tight)

# Compare leaderboards: a tighter prior pools strengths toward 0 (league average).
compare = tibble(
    team       = teams,
    mean_orig  = colMeans(draws$strength),
    mean_tight = colMeans(draws_tight$strength)
) |> arrange(desc(mean_orig))
print(compare)

# Spread of team strengths under each prior (smaller spread = more pooling).
c(orig = sd(colMeans(draws$strength)), tight = sd(colMeans(draws_tight$strength)))

# Extending model: Let each team's strength evolve across seasons s via
#   beta[j, s] ~ normal(gamma * beta[j, s-1], sigma_season),
# so each season's prior is last season's (decayed) posterior: gamma in (0,1)
# carries strength forward toward the mean, sigma_season sets year-to-year drift.
# Fit all seasons jointly instead of one independent fit per season.

