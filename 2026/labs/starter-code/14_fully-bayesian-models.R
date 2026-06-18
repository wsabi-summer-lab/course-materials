#######################
### Authors: RB, JP ###
#######################

#############
### SETUP ###
#############

# install.packages(c("bayesplot", "ggplot2", "rstan", "tidyverse"))
# Windows users: if rstan says additional tools are required, use cmdstanr
# instead. It has a clearer Windows toolchain setup:
# install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
# library(cmdstanr)
# cmdstanr::check_cmdstan_toolchain(fix = TRUE)
# cmdstanr::install_cmdstan()
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

nfl = read_csv("../data/14_nfl-games.csv") |>
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

model = stan_model("14_nfl-model.stan")

# TODO: fit with at least four chains and inspect diagnostics.
# fit = sampling(model, data = stan_data, chains = 4, iter = 2000, seed = 14)
# rstan::check_hmc_diagnostics(fit)

# TODO: extract posterior draws. Each row of draws$strength is one plausible
# football world after seeing the data.
# draws = rstan::extract(fit)

# dal = which(teams == "DAL")
# phi = which(teams == "PHI")
#
# TODO: estimate P(beta_DAL > beta_PHI | data) by counting posterior draws.
# mean(draws$strength[, dal] > draws$strength[, phi])
#
# TODO: compute each team's probability of ranking in the top five.
# ranks = t(apply(draws$strength, 1, function(x) rank(-x, ties.method = "random")))
# top_five_prob = colMeans(ranks <= 5)

# TODO: simulate a neutral-site Dallas vs Philadelphia game from each draw.
# y_future = rnorm(
#     n = length(draws$sigma_game),
#     mean = draws$strength[, dal] - draws$strength[, phi],
#     sd = draws$sigma_game
# )
# TODO: compare mean(y_future > 0) with mean(draws$strength[, dal] > draws$strength[, phi]).

# TODO: use draws$y_rep for posterior predictive checks.
# TODO: find a neutral-site spread with posterior predictive cover probability >= 0.55.
