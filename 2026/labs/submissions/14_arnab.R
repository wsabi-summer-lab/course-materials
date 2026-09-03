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
fit = sampling(model, data = stan_data, chains = 4, iter = 2000, seed = 14)
rstan::check_hmc_diagnostics(fit)

#no issues from diagnostic.

# TODO: extract posterior draws. Each row of draws$strength is one plausible
# football world after seeing the data.
draws = rstan::extract(fit)

dal = which(teams == "DAL")
phi = which(teams == "PHI")
#
# TODO: estimate P(beta_DAL > beta_PHI | data) by counting posterior draws.
p_dal_phi <- mean(draws$strength[, dal] > draws$strength[, phi])
#
# TODO: compute each team's probability of ranking in the top five.
ranks = t(apply(draws$strength, 1, function(x) rank(-x, ties.method = "random")))
top_five_prob = colMeans(ranks <= 5)
top_five_prob

# TODO: simulate a neutral-site Dallas vs Philadelphia game from each draw.
y_future = rnorm(n = length(draws$sigma_game), mean = draws$strength[, dal] - draws$strength[, phi],sd = draws$sigma_game)
print(mean(y_future))
ppi_95 = quantile(y_future, c(0.025, 0.975))

ppi_95
future_mean = mean(y_future > 0) 
mean_other = mean(draws$strength[, dal] > draws$strength[, phi])
future_mean
mean_other 
#the posterior draws mean seems very inaccurate, with a 97.3% win prob, while the future neab, of 69.5% is more plausible. 

# TODO: use draws$y_rep for posterior predictive checks.


y_obs = nfl$pts_H_minus_A

hist(y_obs,
     breaks = 30,
     main = "Observed Score Differentials",
     xlab = "Home - Away Points")

hist(as.vector(draws$y_rep),
     breaks = 30,
     main = "Replicated Score Differentials",
     xlab = "Home - Away Points")

mean(y_obs)
mean(draws$y_rep)

sd(y_obs)
sd(as.vector(draws$y_rep))


obs_home_win = mean(y_obs > 0)

rep_home_win = rowMeans(draws$y_rep > 0)

obs_home_win
mean(rep_home_win)

hist(rep_home_win,
     main = "Posterior Predictive Home Win Rate",
     xlab = "Home Win Rate")
abline(v = obs_home_win, col = "red", lwd = 2)


obs_blowout = mean(abs(y_obs) >= 21)

rep_blowout = rowMeans(abs(draws$y_rep) >= 21)

obs_blowout
mean(rep_blowout)

hist(rep_blowout,
     main = "Posterior Predictive Blowout Rate (>=21 pts)",
     xlab = "Proportion of Games")
abline(v = obs_blowout, col = "red", lwd = 2)


# Posterior predictive p-values
ppp_home_win = mean(rep_home_win >= obs_home_win)
ppp_blowout = mean(rep_blowout >= obs_blowout)

ppp_home_win
ppp_blowout

#the predictive blowout rates and home win rates look too high, Two variables that could be important are weather conditions, as that affects home advantage, as well as injuries.
# TODO: find a neutral-site spread with posterior predictive cover probability >= 0.55.
spreads = seq(-20, 20, by = 0.5)

pred_prob = sapply(spreads, function(spread) {
  y_future = rnorm(
    n = nrow(draws$strength),
    mean = draws$strength[, dal] - draws$strength[, phi],
    sd = draws$sigma_game
  )
  
  mean(y_future > spread)
})

results = data.frame(
  spread = spreads,
  prob = pred_prob
)

results[results$prob >= 0.55, ]

#largest spread is 5. It doffers from asking whether Team A is just stronger because it contextualizes possibilities at a +50 probability.