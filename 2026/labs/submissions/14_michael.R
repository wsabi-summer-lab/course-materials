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

#Task 1
fit = sampling(model, data = stan_data, chains = 4, iter = 2000, seed = 14)

# see what the Stan file actually calls things
print(fit, pars = c("lp__"), include = FALSE)
names(fit)

# warnings + divergences
check_hmc_diagnostics(fit)
sum(get_divergent_iterations(fit))

# Rhat / ESS — use the scalar param names as they appear in names(fit)
pars_to_check = intersect(c("beta0", "beta_0", "alpha", "home_adv",
                            "sigma_game", "sigma_team", "tau", "tau_team"),
                          names(fit))
summary(fit, pars = pars_to_check)$summary[, c("mean", "n_eff", "Rhat")]

# trace plots
traceplot(fit, pars = pars_to_check, inc_warmup = FALSE)


#Task 2

draws = rstan::extract(fit)

# posterior means + 95% credible intervals per team
strength_summary = tibble(
  team = teams,
  mean = colMeans(draws$strength),
  lower = apply(draws$strength, 2, quantile, 0.025),
  upper = apply(draws$strength, 2, quantile, 0.975)
) |> arrange(desc(mean))

ggplot(strength_summary, aes(x = reorder(team, mean), y = mean)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  coord_flip() +
  labs(x = NULL, y = "Team strength (posterior)")

# P(beta_DAL > beta_PHI | data)
dal = which(teams == "DAL")
phi = which(teams == "PHI")
mean(draws$strength[, dal] > draws$strength[, phi])

# top-five probability per team
ranks = t(apply(draws$strength, 1, function(x) rank(-x, ties.method = "random")))
top_five_prob = tibble(team = teams, p_top5 = colMeans(ranks <= 5)) |>
  arrange(desc(p_top5))
top_five_prob

# highest posterior mean vs most likely to rank #1
best_mean_team = strength_summary$team[1]
rank1_prob = tibble(team = teams, p_rank1 = colMeans(ranks == 1)) |>
  arrange(desc(p_rank1))
best_mean_team
rank1_prob$team[1]

#1. 0.97375
#2. resutls printed.
#3. BAL for boht, but they do not have to be the same. Highest posterior mean asks for accross all 4000 posterior draw who has the largest average strength whereas most likely to rnak 1 is asking for in each round what team finishecd first the most.


#Task 3
# choose a neutral-site matchup (DAL vs PHI, continuing from Task 2)
dal = which(teams == "DAL")
phi = which(teams == "PHI")

# strength difference (credible interval) — no game noise, "who's better"
strength_diff = draws$strength[, dal] - draws$strength[, phi]
mean(strength_diff)
quantile(strength_diff, c(0.025, 0.975))

# posterior predictive simulation — adds game-level noise, "who wins this game"
y_future = rnorm(
  n = length(draws$sigma_game),
  mean = strength_diff,
  sd = draws$sigma_game
)

# expected score differential
mean(y_future)

# predictive win probability (DAL favored if diff > 0)
mean(y_future > 0)

# 95% posterior predictive interval
quantile(y_future, c(0.025, 0.975))

# compare width: predictive interval vs credible interval for strength diff
cat("Credible interval (strength only) width:",
    diff(quantile(strength_diff, c(0.025, 0.975))), "\n")
cat("Predictive interval (strength + game noise) width:",
    diff(quantile(y_future, c(0.025, 0.975))), "\n")

# find largest spread s such that P(y_future > s) >= 0.55
s_grid = seq(0, 15, by = 0.01)
cover_prob = sapply(s_grid, function(s) mean(y_future > s))
s_max = max(s_grid[cover_prob >= 0.55])
s_max

#The interval for a single game of DAL beating PHIL is a lot smaller than the interval of the true skill difference of PHIL being better because a single game has so much variance and rnadom outcome...the better team does not always win.
#s=4.93 because of all the game noise even though they are favored by 6.6 

#Task 4

# observed data
y_obs = nfl$pts_H_minus_A

# replicated data: draws$y_rep is 4000 x N_games matrix
y_rep = draws$y_rep

# 1. score-differential distributions: observed vs replicated
ppc_dens_overlay(y_obs, y_rep[1:200, ])  # subsample 200 draws for speed

# 2. home win rate: observed vs replicated
home_win_obs = mean(y_obs > 0)
home_win_rep = apply(y_rep, 1, function(x) mean(x > 0))

home_win_obs
mean(home_win_rep)
quantile(home_win_rep, c(0.025, 0.975))

ggplot(tibble(home_win_rep), aes(x = home_win_rep)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = home_win_obs, color = "red", linewidth = 1) +
  labs(title = "Replicated home win rate vs observed (red line)")

# 3. rate of games decided by >= 21 points: observed vs replicated
blowout_obs = mean(abs(y_obs) >= 21)
blowout_rep = apply(y_rep, 1, function(x) mean(abs(x) >= 21))

blowout_obs
mean(blowout_rep)
quantile(blowout_rep, c(0.025, 0.975))

ggplot(tibble(blowout_rep), aes(x = blowout_rep)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = blowout_obs, color = "red", linewidth = 1) +
  labs(title = "Replicated blowout rate vs observed (red line)")

#One systematic miss is how football points are scored (3, 6, 7 most typically) so the data will cluster more and the model assums a normal smooth distibution but that way of scoring doesnt often produce differencials like 5 but does a lot of 3, so its not really smooth

#Two ommitted football variables are short weeks and bye weeks , and player injury espcially those around significant positions

#task 5

stan_code = readLines("14_nfl-model.stan")

writeLines(gsub("sigma_team ~ normal\\(0, 7\\);", "sigma_team ~ normal(0, 1);", stan_code), "14_nfl-model_tight.stan")
writeLines(gsub("sigma_team ~ normal\\(0, 7\\);", "sigma_team ~ normal(0, 20);", stan_code), "14_nfl-model_wide.stan")

model_tight = stan_model("14_nfl-model_tight.stan", auto_write = FALSE)
fit_tight = sampling(model_tight, data = stan_data, chains = 4, iter = 2000, seed = 14)

model_wide = stan_model("14_nfl-model_wide.stan", auto_write = FALSE)
fit_wide = sampling(model_wide, data = stan_data, chains = 4, iter = 2000, seed = 14)

draws_tight = rstan::extract(fit_tight)
draws_wide  = rstan::extract(fit_wide)

leaderboard_compare = tibble(
  team = teams,
  original = colMeans(draws$strength),
  tight    = colMeans(draws_tight$strength),
  wide     = colMeans(draws_wide$strength)
) |> arrange(desc(original))

leaderboard_compare

tibble(
  prior = c("original", "tight", "wide"),
  sigma_team_mean = c(mean(draws$sigma_team), mean(draws_tight$sigma_team), mean(draws_wide$sigma_team)),
  strength_range  = c(diff(range(colMeans(draws$strength))),
                      diff(range(colMeans(draws_tight$strength))),
                      diff(range(colMeans(draws_wide$strength))))
)

#if we tighten the model its basically saying we dont think that teams vary that much and that woudl cause our best team to be pulled towards the mean the most and same as the worst team. IF its a wide prior its the opposite.
#we could use one season ratings to be a B in the next season