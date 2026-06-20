#############
### SETUP ###
#############

install.packages(c("bayesplot", "ggplot2", "rstan", "tidyverse"))
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

nfl = read_csv("/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/14_nfl-games.csv") |>
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

model = stan_model("/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/starter-code/14_nfl-model.stan")

###############################################
### FIT MODEL + AUTOMATIC DIAGNOSTICS FIXED ###
###############################################

fit = sampling(model, data = stan_data, chains = 4, iter = 2000, seed = 14)

# HMC diagnostics
rstan::check_hmc_diagnostics(fit)

# Automatically detect parameter names
param_names <- names(fit)
intercept_name <- param_names[grepl("beta0|b0|beta_0|intercept|home", param_names)]

# Print summary safely
print(fit, pars = c(intercept_name, "sigma_game", "sigma_team"),
      probs = c(0.025, 0.5, 0.975))

# ---- FIXED DIVERGENT COUNTER ----
sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)

divs <- sum(sapply(sampler_params, function(chain) {
  if (is.data.frame(chain)) {
    return(sum(chain$divergent__))
  } else {
    return(0)
  }
}))

cat("Divergent transitions:", divs, "\n")

# Trace plots
mcmc_trace(as.array(fit), pars = c(intercept_name, "sigma_game", "sigma_team"))

###############################
### EXTRACT POSTERIOR DRAWS ###
###############################

draws = rstan::extract(fit)

#############################################
### POSTERIOR MEANS + 95% CI LEADERBOARD ###
#############################################

strength_mean  = apply(draws$strength, 2, mean)
strength_lower = apply(draws$strength, 2, quantile, 0.025)
strength_upper = apply(draws$strength, 2, quantile, 0.975)

leaderboard = tibble(
  team = teams,
  mean = strength_mean,
  lower = strength_lower,
  upper = strength_upper
)

ggplot(leaderboard, aes(x = reorder(team, mean), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(x = "Team", y = "Posterior mean strength",
       title = "Posterior means and 95% credible intervals")

###############################################
### P(beta_DAL > beta_PHI | data) + RANKING ###
###############################################

dal = which(teams == "DAL")
phi = which(teams == "PHI")

p_dal_gt_phi = mean(draws$strength[, dal] > draws$strength[, phi])
cat("P(DAL > PHI | data):", p_dal_gt_phi, "\n")

# Probability each team is top 5
ranks = t(apply(draws$strength, 1, function(x) rank(-x, ties.method = "random")))
top_five_prob = colMeans(ranks <= 5)

top_five_tbl = tibble(team = teams, prob_top5 = top_five_prob) |>
  arrange(desc(prob_top5))

print(top_five_tbl)

# Highest posterior mean
team_highest_mean = leaderboard |> arrange(desc(mean)) |> slice(1)
print(team_highest_mean)

# Most likely to rank #1
prob_rank1 = colMeans(ranks == 1)
team_most_likely_first = tibble(team = teams, prob_rank1 = prob_rank1) |>
  arrange(desc(prob_rank1)) |> slice(1)
print(team_most_likely_first)

###########################################
### POSTERIOR PREDICTION (NEUTRAL SITE) ###
###########################################

y_future = rnorm(
  n = length(draws$sigma_game),
  mean = draws$strength[, dal] - draws$strength[, phi],
  sd   = draws$sigma_game
)

mean_y_future = mean(y_future)
pred_win_prob = mean(y_future > 0)
pp_interval = quantile(y_future, c(0.025, 0.975))

cat("Expected score diff:", mean_y_future, "\n")
cat("Predictive win prob:", pred_win_prob, "\n")
cat("95% predictive interval:", pp_interval, "\n")

# Compare to strength difference CI
strength_diff = draws$strength[, dal] - draws$strength[, phi]
cred_interval_diff = quantile(strength_diff, c(0.025, 0.975))
cat("95% CI for strength difference:", cred_interval_diff, "\n")

########################################################
### FIND SPREAD WHERE COVER PROBABILITY >= 0.55 (DAL) ###
########################################################

s_grid = seq(-10, 20, by = 0.5)
cover_prob = sapply(s_grid, function(s) mean(y_future > s))

spread_tbl = tibble(spread = s_grid, cover_prob = cover_prob)
spread_55 = spread_tbl |> filter(cover_prob >= 0.55) |> slice_tail(n = 1)

print(spread_55)

###########################################
### POSTERIOR PREDICTIVE CHECKS (y_rep) ###
###########################################

y_rep = draws$y_rep
obs_y = stan_data$y

# Distribution overlay
ppc_dens_overlay(y = obs_y, yrep = y_rep[1:100, ])

# Home win rate
home_win_obs = mean(obs_y > 0)
home_win_rep = apply(y_rep, 1, function(x) mean(x > 0))
cat("Observed home win rate:", home_win_obs, "\n")
cat("Replicated home win rate mean:", mean(home_win_rep), "\n")
cat("Replicated 95% interval:", quantile(home_win_rep, c(0.025, 0.975)), "\n")

# Blowouts (>= 21 points)
blowout_obs = mean(abs(obs_y) >= 21)
blowout_rep = apply(y_rep, 1, function(x) mean(abs(x) >= 21))
cat("Observed blowout rate:", blowout_obs, "\n")
cat("Rep blowout mean:", mean(blowout_rep), "\n")
cat("Rep blowout 95% interval:", quantile(blowout_rep, c(0.025, 0.975)), "\n")
