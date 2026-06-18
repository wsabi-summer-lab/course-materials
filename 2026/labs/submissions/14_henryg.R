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
# TODO: fit with at least four chains and inspect diagnostics.


fit = sampling(model, data = stan_data, chains = 4, iter = 2000, seed = 14)
rstan::check_hmc_diagnostics(fit)

key_pars = c("home_field", "sigma_game", "sigma_team")  # home_field = beta0

# 2. Rhat and effective sample size
summ = summary(fit, pars = c(key_pars, "strength"))$summary
print(round(summ[key_pars, c("mean", "se_mean", "sd", "n_eff", "Rhat")], 3))

# worst-case across the 32 team strengths (the usual trouble spots)
srows = grep("^strength\\[", rownames(summ))
cat("Max Rhat (strength): ", max(summ[srows, "Rhat"]),  "\n")
cat("Min n_eff (strength):", min(summ[srows, "n_eff"]), "\n")

# 3. Divergent transitions (post-warmup, summed over chains)
sp = rstan::get_sampler_params(fit, inc_warmup = FALSE)
cat("Divergent transitions:", sum(sapply(sp, function(x) sum(x[, "divergent__"]))), "\n")

# 4. Trace plots for beta0 (home_field), sigma_game, sigma_team
mcmc_trace(as.array(fit, pars = key_pars), pars = key_pars) +
  ggtitle("Trace plots: home_field (beta0), sigma_game, sigma_team")





#Task 2
# TODO: extract posterior draws. Each row of draws$strength is one plausible
# football world after seeing the data.
# draws = rstan::extract(fit)

# dal = which(teams == "DAL")
# phi = which(teams == "PHI")
#


draws = rstan::extract(fit)

# Posterior means + 95% CIs for every team's strength
strength_summary = tibble(
  team  = teams,
  mean  = colMeans(draws$strength),
  lower = apply(draws$strength, 2, quantile, 0.025),
  upper = apply(draws$strength, 2, quantile, 0.975)
) |>
  arrange(desc(mean)) |>
  mutate(team = factor(team, levels = team))

ggplot(strength_summary, aes(x = mean, y = team)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0) +
  geom_point() +
  labs(x = "Team strength (points)", y = NULL,
       title = "Posterior team strengths, 2023 (mean ± 95% CI)") +
  theme_minimal()

# 1. P(beta_DAL > beta_PHI | data)
dal = which(teams == "DAL"); phi = which(teams == "PHI")
p_dal_gt_phi = mean(draws$strength[, dal] > draws$strength[, phi])

# 2. Each team's probability of being top five
ranks = t(apply(draws$strength, 1, function(x) rank(-x, ties.method = "random")))
top_five_prob = tibble(team = teams, p_top5 = colMeans(ranks <= 5)) |>
  arrange(desc(p_top5))

# 3. Highest posterior mean vs most likely to rank first
highest_mean      = teams[which.max(colMeans(draws$strength))]
p_rank1           = colMeans(ranks == 1)
most_likely_first = teams[which.max(p_rank1)]

cat("P(DAL > PHI):", round(p_dal_gt_phi, 3), "\n")
print(top_five_prob, n = 32)
cat("Highest posterior mean:", highest_mean,
    "| Most likely #1:", most_likely_first, "\n")






#Task 3
n_draws = length(draws$sigma_game)
y_future = rnorm(n_draws,
                 mean = draws$strength[, dal] - draws$strength[, phi],
                 sd   = draws$sigma_game)

exp_diff = mean(y_future)
win_prob = mean(y_future > 0)
pred_int = quantile(y_future, c(0.025, 0.975))

strength_diff = draws$strength[, dal] - draws$strength[, phi]
cred_int = quantile(strength_diff, c(0.025, 0.975))

cat("Expected differential (DAL - PHI):", round(exp_diff, 2), "\n")
cat("Predictive win prob (DAL):        ", round(win_prob, 3), "\n")
cat("95% predictive interval:", round(pred_int, 1), "\n")
cat("95% credible interval (strength diff):", round(cred_int, 1), "\n")

# Largest s with P(DAL covers a -s spread) >= 0.55  i.e.  P(y_future > s) >= 0.55
s_grid = seq(-20, 20, by = 0.1)
cover_prob = sapply(s_grid, function(s) mean(y_future > s))
s_max = max(s_grid[cover_prob >= 0.55])
cat("Largest s with cover prob >= 0.55:", s_max, "\n")





#Task 4
y_obs = nfl$pts_H_minus_A
y_rep = draws$y_rep            # [draws x games]

# 1. Score-differential distribution
ppc_dens_overlay(y_obs, y_rep[1:100, ]) +
  ggtitle("PPC: score-differential density")

# 2. Home win rate
home_win = function(y) mean(y > 0)
ppc_stat(y_obs, y_rep, stat = "home_win") +
  ggtitle("PPC: home win rate")

# 3. Rate of margins >= 21
blowout = function(y) mean(abs(y) >= 21)
ppc_stat(y_obs, y_rep, stat = "blowout") +
  ggtitle("PPC: rate of margins >= 21")






#Task 5
# --- Task 5: prior sensitivity (no new files) ---

# Read the existing model, swap only the sigma_team prior line, compile from a string
stan_src = readLines("14_nfl-model.stan")
idx = grep("sigma_team\\s*~", stan_src)          # prior statement (has ~), not the declaration
stopifnot(length(idx) == 1)
cat("Original prior line:", trimws(stan_src[idx]), "\n")

stan_src[idx] = "  sigma_team ~ normal(0, 0.5);"  # tighter; use normal(0, 10) for wider
model_tight = stan_model(model_code = paste(stan_src, collapse = "\n"))

fit_tight   = sampling(model_tight, data = stan_data, chains = 4, iter = 2000, seed = 14)
draws_tight = rstan::extract(fit_tight)

# Quick diagnostic check before trusting it
cat("Divergences (tight):",
    sum(sapply(rstan::get_sampler_params(fit_tight, inc_warmup = FALSE),
               function(x) sum(x[, "divergent__"]))), "\n")

# Compare leaderboards
compare = tibble(
  team     = teams,
  baseline = colMeans(draws$strength),
  tighter  = colMeans(draws_tight$strength)
) |>
  arrange(desc(baseline))
print(compare, n = 32)

cat("sigma_team posterior mean — baseline:", round(mean(draws$sigma_team), 2),
    "| tighter:", round(mean(draws_tight$sigma_team), 2), "\n")





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
