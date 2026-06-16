#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

set.seed(11)

########################
### HELPER FUNCTIONS ###
########################

beta_summary = function(wins, attempts, alpha, beta, level = 0.95) {
  losses = attempts - wins
  alpha_post = alpha + wins
  beta_post = beta + losses
  tail = (1 - level) / 2
  
  tibble(
    posterior_mean = alpha_post / (alpha_post + beta_post),
    posterior_map = if_else(
      alpha_post > 1 & beta_post > 1,
      (alpha_post - 1) / (alpha_post + beta_post - 2),
      NA_real_
    ),
    lower = qbeta(tail, alpha_post, beta_post),
    upper = qbeta(1 - tail, alpha_post, beta_post)
  )
}

prior_from_center_strength = function(center, strength) {
  tibble(
    alpha = center * strength,
    beta = (1 - center) * strength
  )
}

beta_predictive_summary = function(
    wins, attempts, alpha, beta, future_attempts = 50, draws = 5000
) {
  alpha_post = alpha + wins
  beta_post = beta + attempts - wins
  p_draws = rbeta(draws, alpha_post, beta_post)
  future_wins = rbinom(draws, future_attempts, p_draws)
  
  tibble(
    predictive_mean = mean(future_wins / future_attempts),
    analytic_mean = alpha_post / (alpha_post + beta_post),
    lower = quantile(future_wins / future_attempts, 0.025),
    upper = quantile(future_wins / future_attempts, 0.975)
  )
}

#######################
### NBA FREE THROWS ###
#######################

nba_raw = read_delim("/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/11_nba-free-throws.csv", delim = ";")

nba_players = nba_raw |>
  filter(Tm == "TOT" | !Player %in% nba_raw$Player[nba_raw$Tm == "TOT"]) |>
  transmute(
    Player,
    wins = round(G * FT),
    attempts = round(G * FTA),
    losses = attempts - wins,
    mle = wins / attempts
  ) |>
  filter(attempts > 0)

league_rate = sum(nba_players$wins) / sum(nba_players$attempts)

priors = tribble(
  ~prior, ~alpha, ~beta,
  "Weak", 2, 2,
  "League centered", 30 * league_rate, 30 * (1 - league_rate),
  "Elite shooter", 90, 10
)

# TODO: select players spanning low, medium, and high attempt totals.
# pick 6 players spanning low, medium, high attempts
selected_players = nba_players |>
  arrange(attempts) |>
  slice(
    1, 2,                      # low attempts
    n() %/% 2, n() %/% 2 + 1,  # medium attempts
    n() - 1, n()               # high attempts
  )
posterior_summaries = selected_players |>
  crossing(priors) |>
  rowwise() |>
  mutate(
    beta_summ = list(
      beta_summary(
        wins = wins,
        attempts = attempts,
        alpha = alpha,
        beta = beta,
        level = 0.95
      )
    )
  ) |>
  unnest(beta_summ)

# TODO: apply beta_summary() for every selected player and prior.
mle_points = selected_players |>
  crossing(priors) |>
  mutate(x_prior = prior)

ggplot(posterior_summaries,
       aes(x = prior, y = posterior_mean, color = prior)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.5)) +
  geom_point(
    data = mle_points,
    aes(x = x_prior, y = mle),
    inherit.aes = FALSE,
    color = "black",
    size = 2
  ) +
  facet_wrap(~ Player) +
  labs(
    title = "MLE vs posterior means and 95% credible intervals",
    y = "Free-throw probability",
    x = "Prior"
  )

strengths = c(2, 10, 30, 100, 300)

# choose one low-attempt and one high-attempt player
low_player = nba_players |> arrange(attempts) |> slice(1)
high_player = nba_players |> arrange(desc(attempts)) |> slice(1)

prior_sensitivity = bind_rows(
  low_player |> mutate(type = "Low attempts"),
  high_player |> mutate(type = "High attempts")
) |>
  crossing(strength = strengths) |>
  rowwise() |>
  mutate(
    prior_params = prior_from_center_strength(
      center = league_rate,
      strength = strength
    ),
    alpha = prior_params$alpha,
    beta = prior_params$beta,
    beta_summ = list(
      beta_summary(
        wins = wins,
        attempts = attempts,
        alpha = alpha,
        beta = beta
      )
    )
  ) |>
  unnest(beta_summ)

ggplot(prior_sensitivity,
       aes(x = strength, y = posterior_mean, color = type)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  labs(
    title = "Posterior mean vs prior strength",
    x = "Prior strength (log scale)",
    y = "Posterior mean"
  )

# TODO: verify that sequential and all-at-once updating give the same posterior.
# pick one player to split attempts
split_player = nba_players |> arrange(desc(attempts)) |> slice(1)

# split into two batches (roughly half)
batch1_attempts = floor(split_player$attempts / 2)
batch2_attempts = split_player$attempts - batch1_attempts

batch1_wins = round(split_player$mle * batch1_attempts)
batch2_wins = split_player$wins - batch1_wins

# league-centered prior
lc_prior = prior_from_center_strength(league_rate, 30)
alpha0 = lc_prior$alpha
beta0 = lc_prior$beta

# sequential updating
alpha1 = alpha0 + batch1_wins
beta1 = beta0 + (batch1_attempts - batch1_wins)

alpha2_seq = alpha1 + batch2_wins
beta2_seq = beta1 + (batch2_attempts - batch2_wins)

# all-at-once updating
alpha2_all = alpha0 + split_player$wins
beta2_all = beta0 + (split_player$attempts - split_player$wins)

tibble(
  method = c("Sequential", "All at once"),
  alpha_post = c(alpha2_seq, alpha2_all),
  beta_post = c(beta2_seq, beta2_all),
  posterior_mean = alpha_post / (alpha_post + beta_post)
)

# TODO: simulate posterior predictive outcomes for the next 50 attempts.

predictive_summaries = selected_players |>
  crossing(priors) |>
  rowwise() |>
  mutate(
    pred = list(
      beta_predictive_summary(
        wins = wins,
        attempts = attempts,
        alpha = alpha,
        beta = beta,
        future_attempts = 50,
        draws = 5000
      )
    )
  ) |>
  unnest(pred)

# check analytic mean vs predictive mean
predictive_summaries |> 
  select(Player, prior, predictive_mean, analytic_mean)

# compare predictive intervals vs credible intervals
posterior_vs_predictive = posterior_summaries |>
  rename(
    cred_lower = lower,
    cred_upper = upper,
    cred_mean = posterior_mean
  ) |>
  left_join(
    predictive_summaries |> 
      rename(
        pred_lower = lower,
        pred_upper = upper,
        pred_mean = predictive_mean
      ),
    by = c("Player", "prior")
  )

posterior_vs_predictive