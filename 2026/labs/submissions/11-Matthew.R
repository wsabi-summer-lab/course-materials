#######################
### Authors: JP, RB ###
#######################

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

nba_raw = read_delim("../data/11_nba-free-throws.csv", delim = ";")

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
colnames(nba_players)
unique(nba_players$Player)
selected=nba_players|>
  filter(Player %in% c("Stephen Curry", "Pat Connaughton","Patrick Beverley", "Mo Bamba", "Scottie Barnes","OG Anunoby"))

# TODO: apply beta_summary() for every selected player and prior.
posterior_results <- crossing(selected, priors) |>
  rowwise() |>
  mutate(
    summary = list(
      beta_summary(
        wins = wins,
        attempts = attempts,
        alpha = alpha,
        beta = beta
      )
    )
  ) |>
  tidyr::unnest(summary) |>
  ungroup()

posterior_results |>
  select(
    Player,
    attempts,
    prior,
    posterior_mean,
    posterior_map,
    lower,
    upper
  )

# TODO: verify that sequential and all-at-once updating give the same posterior.
verification <- selected |>
  slice(1) |>
  rowwise() |>
  mutate(
    alpha0 = 1,
    beta0 = 1,
    
    # all-at-once update
    alpha_all = alpha0 + wins,
    beta_all  = beta0 + losses,
    
    # sequential update
    alpha_seq = {
      a <- alpha0
      for(i in seq_len(wins)) a <- a + 1
      a
    },
    
    beta_seq = {
      b <- beta0
      for(i in seq_len(losses)) b <- b + 1
      b
    },
    
    same_posterior =
      (alpha_all == alpha_seq) &
      (beta_all == beta_seq)
  ) |>
  ungroup() |>
  select(
    Player,
    alpha_all,
    beta_all,
    alpha_seq,
    beta_seq,
    same_posterior
  )

verification

# TODO: simulate posterior predictive outcomes for the next 50 attempts.
predictive_results <- crossing(selected, priors) |>
  rowwise() |>
  mutate(
    predictive = list(
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
  tidyr::unnest(predictive) |>
  ungroup()

predictive_results |>
  select(
    Player,
    prior,
    predictive_mean,
    analytic_mean,
    lower,
    upper
  )
