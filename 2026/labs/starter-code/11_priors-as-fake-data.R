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
# TODO: apply beta_summary() for every selected player and prior.
# TODO: verify that sequential and all-at-once updating give the same posterior.
# TODO: simulate posterior predictive outcomes for the next 50 attempts.
