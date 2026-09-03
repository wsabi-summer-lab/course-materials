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

nba_raw = read_delim("C:/Users/sundw/Downloads/11_nba-free-throws.csv", delim = ";")

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

summary(nba_players)
summary(nba_raw)
head(nba_players)

# TODO: select players spanning low, medium, and high attempt totals.

nba_filtered <- nba_players %>% filter(attempts >= 4)

quantile_breaks <- quantile(nba_filtered$attempts, probs = c(0.25, 0.75))

low    <- nba_filtered %>% filter(attempts <= quantile_breaks[1])
medium <- nba_filtered %>% filter(attempts > quantile_breaks[1] & attempts <= quantile_breaks[2])
high   <- nba_filtered %>% filter(attempts > quantile_breaks[2])

nba_6_players <- bind_rows(
  slice_sample(low,    n = 2),
  slice_sample(medium, n = 2),
  slice_sample(high,   n = 2)
)

# TODO: apply beta_summary() for every selected player and prior.

nba_6_players %>%
  cross_join(priors) %>%
  rowwise() %>%
  mutate(beta_summary(wins, attempts, alpha, beta)) %>%
  ungroup() %>%
  select(Player, prior, posterior_mean, posterior_map, lower, upper)

posterior_results <- nba_6_players %>%
  cross_join(priors) %>%
  rowwise() %>%
  mutate(beta_summary(wins, attempts, alpha, beta)) %>%
  ungroup() %>%
  select(Player, prior, posterior_mean, posterior_map, lower, upper)

# TODO: verify that sequential and all-at-once updating give the same posterior.

test_player <- nba_6_players %>% slice(1)

# All-at-once update
all_at_once <- beta_summary(test_player$wins, test_player$attempts, 2, 2)

# Sequential update: one free throw at a time
alpha_seq <- 2
beta_seq <- 2
outcomes <- c(rep(1, test_player$wins), rep(0, test_player$losses))

for (outcome in outcomes) {
  alpha_seq <- alpha_seq + outcome
  beta_seq  <- beta_seq + (1 - outcome)
}
sequential <- beta_summary(0, 0, alpha_seq, beta_seq)

bind_rows(
  all_at_once %>% mutate(method = "All at once"),
  sequential  %>% mutate(method = "Sequential")
)

# TODO: simulate posterior predictive outcomes for the next 50 attempts.

nba_6_players %>%
  rowwise() %>%
  mutate(beta_predictive_summary(wins, attempts, alpha = 2, beta = 2, future_attempts = 50)) %>%
  ungroup() %>%
  select(Player, predictive_mean, analytic_mean, lower, upper)
