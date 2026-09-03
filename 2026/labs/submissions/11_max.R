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

nba_raw = read_delim("../data/11_nba-free-throws.csv.gz", delim = ";")

bucketed = nba_players = nba_raw |>
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
    "Elite shooter", 100 * 0.9, 100 * (1 - 0.9)
)

glimpse(nba_players)

nba_players |>
  mutate(bucket = case_when(
    attempts <= 30  ~ "low",
    attempts <= 150 ~ "medium",
    TRUE            ~ "high"
  )) |>
  group_by(bucket) |>
  slice_max(order_by = attempts == median(attempts), n = 1)

selected = nba_players |>
  mutate(bucket = case_when(
    attempts <= 30  ~ "low",
    attempts <= 150 ~ "medium",
    TRUE            ~ "high"
  )) |>
  group_by(bucket) |>
  slice_sample(n = 2) |>
  ungroup() |>
  arrange(attempts)

selected

ft_table = selected |>
  select(Player, wins, attempts, mle) |>
  cross_join(priors) |>
  rowwise() |>
  reframe(
    Player, prior, wins, attempts, mle,
    beta_summary(wins, attempts, alpha, beta)
  ) |>
  transmute(
    Player,
    prior_type        = prior,
    mle               = round(mle, 3),
    posterior_mean    = round(posterior_mean, 3),
    posterior_map     = round(posterior_map, 3),
    credible_interval = sprintf("[%.3f, %.3f]", lower, upper)
  ) |>
  arrange(Player, prior_type)

ft_table

ft_table_elite = selected |>
  select(Player, wins, attempts, mle) |>
  cross_join(filter(priors, prior == "Elite shooter")) |>
  rowwise() |>
  reframe(Player, mle, attempts, beta_summary(wins, attempts, alpha, beta))

ggplot(ft_table_elite, aes(y = reorder(Player, attempts))) +
  geom_vline(xintercept = 0.90, linetype = "dashed", color = "gray") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "steelblue") +
  geom_point(aes(x = posterior_mean), color = "steelblue", size = 2) +
  geom_point(aes(x = mle), color = "firebrick", shape = 18, size = 3) +
  labs(x = "Free-throw rate", y = NULL,
       title = "MLE (red) vs Elite-prior posterior mean (blue) with 95% CI")

#weak prior: roughly 4 attempts of confidence at 50%
#league centered prior: strength of 30centered at league average FT% 
#Elite: strength of 100 attempts, centred at 90%
#Biggest gap is by far ben simmons, who is in the low attempts bucket with a low MLE, which makes the strong elite prior very significant.

players_two = nba_players |>
  filter(Player %in% c("Ben Simmons", "Kyrie Irving")) |>
  select(Player, wins, attempts)

strengths = c(2,10,30,100,300)

strength_table = players_two |>
  cross_join(tibble(prior_strength = strengths)) |>
  mutate(
    alpha = league_rate * prior_strength,
    beta  = (1 - league_rate) * prior_strength,
    posterior_mean = (alpha + wins) / (alpha + beta + attempts)
  ) |>
  select(Player, prior_strength, posterior_mean) |>
  arrange(Player, prior_strength)
strength_table

ggplot(strength_table, aes(x = prior_strength, y = posterior_mean, color = Player)) +
  geom_hline(yintercept = league_rate, linetype = "dashed", color = "gray") +
  geom_line() +
  geom_point(size = 2) +
  scale_x_log10(breaks = strengths) +
  labs(x = "Prior strength (alpha + beta)", y = "Posterior mean",
       title = "Prior strength vs posterior mean (prior centered at league rate)")

player = nba_players |> filter(Player == "Kyrie Irving")

outcomes = sample(c(rep(1, player$wins), rep(0, player$losses)))

half   = floor(length(outcomes) / 2)
batch1 = outcomes[1:half]
batch2 = outcomes[(half + 1):length(outcomes)]

b1 = tibble(wins = sum(batch1), attempts = length(batch1))
b2 = tibble(wins = sum(batch2), attempts = length(batch2))

p = priors |> filter(prior == "League centered")

post1 = tibble(
  alpha = p$alpha + b1$wins,
  beta  = p$beta  + (b1$attempts - b1$wins)
)

post2 = tibble(
  alpha = post1$alpha + b2$wins,
  beta  = post1$beta  + (b2$attempts - b2$wins)
)


#Prior dominates data quickly at 100+ for ben simmons, 300 pulls kyrie a decent amount

predictions = selected |>
  select(Player, wins, attempts) |>
  cross_join(priors) |>
  rowwise() |>
  reframe(
    Player, prior,
    beta_predictive_summary(wins, attempts, alpha, beta,
                            future_attempts = 50, draws = 5000)
  )
predictions

#issues: 1. Prior's aren't generated from data specific to that player and treats everyone in the league equally. This could be improved by developing a model based on past player data to estimate alpha/beta
#2. Relies on aggregate data, perhaps a more accurate could develop priors/posteriors per FT