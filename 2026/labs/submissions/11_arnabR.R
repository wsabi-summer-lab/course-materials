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
#Jose Alvarado, Kyle Anderson, Lamelo Ball, Giannis Antetokounmpo, Jimmy butler, Jarrett Allen.


beta_summary(
  wins = nba_players$wins[nba_players$Player == "Jose Alvarado"],
  attempts = nba_players$attempts[nba_players$Player == "Jose Alvarado"],
  alpha = priors$alpha[priors$prior == "Weak"],
  beta  = priors$beta[priors$prior == "Weak"]
)


beta_summary(
  wins = nba_players$wins[nba_players$Player == "Kyle Anderson"],
  attempts = nba_players$attempts[nba_players$Player == "Kyle Anderson"],
  alpha = priors$alpha[priors$prior == "Weak"],
  beta  = priors$beta[priors$prior == "Weak"]
)


beta_summary(
  wins = nba_players$wins[nba_players$Player == "LaMelo Ball"],
  attempts = nba_players$attempts[nba_players$Player == "LaMelo Ball"],
  alpha = priors$alpha[priors$prior == "League centered"],
  beta  = priors$beta[priors$prior == "League centered"]
)

beta_summary(
  wins = nba_players$wins[nba_players$Player == "Jarrett Allen"],
  attempts = nba_players$attempts[nba_players$Player == "Jarrett Allen"],
  alpha = priors$alpha[priors$prior == "League centered"],
  beta  = priors$beta[priors$prior == "League centered"]
)


beta_summary(
  wins = nba_players$wins[nba_players$Player == "Giannis Antetokounmpo"],
  attempts = nba_players$attempts[nba_players$Player == "Giannis Antetokounmpo"],
  alpha = priors$alpha[priors$prior == "Elite shooter"],
  beta  = priors$beta[priors$prior == "Elite shooter"]
)


beta_summary(
  wins = nba_players$wins[nba_players$Player == "Jimmy Butler"],
  attempts = nba_players$attempts[nba_players$Player == "Jimmy Butler"],
  alpha = priors$alpha[priors$prior == "Elite shooter"],
  beta  = priors$beta[priors$prior == "Elite shooter"]
)


players <- tibble(
  Player = c(
    "Jose Alvarado",
    "Kyle Anderson",
    "LaMelo Ball",
    "Jarrett Allen",
    "Giannis Antetokounmpo",
    "Jimmy Butler"
  ),
  prior = c(
    "Weak",
    "Weak",
    "League centered",
    "League centered",
    "Elite shooter",
    "Elite shooter"
  )
)

plot_data <- players %>%
  left_join(nba_players, by = "Player") %>%
  left_join(priors, by = "prior") %>%
  rowwise() %>%
  mutate(
    mle = wins / attempts,
    mle_lower = mle - 1.96 * sqrt(mle * (1 - mle) / attempts),
    mle_upper = mle + 1.96 * sqrt(mle * (1 - mle) / attempts),
    
    posterior_mean = beta_summary(
      wins, attempts, alpha, beta
    )$posterior_mean,
    
    lower = beta_summary(
      wins, attempts, alpha, beta
    )$lower,
    
    upper = beta_summary(
      wins, attempts, alpha, beta
    )$upper
  ) %>%
  ungroup()

ggplot(plot_data, aes(y = Player)) +
  geom_point(aes(x = mle, color = "MLE"), size = 3) +
  geom_errorbarh(
    aes(xmin = mle_lower, xmax = mle_upper, color = "MLE"),
    height = 0.15
  ) +
  geom_point(aes(x = posterior_mean, color = "Posterior"), size = 3) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper, color = "Posterior"),
    height = 0.15
  ) +
  labs(
    title = "MLE vs Posterior Estimates",
    x = "Free Throw Percentage",
    y = NULL,
    color = NULL
  ) +
  theme_minimal()

# The players with less attempts move the most, as the alpha and beta values affect it less.

strengths <- c(2, 10, 30, 100, 300)

strength_plot_data <- tibble(
  Player = c("Jose Alvarado", "Jimmy Butler"),
  prior = c("Weak", "Elite shooter")
) %>%
  left_join(nba_players, by = "Player") %>%
  left_join(priors, by = "prior") %>%
  mutate(center = alpha / (alpha + beta)) %>%
  select(Player, wins, attempts, center) %>%
  crossing(strength = strengths) %>%
  rowwise() %>%
  mutate(
    new_prior = list(prior_from_center_strength(center, strength)),
    alpha_new = new_prior$alpha,
    beta_new = new_prior$beta,
    posterior_mean = beta_summary(
      wins, attempts, alpha_new, beta_new
    )$posterior_mean
  ) %>%
  ungroup()

ggplot(strength_plot_data,
       aes(x = strength, y = posterior_mean, color = Player)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = strengths) +
  labs(
    title = "Posterior Mean vs Prior Strength",
    x = "Prior strength",
    y = "Posterior mean",
    color = "Player"
  ) +
  theme_minimal()
#Prior starts dominating at 30. 



wins_total <- nba_players$wins[nba_players$Player == "Jimmy Butler"]
attempts_total <- nba_players$attempts[nba_players$Player == "Jimmy Butler"]
losses_total <- attempts_total - wins_total

alpha0 <- priors$alpha[priors$prior == "Elite shooter"]
beta0  <- priors$beta[priors$prior == "Elite shooter"]

attempts_1 <- floor(attempts_total / 2)
attempts_2 <- attempts_total - attempts_1

wins_1 <- round(wins_total * attempts_1 / attempts_total)
wins_2 <- wins_total - wins_1

losses_1 <- attempts_1 - wins_1
losses_2 <- attempts_2 - wins_2

alpha_all <- alpha0 + wins_total
beta_all  <- beta0 + losses_total

alpha_after_1 <- alpha0 + wins_1
beta_after_1  <- beta0 + losses_1

alpha_after_2 <- alpha_after_1 + wins_2
beta_after_2  <- beta_after_1 + losses_2

tibble(
  alpha_all = alpha_all,
  beta_all = beta_all,
  alpha_sequential = alpha_after_2,
  beta_sequential = beta_after_2,
  same_alpha = alpha_all == alpha_after_2,
  same_beta = beta_all == beta_after_2
)

players <- tibble(
  Player = c(
    "Jose Alvarado",
    "Kyle Anderson",
    "LaMelo Ball",
    "Jarrett Allen",
    "Giannis Antetokounmpo",
    "Jimmy Butler"
  ),
  prior = c(
    "Weak",
    "Weak",
    "League centered",
    "League centered",
    "Elite shooter",
    "Elite shooter"
  )
)

predictive_results <- players %>%
  left_join(nba_players, by = "Player") %>%
  left_join(priors, by = "prior") %>%
  rowwise() %>%
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
  ) %>%
  unnest(predictive) %>%
  select(
    Player,
    predictive_mean,
    lower,
    upper
  )

predictive_results
# The posterior predictive interval seems wider.
# TODO: apply beta_summary() for every selected player and prior.
# TODO: verify that sequential and all-at-once updating give the same posterior.
# TODO: simulate posterior predictive outcomes for the next 50 attempts.


#probability of success with free throws probably is constant. Also it allows for greater variance. 