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

index = sample(1:nrow(nba_players),6)
sample_players = nba_players[index, ]


#Task 2

with_priors = sample_players %>%
  mutate(
    posterior_mean = beta_summary(wins,attempts,2,2)$posterior_mean,
    posterior_map = beta_summary(wins,attempts,2,2)$posterior_map,
    posterior_upper = beta_summary(wins,attempts,2,2)$upper,
    posterior_lower = beta_summary(wins,attempts,2,2)$lower,
    
    league_mean = beta_summary(wins,attempts,30*league_rate,30*(1-league_rate))$posterior_mean,
    league_map = beta_summary(wins,attempts,30*league_rate,30*(1-league_rate))$posterior_map,
    league_upper = beta_summary(wins,attempts,30*league_rate,30*(1-league_rate))$upper,
    league_lower = beta_summary(wins,attempts,30*league_rate,30*(1-league_rate))$lower,
    
    elite_mean = beta_summary(wins,attempts,90,10)$posterior_mean,
    elite_map = beta_summary(wins,attempts,90,10)$posterior_map,
    elite_upper = beta_summary(wins,attempts,90,10)$upper,
    elite_lower = beta_summary(wins,attempts,90,10)$lower
  )


#Task 3

two_players = nba_players %>%
  filter(Player == "Bol Bol" | Player == "LeBron James")

n_values <- c(2, 10, 30, 100, 300)

two_player_plot = two_players %>%
  select(Player, wins, attempts, mle) %>%
  crossing(n = n_values) %>%
  mutate(value = (wins + n * league_rate) / (attempts + n)) %>%
  select(n, Player, value) %>%
  pivot_wider(
    names_from = Player,
    values_from = value
  )

plot_df <- two_player_plot %>%
  pivot_longer(
    cols = -n,
    names_to = "Player",
    values_to = "Value"
  )

ggplot(plot_df,
       aes(x = n,
           y = Value,
           color = Player)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_log10(breaks = c(2, 10, 30, 100, 300)) +
  labs(
    title = "Estimate as a Function of Prior Strength (n)",
    x = "n",
    y = "Value"
  ) +
  theme_minimal()


#Picking LeBron James

outcomes = c(rep(1,305),rep(0,100))

sample_lebron = sample(outcomes, size = 202, replace = FALSE)

lebron_posterior = (sum(sample_lebron)+30*league_rate) / (202 + 30)

lebron_second_post = (305-sum(sample_lebron)+30*lebron_posterior) / (203 + 30)
# 0.7320522
# Overall value = 0.755


# Task 4

with_priors = with_priors %>%
  select(Player, wins, attempts, losses, mle, league_mean)

K <- 50   # or whatever your assignment used

posterior_predictions <- with_priors %>%
  mutate(
    alpha = K * league_mean,
    beta  = K * (1 - league_mean),
    post_alpha = alpha + wins,
    post_beta  = beta + (attempts - wins)
  ) %>%
  rowwise() %>%
  mutate(
    p_draws = list(rbeta(5000, post_alpha, post_beta)),
    future_wins = list(rbinom(5000, size = 50, prob = p_draws)),
    future_pct = list(future_wins / 50),
    predictive_mean = mean(future_pct),
    predictive_interval_upper = quantile(future_pct, 0.975),
    predictive_interval_lower = quantile(future_pct, 0.025),
    credible_interval_upper = quantile(p_draws, 0.975),
    credible_interval_lower = quantile(p_draws, 0.025),
    beta_binom_ev = (alpha+wins) / (alpha + beta + attempts)
  ) %>%
  select(Player,predictive_mean, beta_binom_ev,predictive_interval_lower,predictive_interval_upper,
         credible_interval_lower,credible_interval_upper)

#The predictive mean is roughly the same as the Beta Binomial Expectation


#Task 5: Model Critique

#One assumption is that a player's probability of making a free throw is the same regardless of context
#A fix for this could be accounting for recent performance, injury, home field advantage, or other basketball-related factors

#Another assumption is that the prior is the league rate, which assumes all players have the same skill level
#A fix for this would be creating a prior based on past season performance instead of the league rate


