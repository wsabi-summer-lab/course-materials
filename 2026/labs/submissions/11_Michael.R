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

# Task 1: Select players spanning low, medium, and high attempt totals
selected_players = nba_players |>
  arrange(attempts) |>
  slice(c(1, 10, round(n()/2) - 1, round(n()/2) + 1, n() - 9, n()))

selected_players

# Task 2: Compare Three Priors
results = selected_players |>
  cross_join(priors) |>
  rowwise() |>
  mutate(beta_summary(wins, attempts, alpha, beta)) |>
  ungroup()

# Add MLE rows for plotting
mle_rows = selected_players |>
  mutate(
    prior = "MLE",
    posterior_mean = mle,
    posterior_map = mle,
    lower = mle,
    upper = mle
  )

plot_data = bind_rows(results, mle_rows) |>
  mutate(prior = factor(prior, levels = c("MLE", "Weak", "League centered", "Elite shooter")))

ggplot(plot_data, aes(x = prior, y = posterior_mean, color = prior)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_wrap(~Player, scales = "free_y") +
  labs(
    title = "MLE vs Posterior Means with 95% Credible Intervals",
    x = "Prior", y = "Free Throw Probability"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1), legend.position = "none")

#the players that move the most have extremely few attempts since the "fake" data will change them the most. If htey had one shot, and hten even just a couple shots are added at 50 percent then their shooting percentage will go down a lot.

# Task 3: Prior Sensitivity
strengths = c(2, 10, 30, 100, 300)

low_player  = selected_players |> slice(1)  # Bouknight
high_player = selected_players |> slice(6)  # Giannis

sensitivity = bind_rows(low_player, high_player) |>
  cross_join(tibble(strength = strengths)) |>
  rowwise() |>
  mutate(
    alpha = league_rate * strength,
    beta  = (1 - league_rate) * strength,
    posterior_mean = beta_summary(wins, attempts, alpha, beta)$posterior_mean
  ) |>
  ungroup()

ggplot(sensitivity, aes(x = strength, y = posterior_mean, color = Player)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  labs(
    title = "Prior Sensitivity: Posterior Mean vs Prior Strength",
    x = "Prior Strength (log scale)", y = "Posterior Mean"
  ) +
  theme_minimal()

# Sequential updating verification — split Giannis into two batches
batch1_wins     = round(high_player$wins / 2)
batch1_attempts = round(high_player$attempts / 2)
batch2_wins     = high_player$wins - batch1_wins
batch2_attempts = high_player$attempts - batch1_attempts

alpha0 = 30 * league_rate
beta0  = 30 * (1 - league_rate)

# Sequential: update on batch 1, then use that posterior as prior for batch 2
post1 = beta_summary(batch1_wins, batch1_attempts, alpha0, beta0)
alpha1 = alpha0 + batch1_wins
beta1  = beta0  + batch1_attempts - batch1_wins
post_sequential = beta_summary(batch2_wins, batch2_attempts, alpha1, beta1)

# All at once
post_all_at_once = beta_summary(high_player$wins, high_player$attempts, alpha0, beta0)

cat("Sequential posterior mean:    ", post_sequential$posterior_mean, "\n")
cat("All-at-once posterior mean:   ", post_all_at_once$posterior_mean, "\n")
cat("Sequential CI:  [", post_sequential$lower, ",", post_sequential$upper, "]\n")
cat("All-at-once CI: [", post_all_at_once$lower, ",", post_all_at_once$upper, "]\n")

#Price sensitivity for Bouknight is a lot more sensititve and drops rapidly towards the league average almost immediately, whereas Giannis does not increase too much even when a lot of strength on a prior odds, and does strongly effect him till 300.
#Sequential and all at once updating product identical posteriors, which makes sense because your results are gonna be the same either way.

# Task 4: Posterior Prediction
predictive_results = selected_players |>
  rowwise() |>
  mutate(
    pred = list(beta_predictive_summary(wins, attempts, 
                                        30 * league_rate, 
                                        30 * (1 - league_rate))),
    ci   = list(beta_summary(wins, attempts, 
                             30 * league_rate, 
                             30 * (1 - league_rate)))
  ) |>
  unnest_wider(pred) |>
  unnest_wider(ci, names_sep = "_") |>
  ungroup() |>
  select(Player, attempts, predictive_mean, analytic_mean, 
         pred_lower = lower, pred_upper = upper,
         ci_lower = ci_lower, ci_upper = ci_upper)

predictive_results
print(predictive_results, width = Inf)

#The predictive means are close to the analytics means  showing that the models expectations holds.
#Credible intervals are tighter since they captures the uncertainty of capturing the right probability, whereas predictive interval includes the layer of binomial sampling variability too. 

#assuming that the distributions are iid, certianly the probability of making a free throw varies over a season and over a game, given different levels of fatigue, and streaks and the psycological pressure of different situations. Also, the result of the first free throw definitely changes the next shot since the athlete has more feed back on his preformance in that moment. 