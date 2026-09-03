#######################
### Authors: RB, JP ###
#######################

#############
### SETUP ###
#############

# install.packages("tidyverse")
suppressPackageStartupMessages({
  library(tidyverse)
})

set.seed(12)

########################
### HELPER FUNCTIONS ###
########################

rmse = function(truth, prediction) {
  sqrt(mean((truth - prediction)^2))
}

beta_prior_from_rates = function(made, attempts) {
  raw_rate = made / attempts
  mu = sum(made) / sum(attempts)
  sampling_noise = mean(mu * (1 - mu) / attempts)
  tau2 = max(var(raw_rate) - sampling_noise, .Machine$double.eps)
  strength = mu * (1 - mu) / tau2 - 1
  
  tibble(
    alpha = mu * strength,
    beta = (1 - mu) * strength,
    center = mu,
    tau2 = tau2,
    strength = strength
  )
}

########################
### BATTING AVERAGES ###
########################

raw_data = read_csv("../data/12_ba-2020-2021.csv.gz", show_col_types = FALSE) |>
  filter(AB_2020 > 0)

Z# Method-of-moments estimated prior from the 2020 player population.
mu_hat = with(batting, sum(H_2020) / sum(AB_2020))
C_hat = mu_hat * (1 - mu_hat)
tau2_hat = max(var(batting$BA_2020) - mean(C_hat / batting$AB_2020), 0)

# Beta prior with the same mean and between-player variance.
beta_strength = mu_hat * (1 - mu_hat) / tau2_hat - 1
alpha_hat = mu_hat * beta_strength
beta_hat = (1 - mu_hat) * beta_strength

batting = raw_data |>
  mutate(
    mle = BA_2020,
    complete_pooling = mu_hat,
    sigma2 = C_hat / AB_2020,
    posterior_variance = 1 / (1 / sigma2 + 1 / tau2_hat),
    lambda = tau2_hat / (tau2_hat + sigma2),
    empirical_bayes = mu_hat + lambda * (mle - mu_hat),
    beta_binomial_eb = (H_2020 + alpha_hat) /
      (AB_2020 + alpha_hat + beta_hat)
  )

batting_eval = batting |>
  filter(AB_2021 >= 100)

#Task 2

ggplot(batting) +
  geom_segment(
    aes(
      x = mle,
      xend = empirical_bayes,
      y = AB_2020,
      yend = AB_2020,
      color = lambda
    ),
    arrow = arrow(length = unit(0.15, "cm")),
    alpha = 0.7
  ) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    x = "MLE",
    y = "At-bats (2020)",
    color = "Shrinkage weight λ",
    title = "Shrinkage from MLE to EB, Colored by λ"
  ) +
  theme_minimal()


#Task 3

low_sample = batting |>
  filter(AB_2021 < 100)

overall_rmse = summarise(batting, rmse = sqrt(mean((empirical_bayes - BA_2021)^2)))
# 0.0371
highab_rmse = summarise(batting_eval, rmse = sqrt(mean((empirical_bayes - BA_2021)^2)))
# 0.0333
lowab_rmse = summarise(low_sample, rmse = sqrt(mean((empirical_bayes - BA_2021)^2)))
# 0.0779



#Task 4

s = (mu_hat * (1-mu_hat))/tau2_hat - 1

alpha_hat = mu_hat * s
beta_hat = (1 - mu_hat) * s

batting = batting %>%
  mutate(
    estimated_prior = (alpha_hat+H_2020)/(alpha_hat+beta_hat+AB_2020)
  )


#Task 5
tau_half = tau2_hat / 2
tau_double = tau2_hat * 2

batting_tau_half = raw_data |>
  mutate(
    mle = BA_2020,
    complete_pooling = mu_hat,
    sigma2 = C_hat / AB_2020,
    posterior_variance = 1 / (1 / sigma2 + 1 / tau_half),
    lambda = tau_half / (tau_half + sigma2),
    empirical_bayes = mu_hat + lambda * (mle - mu_hat),
    beta_binomial_eb = (H_2020 + alpha_hat) /
      (AB_2020 + alpha_hat + beta_hat)
  )

batting_tau_double = raw_data |>
  mutate(
    mle = BA_2020,
    complete_pooling = mu_hat,
    sigma2 = C_hat / AB_2020,
    posterior_variance = 1 / (1 / sigma2 + 1 / tau_double),
    lambda = tau_double / (tau_double + sigma2),
    empirical_bayes = mu_hat + lambda * (mle - mu_hat),
    beta_binomial_eb = (H_2020 + alpha_hat) /
      (AB_2020 + alpha_hat + beta_hat)
  )

rmse_dbl = summarise(batting_tau_double, rmse = sqrt(mean((empirical_bayes - BA_2021)^2)))
#0.0376
rmse_half = summarise(batting_tau_half, rmse = sqrt(mean((empirical_bayes - BA_2021)^2)))
#0.0377



###################
### FIELD GOALS ###
###################

field_goals = read_csv("../data/12_field-goals.csv.gz", show_col_types = FALSE) |>
  mutate(
    distance_group = case_when(
      ydl <= 20 ~ "short",
      ydl <= 35 ~ "medium",
      TRUE ~ "long"
    )
  )



kicker_grouped <- field_goals %>%
  group_by(kicker, distance_group) %>%
  summarise(
    attempts = n(),
    made = sum(fg_made),
    raw_rate = made / attempts,
    .groups = "drop"
  ) %>%
  filter(attempts >= 5)

estimate_beta_prior <- function(df) {
  p <- df$raw_rate
  mu <- mean(p)
  v  <- var(p)
  
  # Avoid degenerate cases
  if (v == 0) v <- 1e-6
  
  alpha <- mu * (mu * (1 - mu) / v - 1)
  beta  <- (1 - mu) * (mu * (1 - mu) / v - 1)
  
  tibble(alpha = alpha, beta = beta)
}

priors_by_group <- kicker_grouped %>%
  group_by(distance_group) %>%
  do(estimate_beta_prior(.)) %>%
  ungroup()

kicker_EB <- kicker_grouped %>%
  left_join(priors_by_group, by = "distance_group") %>%
  mutate(
    EB_rate = (made + alpha) / (attempts + alpha + beta)
  )


ggplot(kicker_EB, aes(x = raw_rate, y = EB_rate)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ distance_group) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Raw vs Empirical-Bayes FG Make Rates",
    x = "Raw Make Rate",
    y = "EB Posterior Mean"
  )


common_prior <- estimate_beta_prior(kicker_grouped)

kicker_EB_common <- kicker_grouped %>%
  mutate(
    EB_rate_common = (made + common_prior$alpha) /
      (attempts + common_prior$alpha + common_prior$beta)
  )

ggplot(kicker_EB_common, aes(x = raw_rate, y = EB_rate_common)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ distance_group) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Raw vs EB Make Rates (Common Prior)",
    x = "Raw Make Rate",
    y = "EB Posterior Mean (Common Prior)"
  )


#Long field games are the ones that disagree with their raw make rate the most
#This could be because long field goals are generally taken by better kickers, and the prediction doesn't account for that


#The common prior seems to work better for medium and short whereas they both work equally bad for long

