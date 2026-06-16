#############
### SETUP ###
#############

install.packages("tidyverse")
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

batting = read_csv("/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/12_ba-2020-2021.csv", show_col_types = FALSE) |>
  filter(AB_2020 > 0)

# Method-of-moments estimated prior from the 2020 player population.
mu_hat = with(batting, sum(H_2020) / sum(AB_2020))
C_hat = mu_hat * (1 - mu_hat)
tau2_hat = max(var(batting$BA_2020) - mean(C_hat / batting$AB_2020), 0)

# Beta prior with the same mean and between-player variance.
beta_strength = mu_hat * (1 - mu_hat) / tau2_hat - 1
alpha_hat = mu_hat * beta_strength
beta_hat = (1 - mu_hat) * beta_strength

batting = batting |>
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

# TODO: make a shrinkage-arrow plot from mle to empirical_bayes.
batting |>
  mutate(
    distance_from_mean = abs(mle - mu_hat)
  ) |>
  ggplot(aes(x = mle, y = BA_2020)) +
  geom_segment(
    aes(
      xend = empirical_bayes,
      yend = BA_2020,
      color = AB_2020
    ),
    alpha = 0.6
  ) +
  geom_point(aes(color = AB_2020), size = 1) +
  scale_color_viridis_c() +
  labs(
    x = "2020 MLE batting average",
    y = "2020 batting average",
    color = "AB 2020",
    title = "Shrinkage from MLE to Empirical Bayes"
  )

# TODO: compare estimators against BA_2021 using batting_eval and rmse().
batting_eval_rmse = batting_eval |>
  summarise(
    rmse_mle = rmse(BA_2021, mle),
    rmse_complete_pooling = rmse(BA_2021, complete_pooling),
    rmse_eb = rmse(BA_2021, empirical_bayes)
  )

batting_eval_rmse

# RMSE by low/high 2021 AB
batting_eval |>
  mutate(
    ab_group = if_else(AB_2021 < 300, "low_AB_2021", "high_AB_2021")
  ) |>
  group_by(ab_group) |>
  summarise(
    rmse_mle = rmse(BA_2021, mle),
    rmse_complete_pooling = rmse(BA_2021, complete_pooling),
    rmse_eb = rmse(BA_2021, empirical_bayes),
    .groups = "drop"
  )

# Prediction vs 2021 batting average
batting_eval |>
  pivot_longer(
    cols = c(mle, complete_pooling, empirical_bayes),
    names_to = "estimator",
    values_to = "prediction"
  ) |>
  ggplot(aes(x = prediction, y = BA_2021, color = estimator)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Predicted batting average (2020-based)",
    y = "2021 batting average",
    title = "Prediction vs 2021 batting average"
  )
# TODO: compare the Normal-Normal and exact Beta-Binomial EB estimates.
batting |>
  ggplot(aes(x = empirical_bayes, y = beta_binomial_eb)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Normal-Normal EB estimate",
    y = "Beta-Binomial EB estimate",
    title = "Comparison of EB approximations"
  )

# Where approximation matters most: look at small AB_2020
batting |>
  mutate(diff = beta_binomial_eb - empirical_bayes) |>
  arrange(desc(abs(diff))) |>
  select(playerID, AB_2020, mle, empirical_bayes, beta_binomial_eb, diff) |>
  head()

# TODO: repeat with 0.5 * tau2_hat and 2 * tau2_hat.

# Refit with 0.5 * tau2_hat and 2 * tau2_hat
tau2_half = 0.5 * tau2_hat
tau2_double = 2 * tau2_hat

batting_sensitivity = batting |>
  mutate(
    posterior_var_half = 1 / (1 / sigma2 + 1 / tau2_half),
    lambda_half = tau2_half / (tau2_half + sigma2),
    eb_half = mu_hat + lambda_half * (mle - mu_hat),
    
    posterior_var_double = 1 / (1 / sigma2 + 1 / tau2_double),
    lambda_double = tau2_double / (tau2_double + sigma2),
    eb_double = mu_hat + lambda_double * (mle - mu_hat)
  )

# RMSE under different tau2 for players with 2021 data
batting_sensitivity_eval = batting_sensitivity |>
  filter(AB_2021 >= 100) |>
  summarise(
    rmse_mle = rmse(BA_2021, mle),
    rmse_eb = rmse(BA_2021, empirical_bayes),
    rmse_eb_half = rmse(BA_2021, eb_half),
    rmse_eb_double = rmse(BA_2021, eb_double)
  )
batting_sensitivity_eval


###################
### FIELD GOALS ###
###################

field_goals = read_csv("/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/12_field-goals.csv", show_col_types = FALSE) |>
  mutate(
    distance_group = case_when(
      ydl <= 20 ~ "short",
      ydl <= 35 ~ "medium",
      TRUE ~ "long"
    )
  )

# TODO: aggregate by kicker and distance_group.
fg_kicker_group = field_goals |>
  group_by(kicker, distance_group) |>
  summarise(
    made = sum(fg_made),
    attempts = n(),
    raw_rate = made / attempts,
    .groups = "drop"
  ) |>
  filter(attempts >= 5)

# TODO: estimate one Beta prior within each distance_group using beta_prior_from_rates().
fg_group_priors = fg_kicker_group |>
  group_by(distance_group) |>
  summarise(
    beta_prior_from_rates(made, attempts),
    .groups = "drop"
  )

# TODO: compute the group-specific Beta-Binomial EB estimate for each kicker-group.
fg_kicker_group_eb = fg_kicker_group |>
  left_join(fg_group_priors, by = "distance_group") |>
  mutate(
    eb_group = (made + alpha) / (attempts + alpha + beta)
  )


# TODO: repeat with one common Beta prior for all field-goal attempts.
fg_common_prior = beta_prior_from_rates(
  made = fg_kicker_group$made,
  attempts = fg_kicker_group$attempts
)

fg_kicker_group_eb = fg_kicker_group_eb |>
  mutate(
    alpha_common = fg_common_prior$alpha,
    beta_common = fg_common_prior$beta,
    eb_common = (made + alpha_common) / (attempts + alpha_common + beta_common)
  )


# TODO: plot raw, group-specific EB, and common-prior EB make rates by distance_group.
fg_kicker_group_eb_long = fg_kicker_group_eb |>
  select(kicker, distance_group, attempts, raw_rate, eb_group, eb_common) |>
  pivot_longer(
    cols = c(raw_rate, eb_group, eb_common),
    names_to = "estimate_type",
    values_to = "rate"
  )

ggplot(fg_kicker_group_eb_long,
       aes(x = attempts, y = rate, color = estimate_type)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ distance_group) +
  labs(
    x = "Attempts",
    y = "Make rate",
    color = "Estimate",
    title = "Raw vs EB Make Rates by Distance Group"
  )


# TODO: contrast which distance groups move differently under the two pooling strategies.
fg_kicker_group_eb |>
  mutate(
    diff_group_vs_common = eb_group - eb_common
  ) |>
  group_by(distance_group) |>
  summarise(
    mean_diff = mean(diff_group_vs_common),
    max_abs_diff = max(abs(diff_group_vs_common)),
    .groups = "drop"
  )