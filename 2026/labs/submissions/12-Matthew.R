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

batting = read_csv("../data/12_ba-2020-2021.csv.gz", show_col_types = FALSE) |>
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
  arrange(mle) |>
  mutate(player_rank = row_number()) |>
  ggplot(aes(y = player_rank)) +
  geom_segment(
    aes(x = mle, xend = empirical_bayes, yend = player_rank),
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
    color = "steelblue", alpha = 0.5
  ) +
  geom_point(aes(x = mle), color = "tomato", size = 1.5, alpha = 0.6) +
  geom_vline(xintercept = mu_hat, linetype = "dashed", color = "gray40") +
  labs(
    title = "Shrinkage of MLE toward Empirical Bayes Estimate",
    x = "Batting Average",
    y = "Player (ranked by 2020 MLE)",
    caption = "Red = MLE, Arrow tip = EB estimate, Dashed = population mean"
  ) +
  theme_minimal()
# TODO: compare estimators against BA_2021 using batting_eval and rmse().
estimators = c("mle", "complete_pooling", "empirical_bayes", "beta_binomial_eb")

rmse_results = tibble(
  estimator = estimators,
  rmse = map_dbl(estimators, ~ rmse(batting_eval$BA_2021, batting_eval[[.x]]))
)

print(rmse_results)
# TODO: compare the Normal-Normal and exact Beta-Binomial EB estimates.
# TODO: repeat with 0.5 * tau2_hat and 2 * tau2_hat.
# --- 2. Repeat with 0.5 * tau2_hat and 2 * tau2_hat ---
compute_eb_rmse = function(tau2_scale) {
  tau2 = tau2_scale * tau2_hat
  strength = mu_hat * (1 - mu_hat) / tau2 - 1
  alpha = mu_hat * strength
  beta  = (1 - mu_hat) * strength
  
  batting_eval |>
    mutate(
      sigma2       = C_hat / AB_2020,
      lambda       = tau2 / (tau2 + sigma2),
      nn_eb        = mu_hat + lambda * (mle - mu_hat),
      bb_eb        = (H_2020 + alpha) / (AB_2020 + alpha + beta)
    ) |>
    summarise(
      tau2_scale   = tau2_scale,
      rmse_nn_eb   = rmse(BA_2021, nn_eb),
      rmse_bb_eb   = rmse(BA_2021, bb_eb)
    )
}

tau2_comparison = map(c(0.5, 1, 2), compute_eb_rmse) |>
  list_rbind()

print(tau2_comparison)

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
colnames(field_goals)
# --- 1. Aggregate by kicker and distance_group ---
fg_agg = field_goals |>
  group_by(kicker, distance_group) |>
  summarise(
    made     = sum(fg_made),   # was sum(made)
    attempts = n(),
    raw_rate = made / attempts,
    .groups  = "drop"
  )

# --- 2. Estimate one Beta prior per distance_group ---
fg_priors = fg_agg |>
  group_by(distance_group) |>
  group_modify(~ beta_prior_from_rates(.x$made, .x$attempts)) |>
  ungroup()

print(fg_priors)

# --- 3. Group-specific Beta-Binomial EB estimate ---
fg_agg = fg_agg |>
  left_join(fg_priors, by = "distance_group") |>
  mutate(
    eb_group = (made + alpha) / (attempts + alpha + beta)
  )

# --- 4. Common Beta prior across all attempts ---
common_prior = beta_prior_from_rates(fg_agg$made, fg_agg$attempts)

fg_agg = fg_agg |>
  mutate(
    eb_common = (made + common_prior$alpha) /
      (attempts + common_prior$alpha + common_prior$beta)
  )

# --- 5. Plot raw, group-specific EB, and common-prior EB by distance_group ---
fg_agg |>
  pivot_longer(
    cols      = c(raw_rate, eb_group, eb_common),
    names_to  = "estimator",
    values_to = "make_rate"
  ) |>
  mutate(
    estimator     = factor(estimator,
                           levels = c("raw_rate", "eb_group", "eb_common"),
                           labels = c("Raw", "Group EB", "Common EB")),
    distance_group = factor(distance_group, levels = c("short", "medium", "long"))
  ) |>
  ggplot(aes(x = estimator, y = make_rate, fill = estimator)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.4) +
  facet_wrap(~ distance_group) +
  scale_fill_manual(values = c("tomato", "steelblue", "seagreen")) +
  labs(
    title   = "Field Goal Make Rates by Distance Group",
    subtitle = "Raw vs. Group-Specific EB vs. Common-Prior EB",
    x       = NULL,
    y       = "Make Rate",
    fill    = "Estimator"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

fg_agg |>
  filter(distance_group == "long") |>
  arrange(desc(eb_group)) |>
  select(kicker, made, attempts, raw_rate, eb_group) |>
  head(5)
