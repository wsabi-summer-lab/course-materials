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

cat("Max precision discrepancy:", max(abs(1/batting$posterior_variance - (1/batting$sigma2 + 1/tau2_hat))), "\n")


ggplot(batting, aes(x = mle, xend = empirical_bayes, y = AB_2020, yend = AB_2020, color = lambda)) +
  geom_segment(aes(alpha = 0.7), arrow = arrow(length = unit(0.15, "cm"))) +
  scale_color_gradient(low = "red", high = "blue", name = "Shrinkage\nWeight λ") +
  scale_y_log10() +
  geom_vline(xintercept = mu_hat, linetype = "dashed", color = "black") +
  labs(
    title = "Empirical Bayes Shrinkage: MLE → EB Estimate",
    x = "Batting Average",
    y = "2020 At-Bats (log scale)"
  ) +
  theme_minimal()

batting_eval = batting |>
    filter(AB_2021 >= 100)

# Task 3: Evaluate predictions against 2021 held-out batting averages
# batting_eval already filters to AB_2021 >= 100

# Overall RMSE for each estimator
rmse_overall = tibble(
  method = c("MLE", "Complete Pooling", "Empirical Bayes"),
  rmse = c(
    rmse(batting_eval$BA_2021, batting_eval$mle),
    rmse(batting_eval$BA_2021, batting_eval$complete_pooling),
    rmse(batting_eval$BA_2021, batting_eval$empirical_bayes)
  )
)
print(rmse_overall)

# Split into low and high AB groups, then compute RMSE separately
batting_eval = batting_eval |>
  mutate(ab_group = ifelse(AB_2020 <= 60, "Low AB", "High AB"))

rmse_by_group = batting_eval |>
  group_by(ab_group) |>
  summarise(
    mle_rmse = rmse(BA_2021, mle),
    pooling_rmse = rmse(BA_2021, complete_pooling),
    eb_rmse = rmse(BA_2021, empirical_bayes)
  )
print(rmse_by_group)

# Plot predictions vs 2021 batting average
batting_eval |>
  pivot_longer(cols = c(mle, complete_pooling, empirical_bayes),
               names_to = "method", values_to = "prediction") |>
  mutate(method = recode(method,
                         mle = "MLE",
                         complete_pooling = "Complete Pooling",
                         empirical_bayes = "Empirical Bayes"
  )) |>
  ggplot(aes(x = prediction, y = BA_2021, color = method)) +
  geom_point(alpha = 0.4, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~method) +
  labs(
    title = "2020 Predictions vs 2021 Batting Average",
    x = "2020 Prediction", y = "2021 Batting Average"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Task 4: Compare Normal-Normal EB vs Beta-Binomial EB
batting |>
  ggplot(aes(x = empirical_bayes, y = beta_binomial_eb, color = AB_2020)) +
  geom_point(alpha = 0.5, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_gradient(low = "red", high = "blue", name = "2020 AB") +
  labs(
    title = "Normal-Normal vs Beta-Binomial EB Estimates",
    x = "Normal-Normal EB",
    y = "Beta-Binomial EB"
  ) +
  theme_minimal()

#it matters the most for players woith low AB and crazy averages. 


# Task 5: Sensitivity to tau2 — refit with tau2 scaled by 0.5 and 2
batting = batting |>
  mutate(
    # Half the between-player variance — stronger shrinkage toward mean
    lambda_half = (tau2_hat * 0.5) / (tau2_hat * 0.5 + sigma2),
    eb_half = mu_hat + lambda_half * (mle - mu_hat),
    
    # Double the between-player variance — weaker shrinkage, closer to MLE
    lambda_double = (tau2_hat * 2) / (tau2_hat * 2 + sigma2),
    eb_double = mu_hat + lambda_double * (mle - mu_hat)
  )

# RMSE comparison across all three tau2 choices (eval set only)
batting_eval = batting |> filter(AB_2021 >= 100)

tibble(
  tau2_scale = c("0.5x", "1x (original)", "2x"),
  rmse = c(
    rmse(batting_eval$BA_2021, batting_eval$eb_half),
    rmse(batting_eval$BA_2021, batting_eval$empirical_bayes),
    rmse(batting_eval$BA_2021, batting_eval$eb_double)
  )
) |> print()

#Estimates change because tau is the estimate for the player variation so if the tua is smaller is assumes more shrinkage and vice versa. The EB intervals are too narrow because it ignores a lot of uncertainty obtaining the values from the data. 

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

# Aggregate by kicker and distance group, keep kicker-groups with 5+ attempts
fg_agg = field_goals |>
  group_by(kicker, distance_group) |>
  summarise(made = sum(fg_made), attempts = n(), .groups = "drop") |>
  filter(attempts >= 5)

# Group-specific Beta prior and EB estimate for each distance group
fg_group = fg_agg |>
  group_by(distance_group) |>
  mutate(beta_prior_from_rates(made, attempts)) |>
  ungroup() |>
  mutate(
    raw_rate = made / attempts,
    eb_group = (made + alpha) / (attempts + alpha + beta)
  )

# Common prior across all attempts
common_prior = beta_prior_from_rates(fg_agg$made, fg_agg$attempts)
fg_group = fg_group |>
  mutate(
    eb_common = (made + common_prior$alpha) /
      (attempts + common_prior$alpha + common_prior$beta)
  )

# Plot raw vs group-specific EB vs common-prior EB by distance group
fg_group |>
  pivot_longer(cols = c(raw_rate, eb_group, eb_common),
               names_to = "method", values_to = "rate") |>
  mutate(method = recode(method,
                         raw_rate = "Raw",
                         eb_group = "Group-Specific EB",
                         eb_common = "Common Prior EB"
  )) |>
  ggplot(aes(x = method, y = rate, color = method)) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  facet_wrap(~distance_group) +
  labs(
    title = "Raw vs EB Make Rates by Distance Group",
    x = "", y = "Make Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#The long kicks are moved significantly since the raw data is so dispersed, compared to the short field goal which is already close to the spread. The Group specificn emperical bayes is the most defensible since 25 yarder and 55 yarder are not exchangable which is why grouping by distance is important.

