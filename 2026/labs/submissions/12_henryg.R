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

batting = read_csv("../data/12_ba-2020-2021.csv", show_col_types = FALSE) |>
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
        #Shrinkage estimate
        empirical_bayes = mu_hat + lambda * (mle - mu_hat),
        beta_binomial_eb = (H_2020 + alpha_hat) /
            (AB_2020 + alpha_hat + beta_hat)
    )

batting_eval = batting |>
    filter(AB_2021 >= 100)





# TODO: make a shrinkage-arrow plot from mle to empirical_bayes.

ggplot(batting, aes(x = mle, xend = empirical_bayes, y = AB_2020, yend = AB_2020, color = lambda)) +
  geom_segment(arrow = arrow(length = unit(0.15, "cm")), alpha = 0.7, linewidth = 0.6) +
  geom_vline(xintercept = mu_hat, linetype = "dashed", color = "gray40") +
  scale_color_viridis_c(name = "λ (shrinkage\nweight)", option = "plasma", direction = -1) +
  facet_wrap(~ cut(AB_2020, breaks = c(0, 30, 100, Inf),
                   labels = c("Low AB (≤30)", "Mid AB (31–100)", "High AB (>100)")),
             ncol = 1, scales = "free_y") +
  labs(
    x = "Batting Average",
    y = "2020 At-Bats",
    title = "Shrinkage: MLE → Empirical Bayes",
    subtitle = "Color = λ (higher = less shrinkage) | Dashed line = league mean | Rows = AB tier"
  )






# TODO: compare estimators against BA_2021 using batting_eval and rmse().

rmse_table <- tibble(
  Estimator = c("MLE", "Complete Pooling", "Empirical Bayes"),
  RMSE = c(
    rmse(batting_eval$BA_2021, batting_eval$mle),
    rmse(batting_eval$BA_2021, batting_eval$complete_pooling),
    rmse(batting_eval$BA_2021, batting_eval$empirical_bayes)
  )
)
print(rmse_table)

# 2. RMSE by AB tier (low = 100–200, high = >200)
batting_eval |>
  mutate(ab_tier = ifelse(AB_2020 <= 200, "Low AB (≤200)", "High AB (>200)")) |>
  group_by(ab_tier) |>
  summarise(
    RMSE_MLE      = rmse(BA_2021, mle),
    RMSE_Pooling  = rmse(BA_2021, complete_pooling),
    RMSE_EB       = rmse(BA_2021, empirical_bayes),
    n = n()
  ) |>
  print()

# 3. Prediction vs 2021 BA plot
batting_eval |>
  pivot_longer(cols = c(mle, complete_pooling, empirical_bayes),
               names_to = "estimator", values_to = "prediction") |>
  mutate(estimator = recode(estimator,
                            mle = "MLE",
                            complete_pooling = "Complete Pooling",
                            empirical_bayes = "Empirical Bayes"
  )) |>
  ggplot(aes(x = prediction, y = BA_2021)) +
  geom_point(aes(color = AB_2020), alpha = 0.6, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_viridis_c(name = "2020 AB") +
  facet_wrap(~ estimator) +
  labs(
    x = "2020 Prediction", y = "2021 Batting Average",
    title = "Prediction vs. 2021 BA by Estimator",
    subtitle = "Dashed line = perfect prediction | Color = 2020 at-bats"
  ) +
  theme_minimal()






# TODO: compare the Normal-Normal and exact Beta-Binomial EB estimates.

# Estimate Beta prior from 2020 population using helper
prior <- beta_prior_from_rates(batting$H_2020, batting$AB_2020)

# Beta-Binomial EB posterior mean: (H + alpha) / (N + alpha + beta)
batting <- batting |>
  mutate(
    beta_binomial_eb = (H_2020 + prior$alpha) / (AB_2020 + prior$alpha + prior$beta)
  )


# RMSE comparison
tibble(
  Estimator = c("Normal-Normal EB", "Beta-Binomial EB"),
  RMSE = c(
    rmse(batting_eval$BA_2021, batting_eval$empirical_bayes),
    rmse(batting_eval$BA_2021, batting_eval$beta_binomial_eb)
  )
) |> print()

# Where does the approximation matter most?
batting_eval |>
  mutate(diff = beta_binomial_eb - empirical_bayes) |>
  ggplot(aes(x = AB_2020, y = diff, color = mle)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_viridis_c(name = "2020 BA (MLE)") +
  labs(
    x = "2020 At-Bats", y = "Beta-Binomial EB − Normal-Normal EB",
    title = "Where do the two EB approximations diverge?",
    subtitle = "Largest gaps at low AB and extreme batting averages"
  ) +
  theme_minimal()




# TODO: repeat with 0.5 * tau2_hat and 2 * tau2_hat.


# Function to compute EB estimates for a given tau2 scaling factor
eb_with_scaled_tau2 <- function(batting, scale) {
  tau2_scaled <- tau2_hat * scale
  lambda_scaled <- tau2_scaled / (tau2_scaled + C_hat / batting$AB_2020)
  
  batting |>
    mutate(
      scale = scale,
      empirical_bayes_scaled = mu_hat + lambda_scaled * (mle - mu_hat),
      lambda_scaled = lambda_scaled
    )
}

# Refit under three tau2 assumptions
scaled_results <- map(c(0.5, 1, 2), ~eb_with_scaled_tau2(batting, .x)) |>
  bind_rows() |>
  filter(AB_2021 >= 100)

# RMSE by scale
scaled_results |>
  group_by(scale) |>
  summarise(RMSE = rmse(BA_2021, empirical_bayes_scaled)) |>
  mutate(tau2_label = paste0("τ² × ", scale)) |>
  print()

# Plot: estimates vs 2021 BA under each scaling
scaled_results |>
  mutate(tau2_label = factor(paste0("τ² × ", scale), levels = c("τ² × 0.5", "τ² × 1", "τ² × 2"))) |>
  ggplot(aes(x = empirical_bayes_scaled, y = BA_2021, color = AB_2020)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_viridis_c(name = "2020 AB") +
  facet_wrap(~tau2_label) +
  labs(
    x = "EB Prediction", y = "2021 Batting Average",
    title = "EB Estimates Under Different τ² Scalings",
    subtitle = "Smaller τ² = more shrinkage toward league mean"
  ) +
  theme_minimal()




###################
### FIELD GOALS ###
###################

field_goals = read_csv("../data/12_field-goals.csv", show_col_types = FALSE) |>
    mutate(
        distance_group = case_when(
            ydl <= 20 ~ "short",
            ydl <= 35 ~ "medium",
            TRUE ~ "long"
        )
    )


groups = field_goals %>%
  group_by(kicker, distance_group) %>%
  summarise(
    made  = sum(fg_made),
    attempts = n(),
    .groups = "drop"
  ) %>%
  filter(attempts >= 5)


# Estimate one Beta prior per distance group using helper
group_priors <- groups |>
  group_by(distance_group) |>
  summarise(
    prior = list(beta_prior_from_rates(made, attempts)),
    .groups = "drop"
  ) |>
  mutate(
    alpha_g = map_dbl(prior, "alpha"),
    beta_g  = map_dbl(prior, "beta")
  ) |>
  select(-prior)

# Group-specific Beta-Binomial EB
groups <- groups |>
  left_join(group_priors, by = "distance_group") |>
  mutate(
    raw_rate = made / attempts,
    eb_group = (made + alpha_g) / (attempts + alpha_g + beta_g)
  )

# Common prior across all attempts
common_prior <- beta_prior_from_rates(groups$made, groups$attempts)

groups <- groups |>
  mutate(
    eb_common = (made + common_prior$alpha) / (attempts + common_prior$alpha + common_prior$beta)
  )

# Plot: raw vs group EB vs common EB by distance group
groups |>
  pivot_longer(cols = c(raw_rate, eb_group, eb_common),
               names_to = "estimator", values_to = "make_rate") |>
  mutate(
    estimator = recode(estimator,
                       raw_rate  = "Raw",
                       eb_group  = "Group-Specific EB",
                       eb_common = "Common-Prior EB"
    ),
    distance_group = factor(distance_group, levels = c("short", "medium", "long"))
  ) |>
  ggplot(aes(x = make_rate, y = estimator, color = estimator)) +
  geom_jitter(height = 0.15, alpha = 0.6, size = 1.8) +
  geom_vline(
    data = group_priors |>
      mutate(
        distance_group = factor(distance_group, levels = c("short", "medium", "long")),
        mu_g = alpha_g / (alpha_g + beta_g)
      ),
    aes(xintercept = mu_g), linetype = "dashed", color = "gray40"
  ) +
  facet_wrap(~ distance_group, ncol = 1) +
  scale_color_brewer(palette = "Set1", guide = "none") +
  labs(
    x = "Make Rate", y = NULL,
    title = "Raw vs. EB Make Rates by Distance Group",
    subtitle = "Dashed line = group mean | Each point = one kicker"
  ) +
  theme_minimal()





# TODO: aggregate by kicker and distance_group.
# TODO: estimate one Beta prior within each distance_group using beta_prior_from_rates().
# TODO: compute the group-specific Beta-Binomial EB estimate for each kicker-group.
# TODO: repeat with one common Beta prior for all field-goal attempts.
# TODO: plot raw, group-specific EB, and common-prior EB make rates by distance_group.
# TODO: contrast which distance groups move differently under the two pooling strategies.
