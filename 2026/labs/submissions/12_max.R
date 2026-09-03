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
            (AB_2020 + alpha_hat + beta_hat),
        shrinkage_weight = sigma2 / (tau2_hat + sigma2),
        data_plus_prior_precision = 1/sigma2 + 1/tau2_hat,
        post_precision = 1/posterior_variance,
        precision_equal = data_plus_prior_precision == post_precision
    )

batting_eval = batting |>
    filter(AB_2021 >= 100)

batting

batting <- batting %>%
  mutate(
    dist     = mle - mu_hat,       
    movement = empirical_bayes - mle,
    side = ifelse(dist >= 0, "Above league mean", "Below league mean")
  )

ggplot(batting) +
  geom_vline(xintercept = mu_hat, linetype = "dashed", color = "grey40") + geom_segment(
    aes(x = mle, xend = empirical_bayes, y = AB_2020, yend = AB_2020),
    arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
    linewidth = 0.4
  ) +
  aes(color = lambda) +
  scale_color_viridis_c(name = expression(lambda[i]), limits = c(0, 1))

#Precision values between posterior and data + prior are essentially equal

batting_eval |>
  summarise(
    rmse_mle  = rmse(BA_2021, mle),
    rmse_pool = rmse(BA_2021, complete_pooling),
    rmse_eb   = rmse(BA_2021, empirical_bayes)
  )

batting_eval |>
  summarise(
    rmse_normal = rmse(BA_2021, empirical_bayes),
    rmse_beta   = rmse(BA_2021, beta_binomial_eb)
  )
#Both eb and beta binomial eb result in similar RMSE

#eb: 0.033, pool: 0.0359, rmse: 0.0459

batting |>
  mutate(diff = beta_binomial_eb - empirical_bayes) |>
  arrange(desc(abs(diff))) |>
  select(playerID, AB_2020, mle, empirical_bayes, beta_binomial_eb, diff) |>
  head(10)
#Approximation matters most for low at bats numbers and extreme MLEs
t1 = tau2_hat / 2
t2 = tau2_hat * 2
batting_eval |>
  mutate(
    lambda1 = t1 / (t1 + sigma2),
    lambda2 = t2 / (t2 + sigma2),
    eb1 = mu_hat + lambda1 * (mle - mu_hat),
    eb2 = mu_hat + lambda2 * (mle - mu_hat)
  ) |>
  summarise(
    rmse_half = rmse(BA_2021, eb1),
    rmse_base = rmse(BA_2021, empirical_bayes),
    rmse_dbl  = rmse(BA_2021, eb2)
  )
#RMSE is larger for both half and double, with double giving a larger discrepancy.
#Since parameters are treated as fixed, it doesn't account for uncertainty within the parameters themselves. 

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

kicker_groups = field_goals |>
  group_by(kicker, distance_group) |>
  summarise(
    made     = sum(fg_made),
    attempts = n(),
    raw_rate = made / attempts,
    .groups  = "drop"
  ) |>
  filter(attempts >= 5)

# One Beta prior per distance group, estimated from the kicker make rates.
group_priors = kicker_groups |>
  group_by(distance_group) |>
  group_modify(~ beta_prior_from_rates(.x$made, .x$attempts)) |>
  ungroup()

# Attach the group prior (alpha, beta, center, tau2, strength) to every kicker-group row.
kicker_groups = kicker_groups |>
  left_join(group_priors, by = "distance_group")
kicker_groups

# Group-specific EB: each cell shrinks toward its own distance-group prior.
kicker_groups = kicker_groups |>
  mutate(eb_group = (made + alpha) / (attempts + alpha + beta))

# Common prior: one Beta prior estimated from ALL kicker-groups, ignoring distance.
common_prior = beta_prior_from_rates(kicker_groups$made, kicker_groups$attempts)

kicker_groups = kicker_groups |>
  mutate(
    eb_common = (made + common_prior$alpha) /
      (attempts + common_prior$alpha + common_prior$beta)
  )

# Compare raw vs both EB estimates, by distance group.
plot_df = kicker_groups |>
  pivot_longer(c(eb_group, eb_common),
               names_to = "prior", values_to = "eb_rate") |>
  mutate(prior = recode(prior,
                        eb_group  = "Group-specific prior",
                        eb_common = "Common prior"))

ggplot(plot_df, aes(raw_rate, eb_rate, color = distance_group, size = attempts)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(alpha = 0.6) +
  facet_wrap(~ prior) +
  scale_size_area(max_size = 4) +
  coord_equal() +
  labs(
    x = "Raw make rate", y = "Empirical-Bayes make rate",
    color = "Distance group", size = "Attempts",
    title = "Raw vs empirical-Bayes field-goal make rates"
  )

# CONTRAST -------------------------------------------------------------------
# Under the GROUP-SPECIFIC prior, each cell shrinks toward its own distance
# group's typical make rate: short attempts toward a high rate, long attempts
# toward a low rate. The distance effect is preserved.
#
# Under the COMMON prior, every cell shrinks toward one global center (a blend
# dominated by the more numerous, higher-rate short/medium attempts). This pulls
# LONG attempts UP (overstating long-range accuracy) and SHORT attempts slightly
# DOWN. Low-attempt long-distance kicker-groups move the most between the two.
#
# The group-specific strategy is more defensible. A short attempt and a long
# attempt are NOT exchangeable -- they have systematically different success
# probabilities -- so treating all attempts as draws from one population
# violates exchangeability. Kickers attempting from similar distances ARE
# plausibly exchangeable, so the distance groups define the right subpopulations
# to pool within. %>%
  mutate(
    p_hat_eb = (made + alpha) / (attempts + alpha + beta),
    p_hat_common = (made + 50) / (attempts + 50 + 10)
  )
