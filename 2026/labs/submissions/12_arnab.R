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
    filter(AB_2021 >= 50)

# TODO: make a shrinkage-arrow plot from mle to empirical_bayes.
batting_eval %>%
  arrange(mle) %>%
  mutate(player = row_number()) %>%
  ggplot() +
  geom_segment(
    aes(
      x = mle,
      xend = empirical_bayes,
      y = player,
      yend = player
    ),
    arrow = arrow(length = unit(0.1, "in")),
    linewidth = 0.4
  ) +
  geom_point(aes(x = mle, y = player),
             size = 2, color = "red") +
  geom_point(aes(x = empirical_bayes, y = player),
             size = 2, color = "blue") +
  labs(
    title = "Shrinkage Plot: MLE vs Empirical Bayes",
    x = "Batting Average Estimate",
    y = "Player"
  ) +
  theme_minimal()

# TODO: compare estimators against BA_2021 using batting_eval and rmse().
rmseMLE <- rmse(batting_eval$BA_2021, batting_eval$mle)
rmseMLE

rmseComPool <- rmse(batting_eval$BA_2021, batting_eval$complete_pooling)
rmseComPool



ggplot(data = batting_eval, aes(x = empirical_bayes, y = BA_2021)) +
  geom_point()
# TODO: compare the Normal-Normal and exact Beta-Binomial EB estimates.
rmseBBEB <- rmse(batting_eval$BA_2021, batting_eval$beta_binomial_eb)
rmseBBEB

rmseEmpiricalBayes <- rmse(batting_eval$BA_2021, batting_eval$empirical_bayes)
rmseEmpiricalBayes

ggplot(batting_eval,
       aes(x = empirical_bayes,
           y = beta_binomial_eb)) +
  geom_point(alpha = .6) +
  geom_abline(slope = 1,
              intercept = 0,
              linetype = "dashed",
              color = "red") +
  labs(
    title = "Normal-Normal vs Beta-Binomial EB Estimates",
    x = "Normal-Normal EB",
    y = "Beta-Binomial EB"
  ) +
  theme_minimal()

#The approximation matters most for beta binomial given its dependent on binary outcomes

# TODO: repeat with 0.5 * tau2_hat and 2 * tau2_hat.
tau_vals <- c(0.5, 1, 2)

map_dfr(tau_vals, function(mult) {
  
  tau2_new <- mult * tau2_hat
  
  pred <- with(
    batting_eval,
    mu_hat +
      tau2_new / (tau2_new + sigma2) *
      (mle - mu_hat)
  )
  
  tibble(
    tau_multiplier = mult,
    rmse = rmse(batting_eval$BA_2021, pred)
  )
})

# modifying tau^2 affects the amount of noise coming in from the population, and thus affecting shrinkage towards mean. going in both directions has increased RMSE

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

# TODO: aggregate by kicker and distance_group.
field_goals_by_kicker <- field_goals |>
  group_by(kicker, distance_group) |>
  summarise(
    attempts = n(),
    made = sum(fg_made),
    fg_pct = made / attempts,
    .groups = "drop"
  )
# TODO: estimate one Beta prior within each distance_group using beta_prior_from_rates().
fg_short <- field_goals_by_kicker |>
  filter(distance_group == "short")
  
fg_med <- field_goals_by_kicker |>
  filter(distance_group == "medium")

fg_long <- field_goals_by_kicker |>
  filter(distance_group == "long")

beta_prior_short <- beta_prior_from_rates(fg_short$made, fg_short$attempts)
beta_prior_med <- beta_prior_from_rates(fg_med$made, fg_med$attempts)
beta_prior_long <- beta_prior_from_rates(fg_long$made, fg_long$attempts)
# TODO: compute the group-specific Beta-Binomial EB estimate for each kicker-group.
priors_by_group <- bind_rows(
  beta_prior_short |> mutate(distance_group = "short"),
  beta_prior_med   |> mutate(distance_group = "medium"),
  beta_prior_long  |> mutate(distance_group = "long")
)

field_goals_group_eb <- field_goals_by_kicker |>
  left_join(priors_by_group, by = "distance_group") |>
  mutate(
    group_beta_binomial_eb =
      (made + alpha) / (attempts + alpha + beta)
  )
# TODO: repeat with one common Beta prior for all field-goal attempts.
common_prior <- beta_prior_from_rates(
  field_goals_by_kicker$made,
  field_goals_by_kicker$attempts
)

field_goals_common_eb <- field_goals_by_kicker |>
  mutate(
    alpha_common = common_prior$alpha,
    beta_common = common_prior$beta,
    common_beta_binomial_eb =
      (made + alpha_common) /
      (attempts + alpha_common + beta_common)
  )
# TODO: plot raw, group-specific EB, and common-prior EB make rates by distance_group.
field_goals_plot <- field_goals_group_eb |>
  left_join(
    field_goals_common_eb |>
      select(kicker, distance_group, common_beta_binomial_eb),
    by = c("kicker", "distance_group")
  ) |>
  select(
    kicker,
    distance_group,
    raw = fg_pct,
    group_specific_eb = group_beta_binomial_eb,
    common_prior_eb = common_beta_binomial_eb
  ) |>
  pivot_longer(
    cols = c(raw, group_specific_eb, common_prior_eb),
    names_to = "method",
    values_to = "make_rate"
  )

ggplot(field_goals_plot,
       aes(x = distance_group,
           y = make_rate,
           color = method)) +
  geom_jitter(
    width = 0.18,
    height = 0,
    alpha = 0.55,
    size = 2
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Raw and Empirical Bayes Field Goal Make Rates",
    x = "Distance Group",
    y = "Make Rate",
    color = "Estimate"
  ) +
  theme_minimal()

# TODO: contrast which distance groups move differently under the two pooling strategies.
# Group specific eb is a lot more clustered with their make rates.Medium and long seem to move sifferently with the different EB's, with common EB being a lot more spread out. 