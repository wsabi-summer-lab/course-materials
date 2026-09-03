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

batting = read_csv("C:/Users/sundw/Downloads/12_ba-2020-2021.csv", show_col_types = FALSE) |>
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
ggplot(batting) +
  geom_segment(
    aes(x = mle, xend = empirical_bayes, y = reorder(playerID, mle), yend = reorder(playerID, mle)),
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
    color = "steelblue"
  ) +
  geom_point(aes(x = mle, y = reorder(playerID, mle)), color = "gray40", size = 1.5) +
  geom_vline(xintercept = mu_hat, linetype = "dashed", color = "firebrick") +
  labs(x = "Batting Average", y = NULL, title = "Shrinkage from MLE to Empirical Bayes") +
  theme_minimal()

# TODO: compare estimators against BA_2021 using batting_eval and rmse().
rmse(batting_eval$BA_2021, batting_eval$mle)
rmse(batting_eval$BA_2021, batting_eval$complete_pooling)
rmse(batting_eval$BA_2021, batting_eval$empirical_bayes)
rmse(batting_eval$BA_2021, batting_eval$beta_binomial_eb)

# > rmse(batting_eval$BA_2021, batting_eval$mle)
# [1] 0.04594797
# > rmse(batting_eval$BA_2021, batting_eval$complete_pooling)
# [1] 0.03588243
# > rmse(batting_eval$BA_2021, batting_eval$empirical_bayes)
# [1] 0.03327683
# > rmse(batting_eval$BA_2021, batting_eval$beta_binomial_eb)
# [1] 0.03327785

# TODO: compare the Normal-Normal and exact Beta-Binomial EB estimates.
batting_eval |>
  summarise(
    rmse_nn = rmse(BA_2021, empirical_bayes),
    rmse_bb = rmse(BA_2021, beta_binomial_eb)
  )

# rmse_nn rmse_bb
# <dbl>   <dbl>
#   1  0.0333  0.0333

# TODO: repeat with 0.5 * tau2_hat and 2 * tau2_hat.
for (scale in c(0.5, 1, 2)) {
  tau2_scaled  <- scale * tau2_hat
  strength_s   <- mu_hat * (1 - mu_hat) / tau2_scaled - 1
  alpha_s      <- mu_hat * strength_s
  beta_s       <- (1 - mu_hat) * strength_s
  
  batting_eval_s <- batting_eval |>
    mutate(
      lambda_s  = tau2_scaled / (tau2_scaled + sigma2),
      eb_nn     = mu_hat + lambda_s * (mle - mu_hat),
      eb_bb     = (H_2020 + alpha_s) / (AB_2020 + alpha_s + beta_s)
    )
  
  cat(sprintf(
    "tau2 scale = %.1f | NN RMSE = %.4f | BB RMSE = %.4f\n",
    scale,
    rmse(batting_eval_s$BA_2021, batting_eval_s$eb_nn),
    rmse(batting_eval_s$BA_2021, batting_eval_s$eb_bb)
  ))
}

# tau2 scale = 0.5 | NN RMSE = 0.0336 | BB RMSE = 0.0336
# tau2 scale = 1.0 | NN RMSE = 0.0333 | BB RMSE = 0.0333
# tau2 scale = 2.0 | NN RMSE = 0.0342 | BB RMSE = 0.0342

###################
### FIELD GOALS ###
###################

field_goals = read_csv("C:/Users/sundw/Downloads/12_field-goals.csv", show_col_types = FALSE) |>
    mutate(
        distance_group = case_when(
            ydl <= 20 ~ "short",
            ydl <= 35 ~ "medium",
            TRUE ~ "long"
        )
    )

# TODO: aggregate by kicker and distance_group.

fg_agg <- field_goals |>
  group_by(kicker, distance_group) |>
  summarise(
    made     = sum(fg_made),
    attempts = n(),
    raw_rate = made / attempts,
    .groups  = "drop"
  )

# TODO: estimate one Beta prior within each distance_group using beta_prior_from_rates().

group_priors <- fg_agg |>
  group_by(distance_group) |>
  group_modify(~ beta_prior_from_rates(.x$made, .x$attempts)) |>
  ungroup()

# TODO: compute the group-specific Beta-Binomial EB estimate for each kicker-group.

fg_agg <- fg_agg |>
  left_join(group_priors, by = "distance_group") |>
  mutate(
    eb_group = (made + alpha) / (attempts + alpha + beta)
  )

# TODO: repeat with one common Beta prior for all field-goal attempts.

common_prior <- beta_prior_from_rates(fg_agg$made, fg_agg$attempts)

fg_agg <- fg_agg |>
  mutate(
    eb_common = (made + common_prior$alpha) / (attempts + common_prior$alpha + common_prior$beta)
  )

# TODO: plot raw, group-specific EB, and common-prior EB make rates by distance_group.

fg_agg |>
  pivot_longer(cols = c(raw_rate, eb_group, eb_common),
               names_to = "estimator", values_to = "rate") |>
  ggplot(aes(x = estimator, y = rate, color = estimator)) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  facet_wrap(~ distance_group) +
  labs(x = NULL, y = "Make Rate", title = "FG Make Rates by Distance Group") +
  theme_minimal() +
  theme(legend.position = "none")

# TODO: contrast which distance groups move differently under the two pooling strategies.

fg_agg |>
  group_by(distance_group) |>
  summarise(
    mean_raw    = mean(raw_rate),
    mean_group  = mean(eb_group),
    mean_common = mean(eb_common),
    move_group  = mean_group  - mean_raw,
    move_common = mean_common - mean_raw,
    .groups = "drop"
  )

# distance_group mean_raw mean_group mean_common move_group move_common
# <chr>             <dbl>      <dbl>       <dbl>      <dbl>       <dbl>
#   1 long              0.496      0.533       0.595    0.0373      0.0994 
# 2 medium            0.653      0.725       0.706    0.0713      0.0526 
# 3 short             0.934      0.941       0.925    0.00723    -0.00837
