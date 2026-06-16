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
        empirical_bayes = mu_hat + lambda * (mle - mu_hat),
        beta_binomial_eb = (H_2020 + alpha_hat) /
            (AB_2020 + alpha_hat + beta_hat)
    )

batting_eval = batting |>
    filter(AB_2021 >= 100)

# TODO: make a shrinkage-arrow plot from mle to empirical_bayes.
# TODO: compare estimators against BA_2021 using batting_eval and rmse().
# TODO: compare the Normal-Normal and exact Beta-Binomial EB estimates.
# TODO: repeat with 0.5 * tau2_hat and 2 * tau2_hat.

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

# TODO: aggregate by kicker and distance_group.
# TODO: estimate one Beta prior within each distance_group using beta_prior_from_rates().
# TODO: compute the group-specific Beta-Binomial EB estimate for each kicker-group.
# TODO: repeat with one common Beta prior for all field-goal attempts.
# TODO: plot raw, group-specific EB, and common-prior EB make rates by distance_group.
# TODO: contrast which distance groups move differently under the two pooling strategies.
