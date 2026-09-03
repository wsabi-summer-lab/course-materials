#######################
### Authors: RB, JP ###
#######################

#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

set.seed(13)

########################
### HELPER FUNCTIONS ###
########################

positive_part_js = function(x, sigma2) {
    # Centered shrinkage estimates one dimension through mean(x), so k >= 4.
    center = mean(x)
    spread = sum((x - center)^2)
    shrinkage_factor = max(0, 1 - ((length(x) - 3) * sigma2) / spread)
    center + shrinkage_factor * (x - center)
}

mse = function(truth, prediction) {
    mean((truth - prediction)^2)
}

####################
### GOLF PUTTING ###
####################

putts_train = read_csv("../data/13_putts-train.csv.gz")
putts_test = read_csv("../data/13_putts-test.csv.gz")

mu_hat = with(putts_train, weighted.mean(X, N))
C_hat = mu_hat * (1 - mu_hat)
tau2_hat = max(var(putts_train$X) - mean(C_hat / putts_train$N), 0)
sigma2_common = mean(C_hat / putts_train$N)

predictions = putts_train |>
    mutate(
        mean = mean(X),
        mle = X,
        empirical_bayes = mu_hat +
            tau2_hat / (tau2_hat + C_hat / N) * (X - mu_hat),
        james_stein = positive_part_js(X, sigma2_common)
    ) |>
    mutate(across(c(mean, mle, empirical_bayes, james_stein), ~ pmin(pmax(.x, 0), 1)))
predictions
#Shrinkage targets:
#MLE: no shrinkage, empirical_bayes: mu hat(population mean), james stein: (league mean)
#Strengths:
#MLE: none, empirical bayes: tau2_hat / (tau2_hat + C_hat / N) * (X - mu_hat), james stein max(0, 1 - ((length(x) - 3) * sigma2) / spread)
#Players with more putts will have a lower variance because there is more data, but replacing with common throws away this information.
#Standardizing as (X - X-bar)/sqrt(C/N) rescales deviations to ~unit variance, recovering per-golfer weighting before a JS-style rule.


methods = c("mean", "mle", "empirical_bayes", "james_stein")

leaderboards = map(methods, ~ predictions |>
    arrange(desc(.data[[.x]])) |>
    transmute(Player, rate = .data[[.x]], rank = row_number())) |>
    set_names(methods)

dispersion = predictions |>
    summarise(across(all_of(methods),
        list(range = ~ max(.x) - min(.x),
             sd = ~ sd(.x),
             move_from_mle = ~ mean(abs(.x - mle))))) |>
    pivot_longer(everything(), names_to = c("method", "stat"), names_pattern = "(.*)_(range|sd|move_from_mle)") |>
    pivot_wider(names_from = stat, values_from = value)
dispersion

top5 = map(leaderboards, ~ head(.x$Player, 5))
bottom5 = map(leaderboards, ~ tail(.x$Player, 5))
top5
bottom5

#The raw MLE leaderboard is most dispersed because observed rates carry sampling noise on top of true skill, exaggerating the extremes; shrinkage discounts that noise so EB/JS compress the range and barely reorder the top/bottom five.

### TASK 3: REVEAL THE TEST SET ###

evaluated = predictions |>
    select(Player, all_of(methods)) |>
    left_join(putts_test |> rename(test = X), by = "Player")

mse_table = evaluated |>
    summarise(across(all_of(methods), ~ mse(test, .x))) |>
    pivot_longer(everything(), names_to = "method", values_to = "mse") |>
    arrange(mse)
mse_table

plot_data = evaluated |>
    pivot_longer(all_of(methods), names_to = "method", values_to = "prediction")

ggplot(plot_data, aes(prediction, test)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_point(alpha = 0.5) +
    facet_wrap(~ method) +
    labs(x = "Prediction (train)", y = "Held-out test rate", title = "Predictions vs. test performance")

#EB wins on MSE, then JS, then the grand mean, with the raw MLE last; shrinking toward the league mean removes sampling noise that the MLE overfits, and EB edges JS by weighting each golfer by their own sample size rather than a common variance.


dilemma = evaluated |>
    transmute(Player, test,
        se_mle = (test - mle)^2,
        se_eb = (test - empirical_bayes)^2,
        winner = if_else(se_eb < se_mle, "empirical_bayes", "mle"))

win_counts = count(dilemma, winner)
win_counts

ggplot(dilemma, aes(se_mle, se_eb, color = winner)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_point(alpha = 0.6) +
    labs(x = "Squared error (MLE)", y = "Squared error (EB)",
         title = "Per-golfer squared error: EB vs. MLE")

#No: EB wins overall MSE but beats the MLE for only ~58% of golfers, so the better aggregate estimator still loses for individuals; this does not contradict the shrinkage result, which guarantees lower total/expected risk, not a per-coordinate win, since shrinking toward the mean hurts golfers whose true skill is genuinely far from it.


# Overrating (prediction > test) is twice as costly as underrating.
asymmetric_loss = function(truth, prediction) {
    error = prediction - truth
    weight = if_else(error > 0, 2, 1)
    mean(weight * error^2)
}

asym_table = evaluated |>
    summarise(across(all_of(methods), ~ asymmetric_loss(test, .x))) |>
    pivot_longer(everything(), names_to = "method", values_to = "asym_loss") |>
    arrange(asym_loss)
asym_table

#The ranking is unchanged (EB best, then JS, mean, MLE), so my preferred estimator stays EB; but every method's loss rises because all four are tuned for symmetric MSE and none deliberately predict low to avoid the costlier overrate. The loss function is part of the modeling decision because the optimal estimator depends on it: under this asymmetry I would shade predictions downward, which symmetric-MSE estimators never do, so picking an estimator without first fixing the loss can leave avoidable cost on the table.


