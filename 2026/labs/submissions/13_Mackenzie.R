#############
### SETUP ###
#############

library(tidyverse)
library(ggplot2)

set.seed(13)

########################
### HELPER FUNCTIONS ###
########################

positive_part_js = function(x, sigma2) {
  center = mean(x)
  spread = sum((x - center)^2)
  shrinkage_factor = max(0, 1 - ((length(x) - 3) * sigma2) / spread)
  center + shrinkage_factor * (x - center)
}

mse = function(truth, prediction) {
  mean((truth - prediction)^2)
}

####################
### LOAD DATA ######
####################

putts_train = read_csv("/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/13_putts-train.csv")
putts_test  = read_csv("/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/13_putts-train.csv")

# TRAIN: Player, N, X
# TEST:  Player, N, X  (test outcomes are also named X)

# Rename test outcome to avoid collision
putts_test = putts_test |> rename(test = X)

###########################################
### SHRINKAGE QUANTITIES (TRAIN ONLY) #####
###########################################

mu_hat = with(putts_train, weighted.mean(X, N))
C_hat = mu_hat * (1 - mu_hat)
tau2_hat = max(var(putts_train$X) - mean(C_hat / putts_train$N), 0)
sigma2_common = mean(C_hat / putts_train$N)

###########################################
### FOUR ESTIMATORS #######################
###########################################

predictions = putts_train |>
  mutate(
    mean = mean(X),
    mle = X,
    empirical_bayes = mu_hat +
      tau2_hat / (tau2_hat + C_hat / N) * (X - mu_hat),
    james_stein = positive_part_js(X, sigma2_common)
  ) |>
  mutate(across(c(mean, mle, empirical_bayes, james_stein),
                ~ pmin(pmax(.x, 0), 1)))

###########################################
### TASK 2: LEADERBOARDS ##################
###########################################

leaderboards = predictions |>
  select(Player, mean, mle, empirical_bayes, james_stein) |>
  pivot_longer(
    -Player,
    names_to = "method",
    values_to = "estimate"
  ) |>
  group_by(method) |>
  arrange(desc(estimate), .by_group = TRUE)

# Range and standard deviation of predictions
leaderboard_summary = predictions |>
  summarise(
    mean_range = diff(range(mean)),
    mle_range = diff(range(mle)),
    eb_range = diff(range(empirical_bayes)),
    js_range = diff(range(james_stein)),
    mean_sd = sd(mean),
    mle_sd = sd(mle),
    eb_sd = sd(empirical_bayes),
    js_sd = sd(james_stein)
  )

print(leaderboard_summary)

# Top 5 golfers for each method
top5 = leaderboards |>
  group_by(method) |>
  slice_head(n = 5)

print(top5)

# Bottom 5 golfers for each method
bottom5 = leaderboards |>
  group_by(method) |>
  slice_tail(n = 5)

print(bottom5)

# How far predictions move from the MLE
movement = predictions |>
  summarise(
    mean_avg_move = mean(abs(mean - mle)),
    eb_avg_move = mean(abs(empirical_bayes - mle)),
    js_avg_move = mean(abs(james_stein - mle))
  )

print(movement)

###########################################
### TASK 3: JOIN TEST SET #################
###########################################

eval = predictions |>
  left_join(putts_test, by = "Player")

###########################################
### MSE FOR ALL METHODS ###################
###########################################

mse_results = eval |>
  summarise(
    mean = mse(test, mean),
    mle = mse(test, mle),
    empirical_bayes = mse(test, empirical_bayes),
    james_stein = mse(test, james_stein)
  )

print(mse_results)

###########################################
### PLOT: PREDICTION VS TEST ##############
###########################################

eval_long = eval |>
  pivot_longer(
    c(mean, mle, empirical_bayes, james_stein),
    names_to = "method",
    values_to = "pred"
  )

ggplot(eval_long, aes(pred, test)) +
  geom_point(alpha = .5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  facet_wrap(~method) +
  theme_minimal()

###########################################
### TASK 4: CONSULTANT'S DILEMMA ##########
###########################################

# Determine best shrinkage estimator automatically
best_shrinkage = c(
  empirical_bayes = mse_results$empirical_bayes,
  james_stein = mse_results$james_stein
)

best_method = names(which.min(best_shrinkage))

errors = eval |>
  mutate(
    se_mle = (test - mle)^2
  )

if (best_method == "empirical_bayes") {
  errors = errors |>
    mutate(se_best = (test - empirical_bayes)^2)
} else {
  errors = errors |>
    mutate(se_best = (test - james_stein)^2)
}

print(paste("Best shrinkage estimator:", best_method))

ggplot(errors, aes(se_mle, se_best)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    x = "MLE Squared Error",
    y = paste(best_method, "Squared Error")
  ) +
  theme_minimal()

###########################################
### TASK 5: ASYMMETRIC LOSS ###############
###########################################

asym_loss = function(pred, truth) {
  ifelse(
    pred > truth,
    2 * (pred - truth)^2,
    (pred - truth)^2
  )
}

asym_results = eval |>
  summarise(
    mean = mean(asym_loss(mean, test)),
    mle = mean(asym_loss(mle, test)),
    empirical_bayes = mean(asym_loss(empirical_bayes, test)),
    james_stein = mean(asym_loss(james_stein, test))
  )

print(asym_results)