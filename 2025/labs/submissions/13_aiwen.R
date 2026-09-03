#############
### SETUP ###
#############

rm(list=ls())

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(13)

####################
### GOLF PUTTING ###
####################

# load training data (first half of season)
putts_train = read_csv("../data/13_putts-train.csv.gz")
# preview data
head(putts_train)
names(putts_train)

# load test data (second half of season)
putts_test = read_csv("../data/13_putts-test.csv.gz")
# preview data
head(putts_test)

##########################
### task 1
# eqn 1: overall mean X
mean_X <- mean(putts_train$X)
putts_train$p_hat_mean <- mean_X

# eqn 2: MLE for each player
putts_train$p_hat_mle <- putts_train$X

# eqn 3: efron-morris
C <- mean(putts_train$X) * (1 - mean(putts_train$X))

putts_train <- putts_train %>%
  mutate(
    sigma_i = sqrt(C / N),
    X_tilde = X / sigma_i
  )

X_tilde_bar <- mean(putts_train$X_tilde)

S2_X_tilde <- sum((putts_train$X_tilde - X_tilde_bar)^2)

putts_train <- putts_train %>%
  mutate(
    theta_hat_em1 = X_tilde_bar + (1 - (nrow(putts_train) - 1) / S2_X_tilde) * (X_tilde - X_tilde_bar),
    p_hat_em1 = theta_hat_em1 * sigma_i
  )

# eqn 4: efron-morris part 2
putts_train <- putts_train %>%
  mutate(
    H = N * X, # number of putts made 
    X_tilde_tilde = asin(sqrt((H + 3/8) / (N + 3/4))),
    nu_i = sqrt(1 / (4 * N)),
    X_standardized = X_tilde_tilde / nu_i
  )

X_standardized_bar <- mean(putts_train$X_standardized)
S2_X_standardized <- sum((putts_train$X_standardized - X_standardized_bar)^2)

putts_train <- putts_train %>%
  mutate(
    theta_hat_em2 = X_standardized_bar + (1 - (nrow(putts_train) - 1) / S2_X_standardized) * (X_standardized - X_standardized_bar),
    p_hat_em2 = sin(theta_hat_em2 * nu_i)^2
  )

# eqn 5: empirical bayes 1
mu_hat <- mean(putts_train$X)
S2_X <- var(putts_train$X)
tau2_hat <- S2_X - C * mean(1 / putts_train$N)

putts_train <- putts_train %>%
  mutate(
    p_hat_eb1 = mu_hat + (tau2_hat / (tau2_hat + C / N)) * (X - mu_hat)
  )

# eqn 6: empirical bayes 2
putts_train <- putts_train %>%
  mutate(
    X_tilde = asin(sqrt((H + 3/8) / (N + 3/4))),
    nu2_i = 1 / (4 * N)
  )

mu_tilde <- mean(putts_train$X_tilde)
S2_X_tilde <- var(putts_train$X_tilde)
mean_nu2 <- mean(putts_train$nu2_i)
tau2_tilde <- S2_X_tilde - mean_nu2

putts_train <- putts_train %>%
  mutate(
    theta_hat_eb2 = mu_tilde + (tau2_tilde / (tau2_tilde + nu2_i)) * (X_tilde - mu_tilde),
    p_hat_eb2 = sin(theta_hat_eb2)^2
  )

##########################
### task 2

# store results in here
mse_results <- data.frame(
  estimator = character(),
  mse = numeric(),
  stringsAsFactors = FALSE
)

names(putts_train)
names(putts_test)

mse_mean <- mean((putts_train$p_hat_mean - putts_test$X)^2)
mse_results <- rbind(mse_results, data.frame(estimator = "p_hat_mean", mse = mse_mean))

mse_mle <- mean((putts_train$p_hat_mle - putts_test$X)^2)
mse_results <- rbind(mse_results, data.frame(estimator = "p_hat_mle", mse = mse_mle))

mse_em1 <- mean((putts_train$p_hat_em1 - putts_test$X)^2)
mse_results <- rbind(mse_results, data.frame(estimator = "p_hat_em1", mse = mse_em1))

mse_em2 <- mean((putts_train$p_hat_em2 - putts_test$X)^2)
mse_results <- rbind(mse_results, data.frame(estimator = "p_hat_em2", mse = mse_em2))

mse_eb1 <- mean((putts_train$p_hat_eb1 - putts_test$X)^2)
mse_results <- rbind(mse_results, data.frame(estimator = "p_hat_eb1", mse = mse_eb1))

mse_eb2 <- mean((putts_train$p_hat_eb2 - putts_test$X)^2)
mse_results <- rbind(mse_results, data.frame(estimator = "p_hat_eb2", mse = mse_eb2))

##########################
### task 3

mse_results <- mse_results[order(mse_results$mse), ]

## empirical bayes 2 best, MLE worst

##########################
### task 4

combined_data <- data.frame(
  player_id = 1:nrow(putts_test),
  true_X = putts_test$X,
  p_hat_mean = putts_train$p_hat_mean,
  p_hat_mle = putts_train$p_hat_mle,
  p_hat_em1 = putts_train$p_hat_em1,
  p_hat_em2 = putts_train$p_hat_em2,
  p_hat_eb1 = putts_train$p_hat_eb1,
  p_hat_eb2 = putts_train$p_hat_eb2
)

# convert to long format for ggplot
plot_data_long <- combined_data %>%
  pivot_longer(
    cols = starts_with("p_hat"),
    names_to = "estimator",
    values_to = "prediction"
  )

ggplot(plot_data_long, aes(x = true_X, y = prediction, color = estimator)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") + # y = x line
  labs(
    title = "Predicted vs. Actual Second-Half Putting Performance",
    x = "Actual Second-Half Putting Percentage (X')",
    y = "Predicted Putting Percentage (p̂)",
    color = "Estimator"
  ) +
  theme_minimal()
