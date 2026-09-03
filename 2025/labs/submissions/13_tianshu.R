#############
### SETUP ###
#############

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

k = nrow(putts_train)
X_mean = mean(putts_train$X)
C = X_mean * (1 - X_mean)
putts_train = putts_train %>% 
  mutate(p1 = X_mean) %>%
  mutate(p2 = X) %>%
  mutate(X_tilde = X / sqrt(C / N))
X_tilde_mean = mean(putts_train$X_tilde)
S2_X_tilde = sum((putts_train$X_tilde - X_tilde_mean)^2)
putts_train = putts_train %>%
  mutate(theta_em1 = X_tilde_mean + (1 - (k-1)/S2_X_tilde) * (X_tilde - X_tilde_mean)) %>%
  mutate(p_em1 = sqrt(C / N) * theta_em1) %>% 
  mutate(nu = 1 / (2 * sqrt(N))) %>%
  mutate(X_tilde2 = asin(sqrt((X*N + 3/8) / (N + 3/4)))) %>%
  mutate(X_tilde2_em = X_tilde2 / nu)
X_tilde2_mean = mean(putts_train$X_tilde2)
S2_X_tilde2 = sum((putts_train$X_tilde2 - X_tilde2_mean)^2) / (k-1)
X_tilde2_em_mean = mean(putts_train$X_tilde2_em)
S2_X_tilde2_em = sum((putts_train$X_tilde2_em - X_tilde2_em_mean)^2)
putts_train = putts_train %>%
  mutate(theta_em2 = X_tilde2_em_mean + (1 - (k-1)/S2_X_tilde2_em) * (X_tilde2_em - X_tilde2_em_mean)) %>%
  mutate(p_em2 = sin(theta_em2 * nu) ^ 2)
S2_X = sum((putts_train$X - X_mean)^2) / (k-1)
tau2 = S2_X - C * mean(1 / putts_train$N)
tau2_2 = S2_X_tilde2 - mean(putts_train$nu ^ 2)
putts_train = putts_train %>%
  mutate(p_eb1 = X_mean + tau2 / (tau2 + C/N) * (X - X_mean)) %>%
  mutate(theta_eb2 = X_tilde2_mean + tau2_2 / (tau2_2 + nu^2) * (X_tilde2 - X_tilde2_mean)) %>%
  mutate(p_eb2 = sin(theta_eb2) ^ 2)

# Compare estimators p1, p2, p_em1, p_em2, p_eb1, p_eb2 on putts_test by computing MSE
mse_p1 = mean((putts_test$X - putts_train$p1)^2)
mse_p2 = mean((putts_test$X - putts_train$p2)^2)
mse_p_em1 = mean((putts_test$X - putts_train$p_em1)^2)
mse_p_em2 = mean((putts_test$X - putts_train$p_em2)^2)
mse_p_eb1 = mean((putts_test$X - putts_train$p_eb1)^2)
mse_p_eb2 = mean((putts_test$X - putts_train$p_eb2)^2)
mse_results = data.frame(
  Method = c("p1", "p2", "p_em1", "p_em2", "p_eb1", "p_eb2"),
  MSE = c(mse_p1, mse_p2, mse_p_em1, mse_p_em2, mse_p_eb1, mse_p_eb2)
)
mse_results # p_eb2 is the best (p_eb1 is a close second), p2 is the worst

# Plot X against the six predictors with identity line
putts_train$X2 = putts_test$X

putts_long = putts_train %>%
  pivot_longer(cols = c(p1, p2, p_em1, p_em2, p_eb1, p_eb2),
               names_to = "Estimator",
               values_to = "Estimated")

ggplot(putts_long, aes(x = X2, y = Estimated, color = Estimator)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Comparison of Putts Estimators",
       x = "True Proportion (X)",
       y = "Estimated Proportion") +
  scale_color_manual(values = c("p1" = "blue", "p2" = "red",
                                "p_em1" = "green", "p_em2" = "purple",
                                "p_eb1" = "orange", "p_eb2" = "brown")) +
  guides(color = guide_legend(title = "Estimators")) +
  theme_minimal()

# load test data (second half of season)
putts_test = read_csv("../data/13_putts-test.csv.gz")
# preview data
head(putts_test)