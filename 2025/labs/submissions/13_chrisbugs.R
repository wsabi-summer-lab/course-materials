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

# load test data (second half of season)
putts_test = read_csv("../data/13_putts-test.csv.gz")
# preview data
head(putts_test)

# 13.1.2: Mean value of X
mean_x = mean(putts_train$X)
mean_x

# 13.1.3: EM1
c = mean_x*(1-mean_x)
putts = putts_train %>%
  mutate(
    sigma = sqrt(c / N),
    x_tilda = X / sigma
  )
putts

x_tilda_bar = mean(putts$x_tilda)
S2 = sum((putts$x_tilda - x_tilda_bar)^2)
shrink = 1- (nrow(putts)-1)/S2
x_tilda_bar

putts = putts %>%
  mutate(
    theta_em1 = x_tilda_bar + shrink * (x_tilda - x_tilda_bar),
    phat_em1 = theta_em1 * sigma
  )
putts

# 13.1.4: EM2
putts = putts %>%
  mutate(
    H = N*X,
    x_tilda_tilda = asin(sqrt((H + 3/8) / (N + 3/4))),
    nu2 = sqrt(1 / (4 * N)),
    new_x_tilda   = x_tilda_tilda / nu2
  )

S2_2 = sum((putts$new_x_tilda - mean(putts$new_x_tilda))^2) 
shrink_2 = 1 - (nrow(putts) - 1) / S2_2
theta_em2 = mean(putts$new_x_tilda) + shrink_2 * (putts$new_x_tilda - mean(putts$new_x_tilda))
putts = putts %>%
  mutate(
    theta_em2 = theta_em2,
    phat_em2 = sin(theta_em2 * nu2)^2  
  )
putts

# 13.1.5: Empirical Bayes v1
S2_3 = var(putts$X)
tau_2 = S2_3 - c * (mean(1/putts$N))
putts = putts %>%
  mutate(
    shirnkage_bayes = tau_2 / (tau_2+c/N),
    phat_emp_bayes_v1 = mean_x + shirnkage_bayes * (putts$X-mean_x)
  )

# 13.1.6: Empirical Bayes v2
putts = putts %>%
  mutate(
    nu2_2 = (1 / (4 * N))
  )
S2_4 = var(putts$x_tilda_tilda)
mu = mean(putts$x_tilda_tilda)
tau_2_2 = S2_4 - mean(putts$nu2_2)
putts = putts %>%
  mutate(
    shirnkage_bayes_v2 = tau_2_2 / (tau_2_2 + mean(putts$nu2_2)),
    theta_emp_bayes = mu + shirnkage_bayes_v2 * (putts$x_tilda_tilda - mu),
    phat_emp_bayes_v2 = sin(theta_emp_bayes)^2
  )
putts$phat_emp_bayes_v2

# 13.2 Use the test data to evaluate the models via MSE
mse_results = putts_test %>%
  summarise(
    mse_naive = mean((X - mean_x)^2),
    mse_mle = mean((X - putts$X)^2),
    mse_em1 = mean((X - putts$phat_em1)^2),
    mse_em2 = mean((X - putts$phat_em2)^2),
    mse_emp_bayes_v1 = mean((X - putts$phat_emp_bayes_v1)^2),
    mse_emp_bayes_v2 = mean((X - putts$phat_emp_bayes_v2)^2)
  )
mse_results

# 13.3: Order them top down
mse_results = mse_results %>%
  pivot_longer(cols = everything(), names_to = "model", values_to = "mse") %>%
  arrange(mse)
mse_results

# 13.4: Scatterplot of phats against X's for all 6 esimators on the same plot, with a different color for each estimator, and include an identity line y=x to show perfect predictions
ggplot(putts_test, aes(x = X)) +
  geom_point(aes(y = phat_em1, color = "EM1"), alpha = 0.5) +
  geom_point(aes(y = phat_em2, color = "EM2"), alpha = 0.5) +
  geom_point(aes(y = phat_emp_bayes_v1, color = "Empirical Bayes v1"), alpha = 0.5) +
  geom_point(aes(y = phat_emp_bayes_v2, color = "Empirical Bayes v2"), alpha = 0.5) +
  geom_point(aes(y = X, color = "MLE"), alpha = 0.5) +
  geom_point(aes(y = mean_x, color = "Naive"), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Predicted Probabilities vs Actual Outcomes",
       x = "Actual Outcomes (X)",
       y = "Predicted Probabilities (phat)",
       color = "Estimator") +
  theme_minimal()


ggplot(mapping = aes(x = putts_test$X)) +                 # X-axis from putt_test
  geom_point(aes(y = putts$phat_em1,  colour = "EM1"),  alpha = 0.5) +
  geom_point(aes(y = putts$phat_em2,  colour = "EM2"),  alpha = 0.5) +
  geom_point(aes(y = putts$phat_emp_bayes_v1, colour = "Empirical Bayes v1"),
             alpha = 0.5) +
  geom_point(aes(y = putts$phat_emp_bayes_v2, colour = "Empirical Bayes v2"),
             alpha = 0.5) +
  geom_point(aes(y = putts$X,         colour = "MLE"),  alpha = 0.5) +
  geom_point(aes(y = mean_x,          colour = "Naive"), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title  = "Predicted Probabilities vs Test Outcomes",
       x      = "Actual Outcomes (putt_test$X)",
       y      = "Predicted Probabilities (phat)",
       colour = "Estimator") +
  theme_minimal()
