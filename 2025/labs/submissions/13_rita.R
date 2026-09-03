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

putts_train = putts_train %>%
  rename(
    X_train = X) %>%
  mutate(Ni = N)

putts_test = putts_test %>%
  rename(X_true = X)

k = nrow(putts_train)
X = putts_train$X_train
N = putts_train$Ni

#overall mean
p_mean = rep(mean(X), k)

#MLE
p_mle  = X

#Efron-Morris 1
C = mean(X) * (1 - mean(X))
sigma = sqrt(C / N)
Z = X / sigma
Zbar = mean(Z)
S2 = sum((Z - Zbar)^2)
sh1 = 1 - (k - 1) / S2
theta1 = Zbar + sh1 * (Z - Zbar)
p_em1 = sigma * theta1

#Efron-Morris 2
Xv = asin(sqrt((X * N + 3/8) / (N + 3/4)))
nu2 = 1 / (4 * N)
nu = sqrt(nu2)
Zv2 = Xv / nu
Zv2bar = mean(Zv2)
S2v2 = sum((Zv2 - Zv2bar)^2)
sh2 = 1 - (k - 1) / S2v2
theta2 = Zv2bar + sh2 * (Zv2 - Zv2bar)
p_em2 = sin(theta2 * nu)^2

#Empirical Bayes 1
mu = mean(X)
tau2 = var(X) - C * mean(1/N)
p_eb1 = mu + (tau2 / (tau2 + C/N)) * (X - mu)

#Empirical Bayes 2
Xbarv = mean(Xv)
S2v = sum((Xv - Xbarv)^2) / (k - 1)
tau2v  = S2v - mean(nu2)
thetaE = Xbarv + (tau2v / (tau2v + nu2)) * (Xv - Xbarv)
p_eb2 = sin(thetaE)^2

#MSE
X_true = putts_test$X_true

mse_tbl = tibble(
  method = c("mean","MLE","EM1","EM2","EB1","EB2"),
  mse    = c(
    mean((p_mean - X_true)^2),
    mean((p_mle  - X_true)^2),
    mean((p_em1  - X_true)^2),
    mean((p_em2  - X_true)^2),
    mean((p_eb1  - X_true)^2),
    mean((p_eb2  - X_true)^2)
  )
) %>%
  arrange(mse)

print(mse_tbl)
cat("Best estimator:", mse_tbl$method[1], "\n")
cat("Worst estimator:", mse_tbl$method[nrow(mse_tbl)], "\n")


# Plotting the results
results_tbl <- tibble(
  Player = putts_train$Player,
  N      = putts_train$N,         # first‐half attempt count
  X      = putts_train$X_train,   # first‐half success rate
  mean   = p_mean,                # overall‐mean prediction
  MLE    = p_mle,                 # MLE prediction
  EM1    = p_em1,                 # Efron–Morris v1
  EM2    = p_em2,                 # Efron–Morris v2
  EB1    = p_eb1,                 # Empirical Bayes v1
  EB2    = p_eb2                  # Empirical Bayes v2
)

print(results_tbl)

preds = tibble(
  X_true,
  mean = p_mean, MLE = p_mle,
  EM1  = p_em1,  EM2 = p_em2,
  EB1  = p_eb1,  EB2 = p_eb2
) %>%
  pivot_longer(-X_true, names_to="method", values_to="p_hat")


ggplot(preds, aes(x = X_true, y = p_hat, color = method)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x     = "True Second-Half Putting % (Xʹ)",
    y     = "Predicted Putting %",
    color = "Estimator"
  ) +
  theme_minimal()










