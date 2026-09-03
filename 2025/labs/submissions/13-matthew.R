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
head(putts_train)

# load test data (second half of season)
putts_test = read_csv("../data/13_putts-test.csv.gz")
head(putts_test)

# Calculate constants
mean = mean(putts_train$X)
c = mean * (1 - mean)
Tau = (1 / 196) * sum((putts_train$X - mean)^2) - c * mean(1 / putts_train$N)

# Estimator inputs and transformations
putts_train = putts_train %>%
  mutate(
    Mean = mean,
    MLE = X,
    Xsquiggle = X / sqrt(c / N),
    Xsquiggly = asin(sqrt((N * X + 3 / 8) / (N + 3 / 4))),
    ro = 0.25 / N  # Not used, but kept for compatibility
  )

# Means and variances for EM/EB formulas
squiggle_mean = mean(putts_train$Xsquiggle)
squiggly_mean = mean(putts_train$Xsquiggly)
S1 = sum((putts_train$Xsquiggle - squiggle_mean)^2)
S2 = sum((putts_train$Xsquiggly - squiggly_mean)^2)

# Compute all shrinkage estimators
putts_train = putts_train %>%
  mutate(
    # Efron-Morris Version 1
    EM1 = (squiggle_mean + (1 - 196 / S1) * (Xsquiggle - squiggle_mean)) * sqrt(c / N),
    
    # Efron-Morris Version 2
    EM2 = sin(squiggly_mean + (1 - 196 / S2) * (Xsquiggly - squiggly_mean))^2,
    
    # Empirical Bayes Version 1
    EB1 = mean + Tau / (Tau + c / N) * (X - mean),
    
    # Empirical Bayes Version 2
    EB2 = {
      v2 = 1 / (4 * N)
      mu2 = mean(Xsquiggly)
      sin(mu2 + Tau / (Tau + v2) * (Xsquiggly - mu2))^2
    }
  )

##########################
### EVALUATION & PLOTS ###
##########################

# Join predictions with test data
putts_trained = putts_train %>%
  select(Mean, MLE, EM1, EM2, EB1, EB2, Player) %>%
  left_join(putts_test, by = "Player") %>%
  mutate(
    Mean_error = (Mean - X)^2,
    MLE_error = (MLE - X)^2,
    EM1_error = (EM1 - X)^2,
    EM2_error = (EM2 - X)^2,
    EB1_error = (EB1 - X)^2,
    EB2_error = (EB2 - X)^2
  )

# Compute RMSEs
MeanRMSE = sqrt(mean(putts_trained$Mean_error))
MLERMSE = sqrt(mean(putts_trained$MLE_error))
EM1RMSE = sqrt(mean(putts_trained$EM1_error))
EM2RMSE = sqrt(mean(putts_trained$EM2_error))
EB1RMSE = sqrt(mean(putts_trained$EB1_error))
EB2RMSE = sqrt(mean(putts_trained$EB2_error))

# Collect and print RMSE results
rmse_results <- tibble(
  Method = c("Mean", "MLE", "EM1", "EM2", "EB1", "EB2"),
  RMSE = c(MeanRMSE, MLERMSE, EM1RMSE, EM2RMSE, EB1RMSE, EB2RMSE)
)

print(rmse_results)

# Scatterplot of predictions vs actual
ggplot(putts_trained, aes(x = X)) +
  geom_point(aes(y = Mean, color = "Mean")) +
  geom_point(aes(y = MLE, color = "MLE")) +
  geom_point(aes(y = EM1, color = "EM1")) +
  geom_point(aes(y = EM2, color = "EM2")) +
  geom_point(aes(y = EB1, color = "EB1")) +
  geom_point(aes(y = EB2, color = "EB2")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Golf Putting Estimates vs Actual Values",
    x = "Actual Putts Made",
    y = "Estimated Putts Made",
    color = "Method"
  ) +
  theme_minimal()
