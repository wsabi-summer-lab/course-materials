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

#Task 2: compare predictions

#mle
max(predictions$mle) - min(predictions$mle) #range = 0.156
sd(predictions$mle) #sd = 0.027
#Top 5: 27895, 48084, 143, 15, 82

#empirical_bayes
max(predictions$empirical_bayes) - min(predictions$empirical_bayes) #range = 0.066
sd(predictions$empirical_bayes) #sd = 0.011
#Top 5: 85, 195, 82, 143, 15
summarize(predictions, diff = mean(empirical_bayes - mle)) #-0.000364

#James Stein
max(predictions$james_stein) - min(predictions$james_stein) #range = 0.065
sd(predictions$james_stein) #sd = 0.011
#Top 5: 85, 195, 143, 15, 82
summarize(predictions, diff = mean(james_stein - mle)) # -8.18e-18

#The raw leaderboard is likely more dispersed because outliers have more of an effect than predicted



#Task 3
  
test_predictions = putts_test %>%
  left_join(predictions, by = "Player")

ggplot(data = test_predictions) +
  geom_point(aes(x = X.y, y = X.x)) +
  geom_line(aes(x = X.y, y = mean), color = "gray", linewidth = 1.2) +
  geom_line(aes(x = X.y, y = mle), color = "dodgerblue", linewidth = 1.2) +
  geom_line(aes(x = X.y, y = empirical_bayes), color = "darkred", linewidth = 1.2) +
  geom_line(aes(x = X.y, y = james_stein), color = "gold", linewidth = 1.2)


#mean
summarize(test_predictions, mse = mse(X.x,mean)) # 0.000747
#mle
summarize(test_predictions, mse = mse(X.x,mle)) # 0.000890
#empirical bayes
summarize(test_predictions, mse = mse(X.x,empirical_bayes)) # 0.000623
#james stein
summarize(test_predictions, mse = mse(X.x,james_stein)) # 0.000625

#The empirical bayes is the best shrinkage estimator


#Task 4

test_predictions = test_predictions %>%
  mutate(
    se_mle = (X.x - mle)^2,
    se_eb = (X.x - empirical_bayes)^2,
    winner = ifelse(se_mle >= se_eb, "MLE","EB")
  )

summarize(test_predictions,
  pct_MLE = mean(winner == "MLE") * 100,
  pct_EB  = mean(winner == "EB") * 100
)
#58.4EB MLE, 41.6% EB : MLE is better significantly more often

#This means the better option (EB) doesn't win with every individual.
#This agrees with the lecture concepts, which said that while shrinkage would be the better overall,
#It could give worse results for certain individuals



#Task 5
summarize(test_predictions,
  mse_mean = mean(ifelse(mean > X.x,
                         2 * (mean - X.x)^2,
                         (mean - X.x)^2)),
  mse_mle = mean(ifelse(mle > X.x,
                        2 * (mle - X.x)^2,
                        (mle - X.x)^2)),
  mse_empirical_bayes = mean(ifelse(empirical_bayes > X.x,
                                    2 * (empirical_bayes - X.x)^2,
                                    (empirical_bayes - X.x)^2)),
  mse_james_stein = mean(ifelse(james_stein > X.x,
                                2 * (james_stein - X.x)^2,
                                (james_stein - X.x)^2))
)
  
#mean: 0.00106
#mle: 0.00130
#eb: 0.000875
#js: 0.000888

#In this case, Empirical Bayes remains the best option

#This loss function is a modeling decision because it impacts what the model considers most important to minimize






