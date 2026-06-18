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

putts_train = read_csv("../data/13_putts-train.csv")
putts_test = read_csv("../data/13_putts-test.csv")

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

# TODO: compare the four training-data leaderboards.
summary(predictions$mean)
summary(predictions$mle)
summary(predictions$empirical_bayes)
summary(predictions$james_stein)


predictions[order(-predictions$mean), c("Player","mean")][1:5, ]
predictions[order(predictions$mean), c("Player","mean")][1:5, ]


predictions[order(-predictions$mle), c("Player","mle")][1:5, ]
predictions[order(predictions$mle), c("Player","mle")][1:5, ]


predictions[order(-predictions$empirical_bayes), c("Player","empirical_bayes")][1:5, ]
predictions[order(predictions$empirical_bayes), c("Player","empirical_bayes")][1:5, ]


predictions[order(-predictions$james_stein), c("Player","james_stein")][1:5, ]
predictions[order(predictions$james_stein), c("Player","james_stein")][1:5, ]

# Mean vs MLE
predictions$mean_diff <- abs(predictions$mean - predictions$mle)

head(predictions[order(-predictions$mean_diff),
                 c("Player","mle","mean","mean_diff")], 5)

head(predictions[order(predictions$mean_diff),
                 c("Player","mle","mean","mean_diff")], 5)


# Empirical Bayes vs MLE
predictions$eb_diff <- abs(predictions$empirical_bayes - predictions$mle)

head(predictions[order(-predictions$eb_diff),
                 c("Player","mle","empirical_bayes","eb_diff")], 5)

head(predictions[order(predictions$eb_diff),
                 c("Player","mle","empirical_bayes","eb_diff")], 5)


# James-Stein vs MLE
predictions$js_diff <- abs(predictions$james_stein - predictions$mle)

head(predictions[order(-predictions$js_diff),
                 c("Player","mle","james_stein","js_diff")], 5)

head(predictions[order(predictions$js_diff),
                 c("Player","mle","james_stein","js_diff")], 5)

# TODO: explain the common-variance approximation and a standardization alternative.
# Approximates as binomial, and uses league means. Can standardize by sampling variance

# TODO: join putts_test only when ready to evaluate.

# Join test data to predictions
results <- predictions %>%
  inner_join(putts_test, by = "Player")

# Golfer-level squared errors
results <- results %>%
  mutate(
    se_mean = (mean - X.y)^2,
    se_mle = (mle - X.y)^2,
    se_eb = (empirical_bayes - X.y)^2,
    se_js = (james_stein - X.y)^2
  )

# MSE for each estimator
results %>%
  summarise(
    MSE_mean = mean(se_mean),
    MSE_mle = mean(se_mle),
    MSE_EB = mean(se_eb),
    MSE_JS = mean(se_js)
  )

# TODO: compare overall MSE and golfer-level squared errors.
# EB and JS performed better for the most part overall, as well as individual golfer level