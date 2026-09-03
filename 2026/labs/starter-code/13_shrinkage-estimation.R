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

# TODO: compare the four training-data leaderboards.
# TODO: explain the common-variance approximation and a standardization alternative.
# TODO: join putts_test only when ready to evaluate.
# TODO: compare overall MSE and golfer-level squared errors.
