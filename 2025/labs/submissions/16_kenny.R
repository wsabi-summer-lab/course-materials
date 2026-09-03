library(ggplot2)
library(glmnet)
library(tidyverse)
library(truncnorm)
library(dplyr)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

set.seed(16)

park_effects = read_csv("16_park-effects.csv.gz")

head(park_effects)

n_obs <- nrow(park_effects)
parks <- unique(park_effects$PARK)
off_teams <- unique(park_effects$OT_YR)
def_teams <- unique(park_effects$DT_YR)

beta_0 <- 0.4
beta_park <- rnorm(n_obs, mean = 0.04, sd = 0.065)
beta_off <- rnorm(n_obs, mean = 0.02, sd = 0.045)
beta_def <- rnorm(n_obs, mean = 0.03, sd = 0.07)

park_index <- match(park_effects$PARK, parks)
off_index <- match(park_effects$OT_YR, off_teams)
def_index <- match(park_effects$DT_YR, def_teams)

mu <- beta_0 + beta_park[park_index] + beta_off[off_index] + beta_def[def_index]

M <- 10
y_list <- vector("list", M)

for (m in 1:M) {
  y_list[[m]] <- rtruncnorm(n = n_obs, a = 0, mean = mu, sd = 1)
}

# Doing OLS and Ridge

X <- model.matrix(~ OT_YR + DT_YR + PARK, data = park_effects)

ols_matrix <- matrix(NA, nrow = M, ncol = ncol(X))

for (m in 1:M) {
  y <- y_list[[m]]
  fit <- lm.fit(x = X, y = y)
  ols_matrix[m, ] <- coef(fit)
}

ridge_matrix <- matrix(NA, nrow = M, ncol = ncol(X))
lambda_val <- 1 

for (m in 1:M) {
  y <- y_list[[m]]
  fit <- glmnet(X, y, alpha = 0, lambda = lambda_val, standardize = FALSE)
  ridge_matrix[m, ] <- as.numeric(coef(fit))[-1]
}

ols_matrix
ridge_matrix

mean_ols   <- colMeans(ols_matrix,   na.rm = TRUE)
mean_ridge <- colMeans(ridge_matrix, na.rm = TRUE)

bias_ols = sqrt(sum((mean_ols - beta[2:(n_parks + 1)])^2))
bias_ridge = sqrt(sum((mean_ridge - beta[2:(n_parks + 1)])^2))

coef_cmp <- tibble(
  term       = names(mean_ols),
  ols_mean   = mean_ols,
  ridge_mean = mean_ridge
)

ggplot(coef_cmp, aes(x = ols_mean, y = ridge_mean)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    x = "Average OLS Coefficient",
    y = "Average Ridge Coefficient",
    title = "OLS vs Ridge: Average Coefficients Comparison"
  ) +
  theme_minimal()



