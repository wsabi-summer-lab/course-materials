#############
### SETUP ###
#############

rm(list=ls())

# install.packages(c("ggplot2", "glmnet", "tidyverse", "truncnorm"))
library(ggplot2)
library(glmnet)
library(tidyverse)
library(truncnorm)

set.seed(16)

####################
### PARK EFFECTS ###
####################

# Load data
park_effects <- read_csv("../data/16_park-effects.csv.gz")

####################
# 1. True parameters
parks <- sort(unique(park_effects$PARK))
off_teams <- sort(unique(park_effects$OT_YR))
def_teams <- sort(unique(park_effects$DT_YR))

beta_0 <- 0.4
beta_park <- rnorm(length(parks), mean = 0.04, sd = 0.065)
beta_off <- rnorm(length(off_teams), mean = 0.02, sd = 0.045)
beta_def <- rnorm(length(def_teams), mean = 0.03, sd = 0.07)

names(beta_park) <- parks
names(beta_off) <- off_teams
names(beta_def) <- def_teams

####################
# 2. Design matrix using model.matrix()
# Note: Drops one level for each factor
X <- model.matrix(~ factor(PARK) + factor(OT_YR) + factor(DT_YR), data = park_effects)

# Identify dropped baseline levels
park_baseline <- levels(factor(park_effects$PARK))[1]
off_baseline <- levels(factor(park_effects$OT_YR))[1]
def_baseline <- levels(factor(park_effects$DT_YR))[1]

# Match beta coefficients to model.matrix structure
true_beta_named <- c("(Intercept)" = beta_0)

for (colname in colnames(X)) {
  if (grepl("factor\\(PARK\\)", colname)) {
    park <- sub("factor\\(PARK\\)", "", colname)
    true_beta_named[colname] <- beta_park[park]
  }
  if (grepl("factor\\(OT_YR\\)", colname)) {
    team <- sub("factor\\(OT_YR\\)", "", colname)
    true_beta_named[colname] <- beta_off[team]
  }
  if (grepl("factor\\(DT_YR\\)", colname)) {
    team <- sub("factor\\(DT_YR\\)", "", colname)
    true_beta_named[colname] <- beta_def[team]
  }
}

true_beta_vector <- true_beta_named[colnames(X)]

####################
# 3. Simulate y but do 5 datasets b/c takes super long to run
M <- 5
y_list <- vector("list", M)

for (m in 1:M) {
  mu <- X %*% true_beta_vector
  y_sim <- round(rtruncnorm(length(mu), a = 0, mean = mu, sd = 1))
  y_list[[m]] <- y_sim
}

####################
# 4. Fit models and recover full park coefficients

ols_estimates <- matrix(0, nrow = M, ncol = length(parks))
ridge_estimates <- matrix(0, nrow = M, ncol = length(parks))
colnames(ols_estimates) <- parks
colnames(ridge_estimates) <- parks

for (m in 1:M) {
  y <- y_list[[m]]
  
  # --- OLS ---
  ols_model <- lm(y ~ factor(PARK) + factor(OT_YR) + factor(DT_YR), data = park_effects)
  ols_beta <- coef(ols_model)
  
  # Reconstruct full PARK coefficients (baseline set to 0)
  ols_park_full <- rep(0, length(parks))
  names(ols_park_full) <- parks
  
  for (p in parks) {
    if (p == park_baseline) {
      ols_park_full[p] <- 0
    } else {
      term <- paste0("factor(PARK)", p)
      ols_park_full[p] <- ifelse(term %in% names(ols_beta), ols_beta[term], 0)
    }
  }
  ols_estimates[m, ] <- ols_park_full
  
  # --- Ridge ---
  lambdas <- 10^seq(-3, 3, by = 0.2)
  ridge_model <- cv.glmnet(X, y, alpha = 0, lambda = lambdas, nfolds = 5, standardize = FALSE)
  
  ridge_beta_raw <- coef(ridge_model, s = "lambda.min")
  ridge_beta <- as.vector(ridge_beta_raw)[-1]  # exclude intercept
  names(ridge_beta) <- rownames(ridge_beta_raw)[-1]  # get correct names
  
  # Reconstruct full PARK coefficients
  ridge_park_full <- rep(0, length(parks))
  names(ridge_park_full) <- parks
  
  for (p in parks) {
    if (p == park_baseline) {
      ridge_park_full[p] <- 0
    } else {
      term <- paste0("factor(PARK)", p)
      ridge_park_full[p] <- ifelse(term %in% names(ridge_beta), ridge_beta[term], 0)
    }
  }
  ridge_estimates[m, ] <- ridge_park_full
}

####################
# 5. Bias
avg_ols <- colMeans(ols_estimates)
avg_ridge <- colMeans(ridge_estimates)

bias_ols <- sqrt(sum((beta_park - avg_ols)^2))
bias_ridge <- sqrt(sum((beta_park - avg_ridge)^2))

####################
# 6. Variance
var_ols <- mean(apply(ols_estimates, 2, var))
var_ridge <- mean(apply(ridge_estimates, 2, var))

####################
# 7. Visualization
## compare coefs
coef_df <- tibble(
  PARK = parks,
  OLS = avg_ols,
  Ridge = avg_ridge
)

# Reshape for ggplot
coef_long <- coef_df %>%
  pivot_longer(cols = c("OLS", "Ridge"), names_to = "Model", values_to = "Coefficient")

# Plot
ggplot(coef_long, aes(x = Coefficient, y = fct_reorder(PARK, Coefficient))) +
  geom_point(aes(color = Model), position = position_dodge(width = 0.5), size = 2) +
  labs(title = "Estimated PARK Coefficients: OLS vs Ridge",
       x = "Coefficient Estimate",
       y = "Park",
       color = "Model") +
  theme_minimal()
