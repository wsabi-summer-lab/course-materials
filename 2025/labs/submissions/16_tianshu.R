#############
### SETUP ###
#############

# install.packages(c("ggplot2", "glmnet", "tidyverse"))
library(ggplot2)
library(glmnet)
library(tidyverse)
library(truncnorm)

# set seed
set.seed(16)

####################
### PARK EFFECTS ###
####################

# load park effects data
park_effects = read_csv("../data/16_park-effects.csv.gz")
# preview data
head(park_effects)

parks = sort(unique(park_effects$PARK))
teams = sort(unique(park_effects$OT_YR))
n_parks = length(parks)
n_teams = length(teams)

beta = rep(0, n_parks + 2 * n_teams + 1)
beta[1] = 0.4
beta[2:(n_parks+1)] = rnorm(n_parks, mean=0.04, sd=0.065)
beta[(n_parks+2):(n_parks+n_teams+1)] = rnorm(n_teams, mean=0.02, sd=0.045)
beta[(n_parks+n_teams+2):(n_parks+2*n_teams+1)] = rnorm(n_teams, mean=0.03, sd=0.07)

park_effects = park_effects %>%
  mutate(park_id = as.numeric(factor(PARK)),
         off_id = as.numeric(factor(OT_YR)),
         def_id = as.numeric(factor(DT_YR)))

X = matrix(0, nrow=nrow(park_effects), ncol=1 + n_parks + 2 * n_teams)
colnames(X) = c("Intercept", 
                 paste0("park_", parks), 
                 paste0("off_", teams), 
                 paste0("def_", teams))
X[, "Intercept"] = 1
for (i in seq_len(nrow(park_effects))) {
  X[i, paste0("park_", park_effects$PARK[i])] = 1
  X[i, paste0("off_", park_effects$OT_YR[i])] = 1
  X[i, paste0("def_", park_effects$DT_YR[i])] = 1
}
# Simulate 100 times and store predictions
m = 100
ols_factors = matrix(0, nrow=m, ncol=n_parks)
ridge_factors = matrix(0, nrow=m, ncol=n_parks)
for (i in 1:m) {
  park_effects = park_effects %>%
    mutate(sim_runs = rtruncnorm(
      n = 1, a = 0, b = Inf,
      mean = beta[1] + beta[park_id + 1] + beta[n_parks + off_id + 1] 
      + beta[n_parks + n_teams + def_id + 1],sd = 1
    ))
  
  ols_model = lm(park_effects$sim_runs ~ ., data = as.data.frame(X))
  ridge_model = cv.glmnet(x=X, y=park_effects$sim_runs, nfolds=5, alpha=0, 
                       family="gaussian", standardize=FALSE)
  ridge_best_lambda = ridge_model$lambda.min

  ols_factors[i, ] = coef(ols_model)[3:(n_parks + 2)]
  ols_factors[i, n_parks] = 0
  ridge_factors[i, ] = coef(ridge_model, s=ridge_best_lambda)[3:(n_parks + 2)]
}
avg_ols_factors = colMeans(ols_factors)
avg_ridge_factors = colMeans(ridge_factors)
  
# Compute l2-norm bias comparing to beta[2:(n_parks + 1)]
bias_ols = sqrt(sum((avg_ols_factors - beta[2:(n_parks + 1)])^2))
bias_ridge = sqrt(sum((avg_ridge_factors - beta[2:(n_parks + 1)])^2))
var_ols = mean(apply(ols_factors, 2, var))
var_ridge = mean(apply(ridge_factors, 2, var))

cat(sprintf("OLS Bias: %f, Variance: %f\n", bias_ols, var_ols))
cat(sprintf("Ridge Bias: %f, Variance: %f\n", bias_ridge, var_ridge))

# Plot predictions vs true park factors
park_factors_df = data.frame(
  Park = parks,
  OLS = avg_ols_factors,
  Ridge = avg_ridge_factors,
  True = beta[2:(n_parks + 1)]
)
ggplot(park_factors_df, aes(x = Park)) +
  geom_point(aes(y = OLS, color = "OLS"), size = 2) +
  geom_point(aes(y = Ridge, color = "Ridge"), size = 2) +
  geom_point(aes(y = True, color = "True"), size = 2) +
  labs(title = "Park Factors: OLS vs Ridge vs True",
       x = "Park",
       y = "Factor Value") +
  scale_color_manual(values = c("OLS" = "blue", "Ridge" = "red", "True" = "black")) +
  coord_flip() +
  theme_minimal()