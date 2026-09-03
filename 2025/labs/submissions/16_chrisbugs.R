#############
### SETUP ###
#############

# install.packages(c("ggplot2", "glmnet", "tidyverse"))
library(ggplot2)
library(glmnet)
library(tidyverse)

# set seed
set.seed(16)

####################
### PARK EFFECTS ###
####################

# load park effects data
park_effects = read_csv("../data/16_park-effects.csv.gz")
# preview data
head(park_effects)


# Get number of parks, off, and def teams 
n_park <- length(unique(park_effects$PARK))
n_park
n_off  <- length(unique(park_effects$OT_YR))
n_off
n_def  <- length(unique(park_effects$DT_YR))
n_def

# Generate beta vector
beta0      <- 0.4
beta_park  <- rnorm(n_park, mean = 0.04, sd = sqrt(0.065))
beta_off   <- rnorm(n_off,  mean = 0.02, sd = sqrt(0.045))
beta_def   <- rnorm(n_def,  mean = 0.03, sd = sqrt(0.07))
beta_vector <- c(beta0, beta_park, beta_off, beta_def)

# print beta vector size
length(beta_vector)

# Generate X matrix
# X <- model.matrix(~factor(PARK) + factor(OT_YR) + factor(DT_YR), data = park_effects)

park_levels <- sort(unique(park_effects$PARK))
off_levels  <- sort(unique(park_effects$OT_YR))
def_levels  <- sort(unique(park_effects$DT_YR))

df <- park_effects %>%
  mutate(
    row   = row_number(),
    PARK  = factor(PARK,  levels = park_levels),
    OT_YR = factor(OT_YR, levels = off_levels),
    DT_YR = factor(DT_YR, levels = def_levels)
  )

X_wide <- df %>%
  select(row, PARK, OT_YR, DT_YR) %>%
  pivot_longer(
    cols      = c(PARK, OT_YR, DT_YR),
    names_to  = "factor",
    values_to = "level"
  ) %>%
  mutate(
    value = 1,
    var   = case_when(
      factor == "PARK"  ~ paste0("PARK_", level),
      factor == "OT_YR" ~ paste0("OFF_",  level),
      factor == "DT_YR" ~ paste0("DEF_",  level)
    )
  ) %>%
  select(row, var, value) %>%
  pivot_wider(
    id_cols       = row,
    names_from    = var,
    values_from   = value,
    values_fill   = list(value = 0)
  )

X <- X_wide %>%
  select(
    row,
    paste0("PARK_", park_levels),
    paste0("OFF_",  off_levels),
    paste0("DEF_",  def_levels)
  ) %>%
  mutate(Intercept = 1) %>%
  select(Intercept, everything(), -row) %>% 
  as.matrix()


# Print dimensions of X matrix
dim(X)

# For loop for task 3
mu      <- as.numeric(X %*% beta_vector) 
n_obs   <- length(mu)
n_sims  <- 100

# pre-allocate matrix to hold all simulations
Y_sim <- matrix(NA, nrow = n_obs, ncol = n_sims)

for (m in seq_len(n_sims)) {
  
  # 1) draw raw normals, then reject and redraw any negatives
  y_star <- rnorm(n_obs, mean = mu, sd = 1)
  while (any(y_star < 0)) {
    idx          <- which(y_star < 0)
    y_star[idx]  <- rnorm(length(idx), mean = mu[idx], sd = 1)
  }
  
  # 2) round to integers
  Y_sim[, m] <- round(y_star)
}


X_df  <- as.data.frame(X) %>% select(-Intercept)
X_mat <- as.matrix(X_df)
park_codes   <- sort(unique(park_effects$PARK))
park_vars    <- paste0("PARK_", park_codes)
n_sims   <- ncol(Y_sim)
lambdas  <- 10^seq(-1, 1, by = 0.5)

park_ols_est   <- matrix(NA, n_sims, length(park_codes),
                         dimnames=list(NULL, park_codes))
park_ridge_est <- matrix(NA, n_sims, length(park_codes),
                         dimnames=list(NULL, park_codes))

for(m in seq_len(n_sims)) {
  y <- Y_sim[, m]
  
  # — OLS via lm()
  fit_ols <- lm(y ~ ., data = cbind(y = y, X_df))
  coefs_ols <- coef(fit_ols)
  park_ols_est[m, ] <- coefs_ols[park_vars]
  
  # — Ridge via cv.glmnet()
  fit_ridge_cv <- cv.glmnet(
    x           = X_mat,
    y           = y,
    alpha       = 0,          # ridge
    lambda      = lambdas,
    nfolds      = 5,
    standardize = FALSE,
    intercept   = TRUE
  )
  # extract the λ that minimizes CV error
  λ_best <- fit_ridge_cv$lambda.min
  
  # grab the coefficients at λ_best
  rid_coefs <- coef(fit_ridge_cv, s = "lambda.min")
  rid_vec   <- as.numeric(rid_coefs)
  names(rid_vec) <- rownames(rid_coefs)
  park_ridge_est[m, ] <- rid_vec[park_vars]
}

# Means
mean_ols   <- colMeans(park_ols_est, na.rm=TRUE)
mean_ols
mean_ridge <- colMeans(park_ridge_est, na.rm=TRUE)
mean_ridge

true_park <- set_names(beta_park, park_codes)
# Bias
bias2_ols   <- ( true_park - mean_ols   )^2
bias2_ols
bias2_ridge <- ( true_park - mean_ridge )^2
bias2_ridge

# Variance
var_ols   <- apply(park_ols_est,   2, var)
var_ols
var_ridge <- apply(park_ridge_est, 2, var)
var_ridge


# Assemble into df
results_per_park <- tibble(
  park       = park_codes,
  true       = true_park,
  mean_ols   = mean_ols,
  mean_ridge = mean_ridge,
  bias2_ols   = bias2_ols,
  bias2_ridge = bias2_ridge,
  var_ols     = var_ols,
  var_ridge   = var_ridge
)

# Bias
results_per_park %>%
  select(park, bias2_ols, bias2_ridge) %>%
  pivot_longer(cols = c(bias2_ols, bias2_ridge),
               names_to  = "model",
               values_to = "bias2") %>%
  mutate(model = if_else(model == "bias2_ols", "OLS", "Ridge")) %>%
  ggplot(aes(x = model, y = bias2, fill = model)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Park‐Level Bias²",
    x     = "Model",
    y     = "Bias²"
  ) +
  theme_minimal()

# Variance
results_per_park %>%
  select(park, var_ols, var_ridge) %>%
  pivot_longer(cols = c(var_ols, var_ridge),
               names_to  = "model",
               values_to = "variance") %>%
  mutate(model = if_else(model == "var_ols", "OLS", "Ridge")) %>%
  ggplot(aes(x = model, y = variance, fill = model)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Park‐Level Variance",
    x     = "Model",
    y     = "Variance"
  ) +
  theme_minimal()
