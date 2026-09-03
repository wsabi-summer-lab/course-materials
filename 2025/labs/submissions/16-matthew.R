### SETUP ###

# Uncomment if needed:
# install.packages(c("MASS", "ggplot2", "glmnet", "tidyverse"))

library(MASS)
library(ggplot2)
library(glmnet)
library(tidyverse)

set.seed(16)

#########################
### LOAD PARK EFFECTS ###
#########################

park_effects <- read_csv("../data/16_park-effects.csv.gz")

# Generate true coefficients
B0 <- 0.4
parks <- unique(park_effects$PARK)
offs  <- unique(park_effects$OT_YR)
defs  <- unique(park_effects$DT_YR)

bpark <- rnorm(length(parks), mean = 0.04, sd = sqrt(0.065))
boff  <- rnorm(length(offs), mean = 0.02, sd = sqrt(0.045))
bdef  <- rnorm(length(defs), mean = 0.03, sd = sqrt(0.07))

#############################
### DESIGN MATRIX CREATION ###
#############################

n_games <- nrow(park_effects)
n_cols  <- length(parks) + length(offs) + length(defs)

schedule_mat <- matrix(0, nrow = n_games, ncol = n_cols)
colnames(schedule_mat) <- c(
  paste0("park_", parks),
  paste0("off_", offs),
  paste0("def_", defs)
)

for (i in 1:n_games) {
  park_col <- which(colnames(schedule_mat) == paste0("park_", park_effects$PARK[i]))
  off_col  <- which(colnames(schedule_mat) == paste0("off_", park_effects$OT_YR[i]))
  def_col  <- which(colnames(schedule_mat) == paste0("def_", park_effects$DT_YR[i]))
  schedule_mat[i, c(park_col, off_col, def_col)] <- 1
}

# Add intercept
schedule_mat <- cbind(intercept = 1, schedule_mat)
beta <- c(B0, bpark, boff, bdef)
names(beta) <- colnames(schedule_mat)

# Drop reference levels to avoid singularity
drop_cols <- c(
  "intercept",
  paste0("park_", parks[1]),
  paste0("off_", offs[1]),
  paste0("def_", defs[1])
)
schedule_mat_reduced <- schedule_mat[, !(colnames(schedule_mat) %in% drop_cols)]
beta_reduced <- beta[!(names(beta) %in% drop_cols)]

# Identify park columns
park_cols <- grep("^park_", colnames(schedule_mat_reduced))
park_names <- colnames(schedule_mat_reduced)[park_cols]
true_bpark <- beta_reduced[park_names]

#################################
### SIMULATE OUTCOME VECTORS ###
#################################

sample_pos_normal <- function(n, mean = 0, sd = 1) {
  res <- numeric(n)
  i <- 1
  while (i <= n) {
    candidate <- rnorm(1, mean, sd)
    if (candidate > 0) {
      res[i] <- candidate
      i <- i + 1
    }
  }
  return(res)
}

M <- 100
sigma <- 1

lin_pred <- schedule_mat %*% beta

y_list <- vector("list", M)
for (m in 1:M) {
  noise <- sample_pos_normal(n_games, 0, sigma)
  y_list[[m]] <- round(lin_pred + noise)
}

#############################
### FIT OLS (small lambda) & RIDGE ###
#############################

b_ols_list <- matrix(NA, nrow = M, ncol = length(park_cols))
colnames(b_ols_list) <- park_names

b_ridge_list <- matrix(NA, nrow = M, ncol = length(park_cols))
colnames(b_ridge_list) <- park_names

# Use cross-validation for lambda on first 5 trials to pick average lambda
cv_lambdas <- numeric(5)
for (m in 1:5) {
  y <- y_list[[m]]
  cv_fit <- cv.glmnet(schedule_mat_reduced, y, alpha = 0, intercept = FALSE)
  cv_lambdas[m] <- cv_fit$lambda.min
}
avg_lambda <- mean(cv_lambdas)
cat("Average lambda from CV (first 5 trials):", avg_lambda, "\n")

# Use a very small lambda for OLS approx
small_lambda <- 1e-8

for (m in 1:M) {
  y <- y_list[[m]]
  
  # OLS approx with very small lambda
  ols_fit <- glmnet(schedule_mat_reduced, y, alpha = 0, lambda = small_lambda, intercept = FALSE)
  ols_coefs <- as.vector(coef(ols_fit))[-1]
  b_ols_list[m, ] <- ols_coefs[park_cols]
  
  # Ridge with average lambda
  ridge_fit <- glmnet(schedule_mat_reduced, y, alpha = 0, lambda = avg_lambda, intercept = FALSE)
  ridge_coefs <- as.vector(coef(ridge_fit))[-1]
  b_ridge_list[m, ] <- ridge_coefs[park_cols]
}

# Compute expectations
ED_b_ols <- colMeans(b_ols_list)
ED_b_ridge <- colMeans(b_ridge_list)

# Compute bias (Euclidean norm)
bias_ols <- sqrt(sum((true_bpark - ED_b_ols)^2))
bias_ridge <- sqrt(sum((true_bpark - ED_b_ridge)^2))

# Compute variance (mean squared deviation)
var_ols <- mean(rowSums((b_ols_list - matrix(ED_b_ols, nrow = M, ncol = length(ED_b_ols), byrow = TRUE))^2))
var_ridge <- mean(rowSums((b_ridge_list - matrix(ED_b_ridge, nrow = M, ncol = length(ED_b_ridge), byrow = TRUE))^2))

cat("Bias (OLS approx):", bias_ols, "\n")
cat("Bias (Ridge):", bias_ridge, "\n")
cat("Variance (OLS approx):", var_ols, "\n")
cat("Variance (Ridge):", var_ridge, "\n")

########################
### PLOT ESTIMATES VS TRUE
########################

# Sort parks by true effect
sorted_idx <- order(true_bpark)
sorted_parks <- park_names[sorted_idx]

df_plot <- data.frame(
  Park = factor(rep(sorted_parks, 2), levels = sorted_parks),
  Estimate = c(ED_b_ols[sorted_idx], ED_b_ridge[sorted_idx]),
  Method = rep(c("OLS approx", "Ridge"), each = length(sorted_parks)),
  True = rep(true_bpark[sorted_idx], 2)
)

# --- Point comparison plot ---
p1 <- ggplot(df_plot, aes(x = Park, y = Estimate, color = Method)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_point(aes(y = True), color = "black", shape = 4, size = 3, stroke = 1.5) +
  labs(title = "Estimated Park Effects: Sorted by True Value",
       y = "Effect",
       x = "Park") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# --- Bias-variance tradeoff bar plot ---
df_biasvar <- data.frame(
  Method = c("OLS approx", "Ridge"),
  Bias = c(bias_ols, bias_ridge),
  Variance = c(var_ols, var_ridge)
)

p2 <- ggplot(df_biasvar, aes(x = Method)) +
  geom_bar(aes(y = Bias, fill = "Bias"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Variance, fill = "Variance"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Bias" = "steelblue", "Variance" = "orange")) +
  labs(title = "Bias and Variance Comparison", y = "Value", x = "") +
  theme_minimal()

# --- Distribution of estimated park coefficients ---
df_dist <- rbind(
  b_ols_list[, sorted_idx] |> as.data.frame() |> pivot_longer(cols = everything(), names_to = "Park", values_to = "Estimate") |> mutate(Method = "OLS approx"),
  b_ridge_list[, sorted_idx] |> as.data.frame() |> pivot_longer(cols = everything(), names_to = "Park", values_to = "Estimate") |> mutate(Method = "Ridge")
)

df_dist$Park <- factor(df_dist$Park, levels = colnames(b_ols_list)[sorted_idx])

p3 <- ggplot(df_dist, aes(x = Park, y = Estimate, fill = Method)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.6) +
  labs(title = "Distribution of Estimated Park Effects", x = "Park", y = "Estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# --- Display plots ---
install.packages("gridExtra")  # if you haven’t already
library(gridExtra)

# Sort df_plot by True values for better visualization
df_plot_sorted <- df_plot %>% arrange(True)

# Re-make sorted plot
p1 <- ggplot(df_plot_sorted, aes(x = reorder(Park, True), y = Estimate, color = Method)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_point(aes(y = True), color = "black", shape = 4, size = 3, stroke = 1.5) +
  labs(title = "Estimated Park Effects vs True Values (Sorted)",
       y = "Effect",
       x = "Park") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Bias comparison bar chart
bias_df <- data.frame(Method = c("OLS", "Ridge"), Bias = c(bias_ols, bias_ridge))
p2 <- ggplot(bias_df, aes(x = Method, y = Bias, fill = Method)) +
  geom_col(width = 0.5) +
  theme_minimal() +
  ggtitle("Bias Comparison")

# Variance comparison bar chart
var_df <- data.frame(Method = c("OLS", "Ridge"), Variance = c(var_ols, var_ridge))
p3 <- ggplot(var_df, aes(x = Method, y = Variance, fill = Method)) +
  geom_col(width = 0.5) +
  theme_minimal() +
  ggtitle("Variance Comparison")

# Combine plots vertically
grid.arrange(p1, p2, p3, nrow = 3, heights = c(2, 1, 1))
