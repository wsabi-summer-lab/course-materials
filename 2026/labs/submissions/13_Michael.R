#######################
### Authors: RB, JP ###
#######################

#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

setwd("~/GitHub/lab-materials/2026/labs/data")

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

#Task 1

# --- Check column names ---
colnames(predictions)

# --- Print the four predictions ---
predictions |>
  select(N, X, mean, mle, empirical_bayes, james_stein) |>
  print(n = Inf)

# --- Shrinkage summary table ---
predictions |>
  summarise(
    across(c(mean, mle, empirical_bayes, james_stein), 
           list(avg = ~mean(.), sd = ~sd(.)), 
           .names = "{.col}_{.fn}")
  ) |>
  pivot_longer(everything(), names_to = c("estimator", "stat"), names_sep = "_(?=[^_]+$)") |>
  pivot_wider(names_from = stat, values_from = value) |>
  print()

# --- How much each estimate moves from MLE ---
predictions |>
  mutate(
    move_mean = abs(mean - mle),
    move_eb   = abs(empirical_bayes - mle),
    move_js   = abs(james_stein - mle)
  ) |>
  summarise(across(starts_with("move"), mean)) |>
  print()

#Grand mean puts everyone at the mean which is the most shrinkage. MLE predicts each player to their value with a lightly bigger standard deviation, and EB shrinks each player proportional to their sample size, JS applies a single common shrinkage factro to everyone, and they report similar here because the N must be roughly a similar size for EB and JS.
#Replacing player specific variances C/Ni with a single mean (C/n1) treats allj players as equally reliable or have the same change of being a true or false outlier. High N players will get over shrunk whereas Low N players will get under shrunk.

#Task 2

# --- Rank golfers best to worst for each estimator ---
rankings = predictions |>
  mutate(
    rank_mle = rank(-mle),
    rank_mean = rank(-mean),
    rank_eb = rank(-empirical_bayes),
    rank_js = rank(-james_stein)
  )

# --- Range and SD of predictions ---
predictions |>
  summarise(across(c(mean, mle, empirical_bayes, james_stein),
                   list(range = ~max(.) - min(.), sd = ~sd(.)),
                   .names = "{.col}_{.fn}")) |>
  pivot_longer(everything(), names_to = c("estimator", "stat"), names_sep = "_(?=[^_]+$)") |>
  pivot_wider(names_from = stat, values_from = value) |>
  print()

# --- Top 5 and bottom 5 for each estimator ---
for (est in c("mle", "empirical_bayes", "james_stein")) {
  cat("\n--- Top 5:", est, "---\n")
  predictions |> arrange(desc(.data[[est]])) |> select(Player, N, X, all_of(est)) |> slice(1:5) |> print()
  cat("--- Bottom 5:", est, "---\n")
  predictions |> arrange(.data[[est]])           |> select(Player, N, X, all_of(est)) |> slice(1:5) |> print()
}

# --- How far each prediction moves from MLE ---
predictions |>
  mutate(
    move_eb   = empirical_bayes - mle,
    move_js   = james_stein - mle,
    move_mean = mean - mle
  ) |>
  select(Player, N, X, move_eb, move_js) |>
  arrange(desc(abs(move_eb))) |>
  print(n = 10)

# --- Plot: spread of predictions across estimators ---
predictions |>
  select(Player, mle, empirical_bayes, james_stein, mean) |>
  pivot_longer(-Player, names_to = "estimator", values_to = "prediction") |>
  mutate(estimator = factor(estimator, levels = c("mean", "james_stein", "empirical_bayes", "mle"))) |>
  ggplot(aes(x = prediction, fill = estimator)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribution of Predictions by Estimator",
       x = "Predicted Putting Rate", y = "Density", fill = "Estimator") +
  theme_minimal()

#MLE spreads significantly more than Eb and JS
#However the same players appear at the extremes for the estimators so it doesn't change who the worse or best is just changes how significantly better or worse you are.

#Task 3

# --- Join test outcomes ---
results = predictions |>
  inner_join(putts_test, by = "Player", suffix = c("_train", "_test"))

# --- Compute MSE for each estimator ---
mse_table = tibble(
  estimator = c("mean", "mle", "empirical_bayes", "james_stein"),
  MSE = c(
    mse(results$X_test, results$mean),
    mse(results$X_test, results$mle),
    mse(results$X_test, results$empirical_bayes),
    mse(results$X_test, results$james_stein)
  )
) |> arrange(MSE)

print(mse_table)

# --- Plot: predictions vs test performance for each estimator ---
results |>
  select(Player, X_test, mean, mle, empirical_bayes, james_stein) |>
  pivot_longer(-c(Player, X_test), names_to = "estimator", values_to = "prediction") |>
  mutate(estimator = factor(estimator, 
                            levels = c("mean", "mle", "empirical_bayes", "james_stein"))) |>
  ggplot(aes(x = prediction, y = X_test)) +
  geom_point(alpha = 0.3, size = 1.2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~estimator) +
  labs(title = "Predicted vs Actual Test Putting Rate",
       x = "Prediction", y = "Actual Test Rate") +
  theme_minimal()

#Ranking : EB (0.000623), JS (0.000625), mean (0.000747), mle (0.000890)

#Task 4

# --- Calculate squared error per player for MLE and EB ---
task4 = results |>
  mutate(
    se_mle = (mle - X_test)^2,
    se_eb  = (empirical_bayes - X_test)^2,
    eb_wins = se_eb < se_mle
  )

# --- How often does each win? ---
task4 |>
  summarise(
    eb_wins_pct  = mean(eb_wins),
    mle_wins_pct = 1 - mean(eb_wins)
  ) |>
  print()

# --- Plot: EB vs MLE squared error per player ---
task4 |>
  ggplot(aes(x = se_mle, y = se_eb, color = eb_wins)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                     labels = c("TRUE" = "EB wins", "FALSE" = "MLE wins")) +
  labs(title = "Squared Error: EB vs MLE per Player",
       x = "MLE Squared Error", y = "EB Squared Error", color = "") +
  theme_minimal()

#The model with the best MSe does not win for every individual. This does not contradict the lesson because the model with the best MSE on average will do best, but will hurt some individuals more than other. 

#Task 5

# --- Asymmetric loss function ---
asymmetric_loss = function(actual, prediction) {
  error = prediction - actual
  ifelse(error > 0,   # overrated
         2 * error^2, # double penalty
         1 * error^2) # normal penalty
}

# --- Compute asymmetric loss for each estimator ---
al_table = tibble(
  estimator = c("mean", "mle", "empirical_bayes", "james_stein"),
  asymmetric_loss = c(
    mean(asymmetric_loss(results$X_test, results$mean)),
    mean(asymmetric_loss(results$X_test, results$mle)),
    mean(asymmetric_loss(results$X_test, results$empirical_bayes)),
    mean(asymmetric_loss(results$X_test, results$james_stein))
  )
) |> arrange(asymmetric_loss)

print(al_table)

# --- Compare MSE vs asymmetric loss rankings side by side ---
mse_table |>
  inner_join(al_table, by = "estimator") |>
  arrange(asymmetric_loss) |>
  print()

#The ranking stays the same but the gap between the values increases. 