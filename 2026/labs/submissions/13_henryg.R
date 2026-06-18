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


predictions |>
  select(Player, N, X, mean, mle, empirical_bayes, james_stein) |>
  arrange(desc(X)) |>
  print(n = Inf)

# --- Standardization demo (approximate common-variance approach) ---
# Dividing each deviation (Xi - mu_hat) by sqrt(C_hat / Ni) gives
# approximate Z-scores with unit variance. JS applied to these Z-scores
# uses a single sigma^2 = 1, then predictions are back-transformed.
putts_train_std = putts_train |>
  mutate(
    se_i = sqrt(C_hat / N),
    z_i  = (X - mu_hat) / se_i,
    z_js = positive_part_js(z_i, 1),
    js_standardized = pmin(pmax(mu_hat + z_js * se_i, 0), 1)
  )

cat("\n=== Standardized JS vs Common-Variance JS (first 6 rows) ===\n")
predictions |>
  select(Player, X, james_stein) |>
  left_join(putts_train_std |> select(Player, js_standardized), by = "Player") |>
  head(6) |>
  print()




# TODO: explain the common-variance approximation and a standardization alternative.


##############
### TASK 2 ###
##############

# --- Rank golfers under each estimator ---
rank_by = function(df, col) {
  df |>
    arrange(desc({{ col }})) |>
    mutate(rank = row_number()) |>
    select(rank, Player, {{ col }})
}



# --- Range and SD of predictions ---
pred_spread = predictions |>
  summarise(
    across(c(mean, mle, empirical_bayes, james_stein),
           list(range = ~ max(.x) - min(.x),
                sd    = ~ sd(.x)),
           .names = "{.col}__{.fn}")
  ) |>
  pivot_longer(everything(),
               names_to  = c("estimator", "stat"),
               names_sep = "__") |>
  pivot_wider(names_from = stat, values_from = value)

cat("\n=== Spread of Predictions ===\n")
print(pred_spread)

# --- Top and bottom 5 per estimator ---
top_bottom = function(df, col, label) {
  ranked = df |> arrange(desc({{ col }})) |>
    mutate(rank = row_number()) |>
    select(rank, Player, N, value = {{ col }})
  cat(sprintf("\n--- %s ---\n", label))
  cat("Top 5:\n");    print(head(ranked, 5))
  cat("Bottom 5:\n"); print(tail(ranked, 5))
}

top_bottom(predictions, mean,           "Grand Mean")
top_bottom(predictions, mle,            "MLE")
top_bottom(predictions, empirical_bayes,"Empirical Bayes")
top_bottom(predictions, james_stein,    "James-Stein")

# --- Movement from MLE ---
movement = predictions |>
  mutate(
    move_mean = abs(mean           - mle),
    move_eb   = abs(empirical_bayes - mle),
    move_js   = abs(james_stein    - mle)
  ) |>
  summarise(
    avg_move_mean = mean(move_mean),
    avg_move_eb   = mean(move_eb),
    avg_move_js   = mean(move_js),
    max_move_mean = max(move_mean),
    max_move_eb   = max(move_eb),
    max_move_js   = max(move_js)
  )

cat("\n=== Average and Max Movement from MLE ===\n")
print(movement)

# --- Plot: predictions vs MLE for all methods ---
predictions_long = predictions |>
  select(Player, N, mle, empirical_bayes, james_stein, mean) |>
  pivot_longer(c(empirical_bayes, james_stein, mean),
               names_to  = "estimator",
               values_to = "prediction") |>
  mutate(estimator = recode(estimator,
                            empirical_bayes = "Empirical Bayes",
                            james_stein     = "James-Stein",
                            mean            = "Grand Mean"
  ))

ggplot(predictions_long, aes(x = mle, y = prediction, color = estimator)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(size = N), alpha = 0.75) +
  facet_wrap(~ estimator) +
  scale_color_manual(values = c("Empirical Bayes" = "#E63946",
                                "James-Stein"     = "#457B9D",
                                "Grand Mean"      = "#2A9D8F")) +
  labs(
    title    = "Shrinkage Estimators vs. MLE",
    subtitle = "Points above/below the dashed line are pulled toward the league mean",
    x        = "MLE (observed putting rate)",
    y        = "Shrunken prediction",
    size     = "Putts attempted",
    color    = "Estimator"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")



# TODO: join putts_test only when ready to evaluate.

##############
### TASK 3 ###
##############

# --- Join test outcomes ---
results = predictions |>
  select(Player, N, X, mean, mle, empirical_bayes, james_stein) |>
  left_join(putts_test |> select(Player, X_test = X), by = "Player")

# --- Compute MSE for each estimator ---
mse_table = tibble(
  estimator = c("Grand Mean", "MLE", "Empirical Bayes", "James-Stein"),
  MSE = c(
    mse(results$X_test, results$mean),
    mse(results$X_test, results$mle),
    mse(results$X_test, results$empirical_bayes),
    mse(results$X_test, results$james_stein)
  )
) |> arrange(MSE) |>
  mutate(rank = row_number(),
         MSE  = round(MSE, 8))

cat("=== MSE Rankings ===\n")
print(mse_table)

# --- Plot: predictions vs test performance ---
results_long = results |>
  pivot_longer(c(mean, mle, empirical_bayes, james_stein),
               names_to  = "estimator",
               values_to = "prediction") |>
  mutate(estimator = recode(estimator,
                            mean            = "Grand Mean",
                            mle             = "MLE",
                            empirical_bayes = "Empirical Bayes",
                            james_stein     = "James-Stein"
  ),
  estimator = factor(estimator,
                     levels = mse_table$estimator  # order panels by MSE rank
  ))

# add MSE label for each facet
mse_labels = mse_table |>
  mutate(label = sprintf("MSE = %.6f\nRank #%d", MSE, rank))

ggplot(results_long, aes(x = prediction, y = X_test)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(size = N, color = estimator), alpha = 0.8) +
  geom_text(data = mse_labels,
            aes(label = label),
            x = -Inf, y = Inf,
            hjust = -0.1, vjust = 1.3,
            size = 3, inherit.aes = FALSE) +
  facet_wrap(~ estimator, nrow = 2) +
  scale_color_manual(values = c(
    "Grand Mean"      = "#2A9D8F",
    "MLE"             = "#E9C46A",
    "Empirical Bayes" = "#E63946",
    "James-Stein"     = "#457B9D"
  )) +
  scale_size_continuous(range = c(2, 6)) +
  labs(
    title    = "Predictions vs. Test Putting Rate by Estimator",
    subtitle = "Dashed line = perfect prediction; panels ordered by MSE rank",
    x        = "Predicted putting rate",
    y        = "Observed test putting rate",
    size     = "Train putts (N)",
    color    = "Estimator"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")


# TODO: compare overall MSE and golfer-level squared errors.


##############
### TASK 4 ###
##############

# --- Squared error per golfer: MLE vs best shrinkage estimator ---
# Use empirical_bayes as best shrinkage (lowest MSE from Task 3)
se_comparison = results |>
  mutate(
    se_mle = (X_test - mle)^2,
    se_eb  = (X_test - empirical_bayes)^2,
    winner = if_else(se_eb < se_mle, "Empirical Bayes", "MLE"),
    se_diff = se_mle - se_eb   # positive = EB wins, negative = MLE wins
  ) |>
  arrange(se_diff)

cat("=== Golfer-Level Winner Tally ===\n")
print(count(se_comparison, winner))

cat("\n=== Golfers where MLE beats EB ===\n")
se_comparison |>
  filter(winner == "MLE") |>
  select(Player, N, X, X_test, mle, empirical_bayes, se_mle, se_eb) |>
  print()

# --- Plot: squared error by golfer, colored by winner ---
se_long = se_comparison |>
  select(Player, N, se_mle, se_eb, winner, se_diff) |>
  pivot_longer(c(se_mle, se_eb),
               names_to  = "estimator",
               values_to = "sq_error") |>
  mutate(estimator = recode(estimator,
                            se_mle = "MLE",
                            se_eb  = "Empirical Bayes"
  ))

ggplot(se_comparison, aes(x = reorder(factor(Player), se_diff),
                          y = se_diff, fill = winner)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  scale_fill_manual(values = c("Empirical Bayes" = "#E63946",
                               "MLE"             = "#E9C46A")) +
  labs(
    title    = "Squared Error Difference: MLE minus Empirical Bayes",
    subtitle = "Positive (red) = EB wins; Negative (yellow) = MLE wins",
    x        = "Golfer (Player ID, ordered by EB advantage)",
    y        = "SE(MLE) − SE(EB)",
    fill     = "Winner"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

##############
### TASK 5 ###
##############

# --- Asymmetric loss: overrating twice as costly as underrating ---
# overrating:  prediction > truth  → prediction was too high → loss = 2 * (pred - truth)^2
# underrating: prediction <= truth → prediction was too low  → loss = 1 * (truth - pred)^2
asym_loss = function(truth, prediction) {
  error = prediction - truth
  penalty = if_else(error > 0, 2, 1)
  mean(penalty * error^2)
}

asym_table = tibble(
  estimator = c("Grand Mean", "MLE", "Empirical Bayes", "James-Stein"),
  MSE_symmetric = c(
    mse(results$X_test, results$mean),
    mse(results$X_test, results$mle),
    mse(results$X_test, results$empirical_bayes),
    mse(results$X_test, results$james_stein)
  ),
  Loss_asymmetric = c(
    asym_loss(results$X_test, results$mean),
    asym_loss(results$X_test, results$mle),
    asym_loss(results$X_test, results$empirical_bayes),
    asym_loss(results$X_test, results$james_stein)
  )
) |>
  mutate(
    rank_sym  = rank(MSE_symmetric),
    rank_asym = rank(Loss_asymmetric)
  ) |>
  arrange(Loss_asymmetric)

cat("=== Symmetric vs Asymmetric Loss Rankings ===\n")
print(asym_table)

# --- Plot: symmetric vs asymmetric loss side by side ---
asym_long = asym_table |>
  select(estimator, MSE_symmetric, Loss_asymmetric) |>
  pivot_longer(c(MSE_symmetric, Loss_asymmetric),
               names_to  = "loss_type",
               values_to = "loss") |>
  mutate(
    loss_type = recode(loss_type,
                       MSE_symmetric   = "Symmetric (MSE)",
                       Loss_asymmetric = "Asymmetric (over = 2×)"
    ),
    estimator = factor(estimator,
                       levels = c("Grand Mean", "MLE", "James-Stein", "Empirical Bayes"))
  )

ggplot(asym_long, aes(x = estimator, y = loss, fill = loss_type)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c("Symmetric (MSE)"       = "#457B9D",
                               "Asymmetric (over = 2×)" = "#E63946")) +
  labs(
    title    = "Estimator Loss: Symmetric vs Asymmetric",
    subtitle = "Asymmetric loss penalizes overrating a golfer twice as heavily",
    x        = "Estimator",
    y        = "Loss",
    fill     = "Loss function"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")


#test
