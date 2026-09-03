library(ggplot2)
library(tidyverse)
library(splines)
set.seed(3)

nba_four_factors <- read_csv("02_nba-four-factors.csv.gz")
punts            <- read_csv("02_punts.csv.gz")

# Fix: rename the column, then flip its sign with mutate()
nba <- nba_four_factors %>%
  rename(
    wins          = W,
    x1_shooting   = `Shooting Factor`,
    x2_crashing   = `Crashing Factor`,
    x3_protecting = `Protecting Factor`,
    x4_attacking  = `Attacking Factor`
  ) %>%
  mutate(x3_protecting = -x3_protecting)

# x3 = opp TOV% - TOV%; the CSV stores TOV% - opp TOV%, so we negate so that
# larger x3 = fewer own turnovers = better (consistent with the other factors).

# TASK 1: Get to know the data

# Summary statistics: mean, sd, min, max
summary_stats <- nba %>%
  select(wins, x1_shooting, x2_crashing, x3_protecting, x4_attacking) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value,   na.rm = TRUE),
    min  = min(value,  na.rm = TRUE),
    max  = max(value,  na.rm = TRUE),
    .groups = "drop"
  )
cat("=== Task 1: Summary Statistics ===\n")
print(summary_stats)

# Marginal distributions of each explanatory variable
nba %>%
  select(x1_shooting, x2_crashing, x3_protecting, x4_attacking) %>%
  pivot_longer(everything(), names_to = "factor", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  facet_wrap(~factor, scales = "free") +
  labs(title = "Marginal Distributions of the Four Factors",
       x = "Value", y = "Count") +
  theme_bw()
ggsave("nba_marginal_distributions.png", width = 10, height = 8)

# Scatterplots: wins vs. each factor
nba %>%
  select(wins, x1_shooting, x2_crashing, x3_protecting, x4_attacking) %>%
  pivot_longer(-wins, names_to = "factor", values_to = "value") %>%
  ggplot(aes(x = value, y = wins)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~factor, scales = "free_x") +
  labs(title = "Wins vs. Each Four Factor",
       x = "Factor Value", y = "Wins") +
  theme_bw()
ggsave("nba_wins_scatterplots.png", width = 10, height = 8)

# Pairwise correlations among explanatory variables
cor_predictors <- nba %>%
  select(x1_shooting, x2_crashing, x3_protecting, x4_attacking) %>%
  cor()
cat("\nCorrelation matrix among the four factors:\n")
print(round(cor_predictors, 3))

# Correlations with wins (to identify strongest predictors before modeling)
cor_with_wins <- nba %>%
  select(wins, x1_shooting, x2_crashing, x3_protecting, x4_attacking) %>%
  cor() %>%
  .["wins", ]
cat("\nCorrelations with wins:\n")
print(round(cor_with_wins, 3))
# x1_shooting typically has the highest correlation → most strongly related to wins

# TASK 2: Model the data

model_orig <- lm(wins ~ x1_shooting + x2_crashing + x3_protecting + x4_attacking,
                 data = nba)
cat("\n=== Task 2: Original Model Summary ===\n")
print(summary(model_orig))

cat("\nFitted coefficients:\n")
print(coef(model_orig))
# All four x's are defined so larger = better, so all β estimates should be positive.

# TASK 3: Standardize the predictors

nba <- nba %>%
  mutate(
    z1 = (x1_shooting   - mean(x1_shooting))   / sd(x1_shooting),
    z2 = (x2_crashing   - mean(x2_crashing))   / sd(x2_crashing),
    z3 = (x3_protecting - mean(x3_protecting)) / sd(x3_protecting),
    z4 = (x4_attacking  - mean(x4_attacking))  / sd(x4_attacking)
  )

model_std <- lm(wins ~ z1 + z2 + z3 + z4, data = nba)
cat("\n=== Task 3: Standardized Model Summary ===\n")
print(summary(model_std))

# Rank factors by absolute standardized coefficient (drop intercept)
std_coefs <- coef(model_std)[-1]
cat("\nFactors ranked by |standardized coefficient|:\n")
print(sort(abs(std_coefs), decreasing = TRUE))


# Compare fitted values: original vs. standardized model
max_diff <- max(abs(fitted(model_orig) - fitted(model_std)))
cat("\nMax absolute difference in fitted values (orig vs std):", round(max_diff, 10), "\n")
# TASK 4: Quantify uncertainty

n <- nrow(nba)
p <- length(coef(model_orig))   # 5 (intercept + 4 factors)
rss <- sum(residuals(model_orig)^2)
sigma_hat <- sqrt(rss / (n - p))
cat("\n=== Task 4: Uncertainty ===\n")
cat("Residual standard error (sigma-hat):", round(sigma_hat, 3), "wins\n")
# Interpretation: on average, the model's win predictions are off by ~sigma wins.

cat("\n95% Confidence Intervals for Coefficients:\n")
print(confint(model_orig))
# Factors whose CI excludes 0 have effects clearly distinguishable from zero.
# x1_shooting and x3_protecting typically have the clearest non-zero effects.
# x2_crashing and x4_attacking may span zero → more uncertain.

# Choose one team: use first row
chosen_team <- nba[1, ]
cat("\nChosen team:", chosen_team$TEAM, "\n")
cat("Actual wins:", chosen_team$wins, "\n")

new_obs <- chosen_team %>%
  select(x1_shooting, x2_crashing, x3_protecting, x4_attacking)

# (a) Point prediction
pt_pred <- predict(model_orig, newdata = new_obs)
cat("(a) Point prediction:", round(pt_pred, 2), "wins\n")

# (b) 95% CI for expected wins (uncertainty in the mean response)
ci_result <- predict(model_orig, newdata = new_obs,
                     interval = "confidence", level = 0.95)
cat("(b) 95% CI for expected wins:\n")
print(round(ci_result, 2))

# (c) 95% Prediction Interval for actual wins (accounts for individual variability)
pi_result <- predict(model_orig, newdata = new_obs,
                     interval = "prediction", level = 0.95)
cat("(c) 95% Prediction Interval for actual wins:\n")
print(round(pi_result, 2))
# The PI is always wider than the CI because it accounts for both uncertainty
# in the fitted mean AND the irreducible variability of a single season (sigma^2).


# TASK 5: Out-of-sample prediction

set.seed(3)
n_train   <- floor(0.7 * nrow(nba))
train_idx <- sample(seq_len(nrow(nba)), n_train)
train_nba <- nba[train_idx, ]
test_nba  <- nba[-train_idx, ]

model_orig_train <- lm(wins ~ x1_shooting + x2_crashing + x3_protecting + x4_attacking,
                       data = train_nba)
model_std_train  <- lm(wins ~ z1 + z2 + z3 + z4,
                       data = train_nba)

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

rmse_orig <- rmse(test_nba$wins, predict(model_orig_train, newdata = test_nba))
rmse_std  <- rmse(test_nba$wins, predict(model_std_train,  newdata = test_nba))

cat("\n=== Task 5: Out-of-Sample RMSE ===\n")
cat("Test RMSE — Original model:     ", round(rmse_orig, 3), "\n")
cat("Test RMSE — Standardized model: ", round(rmse_std,  3), "\n")
# Both models span the same column space, so test RMSE is identical.
# Standardizing predictors changes how coefficients are expressed, not what the
# model fits. Neither model "predicts better" — they are mathematically equivalent.

# SECTION 2.2: EXPECTED OUTCOME OF A PUNT

punts <- punts %>%
  rename(y = next_ydl)   # y = opponent yard line after punt

# TASK 1: Explore the data


# y vs. ydl scatter with loess smoother
ggplot(punts, aes(x = ydl, y = y)) +
  geom_point(alpha = 0.15, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +
  labs(title = "Post-Punt Yard Line vs. Starting Yard Line",
       x = "Starting Yard Line (yards from opponent's goal)",
       y = "Post-Punt Yard Line (opponent's perspective)") +
  theme_bw()
ggsave("punt_scatter.png", width = 8, height = 6)
# Shape: roughly linear but flattens/bends as the punter approaches the opponent's
# end zone — the ball can't go through the end zone (touchback at ~20 yards).

# Binned averages by starting field position
punts <- punts %>%
  mutate(ydl_bin = cut(ydl, breaks = seq(0, 100, by = 10), include.lowest = TRUE))

bin_avg <- punts %>%
  group_by(ydl_bin) %>%
  summarise(avg_y = mean(y), n = n(), .groups = "drop")

ggplot(bin_avg, aes(x = ydl_bin, y = avg_y)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.3, size = 3) +
  labs(title = "Average Post-Punt Yard Line by Starting Field Position",
       x = "Starting Yard Line (bin)", y = "Avg Post-Punt Yard Line") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("punt_binned_avg.png", width = 8, height = 6)

# Distribution of punter quality
ggplot(punts, aes(x = pq)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Punter Quality (pq)",
       x = "Punter Quality", y = "Count") +
  theme_bw()
ggsave("punt_pq_dist.png", width = 6, height = 4)

cat("\n=== Punt Task 1: Punter Quality Summary ===\n")
print(summary(punts$pq))

# TASK 2: Fit competing models


set.seed(3)
n_train_p   <- floor(0.8 * nrow(punts))
train_idx_p <- sample(seq_len(nrow(punts)), n_train_p)
train_p     <- punts[train_idx_p, ]
test_p      <- punts[-train_idx_p, ]

# M1: Linear in ydl only
m1 <- lm(y ~ ydl, data = train_p)

# M2: Quadratic in ydl only
m2 <- lm(y ~ ydl + I(ydl^2), data = train_p)

# M3: Quadratic in ydl + punter quality
m3 <- lm(y ~ ydl + I(ydl^2) + pq, data = train_p)

# M4: Natural cubic spline in ydl + punter quality
m4 <- lm(y ~ ns(ydl, df = 6) + pq, data = train_p)

# Test-set RMSE for model selection
rmse_p <- function(model, newdata, actual) {
  sqrt(mean((actual - predict(model, newdata = newdata))^2))
}

cat("\n=== Punt Task 2: Model Test-Set RMSE ===\n")
cat("M1 Linear:             ", round(rmse_p(m1, test_p, test_p$y), 4), "\n")
cat("M2 Quadratic:          ", round(rmse_p(m2, test_p, test_p$y), 4), "\n")
cat("M3 Quadratic + pq:     ", round(rmse_p(m3, test_p, test_p$y), 4), "\n")
cat("M4 Spline + pq:        ", round(rmse_p(m4, test_p, test_p$y), 4), "\n")
# Adding pq improves prediction because punter skill is a real predictor of outcomes.

# Visualize fitted curves (evaluated at mean pq)
mean_pq  <- mean(punts$pq)
ydl_grid <- data.frame(ydl = seq(min(punts$ydl), max(punts$ydl), length.out = 300),
                        pq  = mean_pq)

ydl_grid <- ydl_grid %>%
  mutate(
    m1_fit = predict(m1, newdata = ydl_grid),
    m2_fit = predict(m2, newdata = ydl_grid),
    m3_fit = predict(m3, newdata = ydl_grid),
    m4_fit = predict(m4, newdata = ydl_grid)
  ) %>%
  pivot_longer(cols = ends_with("_fit"), names_to = "model", values_to = "fit") %>%
  mutate(model = recode(model,
    m1_fit = "M1: Linear",
    m2_fit = "M2: Quadratic",
    m3_fit = "M3: Quad + pq",
    m4_fit = "M4: Spline + pq"
  ))

ggplot() +
  geom_point(data = punts, aes(x = ydl, y = y), alpha = 0.08, color = "gray50") +
  geom_line(data = ydl_grid, aes(x = ydl, y = fit, color = model), linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Fitted Punt Models (evaluated at mean punter quality)",
       x = "Starting Yard Line", y = "Post-Punt Yard Line", color = "Model") +
  theme_bw()
ggsave("punt_fitted_curves.png", width = 10, height = 6)

# Best model: refit M4 on full data
best_punt_model <- lm(y ~ ns(ydl, df = 6) + pq, data = punts)
cat("\n--- Best Punt Model: Spline + pq (full data) ---\n")
print(summary(best_punt_model))
pq_coef <- coef(best_punt_model)["pq"]
cat(sprintf(
  "\nInterpretation of pq coefficient: a 1-unit increase in punter quality\nchanges expected post-punt opponent yard line by %.3f yards, holding ydl fixed.\n",
  pq_coef
))
# Negative pq coefficient → better punters pin the opponent deeper (smaller yard line = better).

# TASK 3: Visualize uncertainty in the punt model

pred_grid <- data.frame(ydl = seq(min(punts$ydl), max(punts$ydl), length.out = 300),
                        pq  = mean_pq)

ci_df <- predict(best_punt_model, newdata = pred_grid,
                 interval = "confidence", level = 0.95) %>%
  as.data.frame() %>%
  bind_cols(pred_grid)

pi_df <- predict(best_punt_model, newdata = pred_grid,
                 interval = "prediction", level = 0.95) %>%
  as.data.frame() %>%
  bind_cols(pred_grid)

ggplot() +
  geom_point(data = punts, aes(x = ydl, y = y),
             alpha = 0.08, color = "gray50", size = 0.8) +
  geom_ribbon(data = pi_df, aes(x = ydl, ymin = lwr, ymax = upr),
              fill = "lightblue", alpha = 0.5) +
  geom_ribbon(data = ci_df, aes(x = ydl, ymin = lwr, ymax = upr),
              fill = "steelblue", alpha = 0.7) +
  geom_line(data = ci_df, aes(x = ydl, y = fit),
            color = "navy", linewidth = 1) +
  labs(
    title    = "Expected Post-Punt Yard Line with Uncertainty Bands",
    subtitle = "Dark band = 95% CI for mean response | Light band = 95% PI for one punt",
    x = "Starting Yard Line", y = "Post-Punt Yard Line"
  ) +
  theme_bw()
ggsave("punt_uncertainty_bands.png", width = 10, height = 6)
# CI is narrow — uncertainty in the fitted mean shrinks with more data.
# PI is much wider — it must also capture sigma, the punt-to-punt variability.
# Model is most uncertain near very short or very long yard lines (sparse data).


# TASK 4: Punt Yards Over Expected (PYOE)



# Positive PYOE = punt held opponent to a shorter field position than expected
# (i.e., better-than-expected punt for the kicking team).

punts <- punts %>%
  mutate(
    y_hat = predict(best_punt_model, newdata = punts),
    PYOE  = y_hat - y
  )

punter_summary <- punts %>%
  group_by(punter) %>%
  summarise(
    avg_PYOE = mean(PYOE),
    n_punts  = n(),
    se_PYOE  = sd(PYOE) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  mutate(
    lower_95 = avg_PYOE - 1.96 * se_PYOE,
    upper_95 = avg_PYOE + 1.96 * se_PYOE
  ) %>%
  arrange(desc(avg_PYOE))

cat("\n=== Punt Task 4: Punter Rankings by Average PYOE ===\n")
print(punter_summary)
# Punters with lower_95 > 0 are clearly above average (CI excludes zero).
# Punters with few punts have wide CIs → rankings are unreliable.

# Visualize punter rankings with uncertainty intervals
punter_summary %>%
  mutate(punter = fct_reorder(punter, avg_PYOE)) %>%
  ggplot(aes(x = avg_PYOE, y = punter)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = lower_95, xmax = upper_95),
                 height = 0.3, color = "steelblue", linewidth = 0.8) +
  geom_point(aes(size = n_punts), color = "steelblue") +
  scale_size_continuous(name = "# Punts", range = c(2, 6)) +
  labs(
    title    = "Punter Rankings by Punt Yards Over Expected (PYOE)",
    subtitle = "Error bars = 95% CI  |  Positive PYOE = better than expected",
    x = "Average PYOE (yards)", y = "Punter"
  ) +
  theme_bw()
ggsave("punt_rankings.png", width = 10, height = 8)

# FINAL REFLECTION (printed to console for reference)


cat("
=== Final Reflection ===

1. Adding columns to the design matrix expands the column space, allowing the model
   to fit more complex patterns — e.g., going from a line to a curve to a spline.

2. Extra flexibility helped when the true relationship was nonlinear (punt outcome
   vs. yard line), where the quadratic and spline models captured the touchback
   constraint that the linear model missed.

3. Extra flexibility can hurt when it overfits noise — especially with small samples
   or in regions with sparse data — leading to worse out-of-sample prediction.

4. sigma-hat tells you the typical magnitude of model errors in the outcome's units
   (wins for the NBA model, opponent yards for the punt model). It is the 'noise
   floor' the model cannot explain.

5. A prediction interval is wider than a confidence interval because the CI only
   captures uncertainty about the mean response E[y|x], while the PI also includes
   the irreducible individual variability sigma^2 around that mean.

6. One ranking to be cautious about: punters with few punts in the dataset. Their
   average PYOE has high variance (large SE), so their ranking could shift
   substantially with more data. The CI reflects this instability.
")
