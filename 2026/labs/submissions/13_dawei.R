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

putts_train = read_csv("C:/Users/sundw/Downloads/13_putts-train.csv")
putts_test = read_csv("C:/Users/sundw/Downloads/13_putts-test.csv")

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

methods = c("mean", "mle", "empirical_bayes", "james_stein")

# Summary stats: range and SD of predictions for each method
leaderboard_summary = predictions |>
  summarize(across(all_of(methods),
                   list(min = min, max = max, range = ~ max(.x) - min(.x), sd = sd))) |>
  pivot_longer(everything(), names_to = c("method", "stat"), names_pattern = "(.*)_(min|max|range|sd)") |>
  pivot_wider(names_from = stat, values_from = value)
print(leaderboard_summary)

# Top 5 and bottom 5 golfers (by Player ID) under each method
top5 = predictions |>
  pivot_longer(all_of(methods), names_to = "method", values_to = "pred") |>
  group_by(method) |>
  slice_max(pred, n = 5) |>
  arrange(method, desc(pred)) |>
  select(method, Player, pred)
print(top5)

bottom5 = predictions |>
  pivot_longer(all_of(methods), names_to = "method", values_to = "pred") |>
  group_by(method) |>
  slice_min(pred, n = 5) |>
  arrange(method, pred) |>
  select(method, Player, pred)
print(bottom5)

# leaderboard_summary
# 
# method            min   max  range     sd
# <chr>           <dbl> <dbl>  <dbl>  <dbl>
# 1 mean            0.404 0.404 0      0     
# 2 mle             0.321 0.477 0.156  0.0274
# 3 empirical_bayes 0.367 0.434 0.0662 0.0115
# 4 james_stein     0.370 0.434 0.0646 0.0114

# How far each prediction moves from the MLE
shrinkage_distance = predictions |>
  mutate(
    dist_mean = abs(mean - mle),
    dist_eb = abs(empirical_bayes - mle),
    dist_js = abs(james_stein - mle)
  ) |>
  summarize(across(starts_with("dist_"), list(mean = mean, max = max)))
print(shrinkage_distance)

# Visualize: predictions by method, ordered by MLE rank, to see dispersion
predictions |>
  mutate(Player = factor(Player, levels = Player[order(mle)])) |>
  pivot_longer(all_of(methods), names_to = "method", values_to = "pred") |>
  mutate(method = factor(method, levels = methods)) |>
  ggplot(aes(x = Player, y = pred, color = method)) +
  geom_point(alpha = 0.6) +
  labs(x = "Golfer (ordered by MLE)", y = "Predicted putting rate",
       title = "Leaderboard comparison across estimators") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Top 5 Golfers Empirical Bayes:
# 1. 27895
# 2. 48084
# 3. 27644
# 4. 33399
# 5. 20593

# Top 5 Golfers James Stein:
# 1. 27895
# 2. 48084
# 3. 33399
# 4. 20593
# 5. 27644

# Top 5 Golfers Mean:
# like a million people are tied lol

# Top 5 Golfers MLE:
# 1. 27895
# 2. 48084
# 3. 33399
# 4. 20593
# 5. 27644

# Bottom 5 Golfers Empirical Bayes:
# 1. 28500
# 2. 23353
# 3. 20572
# 4. 33418
# 5. 30692

# Bottom 5 Golfers James Stein:
# 1. 28500
# 2. 46507
# 3. 23353
# 4. 33418
# 5. 20572

# Bottom 5 Golfers Mean:
# like a million people are tied again lol

# Bottom 5 Golfers MLE:
# 1. 28500
# 2. 46507
# 3. 23353
# 4. 33418
# 5. 20572

# TODO: explain the common-variance approximation and a standardization alternative.

# In James Stein, we use the same variance for each golfer despite their different sample sizes,
# but golfers with more samples should actually have lower variance.

# TODO: join putts_test only when ready to evaluate.

results = predictions |>
  select(Player, all_of(methods)) |>
  inner_join(putts_test, by = "Player") |>
  rename(truth = X)

mse_by_method = results |>
  summarize(across(all_of(methods), ~ mse(truth, .x))) |>
  pivot_longer(everything(), names_to = "method", values_to = "MSE") |>
  arrange(MSE)
print(mse_by_method)

# Plot: predictions vs. test performance
results |>
  pivot_longer(all_of(methods), names_to = "method", values_to = "pred") |>
  mutate(method = factor(method, levels = methods)) |>
  ggplot(aes(x = pred, y = truth)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ method) +
  labs(x = "Predicted putting rate", y = "Test putting rate",
       title = "Predictions vs. held-out test performance") +
  theme_minimal()

# TODO: compare overall MSE and golfer-level squared errors.

# Identify the best (lowest-MSE) shrinkage estimator from Task 3
best_method = mse_by_method |>
    filter(method != "mle") |>
    slice_min(MSE, n = 1) |>
    pull(method)

print(paste("Best shrinkage estimator:", best_method))
print(mse_by_method)

# Golfer-level squared errors: MLE vs. best shrinkage estimator
golfer_errors = results |>
    mutate(
        se_mle = (mle - truth)^2,
        se_best = (.data[[best_method]] - truth)^2,
        winner = if_else(se_best < se_mle, best_method, "mle")
    ) |>
    select(Player, truth, mle, !!best_method, se_mle, se_best, winner)

print(golfer_errors)

# How often does each estimator win at the individual level?
win_counts = golfer_errors |>
    count(winner)
print(win_counts)

# Plot: which estimator wins for each golfer
golfer_errors |>
    mutate(Player = factor(Player, levels = Player[order(mle)])) |>
    ggplot(aes(x = Player, y = se_mle - se_best, fill = winner)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Golfer (ordered by MLE)",
         y = paste0("SE(MLE) - SE(", best_method, ")"),
         title = "Per-golfer comparison: positive bars favor shrinkage",
         fill = "Winner") +
    theme_minimal() +
    theme(axis.text.x = element_blank())

# TODO: compare overall MSE and golfer-level squared errors.

# Identify the best (lowest-MSE) shrinkage estimator from Task 3
best_method = mse_by_method |>
  filter(method != "mle") |>
  slice_min(MSE, n = 1) |>
  pull(method)

print(paste("Best shrinkage estimator:", best_method))
print(mse_by_method)

# Golfer-level squared errors: MLE vs. best shrinkage estimator
golfer_errors = results |>
  mutate(
    se_mle = (mle - truth)^2,
    se_best = (.data[[best_method]] - truth)^2,
    winner = if_else(se_best < se_mle, best_method, "mle")
  ) |>
  select(Player, truth, mle, !!best_method, se_mle, se_best, winner)

print(golfer_errors)

# How often does each estimator win at the individual level?
win_counts = golfer_errors |>
  count(winner)
print(win_counts)

# Plot: which estimator wins for each golfer
golfer_errors |>
  mutate(Player = factor(Player, levels = Player[order(mle)])) |>
  ggplot(aes(x = Player, y = se_mle - se_best, fill = winner)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Golfer (ordered by MLE)",
       y = paste0("SE(MLE) - SE(", best_method, ")"),
       title = "Per-golfer comparison: positive bars favor shrinkage",
       fill = "Winner") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
