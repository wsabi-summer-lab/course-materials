#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "readr", "splines", "tidyverse"))
library(ggplot2)
library(nnet)
library(readr)
library(splines)
library(tidyverse)

# set seed
set.seed(9)

rm(list = ls())

##############################
### PART 1: EXPECTED POINTS ###
##############################

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
nfl_data = read_csv("C:/Users/sundw/Downloads/09_expected-points.csv", show_col_types = FALSE)

nfl_model_data = nfl_data |>
    mutate(
        pts_next_score_factor = factor(pts_next_score)
    )

# Task 1:
# - Refit your preferred expected-points model from Lab 5
# - A concrete default is:
#   pts_next_score ~ bs(yardline_100, df = 6) +
#                    factor(down) +
#                    log(ydstogo) +
#                    bs(half_seconds_remaining, df = 5)
# - Convert fitted class probabilities into expected points

# Load data
ep_data <- read.csv("C:/Users/sundw/Downloads/09_expected-points.csv")

# Fit multinomial logistic regression
ep_model <- multinom(
  factor(pts_next_score) ~ bs(yardline_100, df = 6) +
    factor(down) +
    log(ydstogo) +
    bs(half_seconds_remaining, df = 5),
  data = ep_data,
  maxit = 300,
  trace = FALSE
)

# Get class probabilities
prob_matrix <- predict(ep_model, type = "probs")

# Point values for each scoring outcome
point_values <- as.numeric(colnames(prob_matrix))

# Expected points = weighted sum of outcome probabilities
ep_data$ep <- as.vector(prob_matrix %*% point_values)


# Prediction grid
pred_grid <- expand.grid(
  yardline_100          = 1:99,
  down                  = 1:4,
  ydstogo               = 10,
  half_seconds_remaining = 1800
)

# Predicted EP on grid
prob_grid <- predict(ep_model, newdata = pred_grid, type = "probs")
point_values <- as.numeric(colnames(prob_grid))
pred_grid$ep <- as.vector(prob_grid %*% point_values)

# Plot
ggplot() +
  # Smoothed raw data by down
  geom_smooth(
    data = ep_data,
    aes(x = yardline_100, y = ep, color = factor(down)),
    method = "loess", span = 0.3, se = FALSE, linetype = "dashed", linewidth = 0.6
  ) +
  # Model fitted curves
  geom_line(
    data = pred_grid,
    aes(x = yardline_100, y = ep, color = factor(down)),
    linewidth = 1
  ) +
  scale_color_brewer(palette = "Set1", name = "Down") +
  scale_x_reverse(breaks = seq(0, 100, 10)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray40") +
  labs(
    title = "NFL Expected Points by Field Position and Down",
    x     = "Yards to Opponent End Zone",
    y     = "Expected Points"
  ) +
  theme_bw()

# Task 2:
# - Decide which bootstrap variation is most appropriate here:
#   * observation bootstrap
#   * cluster bootstrap
#   * parametric bootstrap
#   * residual bootstrap
# - State your choice in comments
# - Explain why it matches the dependence structure of this dataset

# Cluster bootstrap is likely the best for this dataset. Each play is dependent on the previous few, so
# clustering should handle the dependence structure well.

# The grouping variable that matters here is:
# game_id

# Task 3:
# - Implement your chosen bootstrap with at least B = 200 resamples
# - For each resample:
#   * create a bootstrap dataset
#   * refit the EP model
#   * recompute expected points at target_state

target_state <- data.frame(
  yardline_100           = 35,
  down                   = 1,
  ydstogo                = 10,
  half_seconds_remaining = 1800
)

game_ids <- unique(ep_data$game_id)
B <- 200
boot_ep <- numeric(B)

for (b in 1:B) {
  print(b)
  # Cluster resample at game level
  sampled_games <- sample(game_ids, length(game_ids), replace = TRUE)
  boot_data <- bind_rows(lapply(sampled_games, function(g) ep_data[ep_data$game_id == g, ]))
  
  # Refit model
  boot_fit <- multinom(
    factor(pts_next_score) ~ bs(yardline_100, df = 6) +
      factor(down) +
      log(ydstogo) +
      bs(half_seconds_remaining, df = 5),
    data = boot_data, maxit = 300, trace = FALSE
  )
  
  # Recompute EP at target state
  probs_b <- predict(boot_fit, newdata = target_state, type = "probs")
  boot_ep[b] <- sum(probs_b * point_values)
}

# Percentile 95% CI
ci <- quantile(boot_ep, c(0.025, 0.975))
probs_orig <- predict(ep_model, newdata = target_state, type = "probs")
cat("EP at target state:", round(sum(probs_orig * as.numeric(colnames(probs_orig))), 3), "\n")
cat("95% CI: [", round(ci[1], 3), ",", round(ci[2], 3), "]\n")

# Task 4:
# - Store the bootstrap estimates in a vector
# - Make a plot of the bootstrap distribution

# Plot bootstrap distribution
ggplot(data.frame(ep = boot_ep), aes(x = ep)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = sum(probs_orig * as.numeric(colnames(probs_orig))),
             color = "black", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = ci, color = "red", linewidth = 0.8, linetype = "dotted") +
  labs(
    title = "Bootstrap Distribution of Expected Points",
    subtitle = "1st & 10 at opponent's 35 | dashed = point estimate, dotted = 95% CI",
    x = "Expected Points",
    y = "Count"
  ) +
  theme_bw()

# Task 5:
# - Compute:
#   * the original fitted expected-points estimate
#   * the bootstrap standard error
#   * the 95% percentile interval

ep_original <- sum(probs_orig * as.numeric(colnames(probs_orig)))
ep_se       <- sd(boot_ep)
ep_ci       <- quantile(boot_ep, c(0.025, 0.975))

cat("Original EP estimate:", round(ep_original, 3), "\n")
cat("Bootstrap SE:        ", round(ep_se, 3), "\n")
cat("95% CI:              [", round(ep_ci[1], 3), ",", round(ep_ci[2], 3), "]\n")

# Task 6:
# - In comments, explain why a naive row-by-row observation bootstrap
#   is less appropriate for this dataset

# A row-by-row bootstrap ignores the dependence between subsequent plays, so it wouldn't be
# appropriate here.

# JP: If you're looking at this, there might be errors in here. I ran it once with 200 iterations,
# but found a mistake, so I went back and changed it. But I didn't have time to run it another 200
# times, so this new version might have errors that I didn't notice because I didn't have time to
# run it.

################################
### PART 2: NBA FREE THROWS ####
################################

library(dplyr)

nba_players = read_delim(
  "C:/Users/sundw/Downloads/08_nba-free-throws.csv",
  delim = ";",
  show_col_types = FALSE
)

ft_clean <- nba_players %>%
  mutate(
    G   = as.numeric(G),
    FT  = as.numeric(FT),
    FTA = as.numeric(FTA)
  ) %>%
  group_by(Player) %>%
  filter(if (any(Tm == "TOT")) Tm == "TOT" else TRUE) %>%
  ungroup() %>%
  mutate(
    FT_total  = round(FT  * G),
    FTA_total = round(FTA * G),
    FT_pct    = ifelse(FTA_total > 0, FT_total / FTA_total, NA)
  ) %>%
  select(Player, FT_total, FTA_total, FT_pct)

ft_qualified <- ft_clean %>%
  filter(FT_total >= 25)

# Task 2:
# - For each player, construct a 95% bootstrap confidence interval
#   for free-throw percentage
# - Overlay the bootstrap intervals on your Lab 8 player plot
# - Compare bootstrap, Wald, and Agresti-Coull intervals

ft_qualified <- ft_qualified %>%
  mutate(
    # Wald CI
    wald_se    = sqrt(FT_pct * (1 - FT_pct) / FTA_total),
    wald_lower = FT_pct - 1.96 * wald_se,
    wald_upper = FT_pct + 1.96 * wald_se,
    
    # Agresti-Coull CI
    n_tilde    = FTA_total + 4,
    p_tilde    = (FT_total + 2) / n_tilde,
    ac_se      = sqrt(p_tilde * (1 - p_tilde) / n_tilde),
    ac_lower   = p_tilde - 1.96 * ac_se,
    ac_upper   = p_tilde + 1.96 * ac_se
  ) %>%
  select(-wald_se, -n_tilde, -ac_se)

# Bootstrap CI for each player
B <- 500

boot_cis <- ft_qualified %>%
  rowwise() %>%
  mutate(
    boot_lower = quantile(
      replicate(B, mean(sample(c(rep(1, FT_total), rep(0, FTA_total - FT_total)),
                               FTA_total, replace = TRUE))),
      0.025),
    boot_upper = quantile(
      replicate(B, mean(sample(c(rep(1, FT_total), rep(0, FTA_total - FT_total)),
                               FTA_total, replace = TRUE))),
      0.975)
  ) %>%
  ungroup()

# Plot all three methods
boot_cis %>%
  pivot_longer(
    cols = c(wald_lower, wald_upper, ac_lower, ac_upper, boot_lower, boot_upper),
    names_to = c("method", ".value"),
    names_pattern = "(wald|ac|boot)_(lower|upper)"
  ) %>%
  mutate(method = recode(method, wald = "Wald", ac = "Agresti-Coull", boot = "Bootstrap")) %>%
  ggplot(aes(y = reorder(Player, FT_pct), color = method)) +
  geom_point(aes(x = FT_pct), size = 1.5, color = "black") +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.6)) +
  labs(x = "FT%", y = NULL, color = "Method",
       title = "95% Confidence Intervals for NBA Free Throw %") +
  theme_minimal()

# Task 3:
# - Revisit the Lab 8 simulation study
# - Add bootstrap confidence intervals to the same coverage framework
# - Plot bootstrap coverage probability against p
# - Compare to Wald and Agresti-Coull

n <- 100
n_sims = 100

sim_multi <- map_dfr(1:n_sims, ~tibble(
  sim     = .x,
  attempt = 1:n,
  make    = rbinom(n, 1, 0.75),
  p_hat   = cumsum(make) / attempt
))

# Bootstrap percentile CI at each n
boot_cis <- sim_multi %>%
  group_by(attempt) %>%
  summarise(
    boot_lower = quantile(
      replicate(B, mean(sample(p_hat, length(p_hat), replace = TRUE))),
      0.025),
    boot_upper = quantile(
      replicate(B, mean(sample(p_hat, length(p_hat), replace = TRUE))),
      0.975)
  )

ggplot() +
  geom_line(data = sim_multi, aes(x = attempt, y = p_hat, group = sim), alpha = 0.15) +
  geom_ribbon(data = boot_cis, aes(x = attempt, ymin = boot_lower, ymax = boot_upper),
              fill = "steelblue", alpha = 0.3) +
  geom_hline(yintercept = 0.75, color = "red", linetype = "dashed") +
  labs(x = "Attempt (n)", y = expression(hat(p)),
       title = "100 Simulated Running Free Throw Make % with Bootstrap CI (p = 0.75)") +
  theme_minimal()

# TODO: extend your Lab 8 simulation loop.
# For each simulated Binomial(n, p) count, compute:
# - Wald interval
# - Agresti-Coull interval
# - bootstrap percentile interval
# Then summarize coverage by method, n, and p.

# Here, the bootstrap method seems to have more variance than the other two methods.
