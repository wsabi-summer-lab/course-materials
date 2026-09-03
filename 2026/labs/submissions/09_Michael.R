#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "readr", "splines", "tidyverse"))
library(ggplot2)
library(nnet)
library(readr)
library(splines)
library(tidyverse)
setwd("~/GitHub/lab-materials/2026/labs/data")

# set seed
set.seed(9)
setwd("~/GitHub/lab-materials/2026/labs/data")
##############################
### PART 1: EXPECTED POINTS ###
##############################

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
nfl_data = read_csv("../data/09_expected-points.csv.gz", show_col_types = FALSE)

nfl_model_data = nfl_data |>
  mutate(
    pts_next_score_factor = factor(pts_next_score)
  )

# Task 1:
score_values = sort(unique(nfl_data$pts_next_score))
game_ids = unique(nfl_data$game_id)

ep_formula = pts_next_score_factor ~
  bs(yardline_100, df = 6) +
  factor(down) +
  log(ydstogo) +
  bs(half_seconds_remaining, df = 5)

target_state = tibble(
  yardline_100 = 35,
  down = 1,
  ydstogo = 10,
  half_seconds_remaining = 1800
)

ep_model = multinom(ep_formula, data = nfl_model_data, trace = FALSE)

expected_points_from_probs = function(prob_matrix, score_values) {
  prob_matrix = as.matrix(prob_matrix)
  col_names = as.numeric(colnames(prob_matrix))
  prob_matrix = prob_matrix[, as.character(score_values), drop = FALSE]
  rowSums(prob_matrix * matrix(score_values, nrow = nrow(prob_matrix), 
                               ncol = length(score_values), byrow = TRUE))
}

target_probs = predict(ep_model, newdata = target_state, type = "probs")
ep_estimate = expected_points_from_probs(
  matrix(target_probs, nrow = 1, dimnames = list(NULL, names(target_probs))),
  score_values
)
cat("Expected Points at target state:", ep_estimate, "\n")

# Visualize EP curve
pred_grid = expand.grid(
  yardline_100 = 1:99,
  down = 1:4,
  ydstogo = 10,
  half_seconds_remaining = 1800
)
pred_probs = predict(ep_model, newdata = pred_grid, type = "probs")
pred_grid$EP = expected_points_from_probs(pred_probs, score_values)

ggplot(pred_grid, aes(x = yardline_100, y = EP, color = factor(down))) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Expected Points vs Yard Line by Down",
    x = "Yard Line (distance to opponent end zone)",
    y = "Expected Points",
    color = "Down"
  ) +
  theme_minimal()

print(ep_estimate)
print(
  ggplot(pred_grid, aes(x = yardline_100, y = EP, color = factor(down))) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "Expected Points vs Yard Line by Down",
      x = "Yard Line (distance to opponent end zone)",
      y = "Expected Points",
      color = "Down"
    ) +
    theme_minimal()
)

# Task 2:
# - Decide which bootstrap variation is most appropriate here:
#   * observation bootstrap
#   * cluster bootstrap
#   * parametric bootstrap
#   * residual bootstrap


# A cluster bootstrap is certainly the way to go here. The Expected points vs yard line by down shows that the situation of plays before causes the play before to impact the next (they are not independent) in the different plays so we shoudl cluster by drives to account for this. 

# Task 3:
B = 200
bootstrap_ep = rep(NA_real_, B)

for (b in seq_len(B)) {
  cat("Bootstrap iteration:", b, "of", B, "\n")
  sampled_game_ids = sample(game_ids, size = length(game_ids), replace = TRUE)
  boot_data = nfl_model_data |> filter(game_id %in% sampled_game_ids)
  boot_model = multinom(ep_formula, data = boot_data, trace = FALSE)
  boot_probs = predict(boot_model, newdata = target_state, type = "probs")
  bootstrap_ep[b] = sum(boot_probs * score_values)
}
# Task 4:

bootstrap_ep_df = tibble(ep = bootstrap_ep)

ggplot(bootstrap_ep_df, aes(x = ep)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = ep_estimate, color = "red", linewidth = 1, linetype = "dashed") +
  labs(
    title = "Bootstrap Distribution of Expected Points",
    subtitle = "Target: 1st-and-10 at opponent 35, 1800 seconds remaining",
    x = "Expected Points",
    y = "Count"
  ) +
  theme_minimal()

# Task 5
boot_se = sd(bootstrap_ep, na.rm = TRUE)
boot_ci = quantile(bootstrap_ep, probs = c(0.025, 0.975), na.rm = TRUE)

cat("Original EP estimate:", ep_estimate, "\n")
cat("Bootstrap SE:", boot_se, "\n")
cat("95% Percentile Interval: [", boot_ci[1], ",", boot_ci[2], "]\n")


# Task 6:
#> cat("Original EP estimate:", ep_estimate, "\n")
#Original EP estimate: 3.835655 
#> cat("Bootstrap SE:", boot_se, "\n")
#Bootstrap SE: 0.1002185 
#> cat("95% Percentile Interval: [", boot_ci[1], ",", boot_ci[2], "]\n")
#95% Percentile Interval: [ 3.637398 , 4.022932 ]

#The SE is 0.1 which narrow since we have over 700 games and its centered around an appropriate mean.

library(readr)
################################
### PART 2: NBA FREE THROWS ####
################################

# From Lab 8 - player level dataset
nba_players = read_delim(
  "../data/09_nba-free-throws.csv.gz",
  delim = ";",
  show_col_types = FALSE
)

ft = nba_players %>%
  group_by(Player) %>%
  summarize(
    FT_total  = sum(FT  * G),
    FTA_total = sum(FTA * G),
    .groups = "drop"
  ) %>%
  mutate(FT_percent = FT_total / FTA_total) %>%
  filter(FTA_total >= 25)

z = qnorm(0.975)

ft = ft %>%
  mutate(
    wald_lo = FT_percent - z * sqrt(FT_percent * (1 - FT_percent) / FTA_total),
    wald_hi = FT_percent + z * sqrt(FT_percent * (1 - FT_percent) / FTA_total),
    n_tilde = FTA_total + z^2,
    p_tilde = (FT_total + z^2 / 2) / n_tilde,
    ac_lo   = p_tilde - z * sqrt(p_tilde * (1 - p_tilde) / n_tilde),
    ac_hi   = p_tilde + z * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
  )

# Task 1 & 2: bootstrap CI per player, overlaid on plot
bootstrap_ft_percent = function(ft_made, ft_attempted, B = 1000) {
  shots = c(rep(1, ft_made), rep(0, ft_attempted - ft_made))
  replicate(B, mean(sample(shots, size = ft_attempted, replace = TRUE)))
}

percentile_interval = function(bootstrap_draws, level = 0.95) {
  alpha = 1 - level
  quantile(bootstrap_draws, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
}

ft_boot = ft %>%
  rowwise() %>%
  mutate(
    boot_draws = list(bootstrap_ft_percent(FT_total, FTA_total)),
    boot_lo = percentile_interval(boot_draws)[1],
    boot_hi = percentile_interval(boot_draws)[2]
  ) %>%
  ungroup()

ggplot(ft_boot %>% slice(1:30), aes(y = reorder(Player, FT_percent))) +
  geom_point(aes(x = FT_percent)) +
  geom_errorbar(aes(xmin = wald_lo, xmax = wald_hi, color = "Wald"),
                width = 0.3, orientation = "y") +
  geom_errorbar(aes(xmin = ac_lo, xmax = ac_hi, color = "Agresti-Coull"),
                width = 0.3, orientation = "y") +
  geom_errorbar(aes(xmin = boot_lo, xmax = boot_hi, color = "Bootstrap"),
                width = 0.3, orientation = "y") +
  labs(x = "FT%", y = "Player", color = "Method") +
  theme_minimal()

# Task 3 & 4: simulation study with bootstrap coverage
z_975 = qnorm(0.975)
p_grid = seq(0, 1, length.out = 1000)
sample_sizes = c(10, 50, 100, 250, 500, 1000)
M = 100

results = map_dfr(sample_sizes, function(n) {
  cat("Running n =", n, "\n")
  map_dfr(p_grid, function(p) {
    wald_cover = numeric(M)
    ac_cover   = numeric(M)
    boot_cover = numeric(M)
    
    for (m in 1:M) {
      s = rbinom(1, n, p)
      p_hat = s / n
      
      # Wald
      wald_lo = p_hat - z_975 * sqrt(p_hat * (1 - p_hat) / n)
      wald_hi = p_hat + z_975 * sqrt(p_hat * (1 - p_hat) / n)
      wald_cover[m] = (p >= wald_lo & p <= wald_hi)
      
      # Agresti-Coull
      n_tilde = n + z_975^2
      p_tilde = (s + z_975^2 / 2) / n_tilde
      ac_lo   = p_tilde - z_975 * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
      ac_hi   = p_tilde + z_975 * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
      ac_cover[m] = (p >= ac_lo & p <= ac_hi)
      
      # Bootstrap
      shots = c(rep(1, s), rep(0, n - s))
      boot_draws = replicate(200, mean(sample(shots, size = n, replace = TRUE)))
      boot_lo = quantile(boot_draws, 0.025)
      boot_hi = quantile(boot_draws, 0.975)
      boot_cover[m] = (p >= boot_lo & p <= boot_hi)
    }
    
    tibble(n = n, p = p, wald_cov = mean(wald_cover), 
           ac_cov = mean(ac_cover), boot_cov = mean(boot_cover))
  })
})

results_long = results %>%
  pivot_longer(cols = c(wald_cov, ac_cov, boot_cov),
               names_to  = "method",
               values_to = "coverage") %>%
  mutate(method = recode(method, wald_cov = "Wald", 
                         ac_cov = "Agresti-Coull", 
                         boot_cov = "Bootstrap"))

ggplot(results_long, aes(x = p, y = coverage, color = method)) +
  geom_line(alpha = 0.7) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  facet_wrap(~ n, labeller = label_both) +
  labs(x = "p", y = "Coverage Probability", color = "Method") +
  theme_minimal()

