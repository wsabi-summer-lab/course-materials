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
# - Refit your preferred expected-points model from Lab 5
# - A concrete default is:
#   pts_next_score ~ bs(yardline_100, df = 6) +
#                    factor(down) +
#                    log(ydstogo) +
#                    bs(half_seconds_remaining, df = 5)
# - Convert fitted class probabilities into expected points

# You will probably want helper objects like these:
score_values = sort(unique(nfl_data$pts_next_score))
game_ids = unique(nfl_data$game_id)

# Default model scaffold. You may modify this if you choose a different EP model.
ep_formula = pts_next_score_factor ~
    bs(yardline_100, df = 6) +
    factor(down) +
    log(ydstogo) +
    bs(half_seconds_remaining, df = 5)

# Target state for the bootstrap study:
target_state = tibble(
    yardline_100 = 35,
    down = 1,
    ydstogo = 10,
    half_seconds_remaining = 1800
)

# If your preferred model also uses posteam_spread, add:
# target_state = target_state |>
#     mutate(posteam_spread = 0)

# Optional helper:
# - Write a function that takes a matrix/data frame of predicted class probabilities
#   and converts it to expected points using score_values
expected_points_from_probs = function(prob_matrix, score_values) {
    prob_df = as_tibble(prob_matrix)

    # TODO: make sure the probability columns line up with score_values.
    # Hint: columns from predict(..., type = "probs") are class labels.

    # TODO: return the expected points for each row:
    # rowSums(probabilities * score_values)
}


# Fit the default EP model
ep_model = multinom(ep_formula, data = nfl_model_data, trace = FALSE)

# Fill in the expected_points_from_probs helper
expected_points_from_probs = function(prob_matrix, score_values) {
  prob_df = as_tibble(prob_matrix)
  
  # predict(..., type = "probs") names columns by class label (as character)
  # sort them numerically to align with score_values
  col_order = order(as.numeric(colnames(prob_matrix)))
  prob_aligned = prob_matrix[, col_order, drop = FALSE]
  
  # EP = sum over outcomes of (probability * point value)
  rowSums(sweep(prob_aligned, 2, score_values, `*`))
}

# Predict class probabilities at the target state and convert to EP
target_probs = predict(ep_model, newdata = target_state, type = "probs")
target_ep    = expected_points_from_probs(
  matrix(target_probs, nrow = 1,
         dimnames = list(NULL, names(target_probs))),
  score_values)

cat("Fitted EP at target state:", round(target_ep, 4), "\n")





# Task 2:
# - Decide which bootstrap variation is most appropriate here:
#   * observation bootstrap
#   * cluster bootstrap
#   * parametric bootstrap
#   * residual bootstrap
# - State your choice in comments
# - Explain why it matches the dependence structure of this dataset

# The grouping variable that matters here is:
# game_id


#Cluster bootstrapping makes sense because plays in a game are not indepentent






# Task 3:
# - Implement your chosen bootstrap with at least B = 200 resamples
# - For each resample:
#   * create a bootstrap dataset
#   * refit the EP model
#   * recompute expected points at target_state

B = 200

bootstrap_ep = rep(NA_real_, B)

# If you use a cluster bootstrap, you will likely want to:
# - sample game_ids with replacement
# - rebuild the bootstrap dataset by binding together all rows
#   from each sampled game

for (b in seq_len(B)) {
    # TODO: sample games or rows, depending on your bootstrap choice.
    # sampled_game_ids = sample(game_ids, size = length(game_ids), replace = TRUE)

    # TODO: build the bootstrap dataset.
    # boot_data = ...

    # TODO: fit the model on boot_data.
    # boot_model = multinom(ep_formula, data = boot_data, trace = FALSE)

    # TODO: predict class probabilities at target_state.
    # boot_probs = predict(boot_model, newdata = target_state, type = "probs")

    # TODO: convert probabilities to expected points and store the result.
    # bootstrap_ep[b] = expected_points_from_probs(boot_probs, score_values)
  sampled_game_ids = sample(game_ids, size = length(game_ids), replace = TRUE)
  
  # Use a join instead of map_dfr for faster dataset construction
  boot_data = tibble(game_id = sampled_game_ids) |>
    left_join(nfl_model_data, by = "game_id", relationship = "many-to-many")
  
  # maxit reduced from default 100 to speed up convergence
  boot_model = multinom(ep_formula, data = boot_data, trace = FALSE, maxit = 30)
  
  boot_probs = predict(boot_model, newdata = target_state, type = "probs")
  
  bootstrap_ep[b] = expected_points_from_probs(
    matrix(boot_probs, nrow = 1, dimnames = list(NULL, names(boot_probs))),
    score_values
  )
}

# Task 4:
# - Store the bootstrap estimates in a vector
# - Make a plot of the bootstrap distribution

tibble(ep = bootstrap_ep) |>
  ggplot(aes(x = ep)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = target_ep, color = "red", linewidth = 1, linetype = "dashed") +
  labs(
    title = "Bootstrap Distribution of Expected Points",
    subtitle = "1st & 10 from own 35-yard line, 30 min remaining",
    x = "Expected Points",
    y = "Count",
    caption = "Red dashed line = original fitted EP"
  ) +
  theme_bw()

# Task 5:
# - Compute:
#   * the original fitted expected-points estimate
#   * the bootstrap standard error
#   * the 95% percentile interval




# Original fitted EP estimate
cat("Original fitted EP:      ", round(target_ep, 4), "\n")

# Bootstrap standard error
boot_se = sd(bootstrap_ep, na.rm = TRUE)
cat("Bootstrap standard error:", round(boot_se, 4), "\n")

# 95% percentile interval
boot_ci = quantile(bootstrap_ep, probs = c(0.025, 0.975), na.rm = TRUE)
cat("95% percentile interval: [", round(boot_ci[1], 4), ",", round(boot_ci[2], 4), "]\n")




# Task 6:
# - In comments, explain why a naive row-by-row observation bootstrap
#   is less appropriate for this dataset

#Row by row is less appropriate because there is correlation between plays
#in a game so doing row by row would understate the variances






################################
### PART 2: NBA FREE THROWS ####
################################

nba_players = read_delim(
    "../data/09_nba-free-throws.csv.gz",
    delim = ";",
    show_col_types = FALSE
)

# Task 1:
# - Recreate the player-level free-throw dataset from Lab 8
# - Include:
#   * Player
#   * FT_total = approximate total free throws made across the season
#   * FTA_total = approximate total free throws attempted across the season
#   * FT_percent
# - Basketball Reference reports FT and FTA as per-game values in this table,
#   so convert them to approximate totals using G before treating them as counts
# - Filter to players with at least 25 approximate total free-throw attempts

nba_free_throws = nba_players |>
    mutate(
        FT_total = round(FT * G),
        FTA_total = round(FTA * G),
        FT_percent = FT_total / FTA_total
    )

# TODO: decide how to handle multi-team players.
# Option 1: keep only rows where Tm == "TOT" for players who changed teams.
# Option 2: aggregate team rows yourself.
#
# nba_player_level = nba_free_throws |>
#     ... |>
#     filter(FTA_total >= 25)


nba_player_level = nba_free_throws |>
  group_by(Player) |>
  filter(n() == 1 | Tm == "TOT") |>  # keep TOT row for multi-team players
  ungroup() |>
  filter(FTA_total >= 25)



# Task 2:
# - For each player, construct a 95% bootstrap confidence interval
#   for free-throw percentage
# - Overlay the bootstrap intervals on your Lab 8 player plot
# - Compare bootstrap, Wald, and Agresti-Coull intervals

# You may want helper functions for:
# - one bootstrap resample of a player's free throws
# - a percentile interval from bootstrap draws

bootstrap_ft_percent = function(ft_made, ft_attempted, B = 1000) {
    # TODO: create B bootstrap resamples for one player.
    # Hint: represent the season as a vector with ft_made ones and
    # ft_attempted - ft_made zeros, then sample with replacement.
  shoots = c(rep(1, ft_made), rep(0, ft_attempted - ft_made))
  replicate(B, mean(sample(shoots, size = ft_attempted, replace = TRUE)))
}

percentile_interval = function(bootstrap_draws, level = 0.95) {
    alpha = 1 - level
    quantile(
        bootstrap_draws,
        probs = c(alpha / 2, 1 - alpha / 2),
        na.rm = TRUE
    )
}

player_cis = nba_player_level |>
  rowwise() |>
  mutate(
    boot_draws = list(bootstrap_ft_percent(FT_total, FTA_total, B = 1000)),
    ci_lo      = percentile_interval(boot_draws)[1],
    ci_hi      = percentile_interval(boot_draws)[2]
  ) |>
  ungroup()

# Plot
player_cis |>
  arrange(FT_percent) |>
  mutate(Player = factor(Player, levels = Player)) |>
  ggplot(aes(x = FT_percent, y = Player)) +
  geom_point(size = 1.5) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.3) +
  labs(
    title = "NBA Player Free-Throw Percentage with 95% Bootstrap CIs",
    x = "Free-Throw Percentage",
    y = NULL
  ) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6))





# Task 3:
# - Revisit the Lab 8 simulation study
# - Add bootstrap confidence intervals to the same coverage framework
# - Plot bootstrap coverage probability against p
# - Compare to Wald and Agresti-Coull

# Useful objects from Lab 8:
sample_sizes = c(10, 50, 100, 250, 500, 1000)
p_grid = seq(0, 1, length.out = 1000)
M = 100
z_975 = qnorm(0.975)

bootstrap_coverage = tibble()




for (n in sample_sizes) {
  for (p in p_grid) {
    covered_wald = logical(M)
    covered_ac   = logical(M)
    covered_boot = logical(M)
    
    for (m in seq_len(M)) {
      x     = rbinom(1, size = n, prob = p)
      p_hat = x / n
      
      # Wald
      se_wald         = sqrt(p_hat * (1 - p_hat) / n)
      covered_wald[m] = (p >= p_hat - z_975 * se_wald) &
        (p <= p_hat + z_975 * se_wald)
      
      # Agresti-Coull
      n_tilde       = n + z_975^2
      p_tilde       = (x + z_975^2 / 2) / n_tilde
      se_ac         = sqrt(p_tilde * (1 - p_tilde) / n_tilde)
      covered_ac[m] = (p >= p_tilde - z_975 * se_ac) &
        (p <= p_tilde + z_975 * se_ac)
      
      # Bootstrap (parametric: fast)
      boot_draws      = rbinom(1000, size = n, prob = p_hat) / n
      ci_boot         = quantile(boot_draws, probs = c(0.025, 0.975))
      covered_boot[m] = (p >= ci_boot[1]) & (p <= ci_boot[2])
    }
    
    bootstrap_coverage = bind_rows(bootstrap_coverage, tibble(
      n    = n,
      p    = p,
      wald = mean(covered_wald),
      ac   = mean(covered_ac),
      boot = mean(covered_boot)
    ))
  }
}

# Also add Wald and AC to the player plot now that we have all three
player_cis_all = nba_player_level |>
  rowwise() |>
  mutate(
    boot_draws = list(bootstrap_ft_percent(FT_total, FTA_total, B = 1000)),
    boot_lo    = percentile_interval(boot_draws)[1],
    boot_hi    = percentile_interval(boot_draws)[2],
    wald_lo    = FT_percent - z_975 * sqrt(FT_percent * (1 - FT_percent) / FTA_total),
    wald_hi    = FT_percent + z_975 * sqrt(FT_percent * (1 - FT_percent) / FTA_total),
    n_tilde    = FTA_total + z_975^2,
    p_tilde    = (FT_total + z_975^2 / 2) / n_tilde,
    ac_lo      = p_tilde - z_975 * sqrt(p_tilde * (1 - p_tilde) / n_tilde),
    ac_hi      = p_tilde + z_975 * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
  ) |>
  ungroup()

player_cis_all |>
  arrange(FT_percent) |>
  mutate(Player = factor(Player, levels = Player)) |>
  pivot_longer(
    cols = c(boot_lo, boot_hi, wald_lo, wald_hi, ac_lo, ac_hi),
    names_to  = c("method", ".value"),
    names_pattern = "(.+)_(lo|hi)"
  ) |>
  ggplot(aes(x = FT_percent, y = Player, color = method)) +
  geom_point(size = 1.2) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.3, alpha = 0.6) +
  scale_color_manual(
    values = c(boot = "steelblue", wald = "firebrick", ac = "darkgreen"),
    labels = c(boot = "Bootstrap", wald = "Wald", ac = "Agresti-Coull")
  ) +
  labs(
    title = "NBA Free-Throw % with 95% Confidence Intervals",
    x = "Free-Throw Percentage", y = NULL, color = "Method"
  ) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6))

# Coverage plot
bootstrap_coverage |>
  pivot_longer(c(wald, ac, boot), names_to = "method", values_to = "coverage") |>
  mutate(
    method  = recode(method, wald = "Wald", ac = "Agresti-Coull", boot = "Bootstrap"),
    n_label = paste0("n = ", n)
  ) |>
  ggplot(aes(x = p, y = coverage, color = method)) +
  geom_line(alpha = 0.8) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  facet_wrap(~ n_label) +
  scale_color_manual(
    values = c("Wald" = "firebrick", "Agresti-Coull" = "darkgreen", "Bootstrap" = "steelblue")
  ) +
  labs(
    title = "Coverage Probability: Wald vs Agresti-Coull vs Bootstrap",
    x = "True p", y = "Coverage Probability", color = "Method"
  ) +
  theme_bw()

# TODO: extend your Lab 8 simulation loop.
# For each simulated Binomial(n, p) count, compute:
# - Wald interval
# - Agresti-Coull interval
# - bootstrap percentile interval
# Then summarize coverage by method, n, and p.
