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
expected_points_from_probs = function(prob_object, score_values) {
  
  if (is.null(dim(prob_object))) {
    prob_matrix = matrix(prob_object, nrow = 1)
    colnames(prob_matrix) = names(prob_object)
  } else {
    prob_matrix = as.matrix(prob_object)
  }
  
  score_values_chr = as.character(score_values)
  
  prob_matrix = prob_matrix[, score_values_chr, drop = FALSE]
  
  as.numeric(prob_matrix %*% score_values)
}
# Task 2:
# - Decide which bootstrap variation is most appropriate here:
#   * observation bootstrap
#   * cluster bootstrap
#   * parametric bootstrap
#   * residual bootstrap
# - State your choice in comments
# - Explain why it matches the dependence structure of this dataset
#Cluster bootstrap, where I'd match it up depending on games' because there is dependence within plays in a drive, and dependence between drives. 


# The grouping variable that matters here is:
# game_id

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
  sampled_game_ids = sample(
    game_ids,
    size = length(game_ids),
    replace = TRUE
  )
  
  boot_data = bind_rows(
    lapply(sampled_game_ids, function(g) {
      nfl_model_data[nfl_model_data$game_id == g, ]
    })
  )
  
  boot_model = multinom(
    ep_formula,
    data = boot_data,
    trace = FALSE
  )
  
  boot_probs = predict(
    boot_model,
    newdata = target_state,
    type = "probs"
  )
  
  bootstrap_ep[b] = expected_points_from_probs(
    boot_probs,
    score_values
  )
}

quantile(bootstrap_ep, c(0.025, 0.5, 0.975), na.rm = TRUE)

# Task 4:
# - Store the bootstrap estimates in a vector
# - Make a plot of the bootstrap distribution
bootstrap_df = tibble(ep = bootstrap_ep)

ggplot(bootstrap_df, aes(x = ep)) +
  geom_histogram(bins = 20) +
  labs(
    title = "Bootstrap Distribution of Expected Points",
    x = "Expected Points",
    y = "Count"
  ) +
  theme_minimal()


# Task 5:
# - Compute:
#   * the original fitted expected-points estimate
#   * the bootstrap standard error
#   * the 95% percentile interval

fitted_probs = predict(
  boot_model,
  newdata = target_state,
  type = "probs"
)

fitted_ep = expected_points_from_probs(
  fitted_probs,
  score_values
)

bootstrap_se = sd(bootstrap_ep, na.rm = TRUE)

bootstrap_ci = quantile(
  bootstrap_ep,
  probs = c(0.025, 0.975),
  na.rm = TRUE
)

# Task 6:
# - In comments, explain why a naive row-by-row observation bootstrap
#   is less appropriate for this dataset
#plays are dependent on previous plays. Observation bootstrap assumes independence
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
nba_players_summary <- nba_free_throws %>%
  group_by(Player) %>%
  summarise(
    FT_total = sum(FT_total, na.rm = TRUE),
    FTA_total = sum(FTA_total, na.rm = TRUE),
    FT_percent = FT_total / FTA_total,
    .groups = "drop"
  )
nba_players_summary <- nba_players_summary%>%
  filter(FTA_total>=25)
# Task 2:
# - For each player, construct a 95% bootstrap confidence interval
#   for free-throw percentage
# - Overlay the bootstrap intervals on your Lab 8 player plot
# - Compare bootstrap, Wald, and Agresti-Coull intervals
nba_players_ci <- nba_players_summary %>%
  mutate(
    se = sqrt(FT_percent * (1 - FT_percent) / FTA_total),
    lower_95 = pmax(0, FT_percent - 1.96 * se),
    upper_95 = pmin(1, FT_percent + 1.96 * se)
  )

nba_players_ci

nba_players_ci <- nba_players_ci %>%
  mutate(
    n_tilde = FTA_total + 1.96^2,
    p_tilde = (FT_total + (1.96^2)/2) / n_tilde,
    
    lower_95AC = pmax(
      0,
      p_tilde - 1.96 * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
    ),
    
    upper_95AC = pmin(
      1,
      p_tilde + 1.96 * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
    )
  ) 

nba_players_ci


B <- 1000

bootstrap_ci <- nba_players_summary %>%
  rowwise() %>%
  mutate(
    boot_props = list(
      replicate(
        B,
        mean(sample(
          c(rep(1, FT_total), rep(0, FTA_total - FT_total)),
          size = FTA_total,
          replace = TRUE
        ))
      )
    ),
    lower_95_boot = quantile(boot_props, 0.025),
    upper_95_boot = quantile(boot_props, 0.975)
  ) %>%
  select(Player, lower_95_boot, upper_95_boot)

nba_players_ci <- nba_players_ci %>%
  left_join(bootstrap_ci, by = "Player")



ggplot(nba_players_ci, aes(y = reorder(Player, FT_percent))) +
  
  geom_errorbarh(
    aes(xmin = lower_95, xmax = upper_95),
    height = 0.2,
    linewidth = 0.8
  ) +
  
  geom_errorbarh(
    aes(xmin = lower_95, xmax = upper_95),
    color = "steelblue",
    height = 0.2,
    linewidth = 1
  ) +
  
  # Agresti-Coull
  geom_errorbarh(
    aes(xmin = lower_95AC, xmax = upper_95AC),
    color = "firebrick",
    height = 0.45,
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  # Bootstrap
  geom_errorbarh(
    aes(xmin = lower_95_boot, xmax = upper_95_boot),
    color = "darkgreen",
    height = 0.7,
    linewidth = 1,
    linetype = "dotdash"
  ) +
  
  geom_point(aes(x = FT_percent), size = 2) +
  
  labs(
    x = "Free Throw Percentage",
    y = "Player",
    title = "95% Confidence Intervals for Free Throw Probability",
    subtitle = "Solid = Wald interval; Dashed = Agresti-Coull interval"
  ) +
  
  theme_minimal()

#bootstraap looks to be the tightest. 

# Task 3:
# - Repeat the simulation 100 times
# - Overlay the 100 running-estimate paths on one figure
# - Describe how the variability changes with n

# You may want helper functions for:
# - one bootstrap resample of a player's free throws
# - a percentile interval from bootstrap draws

bootstrap_ft_percent = function(ft_made, ft_attempted, B = 1000) {
  
  season = c(
    rep(1, ft_made),
    rep(0, ft_attempted - ft_made)
  )
  
  boot_estimates = replicate(B, {
    mean(sample(season, size = ft_attempted, replace = TRUE))
  })
  
  boot_estimates
}


percentile_interval = function(bootstrap_draws, level = 0.95) {
    alpha = 1 - level
    quantile(
        bootstrap_draws,
        probs = c(alpha / 2, 1 - alpha / 2),
        na.rm = TRUE
    )
}

ft_made = 80
ft_attempted = 100

R = 100

sim_data = tibble()

for (r in 1:R) {
  
  season = c(
    rep(1, ft_made),
    rep(0, ft_attempted - ft_made)
  )
  
  sampled_season = sample(season, size = ft_attempted, replace = TRUE)
  
  running_estimate = cumsum(sampled_season) / seq_along(sampled_season)
  
  sim_data = bind_rows(
    sim_data,
    tibble(
      simulation = r,
      n = seq_along(sampled_season),
      ft_percent = running_estimate
    )
  )
}
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
wald_cov <- matrix(NA,
                   nrow = length(p_grid),
                   ncol = length(sample_sizes))

ac_cov <- matrix(NA,
                 nrow = length(p_grid),
                 ncol = length(sample_sizes))
boot_cov <- matrix(
  NA,
  nrow = length(p_grid),
  ncol = length(sample_sizes)
)

B <- 500

for(j in 1:length(sample_sizes)) {
  
  n <- sample_sizes[j]
  
  for(i in 1:length(p_grid)) {
    
    p <- p_grid[i]
    
    wald_contains <- rep(FALSE, M)
    ac_contains <- rep(FALSE, M)
    boot_contains <- rep(FALSE, M)
    
    for(m in 1:M) {
      
      x <- rbinom(1, n, p)
      phat <- x / n
      
      # Wald
      se <- sqrt(phat * (1 - phat) / n)
      wald_lower <- phat - z_975 * se
      wald_upper <- phat + z_975 * se
      
      wald_contains[m] <- (wald_lower <= p) & (p <= wald_upper)
      
      # Agresti-Coull
      n_tilde <- n + z_975^2
      p_tilde <- (x + z_975^2 / 2) / n_tilde
      
      se_ac <- sqrt(p_tilde * (1 - p_tilde) / n_tilde)
      ac_lower <- p_tilde - z_975 * se_ac
      ac_upper <- p_tilde + z_975 * se_ac
      
      ac_contains[m] <- (ac_lower <= p) & (p <= ac_upper)
      
      # Bootstrap percentile interval
      boot_x <- rbinom(B, size = n, prob = phat)
      boot_phat <- boot_x / n
      
      boot_ci <- quantile(
        boot_phat,
        probs = c(0.025, 0.975),
        na.rm = TRUE
      )
      
      boot_contains[m] <- (boot_ci[1] <= p) & (p <= boot_ci[2])
    }
    
    wald_cov[i, j] <- mean(wald_contains)
    ac_cov[i, j] <- mean(ac_contains)
    boot_cov[i, j] <- mean(boot_contains)
  }
}
wald_df <- as_tibble(wald_cov) %>%
  mutate(p = p_grid) %>%
  pivot_longer(
    -p,
    names_to = "sample_size",
    values_to = "coverage"
  ) %>%
  mutate(
    sample_size = sample_sizes[as.numeric(gsub("V", "", sample_size))],
    method = "Wald"
  )

ac_df <- as_tibble(ac_cov) %>%
  mutate(p = p_grid) %>%
  pivot_longer(
    -p,
    names_to = "sample_size",
    values_to = "coverage"
  ) %>%
  mutate(
    sample_size = sample_sizes[as.numeric(gsub("V", "", sample_size))],
    method = "Agresti-Coull"
  )

boot_df <- as_tibble(boot_cov) %>%
  mutate(p = p_grid) %>%
  pivot_longer(
    -p,
    names_to = "sample_size",
    values_to = "coverage"
  ) %>%
  mutate(
    sample_size = sample_sizes[as.numeric(gsub("V", "", sample_size))],
    method = "Bootstrap"
  )

coverage_df <- bind_rows(wald_df, ac_df, boot_df)

ggplot(
  coverage_df,
  aes(x = p, y = coverage, color = method)
) +
  geom_line(linewidth = 0.8) +
  geom_hline(
    yintercept = 0.95,
    linetype = "dashed"
  ) +
  facet_wrap(~ sample_size) +
  labs(
    title = "Coverage Probability by Method",
    x = "True Probability (p)",
    y = "Coverage Probability",
    color = "Method"
  ) +
  theme_minimal()


####Bootstrap sems to be tightest. 
