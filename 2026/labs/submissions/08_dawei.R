#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(dplyr)

# set seed
set.seed(8)

#########################
### PART 1: NBA FREE THROWS ###
#########################

# load data
nba_players = read_delim(
  "C:/Users/sundw/Downloads/08_nba-free-throws.csv",
  delim = ";",
  show_col_types = FALSE
)

# Task 1:
# - Modify the dataset to include:
#   * Player
#   * FT_total = total free throws made across the season
#   * FTA_total = total free throws attempted across the season
#   * FT_percent = FT_total / FTA_total
# - Remember that FT and FTA are per-game values, so convert them to totals using G

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

# Task 2:
# - Filter the dataset to players with at least 25 total free-throw attempts
# - Decide how you want to handle players with multiple team rows
# - Make sure the final player-level dataset has one row per player-season

ft_qualified <- ft_clean %>%
  filter(FT_total >= 25)

# Task 3:
# - Construct 95% Wald confidence intervals for each player's free-throw probability
# - Construct 95% Agresti-Coull confidence intervals for each player's free-throw probability
# - Make a plot with:
#   * x-axis = FT_percent
#   * y-axis = player name
#   * both interval types overlaid
# - Comment on which intervals look most different and why

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

ft_qualified %>%
  pivot_longer(
    cols = c(wald_lower, wald_upper, ac_lower, ac_upper),
    names_to = c("method", ".value"),
    names_pattern = "(wald|ac)_(lower|upper)"
  ) %>%
  mutate(method = recode(method, wald = "Wald", ac = "Agresti-Coull")) %>%
  ggplot(aes(y = reorder(Player, FT_pct), color = method)) +
  geom_point(aes(x = FT_pct), size = 1.5, color = "black") +
  geom_linerange(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.5)) +
  labs(x = "FT%", y = NULL, color = "Method",
       title = "95% Confidence Intervals for NBA Free Throw %") +
  theme_minimal()

# Agresti-Coull is wider and accounts better for the extreme values than Wald.

###########################
### PART 2: LLN WARM-UP ###
###########################

lln_p = 0.75
lln_n = 1000
lln_paths = 100

# Task 1:
# - Simulate one Bernoulli sequence with true probability p = 0.75 and length 1000
# - Compute the running estimate
#     phat_n = (1 / n) * sum_{i=1}^n X_i
#   for n = 1, ..., 1000

# Task 2:
# - Plot the running estimate against n
# - Add a horizontal line at p = 0.75
# - Describe what happens as n grows

# Task 3:
# - Repeat the simulation 100 times
# - Overlay the 100 running-estimate paths on one figure
# - Describe how the variability changes with n

n <- 1000

sim <- tibble(
  attempt  = 1:n,
  make     = rbinom(n, 1, 0.75),
  p_hat    = cumsum(make) / attempt
)

ggplot(sim, aes(x = attempt, y = p_hat)) +
  geom_line() +
  geom_hline(yintercept = 0.75, color = "red", linetype = "dashed") +
  labs(x = "Attempt (n)", y = expression(hat(p)),
       title = "Running Free Throw Make % (p = 0.75)") +
  theme_minimal()

# 100 times!

n <- 1000
n_sims <- 100

sim_multi <- map_dfr(1:n_sims, ~tibble(
  sim     = .x,
  attempt = 1:n,
  make    = rbinom(n, 1, 0.75),
  p_hat   = cumsum(make) / attempt
))

ggplot(sim_multi, aes(x = attempt, y = p_hat, group = sim)) +
  geom_line(alpha = 0.15) +
  geom_hline(yintercept = 0.75, color = "red", linetype = "dashed") +
  labs(x = "Attempt (n)", y = expression(hat(p)),
       title = "100 Simulated Running Free Throw Make % (p = 0.75)") +
  theme_minimal()

################################
### PART 3: SIMULATION STUDY ###
################################

# Task 1:
# - Use the grid p_grid as your set of candidate true probabilities
# - For each p and each n in sample_sizes, generate binomial data

# Task 2:
# - Compute the 95% Wald confidence interval
# - Compute the 95% Agresti-Coull confidence interval using
#     n_tilde = n + z_975^2
#     p_tilde = (S_n + z_975^2 / 2) / n_tilde
# - Interpret this as approximately adding 2 successes and 2 failures

# Task 3:
# - Repeat the simulation M = 100 times for each (n, p) pair
# - For each method, estimate coverage as the fraction of intervals that contain p

# Task 4:
# - Plot coverage probability vs p
# - Facet by sample size
# - Include both the Wald and Agresti-Coull methods on the same figure
# - Add a horizontal reference line at 0.95
# - Comment on where the Wald interval undercovers

library(purrr)

p_grid <- seq(0, 1, length.out = 1000)
n_vals <- c(10, 50, 100, 250, 500, 1000)
M      <- 100

coverage <- expand_grid(p = p_grid, n = n_vals) %>%
  mutate(
    results = map2(p, n, function(p, n) {
      sims <- matrix(rbinom(M * n, 1, p), nrow = M, ncol = n)
      ft   <- rowSums(sims)
      fta  <- n
      phat <- ft / fta
      
      # Wald
      wald_se  <- sqrt(phat * (1 - phat) / fta)
      wald_cov <- mean((phat - 1.96 * wald_se <= p) & (p <= phat + 1.96 * wald_se))
      
      # Agresti-Coull
      p_tilde  <- (ft + 2) / (fta + 4)
      ac_se    <- sqrt(p_tilde * (1 - p_tilde) / (fta + 4))
      ac_cov   <- mean((p_tilde - 1.96 * ac_se <= p) & (p <= p_tilde + 1.96 * ac_se))
      
      tibble(wald = wald_cov, ac = ac_cov)
    })
  ) %>%
  unnest(results) %>%
  pivot_longer(c(wald, ac), names_to = "method", values_to = "coverage") %>%
  mutate(method = recode(method, wald = "Wald", ac = "Agresti-Coull"),
         n      = factor(n, levels = n_vals))

ggplot(coverage, aes(x = p, y = coverage, color = method)) +
  geom_line(alpha = 0.7) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  facet_wrap(~n, labeller = label_both) +
  labs(x = "p", y = "Coverage probability", color = "Method",
       title = "95% CI Coverage Probability by Method and Sample Size") +
  theme_minimal()

############################
### PART 4: SKITTLES DEMO ###
############################

# Replace these with your observed counts from the Skittles activity
skittles_n = 115
skittles_r = 26

# Task 1:
# - Compute the observed red proportion r / n
# - Construct a 95% Wald confidence interval for the true red probability

# Task 2:
# - Construct a 95% Agresti-Coull confidence interval for the same probability

skittles_phat <- skittles_r / skittles_n

# Wald
wald_se    <- sqrt(skittles_phat * (1 - skittles_phat) / skittles_n)
wald_lower <- skittles_phat - 1.96 * wald_se
wald_upper <- skittles_phat + 1.96 * wald_se

# Agresti-Coull
p_tilde    <- (skittles_r + 2) / (skittles_n + 4)
ac_se      <- sqrt(p_tilde * (1 - p_tilde) / (skittles_n + 4))
ac_lower   <- p_tilde - 1.96 * ac_se
ac_upper   <- p_tilde + 1.96 * ac_se

wald_lower
wald_upper
ac_lower
ac_upper

# Task 3:
# - Compare the two intervals
# - State which one seems more sensible when the observed red proportion is near 0 or 1

# Wald is more sensible when closer to 0, but Agresti-Coull seems more sensible when closer to 1
