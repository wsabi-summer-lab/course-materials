#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(8)

#########################
### PART 1: NBA FREE THROWS ###
#########################

# load data
nba_players = read_delim(
  "../data/08_nba-free-throws.csv.gz",
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
glimpse(nba_players)
nba_players_short<-nba_players|>
  group_by(Player)|>
  summarize(FT_total=round(sum(FT*G)),
         FTA_total=round(sum(FTA*G)),
         FT_percent=FT_total/FTA_total)|>
  select(Player, FT_total, FTA_total, FT_percent)
glimpse(nba_players_short)

# Task 2:
# - Filter the dataset to players with at least 25 total free-throw attempts
# - Decide how you want to handle players with multiple team rows
# - Make sure the final player-level dataset has one row per player-season
nba_players_short<-nba_players_short|>
  filter(FTA_total>=25)
# Task 3:
# - Construct 95% Wald confidence intervals for each player's free-throw probability
# - Construct 95% Agresti-Coull confidence intervals for each player's free-throw probability
# - Make a plot with:
#   * x-axis = FT_percent
#   * y-axis = player name
#   * both interval types overlaid
# - Comment on which intervals look most different and why
nba_players_short <- nba_players_short |>
  mutate(
    # Wald
    wald_lower = FT_percent -
      1.96 * sqrt(FT_percent * (1 - FT_percent) / FTA_total),
    wald_upper = FT_percent +
      1.96 * sqrt(FT_percent * (1 - FT_percent) / FTA_total),
    
    # Agresti-Coull
    p_tilde = (FT_total + 2) / (FTA_total + 4),
    ac_lower = p_tilde -
      1.96 * sqrt(p_tilde * (1 - p_tilde) / (FTA_total + 4)),
    ac_upper = p_tilde +
      1.96 * sqrt(p_tilde * (1 - p_tilde) / (FTA_total + 4))
  ) |>
  arrange(FT_percent) |>
  mutate(Player = factor(Player, levels = Player))

colnames(nba_players_short)

summary(nba_players_short[, c(
  "FT_percent",
  "wald_lower",
  "wald_upper",
  "ac_lower",
  "ac_upper"
)])
ggplot(nba_players_short) +
  geom_segment(
    aes(
      y = wald_lower,
      yend = wald_upper,
      x = seq_along(FT_percent),
      xend = seq_along(FT_percent),
      color = "Wald"
    )
  ) +
  geom_segment(
    aes(
      y = ac_lower,
      yend = ac_upper,
      x = seq_along(FT_percent),
      xend = seq_along(FT_percent),
      color = "Agresti-Coull"
    ),
    alpha = 0.7
  ) +
  geom_point(
    aes(
      y = FT_percent,
      x = seq_along(FT_percent)
    ),
    size = 1
  ) +
  labs(
    x = "Player Index",
    y = "FT%",
    color = "Interval Type"
  ) +
  theme_minimal()
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
# Task 1: single path
x <- rbinom(lln_n, size = 1, prob = lln_p)
phat <- cumsum(x) / seq_len(lln_n)

# Task 2: plot single running estimate
plot(phat, type = "l",
     xlab = "n",
     ylab = expression(hat(p)[n]),
     main = "LLN: Running Estimate of p")
abline(h = lln_p, col = "red", lwd = 2)

# Task 3: 100 paths
set.seed(2)

mat <- replicate(lln_paths, {
  x <- rbinom(lln_n, size = 1, prob = lln_p)
  cumsum(x) / seq_len(lln_n)
})

matplot(mat, type = "l", lty = 1,
        xlab = "n",
        ylab = expression(hat(p)[n]),
        main = "LLN: 100 Running Estimate Paths")

abline(h = lln_p, col = "red", lwd = 2)
################################
### PART 3: SIMULATION STUDY ###
################################


# ----------------------------
# Parameters
# ----------------------------
z_975 <- qnorm(0.975)
p_grid <- seq(0, 1, length.out = 1000)
sample_sizes <- c(10, 50, 100, 250, 500, 1000)
M <- 100

# ----------------------------
# Storage
# ----------------------------
results <- expand.grid(
  p = p_grid,
  n = sample_sizes,
  method = c("Wald", "Agresti-Coull"),
  rep = 1:M
)

results$cover <- NA

# ----------------------------
# Simulation loop
# ----------------------------
for (i in seq_len(nrow(results))) {
  
  p <- results$p[i]
  n <- results$n[i]
  
  S <- rbinom(1, size = n, prob = p)
  phat <- S / n
  
  if (results$method[i] == "Wald") {
    
    se <- sqrt(phat * (1 - phat) / n)
    lower <- phat - z_975 * se
    upper <- phat + z_975 * se
    
  } else {
    
    n_tilde <- n + z_975^2
    p_tilde <- (S + z_975^2 / 2) / n_tilde
    
    se <- sqrt(p_tilde * (1 - p_tilde) / n_tilde)
    lower <- p_tilde - z_975 * se
    upper <- p_tilde + z_975 * se
  }
  
  results$cover[i] <- (lower <= p && p <= upper)
}

# ----------------------------
# Coverage computation
# ----------------------------
coverage <- results %>%
  group_by(p, n, method) %>%
  summarise(
    coverage = mean(cover),
    .groups = "drop"
  )

# ----------------------------
# Plot
# ----------------------------
ggplot(coverage, aes(x = p, y = coverage, color = method)) +
  geom_line() +
  facet_wrap(~ n, ncol = 3) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(
    x = "True p",
    y = "Coverage Probability",
    color = "Method",
    title = "Coverage of 95% Confidence Intervals"
  ) +
  theme_minimal()
# A convenient helper structure is:
# - loop over p
# - loop over n
# - simulate Binomial(n, p) data
# - compute both intervals
# - record whether each interval contains the true p

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

############################
### PART 4: SKITTLES DEMO ###
############################

# Replace these with your observed counts from the Skittles activity
skittles_n <- 106
skittles_r <- 21

z_975 <- qnorm(0.975)

# ----------------------------
# Task 1: Observed proportion + Wald CI
# ----------------------------
phat <- skittles_r / skittles_n

wald_se <- sqrt(phat * (1 - phat) / skittles_n)

wald_lower <- phat - z_975 * wald_se
wald_upper <- phat + z_975 * wald_se

wald_ci <- c(wald_lower, wald_upper)

# ----------------------------
# Task 2: Agresti–Coull CI
# ----------------------------
n_tilde <- skittles_n + z_975^2
p_tilde <- (skittles_r + z_975^2 / 2) / n_tilde

ac_se <- sqrt(p_tilde * (1 - p_tilde) / n_tilde)

ac_lower <- p_tilde - z_975 * ac_se
ac_upper <- p_tilde + z_975 * ac_se

ac_ci <- c(ac_lower, ac_upper)

# ----------------------------
# Output
# ----------------------------
phat
wald_ci
ac_ci

# ----------------------------
# Task 3: comparison (text answer)
# ----------------------------
# Wald intervals can perform poorly when p is near 0 or 1
# because the normal approximation becomes inaccurate and can
# produce bounds outside [0,1] or too-narrow intervals.
#
# Agresti–Coull adds pseudo-counts (effectively smoothing the estimate),
# which stabilizes the variance estimate and tends to give more reliable
# coverage, especially in small samples or extreme proportions.