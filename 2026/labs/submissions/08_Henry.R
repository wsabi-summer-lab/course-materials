#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(8)

# output folder for plots
if (!dir.exists("plots")) dir.create("plots")

#########################
### PART 1: NBA FREE THROWS ###
#########################

# load data
nba_players = read_delim(
  "08_nba-free-throws.csv.gz",
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

# Players who changed teams have a "TOT" (total) row plus one row per team.
# To get one clean row per player-season we keep the "TOT" row when it exists,
# otherwise the player's single team row. This avoids double counting.
nba_one_row = nba_players %>%
  group_by(Player) %>%
  filter(if (any(Tm == "TOT")) Tm == "TOT" else TRUE) %>%
  slice(1) %>%
  ungroup()

# FT and FTA are per-game; multiply by games (G) to get season totals.
# Round because attempts/makes must be whole counts for the Binomial model.
nba_totals = nba_one_row %>%
  transmute(
    Player,
    FT_total  = round(FT * G),
    FTA_total = round(FTA * G),
    FT_percent = FT_total / FTA_total
  )

# Task 2:
# - Filter the dataset to players with at least 25 total free-throw attempts
# - Decide how you want to handle players with multiple team rows
# - Make sure the final player-level dataset has one row per player-season

# Multiple-team rows were already collapsed via the "TOT" row above, so the
# dataset is one row per player-season here.
nba_final = nba_totals %>%
  filter(FTA_total >= 25)

# Task 3:
# - Construct 95% Wald confidence intervals for each player's free-throw probability
# - Construct 95% Agresti-Coull confidence intervals for each player's free-throw probability
# - Make a plot with:
#   * x-axis = FT_percent
#   * y-axis = player name
#   * both interval types overlaid
# - Comment on which intervals look most different and why

z = qnorm(0.975)

nba_ci = nba_final %>%
  mutate(
    n = FTA_total,
    phat = FT_percent,
    # Wald: phat +/- z * sqrt(phat(1-phat)/n)
    wald_se = sqrt(phat * (1 - phat) / n),
    wald_lo = pmax(0, phat - z * wald_se),
    wald_hi = pmin(1, phat + z * wald_se),
    # Agresti-Coull: shrink n and phat first, then a Wald-style interval
    n_tilde = n + z^2,
    p_tilde = (FT_total + z^2 / 2) / n_tilde,
    ac_se = sqrt(p_tilde * (1 - p_tilde) / n_tilde),
    ac_lo = pmax(0, p_tilde - z * ac_se),
    ac_hi = pmin(1, p_tilde + z * ac_se)
  )

# Build a long table of both interval types for overlaid plotting.
nba_ci_long = nba_ci %>%
  transmute(
    Player, phat,
    Wald_lo = wald_lo, Wald_hi = wald_hi,
    `Agresti-Coull_lo` = ac_lo, `Agresti-Coull_hi` = ac_hi
  ) %>%
  pivot_longer(
    cols = -c(Player, phat),
    names_to = c("method", ".value"),
    names_sep = "_"
  )

# There are many qualifying players, so order by FT% and make a tall figure.
player_order = nba_ci %>% arrange(phat) %>% pull(Player)
nba_ci_long = nba_ci_long %>%
  mutate(Player = factor(Player, levels = player_order))

p1 = ggplot(nba_ci_long, aes(y = Player, color = method)) +
  geom_linerange(aes(xmin = lo, xmax = hi),
                 position = position_dodge(width = 0.6), linewidth = 0.4) +
  geom_point(aes(x = phat), color = "black", size = 0.4) +
  scale_color_manual(values = c("Wald" = "#D55E00", "Agresti-Coull" = "#0072B2")) +
  labs(
    title = "95% CIs for player free-throw probability (Wald vs Agresti-Coull)",
    x = "Free-throw probability", y = "Player", color = "Interval"
  ) +
  theme_minimal(base_size = 6) +
  theme(legend.position = "top")

ggsave("plots/p1_task3_ft_confidence_intervals.png", p1,
       width = 8, height = max(8, nrow(nba_ci) * 0.12), limitsize = FALSE)

# Observation:
# The two intervals are nearly identical for most players, who have moderate
# FT% (roughly 0.5-0.9) and a fair number of attempts -- there the large-sample
# Wald approximation is fine. They look most different for players whose FT% is
# close to 0 or 1 and/or who have few attempts (n near 25). There the Wald
# interval is too narrow and can even be pinned at the 0/1 boundary, while
# Agresti-Coull pulls phat toward 0.5 and widens, giving a more honest interval.

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

one_seq = rbinom(lln_n, size = 1, prob = lln_p)
running = cumsum(one_seq) / seq_len(lln_n)

# Task 2:
# - Plot the running estimate against n
# - Add a horizontal line at p = 0.75
# - Describe what happens as n grows

lln_df = tibble(n = seq_len(lln_n), phat = running)

p2 = ggplot(lln_df, aes(n, phat)) +
  geom_line(color = "#0072B2") +
  geom_hline(yintercept = lln_p, linetype = "dashed", color = "red") +
  labs(title = "Running free-throw estimate vs n (single path)",
       x = "n", y = expression(hat(p)[n])) +
  theme_minimal()

ggsave("plots/p2_task2_lln_single_path.png", p2, width = 8, height = 5)

# As n grows the running estimate is volatile early on (small n) but settles
# down and converges toward the true p = 0.75 -- a direct illustration of the
# Law of Large Numbers.

# Task 3:
# - Repeat the simulation 100 times
# - Overlay the 100 running-estimate paths on one figure
# - Describe how the variability changes with n

paths_df = map_dfr(seq_len(lln_paths), function(k) {
  x = rbinom(lln_n, size = 1, prob = lln_p)
  tibble(path = k, n = seq_len(lln_n), phat = cumsum(x) / seq_len(lln_n))
})

p3 = ggplot(paths_df, aes(n, phat, group = path)) +
  geom_line(alpha = 0.15, color = "#0072B2") +
  geom_hline(yintercept = lln_p, linetype = "dashed", color = "red") +
  labs(title = "100 running-estimate paths (LLN)",
       x = "n", y = expression(hat(p)[n])) +
  theme_minimal()

ggsave("plots/p2_task3_lln_100_paths.png", p3, width = 8, height = 5)

# The spread of the 100 paths is wide for small n and narrows as n grows: the
# paths fan out near n = 1 and funnel toward 0.75. The standard deviation of
# phat_n shrinks like 1/sqrt(n), so variability decreases as n increases.

################################
### PART 3: SIMULATION STUDY ###
################################

z_975 = qnorm(0.975)
p_grid = seq(0, 1, length.out = 1000)
sample_sizes = c(10, 50, 100, 250, 500, 1000)
M = 100

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

# Vectorized coverage: for a given (n, p) draw M binomial counts at once,
# build both intervals, and report the fraction covering the true p.
coverage_for = function(n, p, M, z) {
  S = rbinom(M, size = n, prob = p)
  phat = S / n

  # Wald
  wald_se = sqrt(phat * (1 - phat) / n)
  wald_lo = phat - z * wald_se
  wald_hi = phat + z * wald_se
  wald_cov = mean(p >= wald_lo & p <= wald_hi)

  # Agresti-Coull
  n_tilde = n + z^2
  p_tilde = (S + z^2 / 2) / n_tilde
  ac_se = sqrt(p_tilde * (1 - p_tilde) / n_tilde)
  ac_lo = p_tilde - z * ac_se
  ac_hi = p_tilde + z * ac_se
  ac_cov = mean(p >= ac_lo & p <= ac_hi)

  c(Wald = wald_cov, `Agresti-Coull` = ac_cov)
}

# Task 3:
# - Repeat the simulation M = 100 times for each (n, p) pair
# - For each method, estimate coverage as the fraction of intervals that contain p

sim_grid = expand_grid(n = sample_sizes, p = p_grid)
cov_mat = mapply(function(n, p) coverage_for(n, p, M, z_975),
                 sim_grid$n, sim_grid$p)

sim_results = sim_grid %>%
  mutate(Wald = cov_mat["Wald", ],
         `Agresti-Coull` = cov_mat["Agresti-Coull", ]) %>%
  pivot_longer(c(Wald, `Agresti-Coull`),
               names_to = "method", values_to = "coverage")

# Task 4:
# - Plot coverage probability vs p
# - Facet by sample size
# - Include both the Wald and Agresti-Coull methods on the same figure
# - Add a horizontal reference line at 0.95
# - Comment on where the Wald interval undercovers

p4 = ggplot(sim_results, aes(p, coverage, color = method)) +
  geom_line(alpha = 0.7, linewidth = 0.3) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  facet_wrap(~ n, labeller = label_both) +
  scale_color_manual(values = c("Wald" = "#D55E00", "Agresti-Coull" = "#0072B2")) +
  labs(title = "Coverage of 95% CIs vs true p, by sample size",
       x = "true p", y = "coverage", color = "Method") +
  theme_minimal()

ggsave("plots/p3_task4_coverage.png", p4, width = 10, height = 6)

# The Wald interval undercovers badly when p is near 0 or 1, and the problem is
# worse for small n: at n = 10 its coverage collapses far below 0.95 toward the
# extremes (and is jagged because phat(1-phat) -> 0 makes the interval degenerate).
# Agresti-Coull stays much closer to the nominal 0.95 across the whole range of p,
# including the extremes, which is exactly why it is preferred for proportions.

############################
### PART 4: SKITTLES DEMO ###
############################

# Replace these with your observed counts from the Skittles activity
# (really dont eat candles XD, I will just use approx numbers)
skittles_n = 60
skittles_r = 14

# Task 1:
# - Compute the observed red proportion r / n
# - Construct a 95% Wald confidence interval for the true red probability

sk_phat = skittles_r / skittles_n
sk_wald_se = sqrt(sk_phat * (1 - sk_phat) / skittles_n)
sk_wald = c(lo = sk_phat - z_975 * sk_wald_se,
            hi = sk_phat + z_975 * sk_wald_se)

# Task 2:
# - Construct a 95% Agresti-Coull confidence interval for the same probability

sk_n_tilde = skittles_n + z_975^2
sk_p_tilde = (skittles_r + z_975^2 / 2) / sk_n_tilde
sk_ac_se = sqrt(sk_p_tilde * (1 - sk_p_tilde) / sk_n_tilde)
sk_ac = c(lo = sk_p_tilde - z_975 * sk_ac_se,
          hi = sk_p_tilde + z_975 * sk_ac_se)

cat(sprintf("Skittles: r/n = %d/%d = %.3f\n", skittles_r, skittles_n, sk_phat))
cat(sprintf("Wald 95%% CI:          [%.3f, %.3f]\n", sk_wald["lo"], sk_wald["hi"]))
cat(sprintf("Agresti-Coull 95%% CI: [%.3f, %.3f]\n", sk_ac["lo"], sk_ac["hi"]))

# Task 3:
# - Compare the two intervals
# - State which one seems more sensible when the observed red proportion is near 0 or 1
#
# The two intervals are similar here because the proportion is not extreme.
# When the observed red proportion is close to 0 or 1, the Wald interval becomes
# unreliable: its width sqrt(phat(1-phat)/n) shrinks toward 0 and it can extend
# past [0,1] or collapse to a point (e.g. r = 0 gives a zero-width interval at 0).
# Agresti-Coull is more sensible there: adding the artificial successes/failures
# pulls p_tilde away from the boundary and keeps a sensible, non-degenerate width.
