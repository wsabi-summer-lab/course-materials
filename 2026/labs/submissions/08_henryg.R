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

glimpse(nba_players)
# Task 1:
# - Modify the dataset to include:
#   * Player
#   * FT_total = total free throws made across the season
#   * FTA_total = total free throws attempted across the season
#   * FT_percent = FT_total / FTA_total
# - Remember that FT and FTA are per-game values, so convert them to totals using G

ft_data = nba_players %>%
  group_by(Player) %>%
  filter(n() == 1 | Tm == "TOT") %>%
  ungroup() %>%
  mutate(
    FT_total   = round(FT * G),
    FTA_total  = round(FTA * G),
    FT_percent = FT_total / FTA_total
  ) %>%
  select(Player, FT_total, FTA_total, FT_percent)


#Task 2: 
ft_data = ft_data %>%
  filter(FTA_total >= 25)


#Task 3:
z <- qnorm(0.975)

ft_intervals <- ft_data %>%
  mutate(
    # Wald
    wald_lo = FT_percent - z * sqrt(FT_percent * (1 - FT_percent) / FTA_total),
    wald_hi = FT_percent + z * sqrt(FT_percent * (1 - FT_percent) / FTA_total),
    
    # Agresti-Coull
    n_tilde = FTA_total + z^2,
    p_tilde = (FT_total + z^2 / 2) / n_tilde,
    ac_lo   = p_tilde - z * sqrt(p_tilde * (1 - p_tilde) / n_tilde),
    ac_hi   = p_tilde + z * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
  )

label_players <- ft_intervals %>%
  slice(seq(1, nrow(ft_intervals), length.out = 10)) %>%
  pull(Player)

ggplot(ft_intervals, aes(y = reorder(Player, FT_percent))) +
  geom_segment(aes(x = wald_lo, xend = wald_hi, yend = reorder(Player, FT_percent),
                   color = "Wald"), linewidth = 0.5, alpha = 0.7) +
  geom_segment(aes(x = ac_lo, xend = ac_hi, yend = reorder(Player, FT_percent),
                   color = "Agresti-Coull"), linewidth = 0.5, alpha = 0.7) +
  geom_point(aes(x = FT_percent), size = 0.8) +
  scale_color_manual(values = c("Wald" = "steelblue", "Agresti-Coull" = "tomato")) +
  scale_y_discrete(breaks = label_players) +
  labs(x = "FT%", y = "Player", color = "Method",
       title = "Free Throw % with 95% Confidence Intervals") +
  theme_bw(base_size = 7) +
  theme(axis.text.y = element_text(size = 8))






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

sim_draws <- rbinom(lln_n, size = 1, prob = lln_p)
running_phat <- cumsum(sim_draws) / seq_along(sim_draws)

# Task 2:
# - Plot the running estimate against n
# - Add a horizontal line at p = 0.75
# - Describe what happens as n grows

lln_df <- tibble(n = 1:lln_n, phat = running_phat)

ggplot(lln_df, aes(x = n, y = phat)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = lln_p, linetype = "dashed", color = "red") +
  labs(x = "n", y = expression(hat(p)[n]),
       title = "Running FT% Estimate (Single Path)") +
  theme_bw()

# Task 3:
# - Repeat the simulation 100 times
# - Overlay the 100 running-estimate paths on one figure
# - Describe how the variability changes with n

sim_paths <- replicate(lln_paths, {
  draws <- rbinom(lln_n, size = 1, prob = lln_p)
  cumsum(draws) / seq_along(draws)
})

paths_df <- as_tibble(sim_paths) %>%
  mutate(n = 1:lln_n) %>%
  pivot_longer(-n, names_to = "path", values_to = "phat")

ggplot(paths_df, aes(x = n, y = phat, group = path)) +
  geom_line(alpha = 0.15, color = "steelblue") +
  geom_hline(yintercept = lln_p, linetype = "dashed", color = "red") +
  labs(x = "n", y = expression(hat(p)[n]),
       title = "Running FT% Estimate (100 Paths)") +
  theme_bw()

#As n grows, the variability goes down and the percentage gets closer to 0.75.





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


results <- expand.grid(p = p_grid, n = sample_sizes) %>%
  rowwise() %>%
  mutate(
    sims = list(rbinom(M, size = n, prob = p)),
    
    # Wald coverage
    wald_cover = mean(sapply(sims, function(S) {
      phat <- S / n
      lo <- phat - z_975 * sqrt(phat * (1 - phat) / n)
      hi <- phat + z_975 * sqrt(phat * (1 - phat) / n)
      p >= lo & p <= hi
    })),
    
    # Agresti-Coull coverage
    ac_cover = mean(sapply(sims, function(S) {
      n_tilde <- n + z_975^2
      p_tilde <- (S + z_975^2 / 2) / n_tilde
      lo <- p_tilde - z_975 * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
      hi <- p_tilde + z_975 * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
      p >= lo & p <= hi
    }))
  ) %>%
  ungroup() %>%
  select(p, n, wald_cover, ac_cover) %>%
  pivot_longer(c(wald_cover, ac_cover), names_to = "method", values_to = "coverage") %>%
  mutate(method = recode(method, "wald_cover" = "Wald", "ac_cover" = "Agresti-Coull"))

ggplot(results, aes(x = p, y = coverage, color = method)) +
  geom_line(alpha = 0.8) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  facet_wrap(~ n, labeller = label_both) +
  scale_color_manual(values = c("Wald" = "steelblue", "Agresti-Coull" = "tomato")) +
  labs(x = "p", y = "Coverage Probability", color = "Method",
       title = "Coverage Probability vs p by Sample Size") +
  theme_bw()



############################
### PART 4: SKITTLES DEMO ###
############################

# Replace these with your observed counts from the Skittles activity
skittles_n = 110
skittles_r = 13

# Task 1:
# - Compute the observed red proportion r / n
# - Construct a 95% Wald confidence interval for the true red probability

# Task 1 (typo fix)
p_hat <- skittles_r / skittles_n

sk_wald_lo <- p_hat - z * sqrt(p_hat * (1 - p_hat) / skittles_n)
sk_wald_hi <- p_hat + z * sqrt(p_hat * (1 - p_hat) / skittles_n)

cat("Wald: [", round(sk_wald_lo, 4), ",", round(sk_wald_hi, 4), "]\n")

# Task 2: Agresti-Coull
sk_n_tilde <- skittles_n + z^2
sk_p_tilde <- (skittles_r + z^2 / 2) / sk_n_tilde

sk_ac_lo <- sk_p_tilde - z * sqrt(sk_p_tilde * (1 - sk_p_tilde) / sk_n_tilde)
sk_ac_hi <- sk_p_tilde + z * sqrt(sk_p_tilde * (1 - sk_p_tilde) / sk_n_tilde)

cat("Agresti-Coull: [", round(sk_ac_lo, 4), ",", round(sk_ac_hi, 4), "]\n")


