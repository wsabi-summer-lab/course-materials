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
setwd("C:/Users/micha/OneDrive/Documents/GitHub/lab-materials/2026/labs/data")

# load data
nba_players <- read_delim("08_nba-free-throws.csv.gz", delim = ";", show_col_types = FALSE)

# Task 1: build player-level summary
ft <- nba_players %>%
  group_by(Player) %>%
  summarize(
    FT_total  = sum(FT  * G),
    FTA_total = sum(FTA * G),
    .groups = "drop"
  ) %>%
  mutate(FT_percent = FT_total / FTA_total)

# Task 2: filter to players with at least 25 free throw attempts
ft <- ft %>%
  filter(FTA_total >= 25)

# Task 3: compute CIs and plot
z <- qnorm(0.975)

ft <- ft %>%
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

p <- ggplot(ft %>% slice(1:30), aes(y = reorder(Player, FT_percent))) +
  geom_point(aes(x = FT_percent)) +
  geom_errorbar(aes(xmin = wald_lo, xmax = wald_hi, color = "Wald"),
                width = 0.3, orientation = "y") +
  geom_errorbar(aes(xmin = ac_lo, xmax = ac_hi, color = "Agresti-Coull"),
                width = 0.3, orientation = "y") +
  labs(x = "FT%", y = "Player", color = "Method") +
  theme_minimal()

print(p)

#I notice that the Agressti Coull intervals are slightly wider and shifted to the left compared to Wald.


###########################
### PART 2: LLN WARM-UP ###
###########################
lln_p = 0.75
lln_n = 1000
lln_paths = 100

# Task 1 & 2: one simulated sequence, running estimate, plot
x <- rbinom(lln_n, 1, lln_p)
p_hat <- cumsum(x) / seq_along(x)

plot_data <- tibble(n = 1:lln_n, p_hat = p_hat)

ggplot(plot_data, aes(x = n, y = p_hat)) +
  geom_line() +
  geom_hline(yintercept = lln_p, color = "red", linetype = "dashed") +
  labs(x = "n", y = expression(hat(p)[n])) +
  theme_minimal()

# Task 3: 100 paths overlaid
paths <- map_dfr(1:lln_paths, function(i) {
  x <- rbinom(lln_n, 1, lln_p)
  tibble(path = i, n = 1:lln_n, p_hat = cumsum(x) / seq_along(x))
})

ggplot(paths, aes(x = n, y = p_hat, group = path)) +
  geom_line(alpha = 0.2) +
  geom_hline(yintercept = lln_p, color = "red", linetype = "dashed") +
  labs(x = "n", y = expression(hat(p)[n])) +
  theme_minimal()
#Over the repeated simulations of 100 times the variability of n decreases but in an exponential decay way. 

################################
### PART 3: SIMULATION STUDY ###
################################
z_975 = qnorm(0.975)
p_grid = seq(0, 1, length.out = 1000)
sample_sizes = c(10, 50, 100, 250, 500, 1000)
M = 100

p_grid <- seq(0, 1, length.out = 1000)

results <- map_dfr(sample_sizes, function(n) {
  map_dfr(p_grid, function(p) {
    wald_cover <- numeric(M)
    ac_cover   <- numeric(M)
    
    for (m in 1:M) {
      s <- rbinom(1, n, p)
      p_hat <- s / n
      
      # Wald
      wald_lo <- p_hat - z_975 * sqrt(p_hat * (1 - p_hat) / n)
      wald_hi <- p_hat + z_975 * sqrt(p_hat * (1 - p_hat) / n)
      wald_cover[m] <- (p >= wald_lo & p <= wald_hi)
      
      # Agresti-Coull
      n_tilde <- n + z_975^2
      p_tilde <- (s + z_975^2 / 2) / n_tilde
      ac_lo   <- p_tilde - z_975 * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
      ac_hi   <- p_tilde + z_975 * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
      ac_cover[m] <- (p >= ac_lo & p <= ac_hi)
    }
    
    tibble(
      n        = n,
      p        = p,
      wald_cov = mean(wald_cover),
      ac_cov   = mean(ac_cover)
    )
  })
})

# pivot for plotting
results_long <- results %>%
  pivot_longer(cols = c(wald_cov, ac_cov),
               names_to  = "method",
               values_to = "coverage") %>%
  mutate(method = recode(method, wald_cov = "Wald", ac_cov = "Agresti-Coull"))

ggplot(results_long, aes(x = p, y = coverage, color = method)) +
  geom_line(alpha = 0.7) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  facet_wrap(~ n, labeller = label_both) +
  labs(x = "p", y = "Coverage Probability", color = "Method") +
  theme_minimal()

############################
### PART 4: SKITTLES DEMO ###
############################

skittles_n = 84
skittles_r = 16

p_hat_s <- skittles_r / skittles_n

# Wald
wald_lo_s <- p_hat_s - z_975 * sqrt(p_hat_s * (1 - p_hat_s) / skittles_n)
wald_hi_s <- p_hat_s + z_975 * sqrt(p_hat_s * (1 - p_hat_s) / skittles_n)

# Agresti-Coull
n_tilde_s <- skittles_n + z_975^2
p_tilde_s <- (skittles_r + z_975^2 / 2) / n_tilde_s
ac_lo_s   <- p_tilde_s - z_975 * sqrt(p_tilde_s * (1 - p_tilde_s) / n_tilde_s)
ac_hi_s   <- p_tilde_s + z_975 * sqrt(p_tilde_s * (1 - p_tilde_s) / n_tilde_s)

cat("Wald:          (", round(wald_lo_s, 3), ",", round(wald_hi_s, 3), ")\n")
cat("Agresti-Coull: (", round(ac_lo_s, 3),  ",", round(ac_hi_s, 3),  ")\n")
ff
#Agresti-Coull is better here since the value of 16/84 is close to zero so agresti coull is more likely to capture it. 
