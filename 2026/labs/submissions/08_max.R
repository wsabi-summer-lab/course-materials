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

nba_players <- nba_players %>%
  mutate(
    FT_total = FT * G,
    FTA_total = FTA * G,
    FT_percent = FT_total / FTA_total
  ) %>%
    filter(FTA_total >=25)

nba_players <- nba_players %>%
  group_by(Player) %>%
  filter(n() == 1 | Tm == "TOT") %>%
  ungroup()

nba_players <- nba_players %>%
  mutate(
    Wald_lower = FT_percent - 1.96 * sqrt(FT_percent * (1-FT_percent)/FTA_total),
    Wald_upper = FT_percent + 1.96 * sqrt(FT_percent * (1-FT_percent)/FTA_total),
    n_squiggle = FTA_total + 1.96^2,
    p_squiggle = (FT_total + 1.96^2/2) / (FTA_total + 1.96^2)
  ) %>%
  mutate(
    AC_upper = p_squiggle + 1.96 * sqrt(p_squiggle * (1-p_squiggle)/n_squiggle),
    AC_lower = p_squiggle - 1.96 * sqrt(p_squiggle * (1-p_squiggle)/n_squiggle)
  )

plot_df <- nba_players %>% slice_max(FTA_total, n = 20) 

ggplot(plot_df, aes(y = reorder(Player, FT_percent))) +
  geom_errorbarh(aes(xmin = Wald_lower, xmax = Wald_upper, color = "Wald"),
                 height = 0.25, position = position_nudge(y =  0.15)) +
  geom_errorbarh(aes(xmin = AC_lower, xmax = AC_upper, color = "Agresti-Coull"),
                 height = 0.25, position = position_nudge(y = -0.15)) +
  geom_point(aes(x = FT_percent)) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "FT%", y = NULL, color = "Interval")
# Error bars are narrower at higher FT% for both categories, but both intervals seem very similar
#AC appears to pull most bars to the left

###########################
### PART 2: LLN WARM-UP ###
###########################

lln_p = 0.75
lln_n = 1000
lln_paths = 100

lln_paths_df <- map_dfr(1:lln_paths, function(path) {
  flips <- rbinom(lln_n, size = 1, prob = lln_p)
  tibble(
    path = path,
    n = 1:lln_n,
    phat = cumsum(flips) / (1:lln_n)
  )
})

ggplot(lln_paths_df, aes(x = n, y = phat, group = path)) +
  geom_line(alpha = 0.2) +
  geom_hline(yintercept = lln_p, color = "red", linetype = "dashed") +
  labs(x = "Number of attempts (n)", y = "Running estimate of p")

# Variability decreases a lot as n increases
################################
### PART 3: SIMULATION STUDY ###
################################

z_975 = qnorm(0.975)
p_grid = seq(0, 1, length.out = 1000)
sample_sizes = c(10, 50, 100, 250, 500, 1000)
M = 100

sim <- expand_grid(p = p_grid, n = sample_sizes, rep = 1:M) %>%
  mutate(S = rbinom(n(), size = n, prob = p))

# both intervals for every simulated experiment, then a coverage flag for each
sim <- sim %>%
  mutate(
    phat = S / n,
    Wald_lower = phat - z_975 * sqrt(phat * (1 - phat) / n),
    Wald_upper = phat + z_975 * sqrt(phat * (1 - phat) / n),
    n_tilde = n + z_975^2,
    p_tilde = (S + z_975^2 / 2) / n_tilde,
    AC_lower = p_tilde - z_975 * sqrt(p_tilde * (1 - p_tilde) / n_tilde),
    AC_upper = p_tilde + z_975 * sqrt(p_tilde * (1 - p_tilde) / n_tilde),
    Wald_covered = Wald_lower <= p & p <= Wald_upper,
    AC_covered = AC_lower <= p & p <= AC_upper
  )

# coverage = fraction of the M replications whose interval contains the true p
coverage <- sim %>%
  group_by(p, n) %>%
  summarize(
    Wald = mean(Wald_covered),
    `Agresti-Coull` = mean(AC_covered),
    .groups = "drop"
  ) %>%
  pivot_longer(c(Wald, `Agresti-Coull`),
               names_to = "method", values_to = "coverage")

ggplot(coverage, aes(x = p, y = coverage, color = method)) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  facet_wrap(~ n, labeller = label_both) +
  labs(x = "True probability p", y = "Estimated coverage", color = "Method")

############################
### PART 4: SKITTLES DEMO ###
############################

# Replace these with your observed counts from the Skittles activity
skittles_n = 76
skittles_r = 23

observed = skittles_r/skittles_n

Wald_lower_skittle = observed - z_975 * sqrt(observed * (1 - observed) / skittles_n)
Wald_upper_skittle = observed + z_975 * sqrt(observed * (1 - observed) / skittles_n)
n_tilde_skittle = skittles_n + z_975^2
p_tilde_skittle = (skittles_r + z_975^2 / 2) / n_tilde_skittle
AC_lower_skittle = p_tilde_skittle - z_975 * sqrt(p_tilde_skittle * (1 - p_tilde_skittle) / n_tilde_skittle)
AC_upper_skittle = p_tilde_skittle + z_975 * sqrt(p_tilde_skittle * (1 - p_tilde_skittle) / n_tilde_skittle)

#observed rate: 0.30
#Wald: [0.20, 0.41]
#AC:[0.21, 0.41]
# Both intervals are similar, but Agresti-Coull would work better for extreme proportions