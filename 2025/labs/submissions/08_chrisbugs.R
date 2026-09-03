#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(broom)

# set seed
set.seed(8)

#######################
### NBA FREE THROWS ###
#######################

# load data
nba_players = read.csv("../data/08_nba-free-throws.csv.gz", sep = ";")

nba_players

# Get FT_total, FTA_total, FT_percent 
nba_players_ft = nba_players %>%
  group_by(Player) %>%
  mutate(
    FT_total = round(FT * G),
    FTA_total = round(FTA * G),
  ) %>% 
  summarise(
    FT_total = sum(FT_total, na.rm = TRUE),
    FTA_total = sum(FTA_total, na.rm = TRUE),
    FT_percent = FT_total / FTA_total
  ) %>%
  select(Player, FT_total, FTA_total, FT_percent) 

# Filter out players who have less than 25 free throws
nba_players_ft = nba_players_ft %>%
  filter(FT_total >= 25)

nba_players_ft

z = 1.96

nba_players_ft = nba_players_ft %>%
  mutate(
    ac_FT_percent = (FT_total + 2) / (FTA_total + 4)
  )

# Calculate the confidence interval for each player
nba_players_CI = nba_players_ft %>%
  mutate(
    wald_lower_bound = FT_percent - z * sqrt((FT_percent * (1 - FT_percent)) / FTA_total),
    wald_upper_bound = FT_percent + z * sqrt((FT_percent * (1 - FT_percent)) / FTA_total),
    ac_lower_bound = ac_FT_percent - z * sqrt((ac_FT_percent * (1 - ac_FT_percent)) / (FTA_total + 4)),
    ac_upper_bound = ac_FT_percent + z * sqrt((ac_FT_percent * (1 - ac_FT_percent)) / (FTA_total + 4))
  )

nba_players_CI

# Use slice_sample to only plot a smaller percentage of players
nba_players_CI_plot = nba_players_CI %>%
  slice_sample(prop = 0.1)

# Plot FT_percent vs player name and overlay the confidence intervals
ggplot(nba_players_CI_plot, aes(x = reorder(Player, FT_percent), y = FT_percent)) +
  geom_point() +
  geom_errorbar(aes(ymin = wald_lower_bound, ymax = wald_upper_bound), width = 0.2, color = "blue") +
  geom_errorbar(aes(ymin = ac_lower_bound, ymax = ac_upper_bound), width = 0.2, color = "red") +
  coord_flip() +
  labs(
    title = "NBA Players Free Throw Percentages with Confidence Intervals",
    x = "FT %",
    y = "Player"
  ) +
  theme_minimal() 

# The Wald CI's are larger on the upper bound than the AC's



##### THIS IS THE SIMULATION

# Create an interval from 0 to 1 with 1000 equally spaced points
x = seq(0, 1, length.out = 1000)

p_vec = x

n_vec <- c(10, 50, 100, 250, 500, 1000) 

M <- 100

# Create a function to calculate the Wald CI
wald_CI <- function(p, n, z = 1.96) {
  lower_bound <- p - z * sqrt((p * (1 - p)) / n)
  upper_bound <- p + z * sqrt((p * (1 - p)) / n)
  return(data.frame(lower_bound, upper_bound))
}

# Create a function to calculate the AC CI
ac_CI <- function(p, n, z = 1.96) {
  p_ac <- (p * n + 2) / (n + 4)
  lower_bound <- p_ac - z * sqrt((p_ac * (1 - p_ac)) / (n + 4))
  upper_bound <- p_ac + z * sqrt((p_ac * (1 - p_ac)) / (n + 4))
  return(data.frame(lower_bound, upper_bound))
}

wald_hits <- matrix(0L, nrow = length(p_vec), ncol = length(n_vec))
ac_hits   <- matrix(0L, nrow = length(p_vec), ncol = length(n_vec))

for (j in seq_along (n_vec)) {
  n <- n_vec[j]
  
  for (i in seq_along(p_vec)) {
    p <- p_vec[i]
    
    for (sim in seq_len(M)) {
      # Simulate a binomial distribution
      x <- rbinom(1, n, p)
      
      # Calculate the proportion
      p_sim <- x / n
      
      # Calculate the Wald CI
      wald <- wald_CI(p_sim, n)
      
      # Calculate the AC CI
      ac <- ac_CI(p_sim, n)
      
      # accumulate coverage counts
      wald_hits[i, j] <- wald_hits[i, j] +
        (wald$lower <= p && p <= wald$upper)
      ac_hits[i, j]   <- ac_hits[i, j] +
        (ac$lower   <= p && p <= ac$upper)
    }
  }
}

# Convert to proportions
wald_cov <- wald_hits / M
ac_cov   <- ac_hits / M

## 1.  Assemble a long data-frame --------------------------------------------
coverage_df <- data.frame(
  p       = rep(p_vec,  times = length(n_vec)),
  n       = rep(n_vec,  each  = length(p_vec)),
  Wald    = as.vector(wald_cov),
  Agresti = as.vector(ac_cov)
) %>%
  pivot_longer(cols      = c(Wald, Agresti),
               names_to  = "Method",
               values_to = "Coverage")




## 2.  STEP 5 – heat-map of coverage for every (p, n) -------------------------
ggplot(coverage_df,
       aes(x = factor(n), y = p, fill = Coverage)) +
  geom_tile() +
  facet_wrap(~ Method) +
  scale_fill_viridis_c(limits = c(0, 1), name = "Coverage") +
  labs(
    title = "Empirical Coverage of 95 % Wald vs Agresti-Coull CIs",
    x     = "Sample size n",
    y     = "True probability p"
  ) +
  theme_minimal()

## 3.  STEP 6 – coverage vs p, faceted by n -----------------------------------
ggplot(coverage_df,
       aes(x = p, y = Coverage, colour = Method)) +
  geom_line() +
  facet_wrap(~ n, ncol = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Coverage vs True p (M = 100)",
    subtitle = "Separate panels for each sample size n",
    x = "True probability p",
    y = "Coverage probability"
  ) +
  theme_minimal()



# Probability stuff - problem 2
p_be = 110/210
p_be

p_seq = seq(p_be, 1, length.out = 100)

# Calculate the minimum number of bets required to prove profitability
n_req = ceiling((1.96^2 * p_seq * (1 - p_seq)) / (p_seq - p_be)^2)

# Plot
data.frame(p = p_seq, n = n_req) |>
  ggplot(aes(x = p, y = n)) +
  geom_line() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Bets required to prove profitability at –110 odds",
    subtitle = "95 % Wald lower-bound must exceed break-even rate 52.38 %",
    x = "True win probability p",
    y = "Minimum number of bets n"
  ) +
  theme_minimal()
