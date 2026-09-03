#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(11)

#######################
### NBA FREE THROWS ###
#######################

# load data
nba_players = read_delim("../data/11_nba-free-throws.csv.gz", delim = ";")

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

# Use bootstrapping to calculate the confidence intervals (manually without a package)
# Number of bootstrap samples
b = 1000
# Initialize a data frame to store the bootstrap results
nba_players_bootstrap = nba_players_ft %>%
  select(Player, FT_total, FTA_total, FT_percent) %>%
  mutate(
    wald_lower_bound = NA,
    wald_upper_bound = NA,
    ac_lower_bound = NA,
    ac_upper_bound = NA
  )

for (i in 1:nrow(nba_players_bootstrap)) {
  
  fta_total <- nba_players_bootstrap$FTA_total[i]   # fixed attempts for this player
  p_hat     <- nba_players_bootstrap$FT_percent[i]  # observed FT %
  
  # containers for bootstrap limits
  boot_wald_low <- boot_wald_up <- numeric(b)
  boot_ac_low   <- boot_ac_up   <- numeric(b)
  
  for (j in 1:b) {
    
    # parametric bootstrap: resample the player's attempts
    ft_total  <- rbinom(1, size = fta_total, prob = p_hat)
    p_star    <- ft_total / fta_total
    
    # Wald limits for this replicate
    se_w                <- sqrt(p_star * (1 - p_star) / fta_total)
    boot_wald_low[j]    <- p_star - z * se_w
    boot_wald_up[j]     <- p_star + z * se_w
    
    # Agresti-Coull limits for this replicate
    p_ac                <- (ft_total + 2) / (fta_total + 4)
    se_ac               <- sqrt(p_ac * (1 - p_ac) / (fta_total + 4))
    boot_ac_low[j]      <- p_ac - z * se_ac
    boot_ac_up[j]       <- p_ac + z * se_ac
  }
  
  # 95 % percentile bootstrap CI for each limit
  nba_players_bootstrap$wald_lower_bound[i] <- quantile(boot_wald_low, 0.025, na.rm = TRUE)
  nba_players_bootstrap$wald_upper_bound[i] <- quantile(boot_wald_up,  0.975, na.rm = TRUE)
  nba_players_bootstrap$ac_lower_bound[i]   <- quantile(boot_ac_low,   0.025, na.rm = TRUE)
  nba_players_bootstrap$ac_upper_bound[i]   <- quantile(boot_ac_up,    0.975, na.rm = TRUE)
}

nba_players_bootstrap
nba_players_CI


# Define a grid of alpha and beta values 
alpha_values <- seq(0.01, 0.99, 0.01)
beta_values  <- seq(0.01, 0.99, 0.01)

prior_grid <- expand_grid(alpha = alpha_values,
                          beta  = beta_values)

# 2. Helper: compute a 95 % CI for one (α,β)
cred_int_95 <- function(df, a, b) {
  df %>% mutate(
    credible_lower = qbeta(0.025, a + FT_total, b + FTA_total - FT_total),
    credible_upper = qbeta(0.975, a + FT_total, b + FTA_total - FT_total),
    credible_width = credible_upper - credible_lower,
    prior_alpha    = a,
    prior_beta     = b
  )
}

# 3. Apply over the whole grid
credible_intervals_df <- prior_grid %>%
  mutate(res = map2(alpha, beta,
                    ~ cred_int_95(nba_players_ft, .x, .y))) %>%
  unnest(res)


credible_intervals_df

# Calculate the widths for the intervals in nba_players_CI and nba_players_bootstrap
nba_players_CI <- nba_players_CI %>%
  mutate(
    wald_width = wald_upper_bound - wald_lower_bound,
    ac_width   = ac_upper_bound - ac_lower_bound
  )
nba_players_bootstrap <- nba_players_bootstrap %>%
  mutate(
    wald_width = wald_upper_bound - wald_lower_bound,
    ac_width   = ac_upper_bound - ac_lower_bound
  )

nba_players_CI
nba_players_bootstrap


# Take the first 10 players from boostrap and ci to plot
nba_players_bootstrap_plot = nba_players_bootstrap %>%
  # Manually pick the first 10
  slice(1:10)

nba_players_CI_plot = nba_players_CI %>%
  # Manually pick the first 10
  slice(1:10)

# Manually pick the first 10 from credible_intervals_df
credible_intervals_plot = credible_intervals_df %>%
  slice(1:10)

nba_players_bootstrap_plot
nba_players_CI_plot
credible_intervals_plot

# Summarise credible widths for the top 10 players
cred_width_grid <- credible_intervals_plot %>% 
  group_by(prior_alpha, prior_beta, Player) %>% 
  summarise(mean_width = mean(credible_width), .groups = "drop")

cred_width_grid

# Plot widths from cred_width_grid, nba_players_bootstrap_plot, nba_players_CI_plot as a histogram
# ---------- 1  Collect widths ----------
widths_df <- bind_rows(
  cred_width_grid %>%                      # Bayesian grid (mean width)
    transmute(width = mean_width,
              method = "Bayesian-grid"),
  
  nba_players_bootstrap_plot %>%           # Bootstrap limits
    select(wald_width, ac_width) %>%
    pivot_longer(everything(), names_to = "type",
                 values_to = "width") %>%
    mutate(method = paste0("Bootstrap-", type)) %>%
    select(width, method),
  
  nba_players_CI_plot %>%                  # Closed-form limits
    select(wald_width, ac_width) %>%
    pivot_longer(everything(), names_to = "type",
                 values_to = "width") %>%
    mutate(method = paste0("Closed-form-", type)) %>%
    select(width, method)
)

ggplot(widths_df, aes(x = width,
                      y = reorder(method, width, median),
                      fill = method)) +
  geom_boxplot(alpha = 0.6, scale = 1.2, colour = NA) +
  labs(title = "Width distributions for each interval procedure",
       x = "Interval width",
       y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")



