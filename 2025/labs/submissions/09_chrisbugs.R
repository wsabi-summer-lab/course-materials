#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(8)

# Number of reds: 30
# Number of purples: 19
# Number of yellows: 16
# Number of greens: 22
# Number of oranges: 25
# Total: 112

# Make a dataset with a row for every red and a row for every non-red
skittles = tibble(
  color = c(rep("red", 30), rep("notRed", 82)),
  count = c(rep(1, 30), rep(1, 82))
)

skittles

# 95% wald confidence interval for the proportion of red skittles
wald_ci = skittles %>%
  group_by(color) %>%
  summarise(
    count = sum(count),
  ) %>%
  mutate(
    proportion = count / nrow(skittles),
    z = qnorm(0.975), # 95% CI
    lower_bound = proportion - z * sqrt((proportion * (1 - proportion)) / n),
    upper_bound = proportion + z * sqrt((proportion * (1 - proportion)) / n)
  )

wald_ci

# 95% agresti-couli confidence interval for the proportion of red skittles
ac_ci = skittles %>%
  group_by(color) %>%
  summarise(
    count = sum(count),
  ) %>%
  mutate(
    proportion = count / nrow(skittles),
    z = qnorm(0.975), # 95% CI
    ac_proportion = (count + 2) / (nrow(skittles) + 4),
    lower_bound = ac_proportion - z * sqrt((ac_proportion * (1 - ac_proportion)) / (nrow(skittles) + 4)),
    upper_bound = ac_proportion + z * sqrt((ac_proportion * (1 - ac_proportion)) / (nrow(skittles) + 4))
  )

ac_ci


# Plot both confidence intervals
ggplot() +
  geom_errorbar(data = wald_ci, aes(x = color, ymin = lower_bound, ymax = upper_bound), width = 0.2, color = "blue") +
  geom_errorbar(data = ac_ci, aes(x = color, ymin = lower_bound, ymax = upper_bound), width = 0.2, color = "red") +
  labs(title = "Confidence Intervals for Proportion of Red Skittles",
       x = "Color",
       y = "Proportion") +
  theme_minimal()

# Use bootstrapping to construct 95% confidence intervals for the proportion of red skittles

# Function to calculate the proportion of red skittles
proportion_red <- function(data, indices) {
  d <- data[indices, ]  # allows boot to select a sample
  sum(d$color == "red") / nrow(d)
}
proportion_red_ac <- function(data, indices) {
  d <- data[indices, ]  # allows boot to select a sample
  (sum(d$color == "red") + 2) / (nrow(d) + 4)
}

# Do the bootstrapping manually without using a package

# Number of bootstrap samples
b = 250

# Initialize a vector to store the bootstrap results
bootstrap_results_wald = numeric(b)
bootstrap_results_ac = numeric(b)


for (i in 1:b) {
  # Sample with replacement
  sample_indices = sample(1:nrow(skittles), nrow(skittles), replace = TRUE)
  
  # Calculate the proportion of red skittles for the sample
  bootstrap_results_wald[i] = proportion_red(skittles, sample_indices)
  bootstrap_results_ac[i] = proportion_red_ac(skittles, sample_indices)
}

# Calculate the wald 95% confidence interval from the bootstrap results
wald_lower_bound = quantile(bootstrap_results_wald, 0.025)
wald_upper_bound = quantile(bootstrap_results_wald, 0.975)

# Calculate the agresti-couli 95% confidence interval from the bootstrap results
ac_lower_bound = quantile(bootstrap_results_ac, 0.025)
ac_upper_bound = quantile(bootstrap_results_ac, 0.975)
  

# Plot the bootstrap confidence intervals
ggplot() +
  geom_errorbar(aes(x = "blue", ymin = wald_lower_bound, ymax = wald_upper_bound), width = 0.2, color = "blue") +
  geom_errorbar(aes(x = "red", ymin = ac_lower_bound, ymax = ac_upper_bound), width = 0.2, color = "red") +
  labs(title = "Bootstrap Confidence Intervals for Proportion of Red Skittles",
       x = "Color",
       y = "Proportion") +
  theme_minimal()

# AC was slightly narrower and shifted up a touch. This means the relative coverage of 
# the Wald CI is lower than the AC CI, which makes sense since the Wald CI is based on a normal approximation


#######################
### NBA FREE THROWS ###
#######################

# load data
nba_players = read_delim("../data/09_nba-free-throws.csv.gz", delim = ";")
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


# Take the first 10 players from boostrap and ci to plot
nba_players_bootstrap_plot = nba_players_bootstrap %>%
  # Manually pick the first 10
  slice(1:10)

nba_players_CI_plot = nba_players_CI %>%
  # Manually pick the first 10
  slice(1:10)

# Display the results
nba_players_bootstrap_plot
nba_players_CI_plot




# Plot FT_percent vs player name and overlay the confidence intervals
ggplot(nba_players_CI_plot, aes(x = reorder(Player, FT_percent), y = FT_percent)) +
  geom_point() +
  geom_errorbar(aes(ymin = wald_lower_bound, ymax = wald_upper_bound), width = 0.2, color = "blue") +
  geom_errorbar(aes(ymin = ac_lower_bound, ymax = ac_upper_bound), width = 0.2, color = "red") +
  geom_errorbar(data = nba_players_bootstrap_plot, aes(ymin = wald_lower_bound, ymax = wald_upper_bound), width = 0.2, color = "blue", linetype = "dashed") +
  geom_errorbar(data = nba_players_bootstrap_plot, aes(ymin = ac_lower_bound, ymax = ac_upper_bound), width = 0.2, color = "red", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "NBA Players Free Throw Percentages with Confidence Intervals",
    x = "FT %",
    y = "Player"
  ) +
  theme_minimal() 




############################################################################
# BOOTSTRAP BLOCK — put this immediately after you define `ac_CI()`        #
############################################################################
B <- 100                           # bootstrap replicates

boot_CI <- function(x, n, conf = .95, B = 100) {
  p_hat <- x / n
  # percentile bootstrap using rbinom (faster than resampling 0/1 vector)
  boot_p <- rbinom(B, n, p_hat) / n
  alpha  <- (1 - conf) / 2
  bounds <- quantile(boot_p, c(alpha, 1 - alpha))
  data.frame(lower_bound = bounds[1], upper_bound = bounds[2])
}

boot_hits <- matrix(0L, nrow = length(p_vec), ncol = length(n_vec))

for (j in seq_along(n_vec)) {
  n <- n_vec[j]
  for (i in seq_along(p_vec)) {
    p <- p_vec[i]
    for (sim in seq_len(M)) {
      x      <- rbinom(1, n, p)
      boot   <- boot_CI(x, n, B = B)
      
      boot_hits[i, j] <- boot_hits[i, j] +
        (boot$lower_bound <= p && p <= boot$upper_bound)
    }
  }
}

boot_cov <- boot_hits / M                # empirical coverage
############################################################################
# END BOOTSTRAP BLOCK                                                      #
############################################################################

## --- 1.  Assemble long data-frame (add Bootstrap) ------------------------
coverage_df <- data.frame(
  p       = rep(p_vec,  times = length(n_vec)),
  n       = rep(n_vec,  each  = length(p_vec)),
  Wald    = as.vector(wald_cov),
  Agresti = as.vector(ac_cov),
  Bootstrap = as.vector(boot_cov)
) %>%
  pivot_longer(cols      = c(Wald, Agresti, Bootstrap),
               names_to  = "Method",
               values_to = "Coverage")

## --- 2.  Heat-map ---------------------------------------------------------
ggplot(coverage_df,
       aes(x = factor(n), y = p, fill = Coverage)) +
  geom_tile() +
  facet_wrap(~ Method) +
  scale_fill_viridis_c(limits = c(0, 1), name = "Coverage") +
  labs(
    title = "Empirical Coverage of 95 % CIs: Wald vs Agresti-Coull vs Bootstrap",
    x     = "Sample size n",
    y     = "True probability p"
  ) +
  theme_minimal()

## --- 3.  Coverage vs p, faceted by n -------------------------------------
ggplot(coverage_df,
       aes(x = p, y = Coverage, colour = Method)) +
  geom_line() +
  facet_wrap(~ n, ncol = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title    = "Coverage vs True p (M = 100)",
    subtitle = "Percentile Bootstrap added",
    x = "True probability p",
    y = "Coverage probability"
  ) +
  theme_minimal()
