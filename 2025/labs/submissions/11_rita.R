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
nba = read.csv('../data/08_nba-free-throws.csv.gz', sep = ';')
head(nba)


nba$FTM <- nba$FT. * nba$FTA 
nba$tot_FTA <- nba$FTA * nba$G
nba$tot_FTM <- nba$FTM * nba$G

nba_cleaned <- nba %>% 
  group_by(Player) %>%
  summarise(
    FTA = sum(FTA, na.rm = TRUE),
    tot_FTA = sum(tot_FTA, na.rm = TRUE),
    tot_FTM = sum(tot_FTM, na.rm = TRUE),
    FT_percent = sum(FT., na.rm = TRUE) / n(),  # or compute FT% from FTM/FTA if you have that
    .groups = 'drop'
  ) %>%
  filter(tot_FTA >= 25) %>% 
  select(Player, FTA, tot_FTA, FT_percent, tot_FTM)

head(nba_cleaned)

nba_summary <- nba_cleaned %>%
  mutate(
    wald_se = sqrt((FT_percent * (1 - FT_percent)) / FTA),
    wald_lower = FT_percent - 1.96 * wald_se,
    wald_upper = FT_percent + 1.96 * wald_se
  )

nba_summary <- nba_summary %>%
  mutate(
    n_tilde = tot_FTA + 4,
    p_tilde = (tot_FTM + 2) / n_tilde,
    ac_se = sqrt((p_tilde * (1 - p_tilde)) / n_tilde),
    ac_lower = p_tilde - 1.96 * ac_se,
    ac_upper = p_tilde + 1.96 * ac_se
  )

nba_summary <- nba_summary %>%
  slice_sample( n = 25)


ggplot(nba_summary, aes(x = reorder(Player, -FT_percent), y = FT_percent)) +
  geom_bar(stat = "identity", fill = "orchid", color = "black") +
  geom_errorbar(aes(ymin = wald_lower, ymax = wald_upper), width = 0.2, color = "darkmagenta", alpha = 0.7) +
  geom_errorbar(aes(ymin = ac_lower, ymax = ac_upper), width = 0.2, color = "steelblue", linetype = "dashed", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Free Throw % with Wald and Agresti-Coull Intervals (≥ 25 Attempts)",
    x = "Player",
    y = "Free Throw Percentage"
  ) +
  theme_minimal()

bootstrap_ci <- function(makes, total, reps = 1000) {
  boot_props <- replicate(reps, {
    sample <- sample(c(1, 0), size = total, replace = TRUE, prob = c(makes / total, 1 - makes / total))
    mean(sample)
  })
  quantile(boot_props, probs = c(0.025, 0.975))
}

# Apply per player
boot_intervals <- nba_cleaned %>%
  mutate(
    boot_ci = map2(tot_FTM, tot_FTA, ~bootstrap_ci(.x, .y, n_boot)),
    boot_lower = map_dbl(boot_ci, 1),
    boot_upper = map_dbl(boot_ci, 2)
  ) %>%
  select(-boot_ci)

nba_summary <- nba_summary %>%
  left_join(boot_intervals, by = "Player")

n_boot = 1000

interval = seq(0, 1, length.out = 100)

# 2 - for each n and p generate free throws 
n_values = c(10, 100, 500)
reps <- 1:100
sim_grid <- expand.grid(p = interval, n = n_values, rep = reps)
sim_grid$made_fts <- mapply(function(p, n) rbinom(1, n, p), sim_grid$p, sim_grid$n)
sim_grid$FT_percent <- sim_grid$made_fts / sim_grid$n

bootstrap_ci <- function(makes, total, n_boot = 1000) {
  if (total == 0) return(c(NA, NA))  # skip rows with no attempts
  
  prob <- makes / total
  
  boot_props <- replicate(n_boot, {
    sample_attempts <- sample(c(1, 0), size = total, replace = TRUE, prob = c(prob, 1 - prob))
    mean(sample_attempts)
  })
  
  quantile(boot_props, probs = c(0.025, 0.975))}


#BETA PRIOR

nba_cleaned <- nba %>% 
  group_by(Player) %>%
  summarise(
    tot_FTA    = sum(FTA * G,      na.rm = TRUE),
    tot_FTM    = sum(FT. * FTA * G, na.rm = TRUE),
    FT_percent = tot_FTM / tot_FTA,
    .groups    = "drop"
  ) %>%
  filter(tot_FTA >= 25)

# Expand over (a,b) grid
a <- seq(10, 100, length.out = 20)
b <- seq(10, 100, length.out = 20)
beta_grid <- expand.grid(a = a, b = b)

posterior_df <- crossing(nba_cleaned, beta_grid) %>%
  mutate(
    posterior_lower = qbeta(0.025, tot_FTM + a, tot_FTA - tot_FTM + b),
    posterior_upper = qbeta(0.975, tot_FTM + a, tot_FTA - tot_FTM + b),
    width           = posterior_upper - posterior_lower
  )

names(posterior_df)

# Plot just the beta CIs
ggplot(posterior_df, aes(x = width, y = FT_percent)) +
  geom_point(aes(color = as.factor(a), shape = as.factor(b)), alpha = 0.6) +
  labs(
    title =    "Posterior Interval Width vs. Observed Free-Throw %",
    x =        "Posterior 95% CI Width",
    y =        "Observed Free-Throw %",
    color =    "Beta(a,b)"
  ) 

nba_summary <- nba_summary %>%
  rename(FT_percent = FT_percent.x) %>%
  select(-FT_percent.y)

posterior_beta22 <- posterior_df %>%
  filter(a == 2, b == 2) %>%
  select(Player, posterior_lower, posterior_upper)

nba_summary <- nba_summary %>%
  left_join(posterior_beta22, by = "Player") %>%
  rename(
    cred_lower = posterior_lower,
    cred_upper = posterior_upper
  )

ggplot(nba_summary, aes(x = reorder(Player, -FT_percent), y = FT_percent)) +
  geom_col(fill = "orchid", color = "black") +
  
  # Wald
  geom_errorbar(aes(ymin = wald_lower, ymax = wald_upper),
                width = 0.2, color = "maroon", alpha = 0.7) +
  
  # Agresti–Coull
  geom_errorbar(aes(ymin = ac_lower, ymax = ac_upper),
                width = 0.2, color = "steelblue",
                linetype = "dashed", alpha = 0.7) +
  
  # Bootstrap
  geom_errorbar(aes(ymin = boot_lower, ymax = boot_upper),
                width = 0.2, color = "green",
                linetype = "dotdash", alpha = 0.7) +
  
  # Beta(2,2) credible
  geom_errorbar(aes(ymin = cred_lower, ymax = cred_upper),
                width = 0.2, color = "firebrick",
                linetype = "longdash", alpha = 0.8) +
  
  coord_flip() +
  labs(
    title   = "Free-Throw % with Four Interval Types (≥25 Attempts)",
    x       = "Player",
    y       = "Free-Throw Percentage",
    caption = "Intervals: Wald (solid), Agresti–Coull (dashed), Bootstrap (dotdash), Beta(2,2) credible (longdash)"
  ) +
  theme_minimal()