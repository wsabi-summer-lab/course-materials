#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(9)

##Part 1
x<-c()
for (i in 1:112) {
  x[i] = ifelse(i<18,1,0)
}
x

p=c()
for (i in 1:1000000) {
  sample_x = sample(x, 117, replace = TRUE)
  p[i] = mean(sample_x)
}
# plot the bootstrap distribution
ggplot(data.frame(p), aes(x=p)) +
  geom_histogram(binwidth=1/112, fill="blue", color="black") +
  labs(title="Bootstrap Distribution of Proportion",
       x="Proportion",
       y="Count") +
  theme_minimal()
  
# calculate the mean and standard deviation of the bootstrap distribution
mean_p = mean(p)
sd_p = sd(p)
cat("Mean of bootstrap distribution:", mean_p, "\n")
cat("Standard deviation of bootstrap distribution:", sd_p, "\n")


BSL=quantile(p,.025)
BSH=quantile(p,.975)
BSL
BSH
#calculate wald CI
Sn=17
n=112
prob=Sn/n
# calculate the Wald confidence interval
wald_L = prob - 1.96 * sqrt((prob * (1 - prob)) / n)
wald_H = prob + 1.96 * sqrt((prob * (1 - prob)) / n)
wald_L
wald_H
# calculate AC confidnce interval
ACprob = (Sn + 2) / (n + 4)
AC_L = ACprob - 1.96 * sqrt((ACprob * (1 - ACprob)) / n)
AC_H = ACprob + 1.96 * sqrt((ACprob * (1 - ACprob)) / n)
AC_L
AC_H

#make a bar plot with estimated probability and confidence intervals
ggplot()+
  geom_bar(aes(x=c("Bootstrap", "Wald CI", "AC CI"), 
               y=c(mean(p), prob, ACprob)), 
           stat="identity", fill="skyblue") +
  geom_errorbar(aes(x=c("Bootstrap", "Wald CI", "AC CI"), 
                    ymin=c(BSL, wald_L, AC_L), 
                    ymax=c(BSH, wald_H, AC_H)), 
                width=0.2, color="steelblue") +
  labs(title="Estimated Probability and Confidence Intervals",
       x="Method",
       y="Probability") +
  ylim(0, 1) +
  theme_minimal()
#######################
### NBA FREE THROWS ###
#######################

# Load and clean data
nba_players <- read_delim("../data/09_nba-free-throws.csv.gz", delim = ";")

nba_players <- nba_players %>%
  mutate(
    FTA = round(FTA * G, 0),
    FT = round(FT * G, 0)
  ) %>%
  select(Player, FTA, FT, G) %>%
  group_by(Player) %>%
  summarise(
    FTA = sum(FTA),
    FT = sum(FT),
    FTP = FT / FTA,
    G = sum(G),
    .groups = "drop"
  ) %>%
  filter(FTA > 25) %>%
  slice(1:20)  # First 20 players

# Initialize result storage
bootstrap_results <- list()
n_samples <- 200

# Loop over players
for (i in 1:nrow(nba_players)) {
  player_name <- nba_players$Player[i]
  ft_made <- nba_players$FT[i]
  ft_att <- nba_players$FTA[i]
  
  # Bootstrap
  shots <- c(rep(1, ft_made), rep(0, ft_att - ft_made))
  p_boot <- numeric(n_samples)
  for (j in 1:n_samples) {
    sample_shots <- sample(shots, length(shots), replace = TRUE)
    p_boot[j] <- mean(sample_shots)
  }
  
  # Wald CI
  p_hat <- ft_made / ft_att
  se_wald <- sqrt(p_hat * (1 - p_hat) / ft_att)
  wald_low <- p_hat - 1.96 * se_wald
  wald_high <- p_hat + 1.96 * se_wald
  
  # AC CI
  ac_p_hat <- (ft_made + 2) / (ft_att + 4)
  se_ac <- sqrt(ac_p_hat * (1 - ac_p_hat) / (ft_att + 4))
  ac_low <- ac_p_hat - 1.96 * se_ac
  ac_high <- ac_p_hat + 1.96 * se_ac
  
  # Store results
  bootstrap_results[[player_name]] <- list(
    boot_mean = mean(p_boot),
    boot_sd = sd(p_boot),
    boot_ci_low = quantile(p_boot, 0.025),
    boot_ci_high = quantile(p_boot, 0.975),
    wald_low = wald_low,
    wald_high = wald_high,
    ac_low = ac_low,
    ac_high = ac_high
  )
}

# Add intervals to dataframe
nba_bootstrap_results <- nba_players %>%
  rowwise() %>%
  mutate(
    boot_mean = bootstrap_results[[Player]]$boot_mean,
    boot_sd = bootstrap_results[[Player]]$boot_sd,
    boot_ci_low = bootstrap_results[[Player]]$boot_ci_low,
    boot_ci_high = bootstrap_results[[Player]]$boot_ci_high,
    wald_low = bootstrap_results[[Player]]$wald_low,
    wald_high = bootstrap_results[[Player]]$wald_high,
    ac_low = bootstrap_results[[Player]]$ac_low,
    ac_high = bootstrap_results[[Player]]$ac_high
  )

glimpse(nba_bootstrap_results)

# Reorder players by FTP for cleaner x-axis
nba_bootstrap_results <- nba_bootstrap_results %>%
  mutate(Player = fct_reorder(Player, FTP))

# Plot all 3 CI types for comparison
ggplot(nba_bootstrap_results, aes(x = Player)) +
  geom_point(aes(y = FTP), color = "black", size = 2) +
  
  # Bootstrap CI
  geom_errorbar(aes(ymin = boot_ci_low, ymax = boot_ci_high), 
                color = "blue", width = 0.2) +
  
  # Wald CI
  geom_errorbar(aes(ymin = wald_low, ymax = wald_high), 
                color = "red", width = 0.15, linetype = "dashed") +
  
  # AC CI
  geom_errorbar(aes(ymin = ac_low, ymax = ac_high), 
                color = "green", width = 0.1, linetype = "dotted") +
  
  labs(title = "Free Throw % with 3 Confidence Intervals (First 20 Players)",
       y = "Free Throw Percentage",
       x = "Player",
       caption = "Blue: Bootstrap | Red: Wald | Green: AC") +
  theme_minimal(base_size = 12)
## END OF THE ASSIGNMENT
library(data.table)
n <- 100          # sample size per trial
n_trials <- 100  # trials per p
p_points <- seq(0, 1, length.out = 100)  # p values to test
boot_reps <- 200  # bootstrap replicates

results_list <- vector("list", length(p_points))

for (idx in seq_along(p_points)) {
  p <- p_points[idx]
  
  coverage_boot <- logical(n_trials)
  
  for (i in seq_len(n_trials)) {
    sample_data <- rbinom(n, size = 1, prob = p)
    n_sample <- length(sample_data)
    
    # Bootstrap CI
    boot_phat <- replicate(boot_reps, mean(sample(sample_data, n_sample, replace = TRUE)))
    boot_ci_low <- quantile(boot_phat, 0.025)
    boot_ci_high <- quantile(boot_phat, 0.975)
    
    # Check coverage
    coverage_boot[i] <- (p >= boot_ci_low) & (p <= boot_ci_high)
  }
  
  results_list[[idx]] <- data.table(
    p = p,
    Bootstrap = mean(coverage_boot)
  )
}

results <- rbindlist(results_list)

# Plot only bootstrap coverage
ggplot(results, aes(x = p, y = Bootstrap)) +
  geom_line(color = "olivedrab", size = 1.2) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  labs(
    title = "Bootstrap Coverage Probability vs True Proportion p",
    x = "True Proportion p",
    y = "Coverage Probability",
    caption = "Dashed line = nominal 95% coverage"
  ) +
  theme_minimal()
