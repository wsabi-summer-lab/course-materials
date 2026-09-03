#############
### SETUP ###
#############

rm(list=ls())

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(9)

# 28 red, 82 not red
skittles <- data.frame(
  color = c(rep("red", 28), rep("not_red", 82))
)
# calculate the true probability of getting a red skittle
p_red = sum(skittles$color == "red") / nrow(skittles)

# Construct a 95% Wald and Agresti-Coull confidence interval for the true probability of getting a red skittle
wald_lower_bound = p_red - qnorm(0.975) * sqrt((p_red * (1 - p_red)) / nrow(skittles))
wald_upper_bound = p_red + qnorm(0.975) * sqrt((p_red * (1 - p_red)) / nrow(skittles))
# print wald confidence interval and its length
wald_ci = c(wald_lower_bound, wald_upper_bound)
wald_length = wald_upper_bound - wald_lower_bound
cat("Wald Confidence Interval: [", wald_lower_bound, ",", wald_upper_bound, "] Length:", wald_length, "\n")

# Agresti-Coull confidence interval
p_hat_tilde <- (sum(skittles$color == "red")+2) / (nrow(skittles) +4)

agresti_coull_lower_bound = p_red - qnorm(0.975) * sqrt((p_hat_tilde * (1 - p_hat_tilde)) / (nrow(skittles) + 4))
agresti_coull_upper_bound = p_red + qnorm(0.975) * sqrt((p_hat_tilde * (1 - p_hat_tilde)) / (nrow(skittles) + 4))
# print agresti-coull confidence interval and its length
agresti_coull_ci = c(agresti_coull_lower_bound, agresti_coull_upper_bound)
agresti_coull_length = agresti_coull_upper_bound - agresti_coull_lower_bound
cat("Agresti-Coull Confidence Interval: [", agresti_coull_lower_bound, ",", agresti_coull_upper_bound, "] Length:", agresti_coull_length, "\n")

## Use the bootstrap to construct a 95% confidence interval.
# regenerate skittles data by sampling m observations from skittles with replacement. then, record the
# estimated p-hat from each sample. do this B times. record all the p-hat's.
B <- 1000 # number of bootstrap samples
bootstrap_samples <- replicate(B, {
  sample(skittles$color, size = nrow(skittles), replace = TRUE)
})
# calculate the proportion of red skittles in each bootstrap sample
bootstrap_proportions <- apply(bootstrap_samples, 2, function(sample) {
  sum(sample == "red") / length(sample)
})
# calculate the 2.5th and 97.5th percentiles of the bootstrap proportions
bootstrap_lower_bound <- quantile(bootstrap_proportions, 0.025)
bootstrap_upper_bound <- quantile(bootstrap_proportions, 0.975)
# print bootstrap confidence interval and its length
bootstrap_ci <- c(bootstrap_lower_bound, bootstrap_upper_bound)
bootstrap_length <- bootstrap_upper_bound - bootstrap_lower_bound
cat("Bootstrap Confidence Interval: [", bootstrap_lower_bound, ",", bootstrap_upper_bound, "] Length:", bootstrap_length, "\n")

# Compare the width of the bootstrap interval to the Wald and Agresti-Coull intervals
cat("Wald Interval Length:", wald_length, "\n")
cat("Agresti-Coull Interval Length:", agresti_coull_length, "\n")
cat("Bootstrap Interval Length:", bootstrap_length, "\n")

## bootstrap CI is widest
# but sample of 110 skittles is super small compared to population of all skittles in the world

#######################
### NBA FREE THROWS ###
#######################

# load data

## code from yesterday
nba_players = read_delim("../data/09_nba-free-throws.csv.gz", delim = ";")
names(nba_players)

nba_players_subset <- nba_players %>%
  group_by(Player) %>% 
  mutate(FT = round(FT * G),
         FTA = round(FTA * G)) %>%
  summarise(ft_sum = sum(FT, na.rm = TRUE),
            FTA = sum(FTA, na.rm = TRUE),
            FT_percentage = (ft_sum/FTA))

at_least_25 <- nba_players_subset %>% 
  filter(FTA >= 25)

## make bootstrap confidence intervals:
B <- 1000

# Create a function to bootstrap the free throw percentage
bootstrap_ft_pct <- function(ft_sum, FTA, B = 1000) {
  boot_props <- numeric(B)
  
  for (b in 1:B) {
    # Generate FTA attempts with success probability = ft_sum / FTA
    simulated_shots <- rbinom(n = FTA, size = 1, prob = ft_sum / FTA)
    boot_props[b] <- mean(simulated_shots)
  }
  
  # Return confidence interval
  quantile(boot_props, probs = c(0.025, 0.975))
}

# Apply to each player in the dataframe
results <- at_least_25 %>%
  rowwise() %>%
  mutate(
    ci = list(bootstrap_ft_pct(ft_sum, FTA, B)),
    ci_lower = ci[[1]],
    ci_upper = ci[[2]]
  ) %>%
  select(-ci)  # drop list column if not needed

# Wald CI
wald_ci <- at_least_25 %>%
  mutate(lower_wald = FT_percentage - 1.96 * sqrt((FT_percentage * (1 - FT_percentage)) / FTA),
         upper_wald = FT_percentage + 1.96 * sqrt((FT_percentage * (1 - FT_percentage)) / FTA))
# Agresti-Coull CI
at_least_25 <- at_least_25 %>% 
  mutate(FT_percentage_hat = ((ft_sum + 2)/(FTA + 4)))

agresti_coull_ci <- at_least_25 %>%
  mutate(lower_agresti = FT_percentage - 1.96 * sqrt((FT_percentage_hat * (1 - FT_percentage_hat)) / (FTA + 4)),
         upper_agresti = FT_percentage + 1.96 * sqrt((FT_percentage_hat * (1 - FT_percentage_hat)) / (FTA + 4)))

# Combine the data for plotting
combined_data <- at_least_25 %>%
  select(Player, FT_percentage, FTA) %>%
  left_join(wald_ci %>% select(Player, lower_wald, upper_wald), by = "Player") %>%
  left_join(agresti_coull_ci %>% select(Player, lower_agresti, upper_agresti), by = "Player")

# combine bootstrap results with combined_data
combined_data <- combined_data %>%
  left_join(results %>% select(Player, ci_lower, ci_upper), by = "Player") %>%
  rename(lower_bootstrap = ci_lower, upper_bootstrap = ci_upper)

# Plot with all three confidence intervals
ggplot(combined_data, aes(x = reorder(Player, FT_percentage), y = FT_percentage)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_wald, ymax = upper_wald), color = "blue", width = 0.2) +
  geom_errorbar(aes(ymin = lower_agresti, ymax = upper_agresti), color = "red", width = 0.2) +
  geom_errorbar(aes(ymin = lower_bootstrap, ymax = upper_bootstrap), color = "green", width = 0.2) +
  coord_flip() +
  labs(title = "NBA Free Throw Percentage with all three CI's",
       x = "Player",
       y = "Free Throw Percentage") +
  theme_minimal()

# smaller sample
combined_data_sample <- combined_data %>%
  slice_sample(n = 10)
ggplot(combined_data_sample, aes(x = reorder(Player, FT_percentage), y = FT_percentage)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_wald, ymax = upper_wald), color = "blue", width = 0.2) +
  geom_errorbar(aes(ymin = lower_agresti, ymax = upper_agresti), color = "red", width = 0.2) +
  geom_errorbar(aes(ymin = lower_bootstrap, ymax = upper_bootstrap), color = "green", width = 0.2) +
  coord_flip() +
  labs(title = "NBA Free Throw Percentage with all three CI's (Sample)",
       x = "Player",
       y = "Free Throw Percentage") +
  theme_minimal()


#######################
### BINOMIAL SIMULATION ###
#######################

## code from yesterday
p_values <- seq(0, 1, length.out = 1000)

n_values <- c(10, 50, 100, 250, 500, 1000)
results <- data.frame()

M <- 20
for (n in n_values) {
  for (p in p_values) {
    for (i in 1:M) {
      sample <- rbinom(n, size = 1, prob = p)
      prop_success <- mean(sample)
      results <- rbind(results, data.frame(n = n, p = p, prop_success = prop_success))
    }
  }
}

results <- results %>%
  mutate(
    lower_wald = prop_success - 1.96 * sqrt((prop_success * (1 - prop_success)) / n),
    upper_wald = prop_success + 1.96 * sqrt((prop_success * (1 - prop_success)) / n),
    prop_success_hat = (prop_success * n + 2) / (n + 4),
    lower_agresti = prop_success - 1.96 * sqrt((prop_success_hat * (1 - prop_success_hat)) / (n + 4)),
    upper_agresti = prop_success + 1.96 * sqrt((prop_success_hat * (1 - prop_success_hat)) / (n + 4))
  )

B <- 1000  # number of bootstrap replicates

# Function to calculate bootstrap confidence interval for a given sample
bootstrap_ci <- function(sample, B = 1000) {
  boot_props <- replicate(B, {
    boot_sample <- sample(sample, size = length(sample), replace = TRUE)
    mean(boot_sample)
  })
  quantile(boot_props, probs = c(0.025, 0.975))
}

# Apply bootstrap confidence interval row by row
results <- results %>%
  rowwise() %>%
  mutate(
    # simulate the sample that led to prop_success
    sample_vector = list(c(rep(1, round(n * prop_success)), rep(0, n - round(n * prop_success)))),
    boot_ci = list(bootstrap_ci(sample_vector[[1]], B)),
    lower_bootstrap = boot_ci[[1]],
    upper_bootstrap = boot_ci[[2]]
  ) %>%
  ungroup() %>%
  select(-sample_vector, -boot_ci)

coverage_results <- results %>%
  group_by(n, p) %>%
  summarise(
    coverage_wald = mean(lower_wald <= p & upper_wald >= p),
    coverage_agresti = mean(lower_agresti <= p & upper_agresti >= p),
    coverage_bootstrap = mean(bootstrap_lower <= p & bootstrap_upper >= p),
    .groups = 'drop'
  )

ggplot(coverage_results, aes(x = p, y = coverage_wald, color = as.factor(n))) +
  geom_line() +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  labs(title = "Coverage Probability of Wald Confidence Intervals",
       x = "True Proportion (p)",
       y = "Coverage Probability",
       color = "Sample Size (n)") +
  theme_minimal()

ggplot(coverage_results, aes(x = p, y = coverage_agresti, color = as.factor(n))) +
  geom_line() +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  labs(title = "Coverage Probability of Agresti-Coull Confidence Intervals",
       x = "True Proportion (p)",
       y = "Coverage Probability",
       color = "Sample Size (n)") +
  theme_minimal()

# facet the graphs for each p and each n
coverage_results %>%
  ggplot(aes(x = p, y = coverage_wald, color = as.factor(n))) +
  geom_line() +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  facet_wrap(~ n) +
  labs(title = "Coverage Probability of Wald Confidence Intervals by Sample Size",
       x = "True Proportion (p)",
       y = "Coverage Probability",
       color = "Sample Size (n)") +
  theme_minimal()
coverage_results %>%
  ggplot(aes(x = p, y = coverage_agresti, color = as.factor(n))) +
  geom_line() +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  facet_wrap(~ n) +
  labs(title = "Coverage Probability of Agresti-Coull Confidence Intervals by Sample Size",
       x = "True Proportion (p)",
       y = "Coverage Probability",
       color = "Sample Size (n)") +
  theme_minimal()
coverage_results %>%
  ggplot(aes(x = p, y = coverage_bootstrap, color = as.factor(n))) +
  geom_line() +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  facet_wrap(~ n) +
  labs(title = "Coverage Probability of Bootstrap Confidence Intervals by Sample Size",
       x = "True Proportion (p)",
       y = "Coverage Probability",
       color = "Sample Size (n)") +
  theme_minimal()

# i only used M = 5 since my laptop takes forever to generate samples
# but hopefully the code is correct
