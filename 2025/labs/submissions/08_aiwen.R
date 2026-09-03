#############
### SETUP ###
#############

# rm(list=ls())

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
names(nba_players)

nba_players_subset <- nba_players %>%
  group_by(Player) %>% 
  mutate(FT = round(FT * G),
         FTA = round(FTA * G)) %>%
  summarise(ft_sum = sum(FT, na.rm = TRUE),
            TFA = sum(FTA, na.rm = TRUE),
            FT_percentage = (ft_sum/TFA))

at_least_25 <- nba_players_subset %>% 
  filter(TFA >= 25)

# Wald CI
wald_ci <- at_least_25 %>%
  mutate(lower_wald = FT_percentage - 1.96 * sqrt((FT_percentage * (1 - FT_percentage)) / TFA),
         upper_wald = FT_percentage + 1.96 * sqrt((FT_percentage * (1 - FT_percentage)) / TFA))
# Agresti-Coull CI
at_least_25 <- at_least_25 %>% 
  mutate(FT_percentage_hat = ((ft_sum + 2)/(TFA + 4)))

agresti_coull_ci <- at_least_25 %>%
  mutate(lower_agresti = FT_percentage - 1.96 * sqrt((FT_percentage_hat * (1 - FT_percentage_hat)) / (TFA + 4)),
         upper_agresti = FT_percentage + 1.96 * sqrt((FT_percentage_hat * (1 - FT_percentage_hat)) / (TFA + 4)))

# Combine the data for plotting
combined_data <- at_least_25 %>%
  select(Player, FT_percentage, TFA) %>%
  left_join(wald_ci %>% select(Player, lower_wald, upper_wald), by = "Player") %>%
  left_join(agresti_coull_ci %>% select(Player, lower_agresti, upper_agresti), by = "Player")

# Plot with Wald and Agresti-Coull confidence intervals
ggplot(combined_data, aes(x = reorder(Player, FT_percentage), y = FT_percentage)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_wald, ymax = upper_wald), color = "blue", width = 0.2) +
  geom_errorbar(aes(ymin = lower_agresti, ymax = upper_agresti), color = "red", width = 0.2) +
  coord_flip() +
  labs(title = "NBA Free Throw Percentage with Wald and Agresti-Coull Confidence Intervals",
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
  coord_flip() +
  labs(title = "NBA Free Throw Percentage with Wald and Agresti-Coull Confidence Intervals",
       x = "Player",
       y = "Free Throw Percentage") +
  theme_minimal()

#-----------------------------------------------------------------------------------------------------

## part 2

p_values <- seq(0, 1, length.out = 1000)

n_values <- c(10, 50, 100, 250, 500, 1000)
results <- data.frame()

# # step 2: for each value of p and each n in {10, 50, 100, 250, 500, 1000}, generate n free throws using the binomial
# # distribution with parameter p.
# for (n in n_values) {
#   for (p in p_values) {
#     sample <- rbinom(n, size = 1, prob = p)
#     prop_success <- mean(sample)
#     results <- rbind(results, data.frame(n = n, p = p, prop_success = prop_success))
#   }
# }
# 
# # step 3: Compute the 95% Wald and Agresti-Coull confidence intervals for each value of p and n
# results <- results %>%
#   mutate(
#     lower_wald = prop_success - 1.96 * sqrt((prop_success * (1 - prop_success)) / n),
#     upper_wald = prop_success + 1.96 * sqrt((prop_success * (1 - prop_success)) / n),
#     prop_success_hat = (prop_success * n + 2) / (n + 4),
#     lower_agresti = prop_success_hat - 1.96 * sqrt((prop_success_hat * (1 - prop_success_hat)) / (n + 4)),
#     upper_agresti = prop_success_hat + 1.96 * sqrt((prop_success_hat * (1 - prop_success_hat)) / (n + 4))
#   )

# step 4: Repeat steps 2 and 3 M = 100 times each
M <- 10
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

# step 5: Plot the results
# For each value of p and n, plot the coverage probability of the Wald and Agresti-Coull confidence
# intervals
coverage_results <- results %>%
  group_by(n, p) %>%
  summarise(
    coverage_wald = mean(lower_wald <= p & upper_wald >= p),
    coverage_agresti = mean(lower_agresti <= p & upper_agresti >= p),
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

#-----------------------------------------------------------------------------------------------------

################
# code for math part of lab (betting)

p_be <- 11 / 21
z <- qnorm(0.975)

p_values <- seq(p_be + 1e-4, 1, length.out = 100)

# required n for each p
n_required <- numeric(length(p_values))
for (i in 1:length(p_values)) {
  p <- p_values[i]
  num <- z * sqrt(p * (1 - p))
  denom <- p - p_be
  n_required[i] <- ceiling((num / denom)^2)
}

# dataframe for lpotting
plot_data <- data.frame(
  p = p_values,
  n = n_required
)

ggplot(plot_data, aes(x = p, y = n)) +
  geom_line(color = "blue", size = 1) +
  geom_vline(xintercept = p_be, linetype = "dashed", color = "red") +
  labs(title = "Number of Bets Needed for 95% Confidence of Profitability",
       x = "True Success Rate (p)",
       y = "Minimum Bets Needed (n)") +
  theme_minimal()
