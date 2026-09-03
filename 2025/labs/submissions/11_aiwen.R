#############
### SETUP ###
#############

# rm(list=ls())

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(purrr)


# set seed
set.seed(11)

#######################
### NBA FREE THROWS ###
#######################

# load data
nba_players = read_delim("../data/11_nba-free-throws.csv.gz", delim = ";")

## code from last week: wald and agresti-coulli intervals for each player
nba_players_subset <- nba_players %>%
  group_by(Player) %>% 
  mutate(FT = round(FT * G),
         FTA = round(FTA * G)) %>%
  summarise(ft_sum = sum(FT, na.rm = TRUE),
            FTA = sum(FTA, na.rm = TRUE),
            FT_percentage = (ft_sum/FTA))

at_least_25 <- nba_players_subset %>% 
  filter(FTA >= 25)

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

### 
# bayesian intervals
###

alpha_vals <- seq(0.5, 5, length.out = 10)
beta_vals <- seq(0.5, 5, length.out = 10)
prior_grid <- expand.grid(alpha = alpha_vals, beta = beta_vals)

# make 95% CI's for each player using grid of priors
posterior_results <- prior_grid %>%
  pmap_dfr(function(alpha, beta) {
    at_least_25 %>%
      mutate(
        alpha = alpha,
        beta = beta,
        lower_post = qbeta(0.025, alpha + ft_sum, beta + (FTA - ft_sum)),
        upper_post = qbeta(0.975, alpha + ft_sum, beta + (FTA - ft_sum)),
        width_post = upper_post - lower_post,
        prior = paste0("Beta(", round(alpha, 2), ",", round(beta, 2), ")")
      ) %>%
      select(Player, FTA, FT_percentage, alpha, beta, prior,
             lower_post, upper_post, width_post)
  })

posterior_results %>%
  ggplot(aes(x = FT_percentage, y = width_post, color = prior)) +
  geom_point(alpha = 0.5) +
  labs(title = "Posterior Interval Widths by Prior (10x10 Grid)",
       x = "Observed FT%", y = "Posterior Interval Width") +
  theme_minimal()

# look at a specific players intervals for diff. values of alpha, beta
player_name <- "Stephen Curry" 

posterior_results %>%
  filter(Player == player_name) %>%
  ggplot(aes(x = prior, y = width_post)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = paste("Posterior CI Widths for", player_name),
       x = "Prior (alpha, beta)", y = "Width") +
  theme_minimal()

### compare lengths of wald, agresti-coulli, and bayesian intervals

wald_ci <- wald_ci %>%
  mutate(wald_width = upper_wald - lower_wald)

agresti_coull_ci <- agresti_coull_ci %>%
  mutate(agresti_width = upper_agresti - lower_agresti)

interval_lengths <- at_least_25 %>%
  select(Player, FT_percentage, FTA) %>%
  left_join(wald_ci %>% select(Player, wald_width), by = "Player") %>%
  left_join(agresti_coull_ci %>% select(Player, agresti_width), by = "Player")

avg_bayes_width <- posterior_results %>%
  group_by(Player) %>%
  summarise(mean_bayes_width = mean(width_post, na.rm = TRUE))

# combine all widths together
all_interval_widths <- interval_lengths %>%
  left_join(avg_bayes_width, by = "Player") %>%
  pivot_longer(cols = c(wald_width, agresti_width, mean_bayes_width),
               names_to = "method", values_to = "width") %>%
  mutate(method = recode(method,
                         wald_width = "Wald",
                         agresti_width = "Agresti-Coull",
                         mean_bayes_width = "Bayesian (avg of 100 priors)"))

# visualize distn of interval widths
ggplot(all_interval_widths, aes(x = method, y = width, fill = method)) +
  geom_boxplot() +
  labs(title = "Distribution of Interval Widths by Method",
       x = "Method", y = "Interval Width") +
  theme_minimal()

ggplot(all_interval_widths, aes(x = FT_percentage, y = width, color = method)) +
  geom_point(alpha = 0.6) +
  labs(title = "Interval Widths by Method vs. Observed Free Throw %",
       x = "Observed FT%", y = "Interval Width") +
  theme_minimal()
