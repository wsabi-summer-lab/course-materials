library(ggplot2)
library(tidyverse)

set.seed(11)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

nba_players = read_delim("11_nba-free-throws.csv.gz", delim = ";")

nba_players <- nba_players %>%
  mutate(
    FT = as.numeric(FT),
    G = as.numeric(G),
    Total_Free_Throws = FT * G
  ) %>%
  mutate(
    FTA = as.numeric(FTA),
    Total_Free_Throw_Attempts = FTA * G
  )

selected_data <- nba_players %>%
  select(Player, Total_Free_Throws, Total_Free_Throw_Attempts, G) %>%
  group_by(Player) %>%
  summarise(
    Total_FT = sum(Total_Free_Throws, na.rm = TRUE),
    Total_FTA = sum(Total_Free_Throw_Attempts, na.rm = TRUE),
    Total_G = sum(G, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Free_Throw_Percentage = Total_FT / Total_FTA * 100) %>%
  filter(Total_FT > 25)

selected_data$Total_FT = round(selected_data$Total_FT, 0)
selected_data$Total_FTA = round(selected_data$Total_FTA, 0)


selected_data$Free_Throw_Percentage

# Comparing Confidence intervals

ci_data <- selected_data %>%
  drop_na() %>%
  mutate(
    p_hat = Free_Throw_Percentage/100,
    n = Total_FTA,
    
    wald_se = sqrt(p_hat * (1 - p_hat) / n),
    wald_lower = p_hat - 1.96 * wald_se,
    wald_upper = p_hat + 1.96 * wald_se,
    
    z = 1.96,
    n_ac = n + z^2,
    p_ac = (Total_FT + z^2 / 2) / n_ac,
    ac_se = sqrt(p_ac * (1 - p_ac) / n_ac),
    agresti_lower = p_ac - z * ac_se,
    agresti_upper = p_ac + z * ac_se
)

ci_sample <- ci_data %>%
  slice_sample(n = 25)

# Making Grid of Alpha and Beta

alpha_vals <- 1:100
beta_vals <- 1:100

posterior_results <- list()

for (i in seq_along(alpha_vals)) {
  alpha_prior <- alpha_vals[i]
  beta_prior  <- beta_vals[i]
  
  post_data <- ci_sample %>%
    mutate(
      prior_alpha = alpha_prior,
      prior_beta = beta_prior,
      alpha_post = prior_alpha + Total_FT,
      beta_post = prior_beta + (Total_FTA - Total_FT),
      post_mean = alpha_post / (alpha_post + beta_post),
      post_lower = qbeta(0.025, alpha_post, beta_post),
      post_upper = qbeta(0.975, alpha_post, beta_post),
      prior_index = i
    )
  
  posterior_results[[i]] <- post_data
}

posterior_df <- bind_rows(posterior_results)

head(posterior_df)

ggplot(posterior_df, aes(x = prior_index, y = post_mean)) +
  geom_ribbon(aes(ymin = post_lower, ymax = post_upper), fill = "skyblue", alpha = 0.4) +
  geom_line(color = "blue") +
  facet_wrap(~ Player, scales = "free_y") +
  labs(
    title = "Posterior Credible Intervals Across Priors",
    x = "Prior Combination Index",
    y = "Posterior Free Throw %"
  ) +
  theme_minimal()

ggplot(posterior_df, aes(x = prior_index, y = post_mean)) +
  geom_ribbon(aes(ymin = post_lower, ymax = post_upper), fill = "skyblue", alpha = 0.4) +
  
  geom_line(color = "blue") +
  
  geom_hline(aes(yintercept = wald_lower), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = wald_upper), color = "red", linetype = "dashed") +
  
  geom_hline(aes(yintercept = agresti_lower), color = "darkgreen", linetype = "dotted") +
  geom_hline(aes(yintercept = agresti_upper), color = "darkgreen", linetype = "dotted") +
  
  facet_wrap(~ Player, scales = "free_y") +
  labs(
    title = "Posterior, Wald, and Agresti-Coull Intervals Across Priors",
    x = "Prior Combination Index",
    y = "Estimated Free Throw %"
  ) +
  theme_minimal()