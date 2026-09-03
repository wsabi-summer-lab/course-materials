library(ggplot2)
library(tidyverse)

set.seed(9)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")


# 9.1 Skittles Demo

skittle_counts <- c(Red = 24, Green = 31, Yellow = 22, Purple = 17, Orange = 19)

all_skittles <- unlist(mapply(rep, names(skittle_counts), skittle_counts))

red_notred <- ifelse(all_skittles == "Red", "Red", "Not Red")

skittles_df <- data.frame(Skittle = red_notred)

n <- nrow(skittles_df)
x <- sum(skittles_df$Skittle == "Red")
p_hat <- x / n
z <- qnorm(0.975)

# Wald Interval

wald_lower <- p_hat - z * sqrt(p_hat * (1 - p_hat) / n)
wald_upper <- p_hat + z * sqrt(p_hat * (1 - p_hat) / n)
wald_ci <- c(wald_lower, wald_upper)
wald_ci

# Agresti-Coull Interval

n_tilde <- n + z^2
p_tilde <- (x + z^2 / 2) / n_tilde
ac_se <- sqrt(p_tilde * (1 - p_tilde) / n_tilde)

ac_lower <- p_tilde - z * ac_se
ac_upper <- p_tilde + z * ac_se
ac_ci <- c(ac_lower, ac_upper)
ac_ci

# Bootstrap

n_boot <- 113
n <- nrow(skittles_df)
p_hats <- numeric(n_boot)

for (i in 1:n_boot) {
  
  sample_df <- as.data.frame(skittles_df[sample(n, replace = TRUE), ])  
  
  x <- sum(sample_df$`skittles_df[sample(n, replace = TRUE), ]` == "Red")
  
  p_hat <- x / n
  
  p_hats[i] <- p_hat
}
  
quantile(p_hats, c(0.025, 0.975))

ggplot(data.frame(p_hats), aes(x = p_hats)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(title = "Density Plot of Bootstrap p-hats",
       x = "Proportion of Red Skittles",
       y = "Density") +
  theme_minimal()

# Relative Coverage of Intervals

wald_ci
ac_ci
quantile(p_hats, c(0.025, 0.975))


# 9.2 NBA Free Throws

nba_players = read_delim("09_nba-free-throws.csv.gz", delim = ";")

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


# Bootstrap

num_players <- nrow(selected_data)
n_boot <- 1000

player_bootstrap <- selected_data %>%
  rowwise() %>%
  mutate(
    made = Total_FT,
    missed = Total_FTA - made,
    throws_vec = list(c(rep(1, made), rep(0, missed))),
    boot_pcts = list({
      throws <- unlist(throws_vec)
      sample_means <- replicate(n_boot, {
        mean(sample(throws, length(throws), replace = TRUE))
      })
      sample_means
    }),
    lower_CI = quantile(boot_pcts, 0.025, names = FALSE),
    upper_CI = quantile(boot_pcts, 0.975, names = FALSE)
  ) %>%
  ungroup() %>%
  select(Player, Total_FT, Total_FTA, Free_Throw_Percentage, lower_CI, upper_CI)

plot_data <- player_bootstrap %>%
  slice(1:10)

ggplot(plot_data, aes(x = Player, y = Free_Throw_Percentage/100)) +
  geom_point(color = "darkred", size = 3) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +
  coord_flip() +  # flip axes for readability
  labs(
    title = "Free Throw Percentage with 95% Bootstrap CIs (First 10 Players)",
    x = "Player",
    y = "Free Throw Percentage"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()

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

player_bootstrap <- player_bootstrap %>%
  mutate(p_hat = Free_Throw_Percentage / 100)

comparison_data <- ci_data %>%
  inner_join(player_bootstrap, by = "Player") %>%
  select(Player, 
         wald_lower, wald_upper, 
         agresti_lower, agresti_upper,
         lower_CI, upper_CI) %>%
  slice(1:15)

comparison_data$Player <- factor(comparison_data$Player, levels = comparison_data$Player)

ggplot(comparison_data, aes(x = Player)) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2, color = "blue", position = position_nudge(x = -0.25)) +
  geom_point(aes(y = p_hat), color = "blue", position = position_nudge(x = -0.25), size = 2) +
  
  geom_errorbar(aes(ymin = wald_lower, ymax = wald_upper), width = 0.2, color = "red", position = position_nudge(x = 0)) +
  geom_point(aes(y = p_hat), color = "red", position = position_nudge(x = 0), size = 2, shape = 17) +
  
  geom_errorbar(aes(ymin = agresti_lower, ymax = agresti_upper), width = 0.2, color = "darkgreen", position = position_nudge(x = 0.25)) +
  geom_point(aes(y = p_hat), color = "darkgreen", position = position_nudge(x = 0.25), size = 2, shape = 15) +
  
  coord_flip() +
  labs(title = "Free Throw Percentage CIs by Method (First 10 Players)",
       y = "Free Throw Percentage",
       x = "Player") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.position = "none")

# Bootstrap for Simulation Study

interval <- seq(0, 1, by = 0.01)
n_values <- c(10, 50, 100, 250, 500, 1000)
n_sim <- 100
n_boot <- 100

results <- expand.grid(p = interval, n = n_values) %>%
  rowwise() %>%
  mutate(
    coverage_wald = {
      mean(replicate(n_sim, {
        x <- rbinom(1, n, p)
        p_hat <- x / n
        se <- sqrt(p_hat * (1 - p_hat) / n)
        lower <- p_hat - 1.96 * se
        upper <- p_hat + 1.96 * se
        p >= lower & p <= upper
      }))
    },
    
    coverage_agresti = {
      mean(replicate(n_sim, {
        x <- rbinom(1, n, p)
        z <- 1.96
        n_adj <- n + z^2
        p_adj <- (x + z^2 / 2) / n_adj
        se <- sqrt(p_adj * (1 - p_adj) / n_adj)
        lower <- p_adj - z * se
        upper <- p_adj + z * se
        p >= lower & p <= upper
      }))
    },
    
    coverage_bootstrap = {
      mean(replicate(n_sim, {
        x <- rbinom(1, n, p)
        throws <- c(rep(1, x), rep(0, n - x))
        
        boot_means <- replicate(n_boot, {
          mean(sample(throws, replace = TRUE))
        })
        
        lower <- quantile(boot_means, 0.025, names = FALSE)
        upper <- quantile(boot_means, 0.975, names = FALSE)
        
        p >= lower & p <= upper
      }))
    }
  ) %>%
  ungroup()

# Coverage Probability Plot

ggplot(results, aes(x = p)) +
  geom_line(aes(y = coverage_wald, color = "Wald")) +
  geom_line(aes(y = coverage_agresti, color = "Agresti–Coull")) +
  geom_line(aes(y = coverage_bootstrap, color = "Bootstrap")) +
  facet_wrap(~ n, labeller = label_both) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(title = "Coverage Probability of 95% Confidence Intervals",
       x = "True Proportion (p)",
       y = "Coverage Probability",
       color = "Method") +
  theme_minimal()

