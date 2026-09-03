library(ggplot2)
library(splines)
library(tidyverse)

set.seed(12)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

nba_data = read_csv("12_nba-box-scores.csv.gz")

head(nba_data)

# Task 3: Estimating mu and prior variance

filtered_data <- nba_data %>% 
  filter(possessions != 0) %>% 
  mutate(points_per_possessions = pts / possessions) %>%
  group_by(idPlayer) %>%
  summarise(
    player_name = first(namePlayer),
    total_games = n(),
    avg_pts_per_poss = mean(points_per_possessions, na.rm = TRUE),
  ) %>%
  filter(total_games >= 10)

mu_hat <- mean(filtered_data$avg_pts_per_poss, na.rm = TRUE)

prior_variance <- var(filtered_data$avg_pts_per_poss, na.rm = TRUE)

mu_hat
prior_variance

# Task 4: Variance of player game-level points


std_dev <- sd(filtered_data$avg_pts_per_poss, na.rm = TRUE)

player_means_filtered <- filtered_data %>%
  filter(
    avg_pts_per_poss >= mu_hat - std_dev,
    avg_pts_per_poss <= mu_hat + std_dev
)

selected_ids <- player_means_filtered$idPlayer

selected_games <- nba_data %>%
  filter(idPlayer %in% selected_ids, possessions != 0) %>%
  mutate(points_per_possessions = pts / possessions)

player_variance_df <- selected_games %>%
  group_by(idPlayer) %>%
  summarise(
    player_name = first(namePlayer),
    var_pts_per_poss = var(points_per_possessions, na.rm = TRUE),
    games_played = n()
  )

sigma_hat_squared <- mean(player_variance_df$var_pts_per_poss, na.rm = TRUE)
sigma_hat_squared

# Task 5: Estimating tau squared

nba_filtered <- nba_data %>%
  filter(possessions != 0) %>%
  mutate(points_per_possessions = pts / possessions)

eligible_players <- nba_filtered %>%
  group_by(idPlayer) %>%
  filter(n() >= 10) %>% 
  ungroup()

split_games <- eligible_players %>%
  group_by(idPlayer) %>%
  arrange(idPlayer, idGame) %>% 
  mutate(
    game_index = row_number(),
    total_games = n(),
    half = ifelse(game_index <= total_games / 2, "first_half", "second_half")
  ) %>%
  ungroup()

# Sequence of tau2 values

tau2_values <- 10^seq(-7, -3, length.out = 20)

first_half_filtered_data <- split_games %>% 
  filter(possessions != 0, half == "first_half")

mu_hat_first_half <- mean(first_half_filtered_data$points_per_possessions, na.rm = TRUE)

player_stats <- split_games %>%
  filter(half == "first_half") %>%
  group_by(idPlayer) %>%
  summarise(
    n = n(),
    sigma2 = var(points_per_possessions, na.rm = TRUE),
    mu_sample = mean(points_per_possessions, na.rm = TRUE),
    .groups = "drop"
)

compute_mu_k <- function(tau2) {
  player_stats %>%
    mutate(
      tau2 = tau2,
      mu_k = ((n / sigma2) * mu_sample + (1 / tau2) * mu_hat_first_half) /
        ((n / sigma2) + (1 / tau2))
    )
}

mu_k_results <- purrr::map_dfr(tau2_values, compute_mu_k)

second_half_games <- split_games %>%
  filter(half == "second_half") %>%
  left_join(mu_k_results, by = "idPlayer")

second_half_games$squared_error <- (second_half_games$mu_k-second_half_games$mu_sample)^2

rmse_results <- second_half_games %>%
  group_by(idPlayer, tau2) %>%
  summarise(
    rmse = sqrt(mean(squared_error, na.rm = TRUE)),
    .groups = "drop"
  )

rmse_results

overall_rmse <- rmse_results %>%
  group_by(tau2) %>%
  summarise(
    avg_rmse = mean(rmse, na.rm = TRUE),
    .groups = "drop"
  )

overall_rmse

ggplot(overall_rmse, aes(x = tau2, y = avg_rmse)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  labs(title = "Average RMSE by tau²", x = expression(tau^2), y = "Average RMSE") +
  theme_minimal()

# Task 6: Implementing Model



kick_data = read_csv("12_field-goals.csv.gz")

head(kick_data)

