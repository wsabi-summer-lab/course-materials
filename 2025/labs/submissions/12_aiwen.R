#############
### SETUP ###
#############

rm(list=ls())
# install.packages(c("ggplot2", "splines", "tidyverse"))
library(ggplot2)
library(splines)
library(tidyverse)
library(purrr)
library(splines)

# set seed
set.seed(12)

##########################
### NBA PLAYER QUALITY ###
##########################

# load data
nba_data = read_csv("../data/12_nba-box-scores.csv.gz")
# preview data
head(nba_data)

##########################
## task 3
nba_data <- nba_data %>%
  mutate(points_per_poss = pts / possessions)

# average points per possession per player
player_summary <- nba_data %>%
  group_by(namePlayer) %>%
  summarise(
    G_i = n(),
    M_i = mean(points_per_poss, na.rm = TRUE)
  )

G_star <- 30
M_set <- player_summary %>%
  filter(G_i >= G_star)

mu_hat <- mean(M_set$M_i)
nu2_hat <- var(M_set$M_i)

##########################
## task 4
# variance of points per possession for each player
player_variance <- nba_data %>%
  group_by(namePlayer) %>%
  summarise(
    V_i = var(points_per_poss, na.rm = TRUE),
    M_i = mean(points_per_poss, na.rm = TRUE),
    G_i = n()
  )

# get players whose M_i is close to mu_hat 
tolerance <- 0.05
V_set <- player_variance %>%
  filter(G_i >= G_star, abs(M_i - mu_hat) < tolerance)

# approximation of the variance of the points per possession for average players
sigma2_hat <- mean(V_set$V_i, na.rm = TRUE)

##########################
## task 5
# list of tau^2 values
tau2_grid <- 10^seq(-7, -3, length.out = 10)

tau2_rmse_results <- data.frame(tau2 = tau2_grid, mean_rmse = NA)

players_to_use <- nba_data %>%
  group_by(namePlayer) %>%
  summarise(G_i = n()) %>%
  filter(G_i > G_star) %>%
  slice_sample(n = 5) %>% 
  pull(namePlayer)

# loop over values of tau^2
for (t in seq_along(tau2_grid)) {
  tau2 <- tau2_grid[t]
  rmse_list <- c()
  
  for (player in players_to_use) {
    player_data <- nba_data %>%
      filter(namePlayer == player) %>%
      arrange(idGame) %>%
      mutate(points_per_poss = pts / possessions)
    
    G_i <- nrow(player_data)
    mid_idx <- floor(G_i / 2)
    
    train_data <- player_data[1:mid_idx, ]
    test_data <- player_data[(mid_idx + 1):G_i, ]
    
    mu_prev <- mu_hat
    for (j in 1:mid_idx) {
      y_j <- train_data$points_per_poss[j]
      P_j <- train_data$P[j]
      
      mu_j <- mu_prev + (tau2 / (tau2 + sigma2_hat)) * (y_j - mu_prev)
      mu_prev <- mu_j 
    }
    
    mu_hat_k <- mu_prev  # final posterior mean at end of training
    
    # predict on D2i using mu_hat_k
    rmse <- sqrt(mean((test_data$points_per_poss - mu_hat_k)^2))
    rmse_list <- c(rmse_list, rmse)
  }
  
  tau2_rmse_results$mean_rmse[t] <- mean(rmse_list, na.rm = TRUE)
}

# find lowest mean rmse
best_tau2 <- tau2_rmse_results %>%
  filter(mean_rmse == min(mean_rmse, na.rm = TRUE)) %>%
  pull(tau2)
best_tau2

##########################
## task 6
sigma2 <- sigma2_hat
nu2 <- nu2_hat
tau2 <- best_tau2  # from task 6
mu <- mu_hat

nba_subset <- nba_data %>%
  filter(namePlayer %in% players_to_use) %>%
  arrange(namePlayer, idGame) %>%  # ensure correct game order
  mutate(points_per_poss = pts / possessions)

posterior_results <- nba_subset %>%
  group_by(namePlayer) %>%
  mutate(mu_hat_posterior = NA_real_,
         game_number = row_number()) %>%
  ungroup()

for (player in players_to_use) {
  player_data <- nba_subset %>%
    filter(namePlayer == player) %>%
    arrange(idGame)
  
  mu_vals <- numeric(nrow(player_data))
  
  for (j in seq_len(nrow(player_data))) {
    y_j <- player_data$points_per_poss[j]
    
    if (j == 1) {
      mu_j <- mu + (nu2 / (nu2 + sigma2)) * (y_j - mu)
    } else {
      mu_prev <- mu_vals[j - 1]
      mu_j <- mu_prev + (tau2 / (tau2 + sigma2)) * (y_j - mu_prev)
    }
    
    mu_vals[j] <- mu_j
  }
  
  # Fill in results
  posterior_results$mu_hat_posterior[posterior_results$namePlayer == player] <- mu_vals
}

ggplot(posterior_results, aes(x = game_number, y = mu_hat_posterior, color = namePlayer)) +
  geom_line(size = 1) +
  labs(
    title = "Posterior Scoring Quality Trajectories",
    x = "Game Number in Career",
    y = "Posterior Mean Scoring Quality",
    color = "Player"
  ) +
  theme_minimal()

## results only using 5 players b/c took too long to run otherwise

##########################
### NFL KICKER QUALITY ###
##########################

# load data
kick_data = read_csv("../data/12_field-goals.csv.gz")
# preview data
head(kick_data)

##########################
## task 1
kick_model <- lm(fg_made ~ splines::bs(ydl, degree = 3, df = 5), data = kick_data, family = "binomial")
summary(kick_model)

##########################
## task 3
alpha <- 0.995

kick_data <- kick_data %>%
  arrange(kicker, season, week)

kick_data <- kick_data %>%
  mutate(pred_prob = predict(kick_model, newdata = kick_data, type = "response"),
         FGPA = fg_made - pred_prob)

# compute kicker quality for each kicker
kick_data <- kick_data %>%
  group_by(kicker) %>%
  arrange(season, week, .by_group = TRUE) %>%
  mutate(
    KQ = {
      kq_vals <- numeric(n())
      if (n() > 1) {
        kq_vals[1] <- 0
        for (j in 2:n()) {
          kq_vals[j] <- alpha * kq_vals[j - 1] + FGPA[j - 1]
        }
      } else {
        kq_vals[1] <- 0  # if one kick, set KQ = 0
      }
      kq_vals
    },
    kick_number = row_number()
  ) %>%
  ungroup()

sample_kickers <- sample(unique(kick_data$kicker), 5)

kick_data %>%
  filter(kicker %in% sample_kickers) %>%
  ggplot(aes(x = kick_number, y = KQ, color = kicker)) +
  geom_line(size = 1) +
  labs(title = "Rolling Kicker Quality Estimates Over Time",
       x = "Kick Number in Career",
       y = "Estimated Kicker Quality (KQ)",
       color = "Kicker") +
  theme_minimal()
