#############
### SETUP ###
#############

# install.packages(c("ggplot2", "splines", "tidyverse"))
library(ggplot2)
library(splines)
library(tidyverse)

# set seed
set.seed(12)

##########################
### NBA PLAYER QUALITY ###
##########################

# load data
nba_data = read_csv("../data/12_nba-box-scores.csv.gz")
# preview data
head(nba_data)

nba_data_filtered = nba_data %>%
  mutate(pts_per_pos = pts / possessions) %>%
  group_by(namePlayer) %>%
  summarise(
    avg_pts_per_pos = mean(pts_per_pos, na.rm = TRUE),
    var_pts_per_pos = var(pts_per_pos, na.rm = TRUE),
    games_played = n(),
    .groups = "drop"
  ) %>%
  filter(games_played >= 82)

nba_data_filtered %>%
  arrange(desc(avg_pts_per_pos)) %>%
  head(10)

mu = mean(nba_data_filtered$avg_pts_per_pos)
nu_2 = var(nba_data_filtered$avg_pts_per_pos)
sigma_2 = mean(nba_data_filtered$var_pts_per_pos)

tau_2_grid = c(1e-7, 1e-6, 1e-5, 1e-4, 1e-3)
# Split each player's career into two halves
nba_data_split = nba_data %>%
  group_by(namePlayer) %>% 
  arrange(dateGame) %>% 
  mutate(
    id = row_number(),
    game_played = n(),
    g_half = floor(game_played / 2),
    which = if_else(id <= g_half, "first_half", "second_half")
  ) %>%
  summarize(
    m = mean(pts[which == "first_half"] / possessions[which == "first_half"], na.rm = TRUE),
    d2 = list(pts[which == "second_half"] / possessions[which == "second_half"]),
    k = first(g_half),
    .groups = "drop"
  ) %>%
  filter(k >= 41)  

rmse_results = sapply(tau_2_grid, function(tau_2) {
  per_player_rmse = mapply(function(m, d2, k) {
    mu_ik = (sigma_2 / (sigma_2 + k * tau_2)) * mu + 
      (k * tau_2 / (sigma_2 + k * tau_2)) * m
    sqrt(mean((mu_ik - unlist(d2))^2, na.rm = TRUE))
  },
  m = nba_data_split$m,
  d2 = nba_data_split$d2,
  k = nba_data_split$k
  )
  mean(per_player_rmse, na.rm = TRUE)
})  
# Create a list to store results
rmse_results_df <- data.frame(tau_2 = tau_2_grid, rmse = rmse_results)# F
# Find tau_2 with minimum RMSE
tau_2 = rmse_results_df$tau_2[which.min(rmse_results_df$rmse)]

# Compute posterior means for each player and game
nba_data_posterior <- nba_data %>%
  group_by(namePlayer) %>%
  mutate(
    mu_post = NA_real_,
    var_post = NA_real_
  ) %>%
  group_modify(~ {
    n <- nrow(.x)
    mu_prev <- mu
    var_prev <- nu_2  # ← use nu^2 for game 1
    mu_post_vec <- numeric(n)
    var_post_vec <- numeric(n)
    
    for (j in 1:n) {
      x_obs <- .x$pts[j] / .x$possessions[j]
      
      post_var <- 1 / (1 / var_prev + 1 / sigma_2)
      post_mean <- post_var * (x_obs / sigma_2 + mu_prev / var_prev)
      
      mu_post_vec[j] <- post_mean
      var_post_vec[j] <- post_var
      
      # Update prior for next game using τ^2
      mu_prev <- post_mean
      var_prev <- post_var + tau_2
    }
    
    .x$mu_post <- mu_post_vec
    .x$var_post <- var_post_vec
    .x
  }) %>%
  ungroup()

sampled_players <- nba_data_filtered %>% 
  filter(games_played >= 200) %>%
  .$namePlayer %>%sample(10)
nba_data_posterior %>%
  filter(namePlayer %in% sampled_players) %>%
  arrange(namePlayer, idGame) %>%
  group_by(namePlayer) %>%
  mutate(game_index = row_number()) %>%
  ungroup() %>%
  ggplot(aes(x = game_index, y = mu_post, color = as.factor(namePlayer))) +
  geom_line(alpha = 0.8) +
  labs(
    title = "Posterior Mean of μ Over Career (10 Random Players)",
    x = "Career Game Index",
    y = expression(hat(mu)[ij]),
    color = "Player Name"
  ) +
  theme_minimal()

##########################
### NFL KICKER QUALITY ###
##########################

# load data
kick_data = read_csv("../data/12_field-goals.csv.gz")
# preview data
head(kick_data)

model = glm(
  fg_made ~ splines::bs(ydl, df = 4),
  data = kick_data,
  family = binomial(link = "logit")
)
summary(model)

alpha = 0.995
kick_data = kick_data %>%
  mutate(
    fg_prob = predict(model, type = "response"),
    fgpa = fg_made - fg_prob
  ) %>%
  arrange(kicker) %>%
  group_by(kicker) %>%
  mutate(kick_id = row_number()) %>%
  ungroup()

# kq[i,j] = alpha * kq[i, j-1] + FGPA[i, j-1] (i: kicker, j: kick for kicker i)
kq = matrix(0, nrow = length(unique(kick_data$kicker)), ncol = max(kick_data$kick_id))
for (i in 1:nrow(kick_data)) {
  kicker_index = which(unique(kick_data$kicker) == kick_data$kicker[i])
  kick_id = kick_data$kick_id[i]
  
  if (kick_id == 1) {
    kq[kicker_index, kick_id] = kick_data$fgpa[i]
  } else {
    kq[kicker_index, kick_id] = alpha * kq[kicker_index, kick_id - 1] + kick_data$fgpa[i]
  }
}
# Convert kq to a data frame for plotting
kq_df <- as.data.frame(kq)
kq_df <- kq_df %>%
  mutate(kicker = unique(kick_data$kicker)) %>%
  pivot_longer(-kicker, names_to = "kick_id", values_to = "kq_value") %>%
  mutate(kick_id = as.integer(gsub("V", "", kick_id)))

# Plot kq for 10 random kickers
sampled_kickers <- kick_data %>%
  group_by(kicker) %>%
  summarise(games_played = n(), .groups = "drop") %>%
  filter(games_played >= 50) %>%
  sample_n(10) %>%
  pull(kicker)

kq_df %>%
  filter(kicker %in% sampled_kickers) %>%
  group_by(kicker) %>%
  filter(row_number() <= sum(kq_value != 0)) %>%
  ungroup() %>%
  ggplot(aes(x = kick_id, y = kq_value, color = kicker)) +
  geom_line() +
  labs(
    title = "Posterior Mean of Kicker Quality Over Career (10 Random Kickers)",
    x = "Career Kick Index",
    y = "Kicker Quality (kq)",
    color = "Kicker Name"
  ) +
  theme_minimal()