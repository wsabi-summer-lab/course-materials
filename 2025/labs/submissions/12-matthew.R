# Load packages
library(tidyverse)

# Set seed for reproducibility
set.seed(12)

# Load data
nba_data <- read_csv("../data/12_nba-box-scores.csv.gz") %>%
  mutate(pct = pts / possessions) %>%
  arrange(idPlayer, idGame) %>%
  group_by(idPlayer) %>%
  mutate(game_index = row_number(), n_games = n()) %>%
  filter(n_games >= 10) %>%
  ungroup()

# Estimate player-level statistics
player_stats <- nba_data %>%
  group_by(idPlayer) %>%
  summarise(
    G = n(),
    Mi = mean(pct, na.rm = TRUE),
    Si2 = var(pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(G >= 5)

# Estimate Empirical Bayes hyperparameters
mu <- mean(player_stats$Mi, na.rm = TRUE)
sigma2_i_over_n <- mean(player_stats$Si2 / player_stats$G, na.rm = TRUE)
tau2 <- var(player_stats$Mi, na.rm = TRUE) - sigma2_i_over_n
tau2 <- max(tau2, 1e-6)  # Ensure positivity
sigma2 <- mean(player_stats$Si2, na.rm = TRUE)

cat("Estimated mu:", mu, "\nEstimated tau²:", tau2, "\nEstimated sigma²:", sigma2, "\n")

# Function for rolling posterior updates
rolling_posterior <- function(pcts, possessions, mu, tau2, sigma2) {
  n <- length(pcts)
  post_means <- numeric(n)
  post_vars <- numeric(n)
  
  # Prior initialization
  m_prev <- mu
  v_prev <- tau2
  
  for (j in 1:n) {
    obs <- pcts[j]
    n_j <- possessions[j]
    v_obs <- sigma2 / n_j
    
    # Posterior mean and variance
    v_post <- 1 / (1 / v_prev + 1 / v_obs)
    m_post <- v_post * (m_prev / v_prev + obs / v_obs)
    
    post_means[j] <- m_post
    post_vars[j] <- v_post
    
    # Update prior for next round
    m_prev <- m_post
    v_prev <- v_post
  }
  
  tibble(posterior_mean = post_means, posterior_var = post_vars)
}

# Apply rolling posterior updates to all players
df_roll <- nba_data %>%
  group_by(idPlayer) %>%
  arrange(game_index) %>%
  nest() %>%
  mutate(
    roll = map(data, ~ rolling_posterior(.x$pct, .x$possessions, mu, tau2, sigma2))
  ) %>%
  unnest(cols = c(data, roll))

# Evaluate RMSE on second half of games
evaluate_rmse <- function(df_roll) {
  df_eval <- df_roll %>%
    group_by(idPlayer) %>%
    mutate(n_games = n(), split = floor(n_games / 2)) %>%
    filter(game_index > split) %>%
    mutate(error2 = (posterior_mean - pct)^2)
  
  sqrt(mean(df_eval$error2, na.rm = TRUE))
}

cat("Rolling posterior RMSE:", evaluate_rmse(df_roll), "\n")

# Visualize rolling posterior means for sample players
sample_players <- sample(unique(df_roll$idPlayer), 5)
df_plot <- df_roll %>% filter(idPlayer %in% sample_players)

ggplot(df_plot, aes(x = game_index, y = posterior_mean, color = as.factor(idPlayer))) +
  geom_line(size = 1) +
  geom_point(aes(y = pct), alpha = 0.4, shape = 1) +
  labs(title = "Rolling Empirical Bayes Posterior Mean (Scoring Rate)",
       x = "Game Number", y = "Posterior Mean", color = "Player ID") +
  theme_minimal()
##########################
### NFL KICKER QUALITY ###
##########################

# load data
kick_data = read_csv("../data/12_field-goals.csv.gz")
# preview data
head(kick_data)

#library(splines)  # for natural splines
library(ggplot2)

# Fit logistic regression with spline on ydl
fg_model <- glm(fg_made ~ bs(ydl, df = 5), data = kick_data, family = binomial())

# Create a sequence of yard lines for prediction (e.g., 0 to 60 yards)
yardline_seq <- seq(min(kick_data$ydl), max(kick_data$ydl), by = 1)

# Predict probabilities on this grid
pred_df <- data.frame(ydl = yardline_seq)
pred_df$prob_fg <- predict(fg_model, newdata = pred_df, type = "response")

# Plot estimated probability curve
ggplot(pred_df, aes(x = ydl, y = prob_fg)) +
  geom_line(color = "blue", size = 1.2) +
  labs(title = "Estimated Probability of Field Goal Made vs Yard Line",
       x = "Yard Line",
       y = "Probability of FG Made") +
  theme_minimal()

# 1. Add predicted FG probabilities from your existing fg_model:
kick_data <- kick_data %>%
  mutate(prob_fg = predict(fg_model, newdata = ., type = "response"))

# 2. Compute FGPA for each kick
kick_data <- kick_data %>%
  arrange(kicker, season, week) %>%
  group_by(kicker) %>%
  mutate(
    FGPA = fg_made - prob_fg
  ) %>%
  ungroup()

# 3. Define function to compute kicker quality prior to each kick
estimate_kicker_quality <- function(alpha, df) {
  # Assumes df is sorted by kicker and time (season, week)
  
  df <- df %>%
    group_by(kicker) %>%
    arrange(season, week) %>%
    mutate(
      KQ_prior = 0  # initialize vector
    ) %>%
    ungroup()
  
  kicker_list <- split(df, df$kicker)
  
  kicker_list <- lapply(kicker_list, function(kicker_df) {
    n <- nrow(kicker_df)
    KQ_prior <- numeric(n)
    KQ_prior[1] <- 0  # KQ prior to first kick is zero
    
    if (n > 1) {
      for (j in 2:n) {
        KQ_prior[j] <- alpha * KQ_prior[j-1] + kicker_df$FGPA[j-1]
      }
    }
    
    kicker_df$KQ_prior <- KQ_prior
    return(kicker_df)
  })
  
  df_out <- bind_rows(kicker_list) %>%
    arrange(kicker, season, week)
  
  return(df_out)
}

# 4. Apply with your chosen alpha (e.g., 0.8)
alpha_val <- 0.8
kick_data <- estimate_kicker_quality(alpha_val, kick_data)

# 5. Plot kicker quality trajectories for a few kickers
sample_kickers <- sample(unique(kick_data$kicker), 4)

kick_data %>%
  filter(kicker %in% sample_kickers) %>%
  group_by(kicker) %>%
  mutate(kick_num = row_number()) %>%
  ggplot(aes(x = kick_num, y = KQ_prior, color = kicker, group = kicker)) +
  geom_line(size = 1) +
  geom_point(alpha = 0.7) +
  labs(title = "Kicker Quality Prior to Each Kick",
       x = "Kick Number in Career",
       y = "Kicker Quality (KQ_prior)") +
  theme_minimal()
