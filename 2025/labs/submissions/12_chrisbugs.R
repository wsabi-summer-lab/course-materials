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

# Get player summary for posterior mean
player_summary <- nba_data %>% 
  group_by(idPlayer, namePlayer) %>%                
  summarise(
    pts   = sum(pts,   na.rm = TRUE),
    poss  = sum(possessions, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    X_i    = pts / poss,              # observed scoring rate
    n_i    = poss,                    # sample size
    var_i  = X_i / n_i               # Poisson → variance of mean
  )

# Calculate a mu estimate
mu          <- weighted.mean(player_summary$X_i, w = player_summary$n_i)


# Calculate tau squared
tau_sq      <- max(0, 
                   with(player_summary, 
                        sum(n_i * (X_i - mu)^2) / sum(n_i) - weighted.mean(var_i, n_i))
)

# Calculate sigma squared
sigma_sq    <- weighted.mean(player_summary$var_i, w = player_summary$n_i)

# Get a shrinkage factor (tau_sq / (tau_sq + sigma_sq))
shrinkage   <- tau_sq / (tau_sq + sigma_sq)

# Get theta hat
player_summary <- player_summary %>% 
  mutate(
    theta_hat = mu + shrinkage * (X_i - mu)  
  )

# Display top 10 by theta_hat
top10 <- player_summary %>% 
  arrange(desc(theta_hat)) %>% 
  slice_head(n = 10)

print(top10 %>% 
        select(namePlayer, pts, poss, X_i, theta_hat))


# Task 3:
# Mega function to estimate mu and nu2
estimate_mu_nu2 <- function(data, G_star = 30) {
  
  # Get data in usable format
  player_avg <- data %>% 
    group_by(idPlayer, namePlayer) %>% 
    summarise(
      G_i   = n(),                                    # games played
      M_i   = sum(pts, na.rm = TRUE) /                # Σ points
        sum(possessions, na.rm = TRUE),         # Σ possessions
      .groups = "drop"
    )
  
  # Only take eligible games
  eligible <- player_avg %>% 
    filter(G_i > G_star)
  
  # Get parameter estimates
  mu_hat   <- mean(eligible$M_i, na.rm = TRUE)
  nu2_hat  <- var(eligible$M_i,  na.rm = TRUE)
  
  # 4. return as a list
  list(
    G_star  = G_star,
    n_players_used = nrow(eligible),
    mu_hat  = mu_hat,
    nu2_hat = nu2_hat
  )
}

# Estimate mu and nu2
mu_nu2_estimate <- estimate_mu_nu2(nba_data, G_star = 30)

print(mu_nu2_estimate)

# Task 4:
# Mega function to get sigma2
estimate_sigma2 <- function(data,
                            mu_hat,       
                            G_star = 30,  
                            eps = 0.01) { 
  
  # Get data in usable format
  per_player <- data %>%
    mutate(rate = pts / possessions) %>%        # Scoring rate
    group_by(idPlayer, namePlayer) %>%
    summarise(
      G_i  = n(),                               # games played
      M_i  = mean(rate, na.rm = TRUE),          # player mean
      V_i  = var(rate,  na.rm = TRUE),          # player variance
      .groups = "drop"
    )
  
  # Only eligible players
  average_players <- per_player %>%
    filter(G_i > G_star, abs(M_i - mu_hat) <= eps, !is.na(V_i))
  
  # estimate sigma2
  sigma2_hat <- mean(average_players$V_i)
  
  list(
    mu_hat          = mu_hat,
    G_star          = G_star,
    eps             = eps,
    n_players_used  = nrow(average_players),
    sigma2_hat      = sigma2_hat
  )
}

# Estimate sigma2
sigma2_estimate <- estimate_sigma2(nba_data, 
                                     mu_hat = mu_nu2_estimate$mu_hat, 
                                     G_star = mu_nu2_estimate$G_star)

print(sigma2_estimate)


# Task 5:
posterior_mu_k <- function(df_first, mu_hat, nu2_hat, sigma2_hat, tau2) {
  
  # drop any row with bad possessions just in case
  df_first <- df_first %>% filter(possessions > 0, is.finite(rate))
  
  if (nrow(df_first) == 0) return(NA_real_)
  
  w   <- 1 / (tau2 + nu2_hat / df_first$possessions)
  num <- mu_hat / sigma2_hat + sum(df_first$rate * w)
  den <- 1 / sigma2_hat      + sum(w)
  
  num / den
}

# ------------------------------------------------------------------
# Main tuning routine (robust) -------------------------------------
# ------------------------------------------------------------------
tune_tau2 <- function(data,
                      mu_hat, nu2_hat, sigma2_hat,
                      tau2_grid = 10^seq(-7, -3, length.out = 9),
                      G_star    = 30) {
  
  # prepare data ----------------------------------------------------
  data <- data %>%
    filter(possessions > 0) %>%                 # crucial
    mutate(rate = pts / possessions) %>%
    arrange(idPlayer, dateGame)
  
  # keep players with enough games ---------------------------------
  eligible_ids <- data %>%
    count(idPlayer, namePlayer, name = "Gi") %>%
    filter(Gi > G_star) %>%
    pull(idPlayer)
  
  # grid search -----------------------------------------------------
  grid_results <- map_dfr(tau2_grid, function(tau2) {
    
    rmse_vec <- map_dbl(eligible_ids, function(pid) {
      
      df_player <- data %>% filter(idPlayer == pid)
      Gi <- nrow(df_player); k <- floor(Gi / 2)
      
      # need at least one future game
      if (Gi - k < 1) return(NA_real_)
      
      mu_hat_k <- posterior_mu_k(df_player[1:k, ],
                                 mu_hat, nu2_hat, sigma2_hat, tau2)
      
      if (!is.finite(mu_hat_k)) return(NA_real_)
      
      sqrt(mean((df_player$rate[(k+1):Gi] - mu_hat_k)^2, na.rm = TRUE))
    })
    
    tibble(
      tau2      = tau2,
      mean_RMSE = mean(rmse_vec, na.rm = TRUE)   # drop NAs
    )
  })
  
  # choose τ² with smallest finite RMSE -----------------------------
  tau2_hat <- grid_results %>%
    filter(is.finite(mean_RMSE)) %>%
    slice_min(mean_RMSE, n = 1, with_ties = FALSE) %>%
    pull(tau2)
  
  list(
    tau2_hat   = tau2_hat,
    grid_table = grid_results
  )
}

# Estimate tau2
tau2_estimate <- tune_tau2(nba_data, 
                            mu_hat = mu_nu2_estimate$mu_hat, 
                            nu2_hat = mu_nu2_estimate$nu2_hat, 
                            sigma2_hat = sigma2_estimate$sigma2_hat)
print(tau2_estimate)



# Task 6:
compute_posterior_series <- function(data,
                                     mu_hat,
                                     nu2_hat,
                                     sigma2_hat,
                                     tau2_hat) {
  
  data %>%
    filter(possessions > 0) %>%                      # guard against div-by-zero
    mutate(rate = pts / possessions,
           w    = 1 / (tau2_hat + nu2_hat / possessions)) %>%  # precision of each game
    arrange(idPlayer, dateGame) %>%
    group_by(idPlayer, namePlayer) %>%
    mutate(
      cumW      = cumsum(w),               # Σ w_{ik}
      cumSX     = cumsum(w * rate),        # Σ w_{ik}·X_{ik}
      post_mean = (mu_hat / sigma2_hat + cumSX) /
        (1 / sigma2_hat        + cumW),
      game_idx  = row_number()
    ) %>%
    ungroup()
}

# ------------------------------------------------------------------
# 2.  Convenience plot function ------------------------------------
# ------------------------------------------------------------------
plot_posterior_trajectories <- function(series, players, mu_hat) {
  ggplot(
    series %>% filter(namePlayer %in% players),
    aes(x = game_idx, y = post_mean, colour = namePlayer)
  ) +
    geom_line(size = 0.9) +
    geom_hline(yintercept = mu_hat, linetype = "dashed") +
    labs(
      title = "Rolling Empirical-Bayes Scoring Efficiency",
      x = "Game number",
      y = "Posterior mean (points per possession)"
    ) +
    theme_minimal() +
    theme(legend.title = element_blank())
}

# Compute posterior series
posterior_series <- compute_posterior_series(nba_data, 
                                              mu_hat = mu_nu2_estimate$mu_hat, 
                                              nu2_hat = mu_nu2_estimate$nu2_hat, 
                                              sigma2_hat = sigma2_estimate$sigma2_hat, 
                                              tau2_hat = tau2_estimate$tau2_hat)
print(head(posterior_series))

# Plot posterior trajectories for top players
top_names <- c("Stephen Curry", "Kevin Durant", "LeBron James")

plot_posterior_trajectories(posterior_series, top_names, mu_nu2_estimate$mu_hat) 


##########################
### NFL KICKER QUALITY ###
##########################

# load data
kick_data = read_csv("../data/12_field-goals.csv.gz")
# preview data
head(kick_data)

# drop kickers with less than 10 kicks
kick_data <- kick_data %>%
  group_by(kicker) %>%
  filter(n() >= 10) %>%
  ungroup()


fg_model <- glm(fg_made ~ splines::bs(ydl, df = 4), data    = kick_data, family  = binomial("logit"))

get_fg_model <- function(df, df_spline = 4) {
  glm(
    fg_made ~ ns(ydl, df = df_spline),
    data    = df %>% drop_na(fg_made, ydl),
    family  = binomial(link = "logit")
  )
}

estimate_kq <- function(kicks,
                        alpha    = 0.995,   # decay factor
                        fg_model = NULL) {   # allow re-use of pre-fit model
  
  if (is.null(fg_model)) fg_model <- get_fg_model(kicks)  # fit once if absent
  
  kicks <- kicks %>%
    arrange(kicker, season, week) %>%        # chronological order
    mutate(
      base_prob = predict(fg_model, newdata = ., type = "response"),
      FGPA      = fg_made - base_prob        # field-goal probability added
    )
  
  # ---- compute prior quality before each kick -------------------
  kq_df <- kicks %>%
    group_by(kicker) %>%
    mutate(
      KQ_prior = {
        kq <- numeric(n())                  # KQ_i1 = 0 implicitly
        for (j in 2:length(kq)) {
          kq[j] <- alpha * kq[j - 1] + FGPA[j - 1]
        }
        kq
      },
      kick_idx = row_number()
    ) %>%
    ungroup()
  
  list(kq_series = kq_df, fg_model = fg_model, alpha = alpha)
}

# -----------------------------------------------------------------
# 2.  Plotting helper for any subset of kickers
# -----------------------------------------------------------------
plot_kq <- function(kq_df, kicker_names) {
  ggplot(kq_df %>% filter(kicker %in% kicker_names),
         aes(x = kick_idx, y = KQ_prior, colour = kicker)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Rolling kicker quality (prior to each kick)",
         x = "Kick number in career",
         y = "KQ prior") +
    theme_minimal() +
    theme(legend.title = element_blank())
}

# -----------------------------------------------------------------
# 3.  Example run
# -----------------------------------------------------------------
kq_out <- estimate_kq(kick_data, alpha = 0.995)

plot_kq(
  kq_out$kq_series,
  kicker_names = c("J.Tucker", "M.Bryant")   
)
