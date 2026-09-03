#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

help(expand.grid)
alphas=1+ppoints(90, a = .5)*9
alphas
betas=1+ppoints(90, a = .5)*9
# create a grid of alpha and beta values
grid = expand.grid(alpha = alphas, beta = betas)
grid
set.seed(11)

#######################
### NBA FREE THROWS ###
#######################

# load data
nba_players = read_delim("../data/11_nba-free-throws.csv.gz", delim = ";")
glimpse(nba_players)
nba_players = nba_players %>%
  mutate(FTA=round(FTA*G,0), 
         FT=round(FT*G,0)) %>% 
  select(Player, FTA,FT,G) %>% 
  group_by(Player) %>% 
  summarise(FTA=sum(FTA), FT=sum(FT), FTP=FT/FTA, G=sum(G)) %>%
  filter(FTA > 25) %>% 
  mutate(AC=(FT+2)/(FTA+4))
glimpse(nba_players)

nba_players<-nba_players %>%
  mutate(WaldL=FTP-1.96*sqrt((FTP-FTP^2)/G),
         WaldH=FTP+1.96*sqrt((FTP-FTP^2)/G),
         ACL=AC-1.96*sqrt((AC-AC^2)/(G+4)),
         ACH=AC+1.96*sqrt((AC-AC^2)/(G+4))) %>% 
  slice_sample(n=30)
# plot free throw percentage with confidence intervals
ggplot(nba_players, aes(x=FTP, y=fct_reorder(Player,FTP))) +
  geom_point() +
  geom_errorbarh(aes(xmin=WaldL, xmax=WaldH), height=0.2, color="blue") +
  geom_errorbarh(aes(xmin=ACL, xmax=ACH), height=0.2, color="red") +
  labs(title="NBA Free Throw Percentage with Confidence Intervals",
       x="Free Throw Percentage",
       y="Player") +
  theme_minimal()
#calculate the intervals for each alpha and beta
help(rowwise)
# Make sure grid has 90 alpha-beta pairs
nrow(grid)  # should be 90

# Create expanded grid: 30 players × 90 alpha-beta pairs = 2700 rows
nba_expanded <- nba_players %>%
  mutate(player_id = row_number()) %>% 
  select(player_id, Player, FT, FTA) %>%
  crossing(grid) %>%  # creates all combinations
  mutate(
    BSL = qbeta(0.025, FT + alpha, (FTA - FT) + beta),
    BSH = qbeta(0.975, FT + alpha, (FTA - FT) + beta)
  )
unique(nba_expanded$Player)
#plot the possible confidence intervals using gradients
ggplot(nba_expanded, aes(x = BSL, xend = BSH, y = Player, group = interaction(player_id, alpha, beta))) +
  geom_segment(aes(xend = BSH), color = "blue", size = 0.5) +
  geom_point(aes(x = (BSL + BSH) / 2), color = "red", size = 1.5) +
  labs(title = "Confidence Intervals for NBA Players' Free Throw Percentages",
       x = "Confidence Interval",
       y = "Player ID") +
  theme_minimal() 

# Generate vector of 1000 probabilities using ppoints (avoids 0 and 1)
p_seq <- ppoints(1000, a = 0.5)

# Define prior "weights" — i.e., how strong the prior belief is
weights <- 1:8

# Number of trials per simulation (binomial sample size)
n_trials <- 100

# Number of simulations per (p, weight) pair
n_sim <- 500

# Alpha level for CI
alpha <- 0.025

# Function to simulate whether posterior CI contains true p
simulate_coverage <- function(p_true, weight, n_trials, n_sim) {
  mean(replicate(n_sim, {
    # Simulate binomial observation
    x <- rbinom(1, size = n_trials, prob = p_true)
    
    # Prior is centered at p_true: Beta(weight * p, weight * (1 - p))
    alpha_post <- x + weight * p_true
    beta_post  <- (n_trials - x) + weight * (1 - p_true)
    
    # 95% credible interval
    ci <- qbeta(c(alpha, 1 - alpha), alpha_post, beta_post)
    
    # Check if true p is in the interval
    p_true >= ci[1] && p_true <= ci[2]
  }))
}

##########################
### RUN COVERAGE SIMS  ###
##########################

# Expand grid over p and weight, then simulate coverage
coverage_df <- expand_grid(p_true = p_seq, weight = weights) %>%
  group_by(p_true, weight) %>%
  summarise(
    coverage = simulate_coverage(p_true, weight, n_trials, n_sim),
    .groups = "drop"
  )

####################
### PLOT RESULTS ###
####################

ggplot(coverage_df, aes(x = p_true, y = coverage, color = factor(weight))) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  labs(title = "Bayesian 95% Coverage Across True Probabilities",
       subtitle = paste("Sample Size:", n_trials, "– Simulations per Point:", n_sim),
       x = "True Probability (p)",
       y = "Coverage Probability",
       color = "Prior Weight") +
  theme_minimal()

