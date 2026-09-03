#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(8)

#########################
### PART 1: NBA FREE THROWS ###
#########################

# load data
nba_players = read_delim(
  "../data/08_nba-free-throws.csv.gz",
  show_col_types = FALSE
)

# Task 1:
# - Modify the dataset to include:
#   * Player
#   * FT_total = total free throws made across the season
#   * FTA_total = total free throws attempted across the season
#   * FT_percent = FT_total / FTA_total
# - Remember that FT and FTA are per-game values, so convert them to totals using G

nba_players <-nba_players%>% 
  mutate(FT_total = G*FT, FTA_total = G*FTA, FT_percent = FT_total/FTA_total)%>%
  select(c(Player, FT_total, FTA_total, FT_percent))




# Task 2:
# - Filter the dataset to players with at least 25 total free-throw attempts
# - Decide how you want to handle players with multiple team rows
# - Make sure the final player-level dataset has one row per player-season

nba_players_summary <- nba_players %>%
  group_by(Player) %>%
  summarise(
    FT_total = sum(FT_total, na.rm = TRUE),
    FTA_total = sum(FTA_total, na.rm = TRUE),
    FT_percent = FT_total / FTA_total,
    .groups = "drop"
  )
nba_players_summary <- nba_players_summary%>%
  filter(FTA_total>=25)

# Task 3:
# - Construct 95% Wald confidence intervals for each player's free-throw probability
# - Construct 95% Agresti-Coull confidence intervals for each player's free-throw probability
# - Make a plot with:
#   * x-axis = FT_percent
#   * y-axis = player name
#   * both interval types overlaid
# - Comment on which intervals look most different and why

nba_players_ci <- nba_players_summary %>%
  mutate(
    se = sqrt(FT_percent * (1 - FT_percent) / FTA_total),
    lower_95 = pmax(0, FT_percent - 1.96 * se),
    upper_95 = pmin(1, FT_percent + 1.96 * se)
  )

nba_players_ci

nba_players_ci <- nba_players_ci %>%
  mutate(
    n_tilde = FTA_total + 1.96^2,
    p_tilde = (FT_total + (1.96^2)/2) / n_tilde,
    
    lower_95AC = pmax(
      0,
      p_tilde - 1.96 * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
    ),
    
    upper_95AC = pmin(
      1,
      p_tilde + 1.96 * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
    )
  ) 

nba_players_ci



ggplot(nba_players_ci, aes(y = reorder(Player, FT_percent))) +
  
  geom_errorbarh(
    aes(xmin = lower_95, xmax = upper_95),
    height = 0.2,
    linewidth = 0.8
  ) +
  
  geom_errorbarh(
    aes(xmin = lower_95AC, xmax = upper_95AC),
    height = 0.45,
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  
  geom_point(aes(x = FT_percent), size = 2) +
  
  labs(
    x = "Free Throw Percentage",
    y = "Player",
    title = "95% Confidence Intervals for Free Throw Probability",
    subtitle = "Solid = Wald interval; Dashed = Agresti-Coull interval"
  ) +
  
  theme_minimal()

#looks most different at beginning and end


###########################
### PART 2: LLN WARM-UP ###
###########################

lln_p = 0.75
lln_n = 1000
lln_paths = 100

# Task 1:
# - Simulate one Bernoulli sequence with true probability p = 0.75 and length 1000
# - Compute the running estimate
#     phat_n = (1 / n) * sum_{i=1}^n X_i
#   for n = 1, ..., 1000
bernoulli_seq <- rbinom(n = 1000, size = 1, p = .75)

# Task 2:
# - Plot the running estimate against n
# - Add a horizontal line at p = 0.75
# - Describe what happens as n grows

plot(
  running_estimate,
  type = "l",
  xlab = "n",
  ylab = "Running estimate of p",
  main = "Running Estimate of Bernoulli Probability"
)

abline(h = 0.75, lty = 2)

#As n increases, the bernoulli probability runs below the true probability of .75, albeit pretty close. 

# Task 3:
# - Repeat the simulation 100 times
# - Overlay the 100 running-estimate paths on one figure
# - Describe how the variability changes with n

phat_list <- vector("list", 100)

for(i in 1:100){
  bernoulli_seq <- rbinom(1000, 1, 0.75)
  phat_list[[i]] <- cumsum(bernoulli_seq) / (1:1000)
}

plot(
  phat_list[[1]],
  type = "l",
  ylim = c(0, 1),
  xlab = "n",
  ylab = expression(hat(p)[n]),
  main = "100 Running Estimates"
)

for(i in 2:100){
  lines(phat_list[[i]])
}

abline(h = 0.75, col = "red", lty = 2, lwd = 2)

#converges to around .75.


################################
### PART 3: SIMULATION STUDY ###
################################

z_975 = qnorm(0.975)
p_grid = seq(0, 1, length.out = 1000)
sample_sizes = c(10, 50, 100, 250, 500, 1000)
M = 100

# A convenient helper structure is:
# - loop over p
# - loop over n
# - simulate Binomial(n, p) data
# - compute both intervals
# - record whether each interval contains the true p

# Task 1:
# - Use the grid p_grid as your set of candidate true probabilities
# - For each p and each n in sample_sizes, generate binomial data

# Task 2:
# - Compute the 95% Wald confidence interval
# - Compute the 95% Agresti-Coull confidence interval using
#     n_tilde = n + z_975^2
#     p_tilde = (S_n + z_975^2 / 2) / n_tilde
# - Interpret this as approximately adding 2 successes and 2 failures
wald_cov <- matrix(NA,
                  nrow = length(p_grid),
                  ncol = length(sample_sizes))

ac_cov <- matrix(NA,
                 nrow = length(p_grid),
                 ncol = length(sample_sizes))

for(j in 1:length(sample_sizes)) {
  
  n <- sample_sizes[j]
  
  for(i in 1:length(p_grid)) {
    
    p <- p_grid[i]
    
    wald_contains <- rep(FALSE, M)
    ac_contains <- rep(FALSE, M)
    
    for(m in 1:M) {
      
      x <- rbinom(1, n, p)
      
      phat <- x / n
      
      se <- sqrt(phat * (1 - phat) / n)
      
      wald_lower <- phat - z_975 * se
      wald_upper <- phat + z_975 * se
      
      wald_contains[m] <- (wald_lower <= p) &
        (p <= wald_upper)
      
    
      n_tilde <- n + z_975^2
      p_tilde <- (x + z_975^2 / 2) / n_tilde
      
      se_ac <- sqrt(p_tilde * (1 - p_tilde) / n_tilde)
      
      ac_lower <- p_tilde - z_975 * se_ac
      ac_upper <- p_tilde + z_975 * se_ac
      
      ac_contains[m] <- (ac_lower <= p) &
        (p <= ac_upper)
    }
    
    wald_cov[i, j] <- mean(wald_contains)
    ac_cov[i, j] <- mean(ac_contains)
  }
}
# Task 4:
# - Plot coverage probability vs p
# - Facet by sample size
# - Include both the Wald and Agresti-Coull methods on the same figure
# - Add a horizontal reference line at 0.95
# - Comment on where the Wald interval undercovers

coverage_df <- expand.grid(
  p = p_grid,
  n = sample_sizes
)

coverage_df$wald_coverage <- as.vector(wald_cov)
coverage_df$ac_coverage <- as.vector(ac_cov)

ggplot(coverage_df, aes(x = p)) +
  
  geom_line(aes(y = wald_coverage,
                color = "Wald")) +
  
  geom_line(aes(y = ac_coverage,
                color = "Agresti-Coull")) +
  
  geom_hline(yintercept = 0.95,
             linetype = "dashed") +
  
  facet_wrap(~ n) +
  
  labs(
    x = "True p",
    y = "Coverage Probability",
    color = "Method",
    title = "Coverage Probability vs True p"
  ) +
  
  theme_minimal()
#Wald interval undercovers mostly, but especially at the tails

############################
### PART 4: SKITTLES DEMO ###
############################

# Replace these with your observed counts from the Skittles activity
skittles_n = 106
skittles_r = 16

# Task 1:
# - Compute the observed red proportion r / n
# - Construct a 95% Wald confidence interval for the true red probability
red_prop <- 16/106
upper95 <- red_prop + 1.96*sqrt((red_prop*(1-red_prop))/skittles_r)
lower95 <- red_prop - 1.96*sqrt((red_prop*(1-red_prop))/skittles_r)

print(red_prop)
print(upper95)
print(lower95)



# Task 2:
# - Construct a 95% Agresti-Coull confidence interval for the same probability
n_tilde = skittles_r + 1.96^2
p_tilde = (red_prop + (1.96^2)/2) / n_tilde
print(p_tilde)
lower_95AC = p_tilde - 1.96 * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
upper_95AC = p_tilde + 1.96 * sqrt(p_tilde * (1 - p_tilde) / n_tilde)

print(lower_95AC)
print(upper_95AC)
# Task 3:
# - Compare the two intervals
# - State which one seems more sensible when the observed red proportion is near 0 or 1
#the Wald interval seems much wider than AC as of now. When the observed red proportion gets close to either tail, I think it'll be more inaccurate as it will go into ranges that aren't possible.