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
#   * FT_total = approximate total free throws made across the season
#   * FTA_total = approximate total free throws attempted across the season
#   * FT_percent = FT_total / FTA_total
# - Basketball Reference reports FT and FTA as per-game values in this table,
#   so convert them to approximate totals using G before treating them as counts

# Task 2:
# - Filter the dataset to players with at least 25 approximate total free-throw attempts
# - Decide how you want to handle players with multiple team rows
# - Make sure the final player-level dataset has one row per player-season

# Task 3:
# - Construct 95% Wald confidence intervals for each player's free-throw probability
# - Construct 95% Agresti-Coull confidence intervals for each player's free-throw probability
# - Make a plot with:
#   * x-axis = FT_percent
#   * y-axis = player name
#   * both interval types overlaid
# - Comment on which intervals look most different and why

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

# Task 2:
# - Plot the running estimate against n
# - Add a horizontal line at p = 0.75
# - Describe what happens as n grows

# Task 3:
# - Repeat the simulation 100 times
# - Overlay the 100 running-estimate paths on one figure
# - Describe how the variability changes with n

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

# Task 3:
# - Repeat the simulation M = 100 times for each (n, p) pair
# - For each method, estimate coverage as the fraction of intervals that contain p

# Task 4:
# - Plot coverage probability vs p
# - Facet by sample size
# - Include both the Wald and Agresti-Coull methods on the same figure
# - Add a horizontal reference line at 0.95
# - Comment on where the Wald interval undercovers

############################
### PART 4: SKITTLES DEMO ###
############################

# Replace these with your observed counts from the Skittles activity
skittles_n = NA
skittles_r = NA

# Task 1:
# - Compute the observed red proportion r / n
# - Construct a 95% Wald confidence interval for the true red probability

# Task 2:
# - Construct a 95% Agresti-Coull confidence interval for the same probability

# Task 3:
# - Compare the two intervals
# - State which one seems more sensible when the observed red proportion is near 0 or 1
