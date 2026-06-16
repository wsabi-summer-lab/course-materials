#############
### SETUP ###
#############

# install.packages(c("ggplot2", "dplyr", "purrr", "tibble", "stringr"))
library(ggplot2)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

set.seed(10)

bankroll0 <- 1000

##########################
### HELPER FUNCTIONS #####
##########################

# These low-level pricing helpers are provided for you.
# You do not need to rewrite them unless you want to.

american_to_decimal <- function(a) {
  ifelse(a > 0, 1 + a / 100, 1 + 100 / abs(a))
}

contract_to_decimal <- function(price) {
  1 / price
}

break_even_prob <- function(decimal_odds) {
  1 / decimal_odds
}

expected_value_per_dollar <- function(p, decimal_odds) {
  decimal_odds * p - 1
}

kelly_fraction <- function(p, decimal_odds) {
  pmax(0, (decimal_odds * p - 1) / (decimal_odds - 1))
}

remove_vig_2way <- function(decimal_a, decimal_b) {
  q_a <- 1 / decimal_a
  q_b <- 1 / decimal_b
  tibble(
    raw_a = q_a,
    raw_b = q_b,
    hold = q_a + q_b - 1,
    novig_a = q_a / (q_a + q_b),
    novig_b = q_b / (q_a + q_b)
  )
}

########################
### PART A DATA ########
########################

part_a <- tibble(
  bet = c(
    "Team A moneyline",
    "Team B moneyline",
    "Spread bet",
    "Prediction contract",
    "Underdog",
    "Favorite"
  ),
  quote_type = c("american", "american", "american", "contract", "american", "american"),
  quote = c(140, -160, -110, 0.63, 250, -220),
  model_p = c(0.46, 0.59, 0.55, 0.68, 0.31, 0.66)
)

solve_part_a <- function(part_a, bankroll0 = 1000) {
  # TODO:
  # 1. Create decimal_odds for every row.
  # 2. Compute break-even probability.
  # 3. Compute EV per $1 staked.
  # 4. Compute full / half / quarter Kelly fractions.
  # 5. Convert Kelly fractions into dollar stakes using bankroll0.

  stop("Implement solve_part_a().")
}

########################
### PART B DATA ########
########################

two_sided <- tibble(
  market = c("Game 1", "Game 2", "Game 3"),
  side_a = c(-150, -110, 105),
  side_b = c(130, -110, -125),
  model_p_side_a = c(0.60, 0.53, 0.49)
)

solve_part_b <- function(two_sided) {
  # TODO:
  # 1. Convert side_a and side_b to decimal odds.
  # 2. Compute raw implied probabilities, hold, and no-vig probabilities.
  # 3. Compare model_p_side_a to the no-vig probability for side_a.

  stop("Implement solve_part_b().")
}

############################
### PART C EXTENSIONS ######
############################

parlay <- tibble(
  leg1_p = 0.58,
  leg2_p = 0.54,
  american_odds = 290
)

correlated_example <- tibble(
  bet = c("Eagles -3.5", "QB over 1.5 pass TD"),
  american_odds = c(-110, 120),
  model_p = c(0.55, 0.49)
)

analyze_parlay <- function(leg1_p, leg2_p, american_odds) {
  # TODO:
  # 1. Compute the parlay win probability under independence.
  # 2. Convert the offered odds to decimal odds.
  # 3. Compute break-even probability, EV, and full Kelly.

  stop("Implement analyze_parlay().")
}

############################
### PART D SIMULATION ######
############################

sample_bet <- function(board) {
  board %>% slice_sample(n = 1)
}

strategy_fraction <- function(strategy, p, decimal_odds) {
  # TODO:
  # Return the bankroll fraction implied by:
  # "flat1", "kelly", "half", "quarter", or "reckless10".

  stop("Implement strategy_fraction().")
}

simulate_path <- function(board, n_steps = 500, bankroll0 = 1000, strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  strategy <- match.arg(strategy)

  # TODO:
  # 1. At each step, sample one positive-EV bet from board.
  # 2. Convert the strategy into a bankroll fraction.
  # 3. Simulate win/loss using the true probability.
  # 4. Update bankroll multiplicatively.
  # 5. Return a tibble with step and bankroll.

  stop("Implement simulate_path().")
}

simulate_many_paths <- function(board, n_paths = 1000, n_steps = 500, bankroll0 = 1000, strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  strategy <- match.arg(strategy)

  # TODO:
  # Repeatedly call simulate_path() and combine the results.

  stop("Implement simulate_many_paths().")
}

############################
### PART E UNCERTAINTY #####
############################

add_model_noise <- function(p_true, sd_eps = 0.03) {
  p_hat <- p_true + rnorm(length(p_true), mean = 0, sd = sd_eps)
  pmin(0.99, pmax(0.01, p_hat))
}

# TODO:
# 1. Simulate outcomes using p_true.
# 2. Size bets using noisy p_hat.
# 3. Compare full Kelly, half Kelly, and quarter Kelly under model error.


