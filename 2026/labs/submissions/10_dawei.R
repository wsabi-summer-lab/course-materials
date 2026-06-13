#############
### SETUP ###
#############

install.packages(c("ggplot2", "dplyr", "purrr", "tibble", "stringr"))
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

part_a$decimal <- american_to_decimal(part_a$quote)
part_a$decimal[4] <- contract_to_decimal(part_a$quote[4])
part_a$break_even_prob <- break_even_prob(part_a$decimal)

part_a$diff <- part_a$model_p - part_a$break_even_prob

part_a$ev_label <- ifelse(diff > 0.025, "positive",
                          ifelse(diff < -0.025, "negative", "neutral"))

# 1. Prediction Contract
# 2. Team A moneyline
# 3. Spread bet
# 4. Underdog
# 5. Team B moneyline
# 6. Favorite

########################
### PART B DATA ########
########################

two_sided <- tibble(
  market = c("Game 1", "Game 2", "Game 3"),
  side_a = c(-150, -110, 105),
  side_b = c(130, -110, -125),
  model_p_side_a = c(0.60, 0.53, 0.49)
)

two_sided$decimal_odds_a <- american_to_decimal(two_sided$side_a)
two_sided$decimal_odds_b <- american_to_decimal(two_sided$side_b)

two_sided$implied_p_a <- break_even_prob(two_sided$decimal_odds_a)
two_sided$implied_p_b <- break_even_prob(two_sided$decimal_odds_b)

two_sided$sportsbook_hold <- two_sided$implied_p_a + two_sided$implied_p_b - 1

two_sided$implied_p_a_norm <- two_sided$implied_p_a / (two_sided$implied_p_a + two_sided$implied_p_b)
two_sided$implied_p_b_norm <- two_sided$implied_p_b / (two_sided$implied_p_a + two_sided$implied_p_b)

two_sided$prob_diff <- two_sided$model_p_side_a - two_sided$implied_p_a_norm

# 1. Game 2
# 2. Game 3
# 3. Game 1


solve_part_b <- function(two_sided) {
  # TODO:
  # 1. Convert side_a and side_b to decimal odds.
  # 2. Compute raw implied probabilities, hold, and no-vig probabilities.
  # 3. Compare model_p_side_a to the no-vig probability for side_a.

  stop("Implement solve_part_b().")
}

############################
### PART C #################
############################

part_a_positive <- part_a[-c(2,6),]
part_a_positive$full_kelly_stake <- kelly_fraction(part_a_positive$model_p, part_a_positive$decimal)
part_a_positive$half_kelly_stake <- 0.5 * kelly_fraction(part_a_positive$model_p, part_a_positive$decimal)
part_a_positive$quarter_kelly_stake <- 0.25 * kelly_fraction(part_a_positive$model_p, part_a_positive$decimal)
part_a_positive$onepercent_stake <- 0.01

part_a_positive$full_kelly_dollar <- 1000 * part_a_positive$full_kelly_stake
part_a_positive$half_kelly_dollar <- 1000 * part_a_positive$half_kelly_stake
part_a_positive$quarter_kelly_dollar <- 1000 * part_a_positive$quarter_kelly_stake
part_a_positive$onepercent_dollar <- 1000 * part_a_positive$onepercent_stake

# 1: 3
# 2: 1
# 3: 2
# 4: 4

# It is not always the bet with the highest win probability; it also takes into consideration the odds
# and how good they are compared to the model win probability.

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

Parlay_win_prob = parlay$leg1_p * parlay$leg2_p
Parlay_decimal_odds = american_to_decimal(parlay$american_odds)
Parlay_break_even = break_even_prob(Parlay_decimal_odds)
Parlay_expected_return = Parlay_win_prob / Parlay_break_even
Parlay_kelly_fraction = kelly_fraction(Parlay_win_prob, Parlay_decimal_odds)

# If the two events are correlated, so the Kelly model will fail to account for joint losses.

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

# Betting board - already has all columns
board <- part_a_positive |> filter(ev_label == "positive")

n_paths <- 1000
n_bets  <- 500
starting_bankroll <- 1000

strategies <- list(
  flat       = function(bankroll, kelly) 0.01 * bankroll,
  full_kelly = function(bankroll, kelly) kelly * bankroll,
  half_kelly = function(bankroll, kelly) 0.5 * kelly * bankroll,
  qtr_kelly  = function(bankroll, kelly) 0.25 * kelly * bankroll,
  reckless   = function(bankroll, kelly) 0.10 * bankroll
)

simulate_paths <- function(stake_fn) {
  paths <- matrix(NA, nrow = n_paths, ncol = n_bets + 1)
  paths[, 1] <- starting_bankroll
  
  for (i in 1:n_paths) {
    print(i)
    bankroll <- starting_bankroll
    for (t in 1:n_bets) {
      bet      <- board[sample(nrow(board), 1), ]
      stake    <- stake_fn(bankroll, bet$full_kelly_stake)
      win      <- rbinom(1, 1, bet$model_p)
      bankroll <- if (win) bankroll + stake * (bet$decimal - 1) else bankroll - stake
      bankroll <- max(bankroll, 0.01)
      paths[i, t + 1] <- bankroll
    }
  }
  paths
}

# Run all strategies
results <- lapply(strategies, simulate_paths)

strategy_names <- names(strategies)
steps <- 0:n_bets

# Helper: extract percentile paths
summarise_paths <- function(mat, name) {
  data.frame(
    step     = steps,
    median   = apply(mat, 2, median),
    p10      = apply(mat, 2, quantile, 0.10),
    p90      = apply(mat, 2, quantile, 0.90),
    strategy = name
  )
}

summary_df <- bind_rows(mapply(summarise_paths, results, strategy_names, SIMPLIFY = FALSE))

# Task 1 & 2: Median + percentile paths
ggplot(summary_df, aes(x = step, color = strategy)) +
  geom_line(aes(y = median), linewidth = 1) +
  geom_ribbon(aes(ymin = p10, ymax = p90, fill = strategy), alpha = 0.1, color = NA) +
  scale_y_log10() +
  labs(title = "Bankroll Growth by Strategy", x = "Bet", y = "Bankroll (log scale)") +
  theme_bw()

# Task 3: P(lose 50% of bankroll at some point)
ruin_prob <- sapply(results, function(mat) {
  mean(apply(mat, 1, function(path) any(path <= starting_bankroll * 0.5)))
})
print(round(ruin_prob, 3))

# Task 4: Geometric mean final bankroll
geo_mean_final <- sapply(results, function(mat) {
  exp(mean(log(mat[, n_bets + 1])))
})
print(round(geo_mean_final, 2))

############################
### PART E UNCERTAINTY #####
############################

simulate_paths_noisy <- function(stake_fn) {
  paths <- matrix(NA, nrow = n_paths, ncol = n_bets + 1)
  paths[, 1] <- starting_bankroll
  
  for (i in 1:n_paths) {
    print(i)
    bankroll <- starting_bankroll
    for (t in 1:n_bets) {
      bet <- board[sample(nrow(board), 1), ]
      
      # Add noise to model probability
      p_hat <- bet$model_p + rnorm(1, 0, 0.03)
      p_hat <- pmin(pmax(p_hat, 0.01), 0.99)  # truncate to [0.01, 0.99]
      
      # Compute noisy Kelly stake using p_hat
      noisy_kelly <- (p_hat * bet$decimal - 1) / (bet$decimal - 1)
      noisy_kelly <- max(noisy_kelly, 0)  # don't bet negative
      
      stake    <- stake_fn(bankroll, noisy_kelly)
      win      <- rbinom(1, 1, bet$model_p)  # outcome uses true probability
      bankroll <- if (win) bankroll + stake * (bet$decimal - 1) else bankroll - stake
      bankroll <- max(bankroll, 0.01)
      paths[i, t + 1] <- bankroll
    }
  }
  paths
}

# Only compare full, half, quarter Kelly (as per task 2)
strategies_e <- list(
  full_kelly = function(bankroll, kelly) kelly * bankroll,
  half_kelly = function(bankroll, kelly) 0.5 * kelly * bankroll,
  qtr_kelly  = function(bankroll, kelly) 0.25 * kelly * bankroll
)

results_e <- lapply(strategies_e, simulate_paths_noisy)

strategy_names_e <- names(strategies_e)

summary_df_e <- bind_rows(mapply(summarise_paths, results_e, strategy_names_e, SIMPLIFY = FALSE))

# Task 1 & 2: Plot
ggplot(summary_df_e, aes(x = step, color = strategy)) +
  geom_line(aes(y = median), linewidth = 1) +
  geom_ribbon(aes(ymin = p10, ymax = p90, fill = strategy), alpha = 0.1, color = NA) +
  scale_y_log10() +
  labs(title = "Bankroll Growth Under Model Uncertainty", x = "Bet", y = "Bankroll (log scale)") +
  theme_bw()

# Task 3 & 4: Ruin probability and geometric mean
ruin_prob_e <- sapply(results_e, function(mat) {
  mean(apply(mat, 1, function(path) any(path <= starting_bankroll * 0.5)))
})
print(round(ruin_prob_e, 3))

geo_mean_final_e <- sapply(results_e, function(mat) {
  exp(mean(log(mat[, n_bets + 1])))
})
print(round(geo_mean_final_e, 2))
