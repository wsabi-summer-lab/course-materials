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
  part_a <- part_a %>%
    mutate(
      decimal_odds = if_else(
        quote_type == "american",
        american_to_decimal(quote),
        contract_to_decimal(quote)
      ),
      break_even_prob = break_even_prob(decimal_odds),
      EV_per_dollar = expected_value_per_dollar(model_p, decimal_odds),
      full_kelly = kelly_fraction(model_p, decimal_odds),
      half_kelly = full_kelly / 2,
      quarter_kelly = full_kelly / 4,
      full_kelly_dollar = full_kelly * bankroll0,
      half_kelly_dollar = half_kelly * bankroll0,
      quarter_kelly_dollar = quarter_kelly * bankroll0
    )

  part_a
}

solve_part_a(part_a, bankroll0 = 1000)
#best bets: team A ML(0.1 per 1), Underdog(0.085 per 1), prediction contract(0.0794 per 1)
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
  two_sided <- two_sided %>%
    mutate(
      side_a_dec = american_to_decimal(side_a),
      side_b_dec = american_to_decimal(side_b),
      q_a = 1/side_a_dec,
      q_b = 1/side_b_dec,
      hold = q_a + q_b -1,
      q_a_tilde = q_a/(q_a + q_b),
      q_b_tilde = q_b/(q_a + q_b),
      diff_a = model_p_side_a - q_a_tilde, 
      diff_b = (1-model_p_side_a) - q_b_tilde
    )
  two_sided
  
}
solve_part_b(two_sided)

#biggest differences: side_a on Game 2(0.03), side_a on game 3 (0.0225), side_a on game 1(0.202)
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
  p = leg1_p * leg2_p
  decimal_odds = american_to_decimal(american_odds)
  break_even = 1/decimal_odds
  EV = p * decimal_odds - 1
  full_kelly = kelly_fraction(p, decimal_odds)

  tibble(
    parlay_p = p,
    decimal_odds = decimal_odds,
    break_even = break_even,
    EV_per_dollar = EV,
    full_kelly = full_kelly
  )
}

analyze_parlay(parlay$leg1_p, parlay$leg2_p, parlay$american_odds)

#correlated example: treating as separate Kelly fractions misprices their probabilities are tied, meaning that it is similar to placing a single Kelly bet with the sum of their fractions
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
  b <- break_even_prob(decimal_odds)
  if (strategy == "flat1") {
    if (p > b) 0.01 else 0
  } else if (strategy == "kelly") {
    kelly_fraction(p, decimal_odds)
  } else if (strategy == "half") {
    kelly_fraction(p, decimal_odds) / 2
  } else if (strategy == "quarter") {
    kelly_fraction(p, decimal_odds) / 4
  } else if (strategy == "reckless10") {
    if (p > b) 0.1 else 0
  }
}

simulate_path <- function(board, n_steps = 500, bankroll0 = 1000, strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  strategy <- match.arg(strategy)
  board <- board %>% filter(EV_per_dollar > 0)
  bankroll <- bankroll0
  results <- tibble(step = 0, bankroll = bankroll0)
  for (i in 1:n_steps) {
    bet <- sample_bet(board)
    frac <- strategy_fraction(strategy, bet$model_p, bet$decimal_odds)
    result <- rbinom(1, 1, bet$model_p)
    bankroll <- bankroll * (1 + frac * (bet$decimal_odds * result - 1))
    results <- bind_rows(results, tibble(step = i, bankroll = bankroll))
  }
  results
}
board_a <- solve_part_a(part_a)
strategies <- c("flat1", "kelly", "half", "quarter", "reckless10")

all_strategy_paths <- map_dfr(
  strategies,
  function(s) simulate_path(board_a, strategy = s) %>% mutate(strategy = s)
)

all_strategy_paths
simulate_many_paths <- function(board, n_paths = 1000, n_steps = 500, bankroll0 = 1000, strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  strategy <- match.arg(strategy)

  map_dfr(
    1:n_paths,
    function(path_id) {
      simulate_path(board, n_steps = n_steps, bankroll0 = bankroll0, strategy = strategy) %>%
        mutate(path = path_id)
    }
  ) %>%
    mutate(strategy = strategy)
}
many <- map_dfr(
  strategies,
  function(s) simulate_many_paths(board_a, strategy = s)
)

many %>% filter(step == 500) %>%
  group_by(strategy) %>%
  summarize(mean = mean(bankroll), median = median(bankroll), ruin = mean(bankroll < 1))

many %>%
  group_by(strategy, step) %>%
  summarize(
    median_bankroll = median(bankroll),
    p10 = quantile(bankroll, 0.10),
    p90 = quantile(bankroll, 0.90),
    .groups = "drop"
  ) %>%
  ggplot(aes(step, color = strategy, fill = strategy)) +
  geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.15, color = NA) +
  geom_line(aes(y = median_bankroll)) +
  scale_y_log10() +
  labs(
    x = "Bet number",
    y = "Bankroll ($, log scale)",
    title = "Bankroll over time by staking strategy",
    subtitle = "Line = median, band = 10th-90th percentile"
  )
############################
### PART E UNCERTAINTY #####
############################

add_model_noise <- function(p_true, sd_eps = 0.03) {
  p_hat <- p_true + rnorm(length(p_true), mean = 0, sd = sd_eps)
  pmin(0.99, pmax(0.01, p_hat))
}

simulate_path_noisy <- function(board, n_steps = 500, bankroll0 = 1000,
                                strategy = c("kelly", "half", "quarter"),
                                sd_eps = 0.03) {
  strategy <- match.arg(strategy)
  board <- board %>% filter(EV_per_dollar > 0)
  bankroll <- bankroll0
  results <- tibble(step = 0, bankroll = bankroll0)
  for (i in 1:n_steps) {
    bet   <- sample_bet(board)
    p_hat <- add_model_noise(bet$model_p, sd_eps)            # what you THINK
    frac  <- strategy_fraction(strategy, p_hat, bet$decimal_odds)  # size with belief
    win   <- rbinom(1, 1, bet$model_p)                       # outcome from TRUTH
    bankroll <- bankroll * (1 + frac * (bet$decimal_odds * win - 1))
    results <- bind_rows(results, tibble(step = i, bankroll = bankroll))
  }
  results
}

# TODO:
# 1. Simulate outcomes using p_true.
# 2. Size bets using noisy p_hat.
# 3. Compare full Kelly, half Kelly, and quarter Kelly under model error.
