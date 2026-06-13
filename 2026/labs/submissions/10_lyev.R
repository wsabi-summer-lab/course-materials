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
  
  part_a = part_a %>%
    mutate(
      decimal = case_when(
        quote_type == "american" ~ american_to_decimal(quote),
        quote_type == "contract" ~ contract_to_decimal(quote)
      ),
      break_even_prob = break_even_prob(decimal),
      ev_one = expected_value_per_dollar(p = model_p, decimal_odds = decimal),
      value = case_when(
        ev_one > 0 ~ "Positive",
        ev_one == 0 ~ "Neutral",
        ev_one < 0 ~ "Negative"
      )
    ) %>%
    arrange(-ev_one)
  
  
  part_a = part_a %>%
    mutate(
      full_kelly = bankroll0 * (model_p * decimal - 1) / (decimal - 1),
      half_kelly = full_kelly / 2,
      quarter_kelly = full_kelly / 4,
      flat_one = bankroll0 * 0.01
    )
  
  # stop("Implement solve_part_a().")
}

part_a_solved = solve_part_a(part_a = part_a)

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
  
  two_sided = two_sided %>%
    mutate(
      decimal_a = american_to_decimal(side_a),
      decimal_b = american_to_decimal(side_b),
      implied_a = break_even_prob(decimal_a),
      implied_b = break_even_prob(decimal_b),
      hold = implied_a + implied_b - 1,
      no_vig_a = implied_a / (implied_a + implied_b),
      no_vig_b = implied_b / (implied_a + implied_b)
    )
  #My model overshoots the no vig for all three pretty equally, but game 2 the most
  # stop("Implement solve_part_b().")
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
  
  win_prob = leg1_p * leg2_p
  
  decimal = american_to_decimal(american_odds)
  
  break_even = break_even_prob(decimal)
  
  ev_one = expected_value_per_dollar(decimal_odds = decimal, p = win_prob)
  
  full_kelly_fraction = ifelse(ev_one >0, (win_prob * decimal - 1)/(decimal - 1), 0)
  
  analysis = tibble(win_prob = win_prob,
                    decimal = decimal,
                    break_even = break_even,
                    ev_one = ev_one,
                    full_kelly_fraction = full_kelly_fraction)
  
  # stop("Implement analyze_parlay().")
}

parlay_analyzed = analyze_parlay(0.58,0.54,290)

#Computing kelly bets for a correlated parlay can be inaccurate because the payoff could be the equivalent of doubling down on one bigger kelly bet
# A way to account for that could be to treat it as one bet with a certain payoff, and do kelly odds for that



############################
### PART D SIMULATION ######
############################

part_a_positives = part_a_solved %>%
  filter(value == "Positive")

sample_bet <- function(board) {
  board %>% slice_sample(n = 1)
}

sample_bet(part_a_solved)

strategy_fraction <- function(strategy, p, decimal_odds) {
  # TODO:
  # Return the bankroll fraction implied by:
  # "flat1", "kelly", "half", "quarter", or "reckless10".
  
  kelly = (p * decimal_odds - 1)/(decimal_odds - 1)
  
  if(strategy == "flat1") {
    fraction = 0.01
  } else if (strategy == "kelly") {
    fraction = kelly
  } else if (strategy == "half") {
    fraction = kelly / 2
  } else if (strategy == "quarter") {
    fraction = kelly / 4
  } else if (strategy == "reckless10") {
    fraction = 0.1
  }
    
  return(fraction)
  
  # stop("Implement strategy_fraction().")
}



simulate_path <- function(board, n_steps = 500, bankroll0 = 1000, strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  strategy <- match.arg(strategy)
  
  # TODO:
  # 1. At each step, sample one positive-EV bet from board.
  # 2. Convert the strategy into a bankroll fraction.
  # 3. Simulate win/loss using the true probability.
  # 4. Update bankroll multiplicatively.
  # 5. Return a tibble with step and bankroll.
  
  bankroll <- bankroll0
  out <- tibble::tibble(step = 1:n_steps, bankroll = NA_real_)
  
  for (i in seq_len(n_steps)) {
    
    # 1. Randomly sample one row from the board (treated as a generic EV‑positive event)
    sample <- sample_bet(board)
    
    # 2. Convert strategy into a fraction of bankroll
    fraction <- strategy_fraction(strategy,
                                  p = sample$model_p,
                                  decimal_odds = sample$decimal)
    
    # 3. Compute the amount at risk
    bet_size <- bankroll * fraction
    
    # 4. Simulate outcome using the event's probability
    win <- rbinom(1, 1, sample$model_p)
    
    # 5. Update bankroll multiplicatively
    if (win == 1) {
      bankroll <- bankroll + bet_size * (sample$decimal - 1)
    } else {
      bankroll <- bankroll - bet_size
    }
    
    # 6. Store bankroll
    out$bankroll[i] <- bankroll
  }
  
  # 7. Return tibble
  out
  
  # stop("Implement simulate_path().")
}

simulate_many_paths <- function(board, n_paths = 1000, n_steps = 500, bankroll0 = 1000, strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {

  # TODO:
  # Repeatedly call simulate_path() and combine the results.
  
  strategy <- match.arg(strategy)
  
  # Repeatedly call simulate_path() and combine the results
  paths <- lapply(seq_len(n_paths), function(i) {
    simulate_path(board, n_steps = n_steps, bankroll0 = bankroll0, strategy = strategy) |>
      dplyr::mutate(path = i)
  })
  
  dplyr::bind_rows(paths)
  
  # stop("Implement simulate_many_paths().")
}

plot_path_percentiles <- function(paths_df) {
  
  summary_df <- paths_df %>%
    group_by(step) %>%
    summarise(
      p10 = quantile(bankroll, 0.10),
      median = quantile(bankroll, 0.50),
      p90 = quantile(bankroll, 0.90)
    )
  
  ggplot(summary_df, aes(x = step)) +
    geom_line(aes(y = median), color = "blue", size = 1.2) +
    geom_line(aes(y = p10), color = "red", linetype = "dashed") +
    geom_line(aes(y = p90), color = "red", linetype = "dashed") +
    labs(
      title = "Median, 10th, and 90th Percentile Paths",
      y = "Value",
      x = "Step"
    ) +
    theme_minimal()
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

simulate_path_n <- function(board, n_steps = 500, bankroll0 = 1000,
                          strategy = c("flat1", "kelly", "half", "quarter", "reckless10"),
                          sd_eps = 0.03) {
  
  strategy <- match.arg(strategy)
  bankroll <- bankroll0
  out <- tibble::tibble(step = 1:n_steps, bankroll = NA_real_)
  
  for (i in seq_len(n_steps)) {
    
    # Sample one row
    sample <- sample_bet(board)
    
    # True probability (used for outcome)
    p_true <- sample$model_p
    
    # Noisy model estimate (used for sizing)
    p_hat <- add_model_noise(p_true, sd_eps = sd_eps)
    
    # Fraction based on noisy estimate
    fraction <- strategy_fraction(strategy,
                                  p = p_hat,
                                  decimal_odds = sample$decimal)
    
    # Amount at risk
    bet_size <- bankroll * fraction
    
    # Outcome uses p_true
    win <- rbinom(1, 1, p_true)
    
    # Multiplicative update
    if (win == 1) {
      bankroll <- bankroll + bet_size * (sample$decimal - 1)
    } else {
      bankroll <- bankroll - bet_size
    }
    
    out$bankroll[i] <- bankroll
  }
  
  out
}

simulate_path_n(part_a_positives)

simulate_many_paths <- function(board, n_paths = 1000, n_steps = 500,
                                bankroll0 = 1000,
                                strategy = c("flat1", "kelly", "half", "quarter", "reckless10"),
                                sd_eps = 0.03) {
  
  strategy <- match.arg(strategy)
  
  paths <- lapply(seq_len(n_paths), function(i) {
    simulate_path(board, n_steps = n_steps, bankroll0 = bankroll0,
                  strategy = strategy, sd_eps = sd_eps) |>
      dplyr::mutate(path = i)
  })
  
  dplyr::bind_rows(paths)
}


paths_full    <- simulate_many_paths(board, strategy = "kelly")
paths_half    <- simulate_many_paths(board, strategy = "half")
paths_quarter <- simulate_many_paths(board, strategy = "quarter")

paths_full$strategy    <- "full"
paths_half$strategy    <- "half"
paths_quarter$strategy <- "quarter"

paths_all <- dplyr::bind_rows(paths_full, paths_half, paths_quarter)
