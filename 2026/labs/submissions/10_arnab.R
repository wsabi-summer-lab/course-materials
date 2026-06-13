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
  part_a %>%
    mutate(
      dec_odds = case_when(
        quote_type == "american" ~ american_to_decimal(quote),
        quote_type == "contract" ~ contract_to_decimal(quote),
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(break_even_probs = break_even_prob(dec_odds)) %>%
    mutate(ev_per_dollar = expected_value_per_dollar(model_p, dec_odds))%>%
    mutate(kf = kelly_fraction(model_p, dec_odds))%>%
    mutate(half_kf = .5*kf)%>%
    mutate(q_kf = .25*kf)%>%
    mutate(kf_stake = kf*bankroll0)%>%
    mutate(h_stake = half_kf*bankroll0)%>%
    mutate(q_stake = q_kf*bankroll0)

  # TODO:
  # 1. Create decimal_odds for every row.
  # 2. Compute break-even probability.
  # 3. Compute EV per $1 staked.
  # 4. Compute full / half / quarter Kelly fractions.
  # 5. Convert Kelly fractions into dollar stakes using bankroll0.

}
part_a_solved =solve_part_a(part_a, 1000)
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
  two_sided %>%
    mutate(
      side_a_dec = american_to_decimal(side_a),
      side_b_dec = american_to_decimal(side_b),
      a_prob = 1 / side_a_dec,
      b_prob = 1 / side_b_dec,
      hold = a_prob + b_prob - 1,
      no_vig_probA = a_prob / (a_prob + b_prob),
      no_vig_probB = b_prob / (a_prob + b_prob)
    )
  # 1. Convert side_a and side_b to decimal odds.
  # 2. Compute raw implied probabilities, hold, and no-vig probabilities.
  # 3. Compare model_p_side_a to the no-vig probability for side_a.

}
two_sided_solved <- solve_part_b(two_sided)

#no-vig is less, thus you shouldnt bet it.
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
  parlay_win_probability <- leg1_p*leg2_p
  dec_odds <- american_to_decimal(american_odds)
  break_even <- break_even_prob(dec_odds)
  ev <- expected_value_per_dollar(parlay_win_probability, dec_odds)
  kelly <- kelly_fraction(parlay_win_probability, dec_odds)
  tibble(
    parlay_win_probability = parlay_win_probability,
    dec_odds = dec_odds,
    break_even = break_even,
    ev = ev,
    kelly = kelly)

}
analyze_parlay(
  parlay$leg1_p,
  parlay$leg2_p,
  parlay$american_odds
)
#It overstates because it assumes independence in events that aren't independent. One conservative way to adjust for correlation is to do a fraction. 
############################
### PART D SIMULATION ######
############################

sample_bet <- function(board) {
  board %>% slice_sample(n = 1)
}

strategy_fraction <- function(strategy, p, decimal_odds) {
  if (strategy == "flat1") {
    return(0.01)
    
  } else if (strategy == "kelly") {
    return(kelly_fraction(p, decimal_odds))
    
  } else if (strategy == "half") {
    return(0.5 * kelly_fraction(p, decimal_odds))
    
  } else if (strategy == "quarter") {
    return(0.25 * kelly_fraction(p, decimal_odds))
    
  } else if (strategy == "reckless10") {
    return(0.10)
    
  } else {
    stop("Unknown strategy.")
  }
  # TODO:
  # Return the bankroll fraction implied by:
  # "flat1", "kelly", "half", "quarter", or "reckless10".
}

simulate_path <- function(board, n_steps = 500, bankroll0 = 1000, strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  strategy <- match.arg(strategy)
  bankroll <- bankroll0
  
  out <- tibble(
    step = 0,
    bankroll = bankroll
  )
  
  for (i in 1:n_steps) {
    
    board_pos <- board %>%
      filter(ev_per_dollar > 0)
    
    bet <- board_pos %>%
      slice_sample(n = 1)
    
    frac <- strategy_fraction(strategy, bet$model_p, bet$dec_odds)
    stake <- frac * bankroll
    
    win <- rbinom(1, 1, bet$model_p)
    
    if (win == 1) {
      bankroll <- bankroll + stake * (bet$dec_odds - 1)
    } else {
      bankroll <- bankroll - stake
    }
    
    out <- bind_rows(
      out,
      tibble(step = i, bankroll = bankroll)
    )
  }
  
  out
}

simulate_many_paths <- function(board, n_paths = 1000, n_steps = 500, bankroll0 = 1000, strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  strategy <- match.arg(strategy)
  all_paths <- tibble()
  
  for (j in 1:n_paths) {
    
    one_path <- simulate_path(
      board = board,
      n_steps = n_steps,
      bankroll0 = bankroll0,
      strategy = strategy
    ) %>%
      mutate(path = j)
    
    all_paths <- bind_rows(all_paths, one_path)
  }
  
  all_paths
}

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
  bankroll <- bankroll0
  
  out <- tibble(step = 0, bankroll = bankroll)
  
  for (i in 1:n_steps) {
    
    board_noisy <- board %>%
      mutate(
        p_hat = add_model_noise(model_p, sd_eps),
        ev_hat = expected_value_per_dollar(p_hat, dec_odds)
      ) %>%
      filter(ev_hat > 0)
    
    bet <- board_noisy %>%
      slice_sample(n = 1)
    
    frac <- strategy_fraction(strategy, bet$p_hat, bet$dec_odds)
    stake <- frac * bankroll
    
    win <- rbinom(1, 1, bet$model_p)
    
    if (win == 1) {
      bankroll <- bankroll + stake * (bet$dec_odds - 1)
    } else {
      bankroll <- bankroll - stake
    }
    
    out <- bind_rows(out, tibble(step = i, bankroll = bankroll))
  }
  
  out
}

noisy_full <- simulate_many_paths(part_a_solved, strategy = "kelly")
noisy_half <- simulate_many_paths(part_a_solved, strategy = "half")
noisy_quarter <- simulate_many_paths(part_a_solved, strategy = "quarter")

print(noisy_full)
print(noisy_half)
print(noisy_quarter)
# TODO:
# 1. Simulate outcomes using p_true.
# 2. Size bets using noisy p_hat.
# 3. Compare full Kelly, half Kelly, and quarter Kelly under model error.
