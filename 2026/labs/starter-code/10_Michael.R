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
      decimal_odds = case_when(
        quote_type == "american"  ~ american_to_decimal(quote),
        quote_type == "contract"  ~ contract_to_decimal(quote)
      ),
      break_even_prob = break_even_prob(decimal_odds),
      ev_per_dollar   = expected_value_per_dollar(model_p, decimal_odds),
      kelly_full      = kelly_fraction(model_p, decimal_odds),
      kelly_half      = kelly_full / 2,
      kelly_quarter   = kelly_full / 4,
      stake_full      = kelly_full    * bankroll0,
      stake_half      = kelly_half    * bankroll0,
      stake_quarter   = kelly_quarter * bankroll0
    )
}

solve_part_a(part_a, bankroll0) %>% 
  select(bet, ev_per_dollar, kelly_full, kelly_half, kelly_quarter, stake_full, stake_half, stake_quarter)

#Team A moneyline, Underdog, Prediction contract, spread bet, favorite, team b moneyline
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
      decimal_a = american_to_decimal(side_a),
      decimal_b = american_to_decimal(side_b)
    ) %>%
    bind_cols(
      map2_dfr(.$decimal_a, .$decimal_b, remove_vig_2way)
    ) %>%
    mutate(
      edge_side_a = model_p_side_a - novig_a
    )
}


solve_part_b(two_sided) %>%
  select(market, model_p_side_a, novig_a, edge_side_a)

#My model probability for side A to the no vig prob is most different for game 2. 

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

solve_part_a(part_a, bankroll0) %>%
  filter(ev_per_dollar > 0) %>%
  mutate(stake_flat1 = 0.01 * bankroll0) %>%
  select(bet, stake_full, stake_half, stake_quarter, stake_flat1) %>%
  arrange(desc(stake_full))

analyze_parlay <- function(leg1_p, leg2_p, american_odds) {
  parlay_p  <- leg1_p * leg2_p
  d         <- american_to_decimal(american_odds)
  beq       <- break_even_prob(d)
  ev        <- expected_value_per_dollar(parlay_p, d)
  kelly     <- kelly_fraction(parlay_p, d)
  
  tibble(
    parlay_win_prob = parlay_p,
    decimal_odds    = d,
    break_even_prob = beq,
    ev_per_dollar   = ev,
    kelly_full      = kelly
  )
}

analyze_parlay <- function(leg1_p, leg2_p, american_odds) {
  parlay_p  <- leg1_p * leg2_p
  d         <- american_to_decimal(american_odds)
  beq       <- break_even_prob(d)
  ev        <- expected_value_per_dollar(parlay_p, d)
  kelly     <- kelly_fraction(parlay_p, d)
  
  tibble(
    parlay_win_prob = parlay_p,
    decimal_odds    = d,
    break_even_prob = beq,
    ev_per_dollar   = ev,
    kelly_full      = kelly
  )
}

analyze_parlay(parlay$leg1_p, parlay$leg2_p, parlay$american_odds)
analyze_parlay(parlay$leg1_p, parlay$leg2_p, parlay$american_odds) %>%
  print(width = Inf)


#the bets that receive the largest kelly stakes is the prediction contract, and no it is not always the bet with the highest win probability, since kelly just rewards edge relative to price.

############################
### PART D SIMULATION ######
############################
strategy_fraction <- function(strategy, p, decimal_odds) {
  f_kelly <- kelly_fraction(p, decimal_odds)
  
  switch(strategy,
         flat1      = 0.01,
         kelly      = f_kelly,
         half       = f_kelly / 2,
         quarter    = f_kelly / 4,
         reckless10 = 0.10
  )
}

simulate_path <- function(board, n_steps = 500, bankroll0 = 1000, strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  strategy <- match.arg(strategy)
  
  bankroll <- bankroll0
  results  <- tibble(step = 0:n_steps, bankroll = NA_real_)
  results$bankroll[1] <- bankroll
  
  for (i in seq_len(n_steps)) {
    bet        <- sample_bet(board)
    f          <- strategy_fraction(strategy, bet$model_p, bet$decimal_odds)
    win        <- rbinom(1, 1, bet$model_p)
    multiplier <- ifelse(win, 1 + f * (bet$decimal_odds - 1), 1 - f)
    bankroll   <- bankroll * multiplier
    results$bankroll[i + 1] <- bankroll
  }
  
  results
}

strategies <- c("flat1", "kelly", "half", "quarter", "reckless10")

all_paths <- map_dfr(strategies, function(s) {
  simulate_many_paths(board, n_paths = 200, n_steps = 500, strategy = s) %>%
    mutate(strategy = s)
})

all_paths %>%
  group_by(strategy, step) %>%
  summarise(median_bankroll = median(bankroll), .groups = "drop") %>%
  ggplot(aes(x = step, y = median_bankroll, color = strategy)) +
  geom_line() +
  scale_y_log10() +
  labs(title = "Median Bankroll by Strategy", x = "Step", y = "Bankroll (log scale)")

simulate_many_paths <- function(board, n_paths = 1000, n_steps = 500, bankroll0 = 1000, strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  strategy <- match.arg(strategy)
  
  map_dfr(seq_len(n_paths), function(path_id) {
    simulate_path(board, n_steps, bankroll0, strategy) %>%
      mutate(path = path_id)
  })
}

board <- solve_part_a(part_a, bankroll0) %>%
  filter(ev_per_dollar > 0)

simulate_path(board, strategy = "kelly")

#Kelly is wrong for correlated bets becuase it assumes independency.
#Hlaf or quarter kelly

############################
### PART E UNCERTAINTY #####
############################

add_model_noise <- function(p_true, sd_eps = 0.03) {
  p_hat <- p_true + rnorm(length(p_true), mean = 0, sd = sd_eps)
  pmin(0.99, pmax(0.01, p_hat))
}

simulate_path_noisy <- function(board, n_steps = 500, bankroll0 = 1000, strategy = c("kelly", "half", "quarter"), sd_eps = 0.03) {
  strategy <- match.arg(strategy)
  
  bankroll <- bankroll0
  results  <- tibble(step = 0:n_steps, bankroll = NA_real_)
  results$bankroll[1] <- bankroll
  
  for (i in seq_len(n_steps)) {
    bet    <- sample_bet(board)
    p_true <- bet$model_p
    p_hat  <- add_model_noise(p_true, sd_eps)
    
    # size bet using noisy p_hat
    f <- strategy_fraction(strategy, p_hat, bet$decimal_odds)
    
    # outcome determined by true p
    win        <- rbinom(1, 1, p_true)
    multiplier <- ifelse(win, 1 + f * (bet$decimal_odds - 1), 1 - f)
    bankroll   <- bankroll * multiplier
    results$bankroll[i + 1] <- bankroll
  }
  
  results
}

# compare strategies under model noise
noisy_strategies <- c("kelly", "half", "quarter")

noisy_paths <- map_dfr(noisy_strategies, function(s) {
  map_dfr(1:200, function(path_id) {
    simulate_path_noisy(board, strategy = s) %>%
      mutate(strategy = s, path = path_id)
  })
})

noisy_paths %>%
  group_by(strategy, step) %>%
  summarise(median_bankroll = median(bankroll), .groups = "drop") %>%
  ggplot(aes(x = step, y = median_bankroll, color = strategy)) +
  geom_line() +
  scale_y_log10() +
  labs(title = "Median Bankroll Under Model Noise", x = "Step", y = "Bankroll (log scale)")


#Kelly grows the fastest with the most variance, whereace half and quarter kelly grow more steadly. 
