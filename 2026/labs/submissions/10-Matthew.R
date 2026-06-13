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
  part_a |>
    mutate(
      decimal_odds = ifelse(
        quote_type == "contract",
        contract_to_decimal(quote),
        american_to_decimal(quote)
      ),
      break_even = break_even_prob(decimal_odds),
      ev_per_dollar = expected_value_per_dollar(model_p, decimal_odds),
      kelly_full = kelly_fraction(model_p, decimal_odds),
      kelly_half = kelly_full / 2,
      kelly_quarter = kelly_full / 4,
      stake_full = kelly_full * bankroll0,
      stake_half = kelly_half * bankroll0,
      stake_quarter = kelly_quarter * bankroll0
    )
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
  two_sided |>
    mutate(
      dec_a = american_to_decimal(side_a),
      dec_b = american_to_decimal(side_b)
    ) |>
    mutate(remove_vig_2way(dec_a, dec_b)) |>
    mutate(
      model_p_side_b = 1 - model_p_side_a,
      edge_a = model_p_side_a - novig_a,
      edge_b = model_p_side_b - novig_b,
      bet_side = case_when(
        edge_a > 0 & edge_a >= edge_b ~ "side_a",
        edge_b > 0 & edge_b > edge_a  ~ "side_b",
        TRUE                           ~ "no_bet"
      )
    )
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
  parlay_p   <- leg1_p * leg2_p
  dec_odds   <- american_to_decimal(american_odds)
  be_prob    <- break_even_prob(dec_odds)
  ev         <- expected_value_per_dollar(parlay_p, dec_odds)
  kelly_f    <- kelly_fraction(parlay_p, dec_odds)
  
  tibble(
    parlay_p   = parlay_p,
    dec_odds   = dec_odds,
    break_even = be_prob,
    ev_per_dollar = ev,
    kelly_full = kelly_f
  )
}

############################
### PART D SIMULATION ######
############################

sample_bet <- function(board) {
  board %>% slice_sample(n = 1)
}

strategy_fraction <- function(strategy, p, decimal_odds) {
  kf <- kelly_fraction(p, decimal_odds)
  switch(strategy,
         flat1      = 0.01,
         kelly      = kf,
         half       = kf / 2,
         quarter    = kf / 4,
         reckless10 = 0.10
  )
}

simulate_path <- function(board, n_steps = 500, bankroll0 = 1000,
                          strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  strategy  <- match.arg(strategy)
  bankroll  <- bankroll0
  out       <- vector("double", n_steps + 1)
  out[1]    <- bankroll
  
  for (i in seq_len(n_steps)) {
    bet    <- sample_bet(board)
    f      <- strategy_fraction(strategy, bet$model_p, bet$decimal_odds)
    stake  <- f * bankroll
    win    <- runif(1) < bet$model_p
    bankroll <- bankroll + ifelse(win, stake * (bet$decimal_odds - 1), -stake)
    bankroll <- max(bankroll, 0)
    out[i + 1] <- bankroll
  }
  
  tibble(step = 0:n_steps, bankroll = out)
}

simulate_many_paths <- function(board, n_paths = 1000, n_steps = 500, bankroll0 = 1000,
                                strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  strategy <- match.arg(strategy)
  
  map_dfr(seq_len(n_paths), function(path_id) {
    simulate_path(board, n_steps, bankroll0, strategy) |>
      mutate(path = path_id)
  })
}

############################
### PART E UNCERTAINTY #####
############################

add_model_noise <- function(p_true, sd_eps = 0.03) {
  p_hat <- p_true + rnorm(length(p_true), mean = 0, sd = sd_eps)
  pmin(0.99, pmax(0.01, p_hat))
}

simulate_noisy_paths <- function(board, n_paths = 500, n_steps = 500,
                                 bankroll0 = 1000, sd_eps = 0.03,
                                 strategy = c("kelly", "half", "quarter")) {
  strategy <- match.arg(strategy)
  
  map_dfr(seq_len(n_paths), function(path_id) {
    bankroll <- bankroll0
    out      <- vector("double", n_steps + 1)
    out[1]   <- bankroll
    
    for (i in seq_len(n_steps)) {
      bet    <- sample_bet(board)
      p_hat  <- add_model_noise(bet$model_p, sd_eps)
      f      <- strategy_fraction(strategy, p_hat, bet$decimal_odds)
      stake  <- f * bankroll
      win    <- runif(1) < bet$model_p        # outcome drawn from TRUE probability
      bankroll <- bankroll + ifelse(win, stake * (bet$decimal_odds - 1), -stake)
      bankroll <- max(bankroll, 0)
      out[i + 1] <- bankroll
    }
    
    tibble(step = 0:n_steps, bankroll = out, path = path_id)
  })
}

# Build a positive-EV board with decimal odds attached (needed by simulate_path)
part_a_solved <- solve_part_a(part_a, bankroll0)
ev_board <- part_a_solved |>
  filter(ev_per_dollar > 0) |>
  select(bet, model_p, decimal_odds)

# Part E comparison: Kelly vs Half vs Quarter under sd_eps = 0.03
strategies_e <- c("kelly", "half", "quarter")
noisy_results <- map_dfr(strategies_e, function(s) {
  simulate_noisy_paths(ev_board, n_paths = 300, n_steps = 500,
                       bankroll0 = bankroll0, sd_eps = 0.03, strategy = s) |>
    mutate(strategy = s)
})

noisy_summary <- noisy_results |>
  group_by(strategy, step) |>
  summarise(
    median_bankroll = median(bankroll),
    p10 = quantile(bankroll, 0.10),
    p90 = quantile(bankroll, 0.90),
    ruin_rate = mean(bankroll == 0),
    .groups = "drop"
  )

ggplot(noisy_summary, aes(x = step, y = median_bankroll, colour = strategy)) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(aes(ymin = p10, ymax = p90, fill = strategy), alpha = 0.15, colour = NA) +
  scale_y_log10() +
  labs(
    title = "Median bankroll under model noise (sd = 0.03)",
    subtitle = "Ribbon = 10th–90th percentile across 300 paths",
    x = "Bet number", y = "Bankroll (log scale)"
  ) +
  theme_minimal()