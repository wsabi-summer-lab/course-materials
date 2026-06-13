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
  part_a <- part_a %>%
    mutate(
      decimal_odds = ifelse(
        quote_type == "contract",
        contract_to_decimal(quote),
        american_to_decimal(quote)
      ),
      break_even = break_even_prob(decimal_odds),
      ev = expected_value_per_dollar(model_p, decimal_odds),
      ev_label = case_when(
        ev >  0.01 ~ "positive EV",
        ev < -0.01 ~ "negative EV",
        TRUE       ~ "roughly neutral"
      ),
      ev_rank = rank(-ev)
    ) %>%
    arrange(ev_rank)
  
  return(part_a)
}

updated_a = solve_part_a(part_a)






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
      
      dec_a = american_to_decimal(side_a),
      dec_b = american_to_decimal(side_b),
      
      
      raw_a    = 1 / dec_a,
      raw_b    = 1 / dec_b,
      hold     = raw_a + raw_b - 1,
      novig_a  = raw_a / (raw_a + raw_b),
      novig_b  = raw_b / (raw_a + raw_b),
      
     
      model_vs_novig = model_p_side_a - novig_a,
      edge_label = case_when(
        model_vs_novig >  0.01 ~ "model favors side A",
        model_vs_novig < -0.01 ~ "model fades side A",
        TRUE                   ~ "roughly agree"
      )
    ) %>%
    arrange(desc(abs(model_vs_novig)))
  
  return(two_sided)

  
}

updated_b = solve_part_b(two_sided)
updated_b





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
  
  
  parlay_p = leg1_p * leg2_p
  
  # Step 2: Convert offered odds to decimal
  dec_odds = american_to_decimal(american_odds)
  
  # Step 3: Break-even prob, EV, and full Kelly
  be_prob   = break_even_prob(dec_odds)
  ev        = expected_value_per_dollar(parlay_p, dec_odds)
  kelly     = kelly_fraction(parlay_p, dec_odds)
  
  tibble(
    parlay_p      = parlay_p,
    break_even_p  = be_prob,
    decimal_odds  = dec_odds,
    ev_per_dollar = ev,
    kelly_full    = kelly,
    kelly_half    = kelly / 2,
    kelly_quarter = kelly / 4,
    ev_label      = case_when(
      ev >  0.01 ~ "positive EV",
      ev < -0.01 ~ "negative EV",
      TRUE       ~ "roughly neutral"
    )
  )

  
}

parlay_result = analyze_parlay(parlay$leg1_p, parlay$leg2_p, parlay$american_odds)
parlay_result





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

  switch(strategy,
         flat1      = 0.01,
         kelly      = kelly_fraction(p, decimal_odds),
         half       = kelly_fraction(p, decimal_odds) / 2,
         quarter    = kelly_fraction(p, decimal_odds) / 4,
         reckless10 = 0.10
  )
}

simulate_path <- function(board, n_steps = 500, bankroll0 = 1000, strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  strategy <- match.arg(strategy)

  # TODO:
  # 1. At each step, sample one positive-EV bet from board.
  # 2. Convert the strategy into a bankroll fraction.
  # 3. Simulate win/loss using the true probability.
  # 4. Update bankroll multiplicatively.
  # 5. Return a tibble with step and bankroll.

  bankroll  <- bankroll0
  results   <- tibble(step = 0:n_steps, bankroll = NA_real_)
  results$bankroll[1] <- bankroll
  
  for (i in seq_len(n_steps)) {
    # Step 1: Sample one positive-EV bet from board
    bet <- sample_bet(board)
    
    # Step 2: Get bankroll fraction for this strategy
    frac <- strategy_fraction(strategy, bet$model_p, bet$decimal_odds)
    
    # Step 3: Simulate win/loss using true probability
    win <- rbinom(1, 1, bet$model_p)
    
    # Step 4: Update bankroll multiplicatively
    stake <- frac * bankroll
    if (win) {
      bankroll <- bankroll + stake * (bet$decimal_odds - 1)
    } else {
      bankroll <- bankroll - stake
    }
    bankroll <- max(bankroll, 0)  # floor at 0
    
    results$bankroll[i + 1] <- bankroll
  }
  
  results
}

simulate_many_paths <- function(board, n_paths = 1000, n_steps = 500, bankroll0 = 1000, strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  strategy <- match.arg(strategy)

  # TODO:
  # Repeatedly call simulate_path() and combine the results.

  map_dfr(seq_len(n_paths), function(path_id) {
    simulate_path(board, n_steps, bankroll0, strategy) %>%
      mutate(path = path_id, strategy = strategy)
  })
}


board = updated_a %>%
  filter(ev > 0) %>%
  select(bet, model_p, decimal_odds, ev)

# Simulate all five strategies
strategies = c("flat1", "kelly", "half", "quarter", "reckless10")

all_paths = map_dfr(strategies, ~ simulate_many_paths(board, strategy = .x))

# Summarise median terminal bankroll per strategy
summary_d = all_paths %>%
  filter(step == 500) %>%
  group_by(strategy) %>%
  summarise(
    median_bankroll = median(bankroll),
    mean_bankroll   = mean(bankroll),
    pct_ruin        = mean(bankroll < 1),
    .groups = "drop"
  ) %>%
  arrange(desc(median_bankroll))

summary_d

# Plot paths (sample 50 per strategy for readability)
all_paths %>%
  filter(path <= 50) %>%
  ggplot(aes(x = step, y = bankroll, group = path, color = strategy)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~ strategy, scales = "free_y") +
  scale_y_log10() +
  labs(title = "Simulated Bankroll Paths by Strategy",
       x = "Bet Number", y = "Bankroll (log scale)") +
  theme_bw() +
  theme(legend.position = "none")

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

simulate_path_noisy <- function(board, n_steps = 500, bankroll0 = 1000,
                                strategy = c("kelly", "half", "quarter"),
                                sd_eps = 0.03) {
  strategy <- match.arg(strategy)
  bankroll <- bankroll0
  results  <- tibble(step = 0:n_steps, bankroll = NA_real_)
  results$bankroll[1] <- bankroll
  
  for (i in seq_len(n_steps)) {
    bet    <- sample_bet(board)
    
    # Size bet using noisy p_hat, simulate outcome using true p
    p_hat  <- add_model_noise(bet$model_p, sd_eps)
    frac   <- strategy_fraction(strategy, p_hat, bet$decimal_odds)
    
    win    <- rbinom(1, 1, bet$model_p)  # true probability
    stake  <- frac * bankroll
    if (win) {
      bankroll <- bankroll + stake * (bet$decimal_odds - 1)
    } else {
      bankroll <- bankroll - stake
    }
    bankroll <- max(bankroll, 0)
    results$bankroll[i + 1] <- bankroll
  }
  
  results
}

simulate_many_noisy <- function(board, n_paths = 200, n_steps = 500,
                                bankroll0 = 1000, strategy = "kelly", sd_eps = 0.03) {
  map_dfr(seq_len(n_paths), function(path_id) {
    simulate_path_noisy(board, n_steps, bankroll0, strategy, sd_eps) %>%
      mutate(path = path_id, strategy = strategy)
  })
}

# Compare full / half / quarter Kelly under model noise
noisy_strategies = c("kelly", "half", "quarter")

noisy_paths = map_dfr(noisy_strategies,
                      ~ simulate_many_noisy(board, strategy = .x, sd_eps = 0.03))

summary_e = noisy_paths %>%
  filter(step == 500) %>%
  group_by(strategy) %>%
  summarise(
    median_bankroll = median(bankroll),
    mean_bankroll   = mean(bankroll),
    pct_ruin        = mean(bankroll < 1),
    .groups = "drop"
  ) %>%
  arrange(desc(median_bankroll))

summary_e

# Plot
noisy_paths %>%
  filter(path <= 50) %>%
  ggplot(aes(x = step, y = bankroll, group = path, color = strategy)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~ strategy, scales = "free_y") +
  scale_y_log10() +
  labs(title = "Bankroll Paths Under Model Uncertainty (sd = 0.03)",
       x = "Bet Number", y = "Bankroll (log scale)") +
  theme_bw() +
  theme(legend.position = "none")
