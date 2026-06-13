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
      # Convert American odds or contract prices into decimal odds
      decimal_odds = ifelse(
        quote_type == "american",
        american_to_decimal(quote),
        contract_to_decimal(quote)
      ),
      
      # Break-even probability from the market price
      break_even_prob = break_even_prob(decimal_odds),
      
      # EV per $1 staked using our model probability
      ev_per_dollar = expected_value_per_dollar(model_p, decimal_odds),
      
      # Label the opportunity
      ev_label = case_when(
        ev_per_dollar > 0.005 ~ "Positive EV",
        ev_per_dollar < -0.005 ~ "Negative EV",
        TRUE ~ "Roughly neutral"
      ),
      
      # Kelly fractions
      full_kelly = kelly_fraction(model_p, decimal_odds),
      half_kelly = 0.50 * full_kelly,
      quarter_kelly = 0.25 * full_kelly,
      
      # Dollar stakes using starting bankroll
      full_kelly_stake = bankroll0 * full_kelly,
      half_kelly_stake = bankroll0 * half_kelly,
      quarter_kelly_stake = bankroll0 * quarter_kelly,
      flat_1pct_stake = bankroll0 * 0.01
    ) %>%
    arrange(desc(ev_per_dollar))
}

part_a_results <- solve_part_a(part_a, bankroll0)
part_a_results

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
      # Convert each side to decimal odds
      decimal_a = american_to_decimal(side_a),
      decimal_b = american_to_decimal(side_b),
      
      # Raw implied probabilities
      raw_a = 1 / decimal_a,
      raw_b = 1 / decimal_b,
      
      # Sportsbook hold / vig
      hold = raw_a + raw_b - 1,
      
      # No-vig probabilities
      novig_a = raw_a / (raw_a + raw_b),
      novig_b = raw_b / (raw_a + raw_b),
      
      # Compare our model to the no-vig market estimate
      model_minus_novig_a = model_p_side_a - novig_a,
      abs_disagreement = abs(model_minus_novig_a),
      
      model_view = case_when(
        model_minus_novig_a > 0 ~ "Model likes Side A more than market",
        model_minus_novig_a < 0 ~ "Model likes Side A less than market",
        TRUE ~ "Model agrees with market"
      )
    ) %>%
    arrange(desc(abs_disagreement))
}

part_b_results <- solve_part_b(two_sided)
part_b_results

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
  
  decimal_odds <- american_to_decimal(american_odds)
  parlay_p <- leg1_p * leg2_p
  
  tibble(
    leg1_p = leg1_p,
    leg2_p = leg2_p,
    parlay_p = parlay_p,
    american_odds = american_odds,
    decimal_odds = decimal_odds,
    break_even_prob = break_even_prob(decimal_odds),
    ev_per_dollar = expected_value_per_dollar(parlay_p, decimal_odds),
    full_kelly = kelly_fraction(parlay_p, decimal_odds)
  )
}

parlay_results <- analyze_parlay(
  leg1_p = parlay$leg1_p,
  leg2_p = parlay$leg2_p,
  american_odds = parlay$american_odds
)

parlay_results

# Analyze the correlated-bet example separately
correlated_results <- correlated_example %>%
  mutate(
    decimal_odds = american_to_decimal(american_odds),
    break_even_prob = break_even_prob(decimal_odds),
    ev_per_dollar = expected_value_per_dollar(model_p, decimal_odds),
    full_kelly = kelly_fraction(model_p, decimal_odds),
    half_kelly = 0.50 * full_kelly,
    quarter_kelly = 0.25 * full_kelly
  )

correlated_results

# Written response for correlated bets:
# Computing separate Kelly stakes can overstate total exposure because the two bets
# are not independent. If Eagles -3.5 and QB over 1.5 passing TDs depend on the same
# offensive game script, then one bad game script can cause both bets to lose together.
# A conservative adjustment is to reduce the total stake, use half- or quarter-Kelly,
# or place a cap on total exposure to the same game.

############################
### PART D SIMULATION ######
############################

sample_bet <- function(board) {
  board %>% slice_sample(n = 1)
}

strategy_fraction <- function(strategy, p, decimal_odds) {
  
  strategy <- match.arg(
    strategy,
    choices = c("flat1", "kelly", "half", "quarter", "reckless10")
  )
  
  f <- switch(
    strategy,
    "flat1" = 0.01,
    "kelly" = kelly_fraction(p, decimal_odds),
    "half" = 0.50 * kelly_fraction(p, decimal_odds),
    "quarter" = 0.25 * kelly_fraction(p, decimal_odds),
    "reckless10" = 0.10
  )
  
  # Keep fractions between 0 and 1
  pmin(1, pmax(0, f))
}

simulate_path <- function(board,
                          n_steps = 500,
                          bankroll0 = 1000,
                          strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  
  strategy <- match.arg(strategy)
  
  # We assume board has:
  # bet, decimal_odds, p_true
  # If p_hat exists, it is used for sizing.
  # Otherwise, p_true is used for sizing.
  
  bankroll <- numeric(n_steps + 1)
  bankroll[1] <- bankroll0
  
  for (step in 1:n_steps) {
    
    # If bankroll hits zero, it stays zero
    if (bankroll[step] <= 0) {
      bankroll[step + 1] <- 0
      next
    }
    
    current_bet <- sample_bet(board)
    
    p_true <- current_bet$p_true
    p_size <- if ("p_hat" %in% names(current_bet)) {
      current_bet$p_hat
    } else {
      current_bet$p_true
    }
    
    decimal_odds <- current_bet$decimal_odds
    
    # Determine stake fraction
    f <- strategy_fraction(strategy, p_size, decimal_odds)
    
    # Simulate win/loss using the true probability
    win <- rbinom(1, size = 1, prob = p_true)
    
    # Update bankroll
    stake <- bankroll[step] * f
    
    profit <- ifelse(
      win == 1,
      stake * (decimal_odds - 1),
      -stake
    )
    
    bankroll[step + 1] <- bankroll[step] + profit
  }
  
  tibble(
    step = 0:n_steps,
    bankroll = bankroll
  )
}

simulate_many_paths <- function(board,
                                n_paths = 1000,
                                n_steps = 500,
                                bankroll0 = 1000,
                                strategy = c("flat1", "kelly", "half", "quarter", "reckless10")) {
  
  strategy <- match.arg(strategy)
  
  map_dfr(
    1:n_paths,
    function(path_id) {
      simulate_path(
        board = board,
        n_steps = n_steps,
        bankroll0 = bankroll0,
        strategy = strategy
      ) %>%
        mutate(
          path = path_id,
          strategy = strategy
        )
    }
  )
}

# Build the positive-EV betting board from Part A
positive_a_board <- part_a_results %>%
  filter(ev_per_dollar > 0) %>%
  transmute(
    bet = bet,
    decimal_odds = decimal_odds,
    p_true = model_p,
    source = "Part A"
  )

# Add the parlay if it is positive EV
positive_parlay_board <- parlay_results %>%
  filter(ev_per_dollar > 0) %>%
  transmute(
    bet = "Two-leg parlay",
    decimal_odds = decimal_odds,
    p_true = parlay_p,
    source = "Part C"
  )

board <- bind_rows(positive_a_board, positive_parlay_board)

board

# Simulate all strategies
strategies <- c("flat1", "kelly", "half", "quarter", "reckless10")

all_paths <- map_dfr(
  strategies,
  function(s) {
    simulate_many_paths(
      board = board,
      n_paths = 1000,
      n_steps = 500,
      bankroll0 = bankroll0,
      strategy = s
    )
  }
)

# Summarize bankroll paths over time
path_summary <- all_paths %>%
  group_by(strategy, step) %>%
  summarize(
    median_bankroll = median(bankroll),
    p10_bankroll = quantile(bankroll, 0.10),
    p90_bankroll = quantile(bankroll, 0.90),
    .groups = "drop"
  )

# Plot 1: Median bankroll over time
ggplot(path_summary, aes(x = step, y = median_bankroll, color = strategy)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Median Bankroll Over Time by Strategy",
    x = "Bet Number",
    y = "Median Bankroll",
    color = "Strategy"
  ) +
  theme_minimal()

# Plot 2: 10th and 90th percentile bankroll paths
ggplot(path_summary, aes(x = step, y = median_bankroll, color = strategy, fill = strategy)) +
  geom_ribbon(aes(ymin = p10_bankroll, ymax = p90_bankroll), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  labs(
    title = "Bankroll Range Over Time: 10th to 90th Percentile",
    x = "Bet Number",
    y = "Bankroll",
    color = "Strategy",
    fill = "Strategy"
  ) +
  theme_minimal()

# Risk and final bankroll summaries
strategy_summary <- all_paths %>%
  group_by(strategy, path) %>%
  summarize(
    final_bankroll = bankroll[step == max(step)],
    min_bankroll = min(bankroll),
    lost_50pct = min_bankroll <= 0.50 * bankroll0,
    .groups = "drop"
  ) %>%
  group_by(strategy) %>%
  summarize(
    prob_lost_50pct = mean(lost_50pct),
    mean_final_bankroll = mean(final_bankroll),
    median_final_bankroll = median(final_bankroll),
    geometric_mean_final_bankroll = exp(mean(log(pmax(final_bankroll, 1e-8)))),
    .groups = "drop"
  ) %>%
  arrange(desc(geometric_mean_final_bankroll))

strategy_summary

# Written response for Part D:
# The fastest-growing strategy is usually the one with the largest median or geometric
# mean final bankroll. The strategy with the worst drawdowns is usually full Kelly
# or reckless10, because they stake a larger fraction of bankroll. In practice, many
# bettors prefer half Kelly or quarter Kelly because those strategies trade off growth
# for lower risk of severe drawdowns.

############################
### PART E UNCERTAINTY #####
############################

add_model_noise <- function(p_true, sd_eps = 0.03) {
  p_hat <- p_true + rnorm(length(p_true), mean = 0, sd = sd_eps)
  pmin(0.99, pmax(0.01, p_hat))
}

simulate_many_paths_noisy <- function(board,
                                      n_paths = 1000,
                                      n_steps = 500,
                                      bankroll0 = 1000,
                                      strategy = c("kelly", "half", "quarter"),
                                      sd_eps = 0.03) {
  
  strategy <- match.arg(strategy)
  
  map_dfr(
    1:n_paths,
    function(path_id) {
      
      # Create one noisy model estimate for each bet in this path
      noisy_board <- board %>%
        mutate(
          p_hat = add_model_noise(p_true, sd_eps = sd_eps),
          perceived_ev = expected_value_per_dollar(p_hat, decimal_odds)
        ) %>%
        filter(perceived_ev > 0)
      
      # If noise makes every bet look negative EV, keep bankroll constant
      if (nrow(noisy_board) == 0) {
        return(
          tibble(
            step = 0:n_steps,
            bankroll = bankroll0,
            path = path_id,
            strategy = strategy
          )
        )
      }
      
      simulate_path(
        board = noisy_board,
        n_steps = n_steps,
        bankroll0 = bankroll0,
        strategy = strategy
      ) %>%
        mutate(
          path = path_id,
          strategy = strategy
        )
    }
  )
}

noisy_strategies <- c("kelly", "half", "quarter")

all_noisy_paths <- map_dfr(
  noisy_strategies,
  function(s) {
    simulate_many_paths_noisy(
      board = board,
      n_paths = 1000,
      n_steps = 500,
      bankroll0 = bankroll0,
      strategy = s,
      sd_eps = 0.03
    )
  }
)

# Summarize noisy paths
noisy_path_summary <- all_noisy_paths %>%
  group_by(strategy, step) %>%
  summarize(
    median_bankroll = median(bankroll),
    p10_bankroll = quantile(bankroll, 0.10),
    p90_bankroll = quantile(bankroll, 0.90),
    .groups = "drop"
  )

# Plot model uncertainty results
ggplot(noisy_path_summary, aes(x = step, y = median_bankroll, color = strategy, fill = strategy)) +
  geom_ribbon(aes(ymin = p10_bankroll, ymax = p90_bankroll), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  labs(
    title = "Bankroll Growth Under Model Uncertainty",
    subtitle = "Outcomes use true probabilities, but bet sizing uses noisy model probabilities",
    x = "Bet Number",
    y = "Bankroll",
    color = "Strategy",
    fill = "Strategy"
  ) +
  theme_minimal()

# Risk summary under model uncertainty
noisy_strategy_summary <- all_noisy_paths %>%
  group_by(strategy, path) %>%
  summarize(
    final_bankroll = bankroll[step == max(step)],
    min_bankroll = min(bankroll),
    lost_50pct = min_bankroll <= 0.50 * bankroll0,
    .groups = "drop"
  ) %>%
  group_by(strategy) %>%
  summarize(
    prob_lost_50pct = mean(lost_50pct),
    mean_final_bankroll = mean(final_bankroll),
    median_final_bankroll = median(final_bankroll),
    geometric_mean_final_bankroll = exp(mean(log(pmax(final_bankroll, 1e-8)))),
    .groups = "drop"
  ) %>%
  arrange(desc(geometric_mean_final_bankroll))

noisy_strategy_summary

# Part E:
# Full Kelly is usually the most sensitive to model error because it makes the largest
# bets when the model thinks it has an edge. If the model probability is overestimated,
# full Kelly can badly overbet. Half Kelly and quarter Kelly are more robust because
# they shrink the stake sizes. Under noisy probability estimates, quarter Kelly often
# has the smoothest drawdowns, while half Kelly often gives a reasonable balance
# between growth and risk.