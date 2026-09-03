#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "readr", "splines", "tidyverse"))
library(ggplot2)
library(nnet)
library(readr)
library(splines)
library(tidyverse)

# set seed
set.seed(9)

##############################
### PART 1: EXPECTED POINTS ###
##############################

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
nfl_data = read_csv("../data/09_expected-points.csv.gz", show_col_types = FALSE)

nfl_model_data = nfl_data |>
  mutate(
    pts_next_score_factor = factor(pts_next_score)
  )

# Task 1:
# - Refit your preferred expected-points model from Lab 5
# - A concrete default is:
#   pts_next_score ~ bs(yardline_100, df = 6) +
#                    factor(down) +
#                    log(ydstogo) +
#                    bs(half_seconds_remaining, df = 5)
# - Convert fitted class probabilities into expected points

# You will probably want helper objects like these:
score_values = sort(unique(nfl_data$pts_next_score))
game_ids = unique(nfl_data$game_id)

# Default model scaffold. You may modify this if you choose a different EP model.
ep_model = multinom(pts_next_score_factor ~
                        bs(yardline_100, df = 6) +
                        factor(down) +
                        log(ydstogo) +
                        bs(half_seconds_remaining, df = 5), data = nfl_model_data)

ep_formula = pts_next_score_factor ~
  bs(yardline_100, df = 6) +
  factor(down) +
  log(ydstogo) +
  bs(half_seconds_remaining, df = 5)

preds <- predict(ep_formula, type = "probs")

ep_data <- nfl_model_data %>%
  #select(yardline_100) %>%
  bind_cols(as.data.frame(preds)) %>%
  mutate(
    EP = (-7)*`-7`-3*`-3`-2*`-2`+2*`2`+3*`3`+7*`7`
  )
  

# Target state for the bootstrap study:
target_state = tibble(
  yardline_100 = 35,
  down = 1,
  ydstogo = 10,
  half_seconds_remaining = 1800
)

ts_preds = predict(ep_model, newdata = target_state, type = "probs")
ts_preds = as.data.frame(ts_preds)
ts_preds = ts_preds %>%
  mutate(EP = (-7)*`-7`-3*`-3`-2*`-2`+2*`2`+3*`3`+7*`7`)
  

target_state = target_state %>%
  mutate(EP = ts_preds[1,]$EP)

# Task 2:
# - Decide which bootstrap variation is most appropriate here:
#   * observation bootstrap
#   * cluster bootstrap
#   * parametric bootstrap
#   * residual bootstrap
# - State your choice in comments
# - Explain why it matches the dependence structure of this dataset

# Since we are looking for an estimate of 1st and 10 at the 35, an observation bootstrap would work as well as a cluster, so that's what I choose

# The grouping variable that matters here is:
# game_id

# Task 3:
# - Implement your chosen bootstrap with at least B = 200 resamples
# - For each resample:
#   * create a bootstrap dataset
#   * refit the EP model
#   * recompute expected points at target_state

B = 20

bootstrap_ep = rep(NA_real_, B)

# If you use a cluster bootstrap, you will likely want to:
# - sample game_ids with replacement
# - rebuild the bootstrap dataset by binding together all rows
#   from each sampled game

bootstrap_results = numeric(B)

for (b in seq_len(B)) {
  # TODO: sample games or rows, depending on your bootstrap choice.
  # sampled_game_ids = sample(game_ids, size = length(game_ids), replace = TRUE)
  sample_rows = sample(1:nrow(nfl_model_data), size = nrow(nfl_model_data), replace = TRUE)
  
  # TODO: build the bootstrap dataset.
  # boot_data = ...
  boot_data = nfl_model_data[sample_rows, ]
  
  # TODO: fit the model on boot_data.
  boot_model = multinom(ep_formula, data = boot_data, trace = FALSE)
  
  # TODO: predict class probabilities at target_state.
  boot_probs = predict(boot_model, newdata = target_state, type = "probs")
  target_state_boot = tibble(
    yardline_100 = 35,
    down = 1,
    ydstogo = 10,
    half_seconds_remaining = 1800
  )
  target_state_boot = target_state_boot %>%
    bind_cols(as.data.frame(t((boot_probs)))) %>%
    mutate(EP = (-7)*`-7`-3*`-3`-2*`-2`+2*`2`+3*`3`+7*`7`)

  # TODO: convert probabilities to expected points and store the result.
  # bootstrap_ep[b] = expected_points_from_probs(boot_probs, score_values)
  
  bootstrap_results[b] = target_state_boot$EP
  
  print(b)
}


# Task 4:
# - Store the bootstrap estimates in a vector
# - Make a plot of the bootstrap distribution

ggplot(data = as.data.frame(bootstrap_results)) +
  geom_histogram(aes(x = bootstrap_results), binwidth = 0.005, fill = "dodgerblue")

# Task 5:
# - Compute:
#   * the original fitted expected-points estimate
#   * the bootstrap standard error
#   * the 95% percentile interval

estimate = mean(bootstrap_results)
stde = sd(bootstrap_results)/sqrt(B)
upper = estimate + 1.96 * stde
lower = estimate - 1.96 *stde
#[3.842391,3.900522]


# Task 6:
# - In comments, explain why a naive row-by-row observation bootstrap
#   is less appropriate for this dataset

#In general, a naive row-by-row might not work because plays are very dependent on each other, especially by rows close together (aka consecutive plays)
#However, in this situation when only looking at 1st and 10 plays, it wouldn't be that bad



################################
### PART 2: NBA FREE THROWS ####
################################

nba_players = read_delim(
  "../data/08_nba-free-throws.csv.gz",
  #delim = ";",
  show_col_types = FALSE
)

free_throws = nba_players %>%
  select(Player, Tm, G, FT, FTA, `FT%`)

free_throws <- nba_players %>%
  group_by(Player) %>%
  # Keep only TOT row *if it exists*, otherwise keep all rows
  filter(if ("TOT" %in% Tm) Tm == "TOT" else TRUE) %>%
  ungroup() %>%
  select(Player, Tm, G, FT, FTA, `FT%`) %>%
  mutate(FT = round(FT * G), FTA = round(FTA * G)) %>%
  filter(FTA >= 25)

z = 1.96

free_throw_ci = free_throws %>%
  mutate(
    SE_wald = sqrt(`FT%` * (1 - `FT%`) / FTA),
    CI_low = pmax(0, `FT%` - 1.96 * SE_wald),
    CI_high = pmin(1, `FT%` + 1.96 * SE_wald),
    
    n_tilde = FTA + z^2,
    p_tilde = (FT + z^2 / 2) / n_tilde,
    SE_ac = sqrt(p_tilde * (1 - p_tilde) / n_tilde),
    CI_low_ac = pmax(0, p_tilde - z * SE_ac),
    CI_high_ac = pmin(1, p_tilde + z * SE_ac)
  ) %>%
  select(
    Player, Tm, G, FT, FTA, `FT%`, CI_low, CI_high, CI_low_ac, CI_high_ac
  )

# free_throw_long <- free_throw_ci %>%
#   pivot_longer(
#     cols = c(CI_low, CI_high, CI_low_ac, CI_high_ac),
#     names_to = c("bound", "type"),
#     names_pattern = "CI_(low|high)(_ac)?",
#     values_to = "value"
#   ) %>%
#   mutate(
#     type = ifelse(is.na(type), "Wald", "Agresti-Coull")
#   ) %>%
#   pivot_wider(
#     names_from = bound,
#     values_from = value
#   )

index = sample(1:nrow(free_throw_ci), size = 20)
free_throw_sample = free_throw_ci[index, ]

wald_long <- free_throw_sample %>%
  select(Player, `FT%`, CI_low, CI_high) %>%
  mutate(type = "Wald") %>%
  rename(low = CI_low, high = CI_high)
ac_long <- free_throw_sample %>%
  select(Player, `FT%`, CI_low_ac, CI_high_ac) %>%
  mutate(type = "Agresti-Coull") %>%
  rename(low = CI_low_ac, high = CI_high_ac)
free_throw_long <- bind_rows(wald_long, ac_long)

free_throw_long <- free_throw_long %>%
  mutate(
    Player = factor(Player),
    Player = fct_reorder(Player, `FT%`)
  )


ggplot(free_throw_long, aes(y = Player, x = `FT%`, color = type)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = low, xmax = high), height = 0.25) +
  scale_color_manual(values = c("Wald" = "steelblue", "Agresti-Coull" = "firebrick")) +
  labs(
    title = "Free Throw Percentage with Wald and Agresti–Coull Confidence Intervals",
    x = "Free Throw Percentage",
    y = "Player",
    color = "Interval Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom"
  )

# Task 2:
# - For each player, construct a 95% bootstrap confidence interval
#   for free-throw percentage
# - Overlay the bootstrap intervals on your Lab 8 player plot
# - Compare bootstrap, Wald, and Agresti-Coull intervals

# You may want helper functions for:
# - one bootstrap resample of a player's free throws
# - a percentile interval from bootstrap draws

# ft_made = 80
# ft_attempted = 100

bootstrap_ft_percent = function(ft_made, ft_attempted, B = 1000) {
  # TODO: create B bootstrap resamples for one player.
  # Hint: represent the season as a vector with ft_made ones and
  # ft_attempted - ft_made zeros, then sample with replacement.
  
  bootstrap_results_ft = numeric(B)
  
  for (b in seq_len(B)) {
    
    szn_vector = c(rep(1, ft_made), rep(0, ft_attempted - ft_made))
    sample_rows = sample(1:ft_attempted, size = ft_attempted, replace = TRUE)
    sample_vector = szn_vector[sample_rows]
  
    ft_percentage = mean(sample_vector)
  
    # print(b)
    # print(ft_percentage)
    
    bootstrap_results_ft[b] = ft_percentage
    
  }
 
  ftp_estimate = mean(bootstrap_results_ft)
  ftp_se = sd(bootstrap_results)/sqrt(b)
  
  ftp_upper = ftp_estimate + 1.96 * ftp_se
  ftp_lower = ftp_estimate - 1.96 * ftp_se
  
  c(lower = ftp_lower, upper = ftp_upper)
}

bootstrap_ft_percent(ft_made = 80, ft_attempted = 100, B = 50)

free_throw_sample

bootstrap_table <- free_throw_sample %>%
  rowwise() %>%
  mutate(
    ci = list(bootstrap_ft_percent(FT, FTA)),
    ci_lower = ci["lower"],
    ci_upper = ci["upper"]
  ) %>%
  select(-ci) %>%
  ungroup()

wald_long <- bootstrap_table %>%
  select(Player, `FT%`, CI_low, CI_high) %>%
  mutate(type = "Wald") %>%
  rename(low = CI_low, high = CI_high)
ac_long <- bootstrap_table %>%
  select(Player, `FT%`, CI_low_ac, CI_high_ac) %>%
  mutate(type = "Agresti-Coull") %>%
  rename(low = CI_low_ac, high = CI_high_ac)
boot_long = bootstrap_table %>%
  select(Player, `FT%`, ci_lower, ci_upper) %>%
  mutate(type = "Bootstrap") %>%
  rename(low = ci_lower, high = ci_upper)

free_throw_long <- bind_rows(wald_long, ac_long)

free_throw_long <- free_throw_long %>%
  mutate(
    Player = factor(Player),
    Player = fct_reorder(Player, `FT%`)
  )

ggplot(free_throw_long, aes(y = Player, x = `FT%`, color = type)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = low, xmax = high), height = 0.25) +
  scale_color_manual(values = c("Wald" = "steelblue", "Agresti-Coull" = "firebrick", "Bootstrap" = "gold")) +
  labs(
    title = "Free Throw Percentage with Wald and Agresti–Coull Confidence Intervals",
    x = "Free Throw Percentage",
    y = "Player",
    color = "Interval Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom"
  )

ggplot(coverage_boot, aes(x = p, y = coverage, color = factor(n))) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  labs(
    title = "Bootstrap CI Coverage Probability",
    subtitle = "Dashed line = nominal 95% coverage",
    x = "True FT Probability (p)",
    y = "Coverage Probability",
    color = "Sample Size n"
  ) +
  theme_minimal()


p_vals <- seq(0, 1, length.out = 200)
n_vals <- c(10, 50, 100, 250, 500, 1000)
M <- 100

coverage_boot <- expand_grid(
  p = p_vals,
  n = n_vals,
  rep = 1:M
) %>%
  mutate(
    ft_made = map2_int(p, n, ~ rbinom(1, size = .y, prob = .x)),
    ci = map2(ft_made, n, ~ bootstrap_ft_percent(.x, .y)),
    lower = map_dbl(ci, ~ .x["lower"]),
    upper = map_dbl(ci, ~ .x["upper"]),
    cover = (p >= lower & p <= upper)
  ) %>%
  group_by(p, n) %>%
  summarise(
    coverage = mean(cover),
    .groups = "drop"
  )





# percentile_interval = function(bootstrap_draws, level = 0.95) {
#   alpha = 1 - level
#   quantile(
#     bootstrap_draws,
#     probs = c(alpha / 2, 1 - alpha / 2),
#     na.rm = TRUE
#   )
# }

# Task 3:
# - Revisit the Lab 8 simulation study
# - Add bootstrap confidence intervals to the same coverage framework
# - Plot bootstrap coverage probability against p
# - Compare to Wald and Agresti-Coull

# Useful objects from Lab 8:
sample_sizes = c(10, 50, 100, 250, 500, 1000)
p_grid = seq(0, 1, length.out = 1000)
M = 100
z_975 = qnorm(0.975)

bootstrap_coverage = tibble()

# TODO: extend your Lab 8 simulation loop.
# For each simulated Binomial(n, p) count, compute:
# - Wald interval
# - Agresti-Coull interval
# - bootstrap percentile interval
# Then summarize coverage by method, n, and p.
