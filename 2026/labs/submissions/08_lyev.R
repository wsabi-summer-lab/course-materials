#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(8)

#########################
### PART 1: NBA FREE THROWS ###
#########################

# load data
nba_players = read_delim(
  "../data/08_nba-free-throws.csv.gz",
  delim = ";",
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


###########################
### PART 2: LLN WARM-UP ###
###########################

lln_p = 0.75
lln_n = 1000
lln_paths = 100

sim_df <- tibble(
  path = rep(1:lln_paths, each = lln_n),
  n    = rep(1:lln_n, times = lln_paths),
  ft   = rbinom(lln_paths * lln_n, size = 1, prob = lln_p)
) %>%
  group_by(path) %>%
  arrange(n, .by_group = TRUE) %>%
  mutate(
    running_est = cumsum(ft) / n
  ) %>%
  ungroup()

ggplot(sim_df, aes(x = n, y = running_est, group = path)) +
  geom_line(alpha = 0.2, color = "steelblue") +
  geom_hline(yintercept = lln_p, color = "red", linetype = "dashed") +
  labs(
    title = "Law of Large Numbers: Free Throw Percentage Convergence",
    x = "Number of Free Throws (n)",
    y = "Running Estimate of FT%"
  ) +
  theme_minimal()

################################
### PART 3: SIMULATION STUDY ###
################################

z_975 = qnorm(0.975)
p_grid = seq(0, 1, length.out = 1000)
sample_sizes = c(10, 50, 100, 250, 500, 1000)
M = 100

x_vals <- seq(0, 1, length.out = 1000)
n_vals <- c(10, 50, 100, 250, 500, 1000)


sim_results <- expand_grid(
  x = x_vals,
  n = n_vals,
  rep = 1:M
) %>%
  mutate(
    makes = map2_int(x, n, ~ rbinom(1, size = .y, prob = .x)),
    ft_pct = makes / n
  )

sim_results <- expand_grid(
  x = x_vals,
  n = n_vals,
  rep = 1:M
) %>%
  mutate(
    makes = map2_int(x, n, ~ rbinom(1, size = .y, prob = .x)),
    phat  = makes / n,
    
    # Wald CI
    se_wald = sqrt(phat * (1 - phat) / n),
    wald_low  = pmax(0, phat - z * se_wald),
    wald_high = pmin(1, phat + z * se_wald),
    
    # Agresti–Coull CI
    n_tilde = n + z^2,
    p_tilde = (makes + z^2 / 2) / n_tilde,
    se_ac   = sqrt(p_tilde * (1 - p_tilde) / n_tilde),
    ac_low  = pmax(0, p_tilde - z * se_ac),
    ac_high = pmin(1, p_tilde + z * se_ac)
  )

coverage <- sim_results %>%
  mutate(
    wald_cover = (x >= wald_low & x <= wald_high),
    ac_cover   = (x >= ac_low   & x <= ac_high)
  ) %>%
  group_by(x, n) %>%
  summarise(
    wald_cov = mean(wald_cover),
    ac_cov   = mean(ac_cover),
    .groups = "drop"
  )

coverage_long <- coverage %>%
  pivot_longer(
    cols = c(wald_cov, ac_cov),
    names_to = "method",
    values_to = "coverage"
  ) %>%
  mutate(
    method = recode(method,
                    wald_cov = "Wald",
                    ac_cov   = "Agresti–Coull")
  )


ggplot(coverage_long, aes(x = x, y = coverage, color = method)) +
  geom_line(alpha = 0.8) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  facet_wrap(~ n, ncol = 3) +
  scale_color_manual(values = c("Wald" = "steelblue", "Agresti–Coull" = "firebrick")) +
  labs(
    title = "Coverage Probability of Wald vs Agresti–Coull CIs",
    subtitle = "Dashed line = nominal 95% coverage",
    x = "True FT Probability (p)",
    y = "Coverage Probability",
    color = "Interval Method"
  ) +
  theme_minimal()




############################
### PART 4: SKITTLES DEMO ###
############################

# Replace these with your observed counts from the Skittles activity
skittles_n = 50 + 47 + 15
skittles_r = 12 + 15

ratio = skittles_r / skittles_n
# 0.2410714

#Wald CI
wald_upper = ratio + 1.96 * sqrt(ratio*(1-ratio)/skittles_n)
wald_lower = ratio - 1.96 * sqrt(ratio*(1-ratio)/skittles_n)
#[0.1618541 , 0.3202888]

#AC CI

n_squiggle = skittles_n + 1.96^2
p_squiggle = (skittles_r + 1.96^2)/skittles_n
CI_upper = p_squiggle + 1.96*sqrt(p_squiggle*(1-p_squiggle)/n_squiggle)
CI_lower = p_squiggle - 1.96*sqrt(p_squiggle*(1-p_squiggle)/n_squiggle)
#[0.1940245 , 0.3567183]




