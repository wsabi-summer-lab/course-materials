#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(broom)
library(furrr)

plan(multisession)

# set seed
set.seed(8)

#######################
### NBA FREE THROWS ###
#######################

# load data
nba_players = read.csv("../data/08_nba-free-throws.csv.gz", sep = ";")

nba_players = nba_players %>% 
  mutate(FT_total = FT * G) %>%
  mutate(FTA_total = FTA * G) %>%
  group_by(Player) %>%
  summarise(
    FT_total = sum(FT_total),
    FTA_total = sum(FTA_total),
    FT_pct = FT_total / FTA_total,
    .groups = "drop"
  ) %>%
  ungroup() %>%
  filter(FTA_total >= 25)

nba_players_ci = nba_players %>%
  mutate(
    z = 1.96,
    se = sqrt(FT_pct * (1 - FT_pct) / FTA_total),
    lower_wald = FT_pct - z * se,
    upper_wald = FT_pct + z * se
  ) %>%
  mutate(
    z2 = z^2,
    p_tilde = (FT_total + z2 / 2) / (FTA_total + z2),
    se_tilde = sqrt(p_tilde * (1 - p_tilde) / (FTA_total + z2)),
    lower_agresti = p_tilde - z * se_tilde,
    upper_agresti = p_tilde + z * se_tilde
  )

ggplot(nba_players_ci, aes(x = Player, y = FT_pct)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_wald, ymax = upper_wald), width = 0.2, color = "blue", alpha = 0.5) +
  geom_errorbar(aes(ymin = lower_agresti, ymax = upper_agresti), width = 0.2, color = "red", alpha = 0.5) +
  coord_flip() +
  labs(
    title = "NBA Free Throw % with Wald (blue) and Agresti-Coull (red) CIs",
    x = "Player",
    y = "FT%"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))


# Create grid
sim_grid <- expand.grid(n = c(10, 50, 100, 250, 500, 1000),
                        p = seq(0, 1, length.out = 100),
                        rep = 1:100)

# Convert to tibble
sim_grid <- as_tibble(sim_grid)

# Parallel simulation
results <- sim_grid %>% 
  mutate(sim = future_pmap_dfr(list(n, p, rep), function(n, p, rep) {
    x <- rbinom(1, n, p)
    p_hat <- x / n
    
    # Wald
    se <- sqrt(p_hat * (1 - p_hat) / n)
    lower_wald <- max(0, p_hat - 1.96 * se)
    upper_wald <- min(1, p_hat + 1.96 * se)
    
    # Agresti-Coull
    z2 <- 1.96^2
    n_tilde <- n + z2
    p_tilde <- (x + z2 / 2) / n_tilde
    se_tilde <- sqrt(p_tilde * (1 - p_tilde) / n_tilde)
    lower_agresti <- max(0, p_tilde - 1.96 * se_tilde)
    upper_agresti <- min(1, p_tilde + 1.96 * se_tilde)
    
    tibble(
      x = x,
      p_hat = p_hat,
      lower_wald = lower_wald,
      upper_wald = upper_wald,
      lower_agresti = lower_agresti,
      upper_agresti = upper_agresti
    )
  }))

results <- results %>%
  mutate(
    cover_wald = (p >= lower_wald) & (p <= upper_wald),
    cover_agresti = (p >= lower_agresti) & (p <= upper_agresti)
  )

results %>%
  group_by(n, p) %>%
  summarise(
    coverage_wald = mean(cover_wald),
    coverage_agresti = mean(cover_agresti),
    .groups = "drop"
  ) %>%
  mutate(n = factor(n)) %>%
  ggplot(aes(x = p)) +
  geom_line(aes(y = coverage_wald, color = "Wald CI")) +
  geom_line(aes(y = coverage_agresti, color = "Agresti-Coull CI")) +
  facet_wrap(~n, scales = "free_y") +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray") +
  labs(
    title = "Coverage Probability of Wald and Agresti-Coull 95% Confidence Intervals",
    x = "True Probability p",
    y = "Coverage Probability",
    color = "CI Type"
  ) +
  theme_minimal()


coverage_summary <- results %>%
  group_by(n, p) %>%
  summarise(
    coverage_wald = mean(cover_wald),
    coverage_agresti = mean(cover_agresti),
    .groups = "drop"
  ) %>%
  mutate(n = factor(n)) %>%              # Convert n to factor for plotting
  pivot_longer(
    cols = c(coverage_wald, coverage_agresti),
    names_to = "method",
    values_to = "coverage"
  ) %>%
  mutate(
    method = ifelse(method == "coverage_wald", "Wald", "Agresti-Coull")
  )

ggplot(coverage_summary, aes(x = p, y = coverage, color = method)) +
  geom_line() +
  facet_wrap(~n, scales = "free_y") +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray") +
  labs(
    title = "Coverage Probability of Wald and Agresti-Coull 95% Confidence Intervals",
    x = "True Probability p",
    y = "Coverage Probability",
    color = "CI Method"
  ) +
  theme_minimal()


##########################
p_be = 110 / 210
p_interval = seq(p_be, 1, length.out = 100)
# Compute CI for each p
ci_results <- sapply(p_interval, function(p) {
  n = 210
  x = 110
  
  # Wald CI
  se_wald = sqrt(p * (1 - p) / n)
  lower_wald = max(0, p - 1.96 * se_wald)
  upper_wald = min(1, p + 1.96 * se_wald)
  
  # Agresti-Coull CI
  z2 = 1.96^2
  n_tilde = n + z2
  p_tilde = (x + z2 / 2) / n_tilde
  se_agresti = sqrt(p_tilde * (1 - p_tilde) / n_tilde)
  lower_agresti = max(0, p_tilde - 1.96 * se_agresti)
  upper_agresti = min(1, p_tilde + 1.96 * se_agresti)
  
  c(lower_wald, upper_wald, lower_agresti, upper_agresti)
})

# Plot the results: Count(CI > p_be) vs p
data.frame(
  p = p_interval,
  lower_wald = ci_results[1, ],
  upper_wald = ci_results[2, ],
  lower_agresti = ci_results[3, ],
  upper_agresti = ci_results[4, ]
) %>%
  ggplot(aes(x = p)) +
  geom_ribbon(aes(ymin = lower_wald, ymax = upper_wald), fill = "blue", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_agresti, ymax = upper_agresti), fill = "red", alpha = 0.3) +
  geom_hline(yintercept = p_be, linetype = "dashed", color = "black") +
  labs(
    title = "Confidence Intervals for p_be with Wald (blue) and Agresti-Coull (red)",
    x = "True Probability p",
    y = "Confidence Interval"
  ) +
  theme_minimal()