#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(9)

# red: 20
# total: 111

# Construct Wald CI for the true probability of red balls
wald_ci = function(successes, trials, z = 1.96) {
  p_hat = successes / trials
  se = sqrt(p_hat * (1 - p_hat) / trials)
  lower = p_hat - z * se
  upper = p_hat + z * se
  return(c(lower, upper))
}
# Construct Agresti-Coull CI for the true probability of red balls
agresti_ci = function(successes, trials, z = 1.96) {
  z2 = z^2
  p_tilde = (successes + z2 / 2) / (trials + z2)
  se_tilde = sqrt(p_tilde * (1 - p_tilde) / (trials + z2))
  lower = p_tilde - z * se_tilde
  upper = p_tilde + z * se_tilde
  return(c(lower, upper))
}
# Construct Bootstrap CI
bootstrap_ci = function(successes, trials, n_boot = 250, alpha = 0.05) {
  p_hat = successes / trials
  boot_samples = replicate(n_boot, rbinom(1, trials, p_hat))
  boot_proportions = boot_samples / trials
  lower = quantile(boot_proportions, alpha / 2)
  upper = quantile(boot_proportions, 1 - alpha / 2)
  return(c(lower, upper))
}
# Build a table to compare the three CIs
ci_table = tibble(
  Method = c("Wald", "Agresti-Coull", "Bootstrap"),
  Lower = c(wald_ci(20, 111)[1], agresti_ci(20, 111)[1], bootstrap_ci(20, 111)[1]),
  Upper = c(wald_ci(20, 111)[2], agresti_ci(20, 111)[2], bootstrap_ci(20, 111)[2])
)
ci_table


#######################
### NBA FREE THROWS ###
#######################

# load data
nba_players = read_delim("../data/09_nba-free-throws.csv.gz", delim = ";")

nba_players = nba_players %>% 
  mutate(FT_total = FT * G) %>%
  mutate(FTA_total = FTA * G) %>%
  group_by(Player) %>%
  summarise(
    FT_total = round(sum(FT_total)),
    FTA_total = round(sum(FTA_total)),
    FT_pct = FT_total / FTA_total,
    .groups = "drop"
  ) %>%
  ungroup() %>%
  filter(FTA_total >= 25)

# Construct bootstrap CI for each player
nba_players_ci = nba_players %>%
  rowwise() %>%
  mutate(
    bootstrap_ci = list(bootstrap_ci(FT_total, FTA_total)),
    lower_bootstrap = bootstrap_ci[[1]][1],
    upper_bootstrap = bootstrap_ci[[2]][1]
  ) %>%
  ungroup()

# Compare with Wald and Agresti-Coull CIs
nba_players_ci = nba_players_ci %>%
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
# Plot the results
nba_players_ci %>% filter(FTA_total >= 250) %>%
  ggplot(aes(x = Player, y = FT_pct)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_wald, ymax = upper_wald), width = 0.2, color = "blue", alpha = 0.5) +
  geom_errorbar(aes(ymin = lower_agresti, ymax = upper_agresti), width = 0.2, color = "red", alpha = 0.5) +
  geom_errorbar(aes(ymin = lower_bootstrap, ymax = upper_bootstrap), width = 0.2, color = "green", alpha = 0.5) +
  coord_flip() +
  labs(
    title = "NBA Free Throw % with Wald (blue), Agresti-Coull (red), and Bootstrap (green) CIs",
    x = "Player",
    y = "FT%"
  )

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
    
    # Bootstrap
    bootstrap_ci <- bootstrap_ci(x, n)
    lower_bootstrap <- bootstrap_ci[[1]][1]
    upper_bootstrap <- bootstrap_ci[[2]][1]
    
    tibble(
      x = x,
      p_hat = p_hat,
      lower_wald = lower_wald,
      upper_wald = upper_wald,
      lower_agresti = lower_agresti,
      upper_agresti = upper_agresti,
      lower_bootstrap = lower_bootstrap,
      upper_bootstrap = upper_bootstrap
    )
  }))

results <- results %>%
  mutate(
    cover_wald = (p >= sim$lower_wald) & (p <= sim$upper_wald),
    cover_agresti = (p >= sim$lower_agresti) & (p <= sim$upper_agresti),
    cover_bootstrap = (p >= sim$lower_bootstrap) & (p <= sim$upper_bootstrap)
  )

results %>%
  group_by(n, p) %>%
  summarise(
    coverage_wald = mean(cover_wald),
    coverage_agresti = mean(cover_agresti),
    coverage_bootstrap = mean(cover_bootstrap),
    .groups = "drop"
  ) %>%
  mutate(n = factor(n)) %>%
  ggplot(aes(x = p)) +
  geom_line(aes(y = coverage_wald, color = "Wald CI")) +
  geom_line(aes(y = coverage_agresti, color = "Agresti-Coull CI")) +
  geom_line(aes(y = coverage_bootstrap, color = "Bootstrap CI")) +
  facet_wrap(~n, scales = "free_y") +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray") +
  labs(
    title = "Coverage Probability of Wald, Agresti-Coull, and Bootstrap 95% Confidence Intervals",
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
    coverage_bootstrap = mean(cover_bootstrap),
    .groups = "drop"
  ) %>%
  mutate(n = factor(n)) %>%              # Convert n to factor for plotting
  pivot_longer(
    cols = c(coverage_wald, coverage_agresti, coverage_bootstrap),
    names_to = "method",
    values_to = "coverage"
  ) %>%
  mutate(
    method = ifelse(method == "coverage_wald", "Wald", 
              ifelse(method == "coverage_agresti", "Agresti-Coull", "Bootstrap"))
  )

ggplot(coverage_summary, aes(x = p, y = coverage, color = method)) +
  geom_line() +
  facet_wrap(~n, scales = "free_y") +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray") +
  labs(
    title = "Coverage Probability of Wald, Agresti-Coull, and Bootstrap 95% Confidence Intervals",
    x = "True Probability p",
    y = "Coverage Probability",
    color = "CI Type"
  ) +
  theme_minimal()