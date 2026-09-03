#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(11)

#######################
### NBA FREE THROWS ###
#######################

# load data
nba_players = read_delim("../data/11_nba-free-throws.csv.gz", delim = ";")

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

# Generate 95% posterior CIs for various alpha and beta
generate_posterior_ci <- function(alpha, beta, n) {
  p = rbeta(n, alpha, beta)
  lower = quantile(p, 0.025)
  upper = quantile(p, 0.975)
  return(c(lower, upper))
}
# Combine with player data
nba_players_ci = nba_players %>%
  rowwise() %>%
  mutate(
    posterior_ci = list(generate_posterior_ci(FT_total + 50, FTA_total - FT_total + 10, n = 1000)),
    lower_posterior = posterior_ci[[1]][1],
    upper_posterior = posterior_ci[[2]][1]
  ) %>%
  ungroup()

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

nba_players_ci = nba_players_ci %>%
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

nba_players_ci %>% filter(FTA_total >= 250) %>%
  ggplot(aes(x = Player, y = FT_pct)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_wald, ymax = upper_wald), width = 0.2, color = "blue", alpha = 0.5) +
  geom_errorbar(aes(ymin = lower_agresti, ymax = upper_agresti), width = 0.2, color = "red", alpha = 0.5) +
  geom_errorbar(aes(ymin = lower_bootstrap, ymax = upper_bootstrap), width = 0.2, color = "green", alpha = 0.5) +
  geom_errorbar(aes(ymin = lower_posterior, ymax = upper_posterior), width = 0.2, color = "yellow", alpha = 0.5) +
  coord_flip() +
  labs(
    title = "NBA Free Throw % with Wald (blue), Agresti-Coull (red), Bootstrap (green), and Posterior (alpha=50, beta=10, yellow) CIs",
    x = "Player",
    y = "FT%"
  )
