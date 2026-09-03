#############
### SETUP ###
#############

# install.packages(c("ggplot2", "rstan", "tidyverse"))
library(ggplot2)
library(rstan)
library(tidyverse)

# set seed
set.seed(14)

#################
### NFL GAMES ###
#################

# load NFL games data from 2018-2023
nfl_data = read_csv("../data/14_nfl-games.csv.gz")
# preview data
head(nfl_data)


season_levels <- sort(unique(nfl_data$season))
nfl_data <- nfl_data %>% 
  mutate(S = match(season, season_levels))
nfl_data$S

team_levels <- sort(unique(c(nfl_data$home_team, nfl_data$away_team)))
team_key <- tibble(team = team_levels,
                   idx  = seq_along(team_levels))
nfl_data <- nfl_data %>%
  left_join(team_key, by = c("home_team" = "team")) %>% 
  rename(H = idx) %>% 
  left_join(team_key, by = c("away_team" = "team")) %>% 
  rename(A = idx)
nfl_data$H

nfl_data <- nfl_data %>% 
  mutate(y = pts_H_minus_A)

stan_data <- list(
  N_games   = nrow(nfl_data),
  N_teams   = length(team_levels),
  N_seasons = length(season_levels),
  y = nfl_data$y,
  H = nfl_data$H,
  A = nfl_data$A,
  S = nfl_data$S
)

# print columns of nfl_data
print(colnames(nfl_data))

model = stan_model(file = "../submissions/14_chris-bugs.stan")
fit = sampling(model, data = stan_data, iter = 2000, chains = 4)

# print fit summary
print(fit, pars = c("mu_alpha", "sigma_HFA", "rho",
                    "sigma_games", "sigma_teams", "sigma_seasons"))

# Create 95% credible intervals and posteriors means for the home-field advantage parameters of each team
draws <- rstan::extract(fit, pars = c("mu_alpha", "alpha"), permuted = TRUE)

mu_vec     <- draws$mu_alpha                       # length = N_draws
alpha_arr  <- draws$alpha                          # may be 2-D or 3-D

# Robustly coerce alpha to a 2-D matrix: draws × teams
if (length(dim(alpha_arr)) == 3) {                 # iterations × chains × teams
  alpha_mat <- matrix(aperm(alpha_arr, c(1, 3, 2)),   # bring teams to 2nd dim
                      ncol = dim(alpha_arr)[3])
} else {
  alpha_mat <- alpha_arr                           # already draws × teams
}

hfa_mat <- sweep(alpha_mat, 1, mu_vec, FUN = "+")  # add mu to every column

hfa_summary <- tibble(
  team      = team_levels,
  mean      = colMeans(hfa_mat),
  lower_95  = apply(hfa_mat, 2, quantile, 0.025),
  upper_95  = apply(hfa_mat, 2, quantile, 0.975)
) %>%
  arrange(desc(mean))                              

print(hfa_summary, n = nrow(hfa_summary))

# Plot the posteriors means and 95% credible intervals for the home-field advantage parameters of each team
ggplot(hfa_summary, aes(x = reorder(team, mean), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.2) +
  coord_flip() +
  labs(
    title = "Home-Field Advantage Parameters for NFL Teams",
    x = "Team",
    y = "Home-Field Advantage (HFA)"
  ) +
  theme_minimal()




# Create 95% credible intervals and posteriors means for the team strength parameters of each team
draws   <- rstan::extract(fit, pars = "betas", permuted = TRUE)
beta_ar <- draws$betas 

mean_mat   <- apply(beta_ar, c(2, 3), mean)                      # teams × seasons
lower_mat  <- apply(beta_ar, c(2, 3), quantile, 0.025)
upper_mat  <- apply(beta_ar, c(2, 3), quantile, 0.975)

beta_summary <- tibble(
  team   = rep(team_levels, times = length(season_levels)),
  season = rep(season_levels, each = length(team_levels)),
  mean      = as.vector(mean_mat),
  lower_95  = as.vector(lower_mat),
  upper_95  = as.vector(upper_mat)
)

print(beta_summary, n = nrow(beta_summary))



# Plot posterior means of the team strength parameters for each team along with their 95% credible intervals
ggplot(beta_summary, aes(x = reorder(team, mean), y = mean, color = factor(season))) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.2) +
  coord_flip() +
  labs(
    title = "Team Strength Parameters for NFL Teams",
    x = "Team",
    y = "Team Strength",
    color = "Season"
  ) +
  theme_minimal()


