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

teams = unique(c(nfl_data$home_team, nfl_data$away_team))
teams = tibble(team = sort(teams), team_id = seq_along(teams))
seasons = unique(nfl_data$season)

nfl_data = nfl_data %>%
  mutate(H = match(home_team, teams$team)) %>%
  mutate(A = match(away_team, teams$team)) %>%
  mutate(S = match(season, seasons))

model = stan_model(file = "../submissions/14_tianshu.stan")
model

data_train = list(
  N_games = nrow(nfl_data),
  N_teams = nrow(teams),
  N_seasons = length(unique(nfl_data$S)),
  y = nfl_data$pts_H_minus_A,
  H = nfl_data$H,
  A = nfl_data$A,
  S = nfl_data$S
)

fit = sampling(
  model, data = data_train, iter = 1500, chains = 1, seed = 12345
)
fit

summary_df = summary(fit, pars = "alphas")$summary
rownames(summary_df) <- teams$team
summary_df %>%
  as.data.frame() %>%
  rownames_to_column("team") %>%
  arrange(mean) %>%
  mutate(team = factor(team, levels = unique(team))) %>%
  ggplot(aes(x = team, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.2) +
  coord_flip() +
  labs(
    title = "Estimated Home Field Advantage for NFL Teams",
    x = "Team",
    y = "Estimated Home Field Advantage (mean ± 95% CI)"
  ) +
  theme_minimal()