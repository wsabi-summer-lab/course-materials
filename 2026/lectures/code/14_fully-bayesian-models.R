#######################
### Authors: JP, RB ###
#######################

########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "MASS", "readr", "tidyr"))

library(dplyr)
library(ggplot2)
library(MASS)
library(readr)
library(tidyr)

################
### SETTINGS ###
################

set.seed(14)

data_path = "2026/lectures/data/14_nfl-games.csv.gz"
figure_dir = "2026/lectures/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

sigma_game = 14
sigma_hfa = 7
sigma_team = 7
n_draws = 2500

theme_week = theme_minimal(base_size = 12) +
    theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
    )

################
### LOAD DATA ##
################

nfl = read_csv(data_path, show_col_types = FALSE) |>
    filter(season_type == "REG") |>
    mutate(
        season = as.character(season),
        home_season = paste(home_team, season, sep = "_"),
        away_season = paste(away_team, season, sep = "_")
    )

######################################
### CONJUGATE BAYES POWER RATINGS ####
######################################

team_seasons = sort(unique(c(nfl$home_season, nfl$away_season)))
p = 1 + length(team_seasons)

X = matrix(0, nrow = nrow(nfl), ncol = p)
colnames(X) = c("home_field", team_seasons)
X[, "home_field"] = 1

home_col = match(nfl$home_season, team_seasons) + 1
away_col = match(nfl$away_season, team_seasons) + 1
X[cbind(seq_len(nrow(nfl)), home_col)] = 1
X[cbind(seq_len(nrow(nfl)), away_col)] = -1

prior_variance = c(sigma_hfa^2, rep(sigma_team^2, length(team_seasons)))
prior_precision = diag(1 / prior_variance)

posterior_cov = solve(crossprod(X) / sigma_game^2 + prior_precision)
posterior_mean = posterior_cov %*% crossprod(X, nfl$pts_H_minus_A) / sigma_game^2
posterior_draws = mvrnorm(n_draws, mu = as.vector(posterior_mean), Sigma = posterior_cov)
colnames(posterior_draws) = colnames(X)

###########################
### NON-STRENGTH FIGURE ###
###########################

hfa_draws = posterior_draws[, "home_field"]
even_home_win = pnorm(hfa_draws / sigma_game)

non_strength = bind_rows(
    tibble(parameter = "Home-field advantage (points)", value = hfa_draws),
    tibble(parameter = "P(home wins | equal teams)", value = even_home_win)
) |>
    group_by(parameter) |>
    summarise(
        mean = mean(value),
        lower = quantile(value, 0.025),
        upper = quantile(value, 0.975),
        .groups = "drop"
    )

non_strength_plot = ggplot(non_strength, aes(mean, parameter)) +
    geom_errorbar(
        aes(xmin = lower, xmax = upper),
        orientation = "y",
        width = 0.15,
        linewidth = 0.8
    ) +
    geom_point(size = 2.8, color = "#1F78B4") +
    facet_wrap(~ parameter, scales = "free_x", ncol = 1) +
    labs(
        title = "Posterior Summaries",
        x = "Posterior mean and 95% credible interval",
        y = NULL
    ) +
    theme_week +
    theme(
        strip.text = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    )

ggsave(
    file.path(figure_dir, "14_non-strength.png"),
    non_strength_plot,
    width = 8,
    height = 5.3,
    dpi = 300
)

############################
### TEAM-STRENGTH FIGURE ###
############################

latest_season = max(as.integer(nfl$season))
latest_columns = grep(paste0("_", latest_season, "$"), colnames(posterior_draws), value = TRUE)

team_strength = tibble(
    team = sub(paste0("_", latest_season, "$"), "", latest_columns),
    mean = colMeans(posterior_draws[, latest_columns, drop = FALSE]),
    lower = apply(posterior_draws[, latest_columns, drop = FALSE], 2, quantile, 0.025),
    upper = apply(posterior_draws[, latest_columns, drop = FALSE], 2, quantile, 0.975)
) |>
    arrange(mean) |>
    mutate(team = factor(team, levels = team))

team_plot = ggplot(team_strength, aes(mean, team)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray45") +
    geom_errorbar(
        aes(xmin = lower, xmax = upper),
        orientation = "y",
        width = 0.15,
        color = "#4C78A8"
    ) +
    geom_point(size = 2, color = "#B22222") +
    labs(
        title = paste0(latest_season, " NFL Team Strengths"),
        x = "Team strength in points above average",
        y = NULL
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "14_team-strength.png"),
    team_plot,
    width = 8.5,
    height = 8.5,
    dpi = 300
)

##################################
### POSTERIOR DECISION FIGURE ####
##################################

selected = c("DAL", "PHI", "SF", "KC")
selected_columns = paste(selected, latest_season, sep = "_")
selected_draws = posterior_draws[, selected_columns, drop = FALSE]

decision_data = as.data.frame(selected_draws) |>
    pivot_longer(everything(), names_to = "team_season", values_to = "strength") |>
    mutate(team = sub(paste0("_", latest_season, "$"), "", team_season))

decision_plot = ggplot(decision_data, aes(strength, fill = team, color = team)) +
    geom_density(alpha = 0.22, linewidth = 0.8) +
    labs(
        title = "Posterior Team-Strength Distributions",
        x = "Team strength in points above average",
        y = "Posterior density",
        fill = "Team",
        color = "Team"
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "14_posterior-comparisons.png"),
    decision_plot,
    width = 8.5,
    height = 5.2,
    dpi = 300
)
