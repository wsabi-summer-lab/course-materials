#######################
### Authors: JP, RB ###
#######################

########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "tidyr"))

library(dplyr)
library(ggplot2)
library(tidyr)

################
### SETTINGS ###
################

set.seed(13)

figure_dir = "2026/lectures/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

k = 30
sigma = 1
theta = seq(-2.5, 2.5, length.out = k)

theme_week = theme_minimal(base_size = 12) +
    theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
    )

positive_part_js = function(x, sigma = 1) {
    center = mean(x)
    spread = sum((x - center)^2)
    factor = max(0, 1 - ((length(x) - 3) * sigma^2) / spread)
    center + factor * (x - center)
}

##########################
### ONE NOISY LEADERBOARD
##########################

x = rnorm(k, theta, sigma)
js = positive_part_js(x, sigma)

leaderboard = tibble(
    player = seq_len(k),
    truth = theta,
    MLE = x,
    Shrinkage = js
) |>
    pivot_longer(c(MLE, Shrinkage), names_to = "estimator", values_to = "estimate")

leaderboard_plot = ggplot(leaderboard, aes(truth, estimate, color = estimator)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
    geom_segment(
        aes(xend = truth, yend = truth),
        alpha = 0.35,
        linewidth = 0.6
    ) +
    geom_point(size = 2.2) +
    facet_wrap(~ estimator) +
    scale_color_manual(values = c("MLE" = "#B22222", "Shrinkage" = "#1F78B4")) +
    coord_equal() +
    labs(
        title = "Estimation Error by Estimator",
        x = "True player quality",
        y = "Estimated player quality"
    ) +
    theme_week +
    theme(legend.position = "none")

ggsave(
    file.path(figure_dir, "13_noisy-leaderboard.png"),
    leaderboard_plot,
    width = 9,
    height = 5.2,
    dpi = 300
)

########################
### RISK SIMULATION ####
########################

n_sims = 5000

risk_draws = replicate(n_sims, {
    draw = rnorm(k, theta, sigma)
    shrink = positive_part_js(draw, sigma)
    c(
        MLE = mean((draw - theta)^2),
        Shrinkage = mean((shrink - theta)^2)
    )
})

risk = as.data.frame(t(risk_draws)) |>
    pivot_longer(everything(), names_to = "estimator", values_to = "joint_mse")

risk_summary = risk |>
    group_by(estimator) |>
    summarise(mean_mse = mean(joint_mse), .groups = "drop")

risk_plot = ggplot(risk, aes(joint_mse, fill = estimator)) +
    geom_density(alpha = 0.45) +
    geom_vline(
        data = risk_summary,
        aes(xintercept = mean_mse, color = estimator),
        linetype = "dashed",
        linewidth = 0.9
    ) +
    scale_fill_manual(values = c("MLE" = "#B22222", "Shrinkage" = "#1F78B4")) +
    scale_color_manual(values = c("MLE" = "#B22222", "Shrinkage" = "#1F78B4")) +
    labs(
        title = "Mean Squared Error across Simulations",
        x = "Mean squared error across all 30 players",
        y = "Density",
        fill = "Estimator",
        color = "Estimator"
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "13_risk-comparison.png"),
    risk_plot,
    width = 8.5,
    height = 5.2,
    dpi = 300
)

#################################
### LOSS FUNCTION COMPARISON ####
#################################

coordinate_errors = tibble(
    player = seq_len(k),
    MLE = (x - theta)^2,
    Shrinkage = (js - theta)^2
) |>
    pivot_longer(c(MLE, Shrinkage), names_to = "estimator", values_to = "squared_error")

coordinate_plot = ggplot(coordinate_errors, aes(player, squared_error, color = estimator)) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 1.6) +
    scale_color_manual(values = c("MLE" = "#B22222", "Shrinkage" = "#1F78B4")) +
    labs(
        title = "Player-Level Squared Error",
        x = "Player",
        y = "Squared error",
        color = "Estimator"
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "13_coordinate-errors.png"),
    coordinate_plot,
    width = 9,
    height = 5,
    dpi = 300
)
