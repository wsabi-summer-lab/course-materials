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

set.seed(11)

figure_dir = "2026/lectures/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

theme_week = theme_minimal(base_size = 12) +
    theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold")
    )

#############################
### PRIOR CENTER/STRENGTH ####
#############################

prior_specs = tibble(
    prior = c(
        "Skeptical: Beta(16, 16)",
        "Weak: Beta(3, 3)",
        "Favorite: Beta(21, 11)",
        "Strong favorite: Beta(41, 21)"
    ),
    alpha = c(16, 3, 21, 41),
    beta = c(16, 3, 11, 21)
) |>
    mutate(
        center = alpha / (alpha + beta),
        strength = alpha + beta
    )

prior_curves = crossing(
    prior_specs,
    p = seq(0.001, 0.999, length.out = 600)
) |>
    mutate(density = dbeta(p, alpha, beta))

prior_plot = ggplot(prior_curves, aes(p, density, color = prior)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray45") +
    scale_color_brewer(palette = "Dark2") +
    labs(
        title = "Beta Priors with Different Centers and Strengths",
        x = "Underlying win probability p",
        y = "Prior density",
        color = "Prior"
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "11_beta-priors.png"),
    prior_plot,
    width = 9,
    height = 5.5,
    dpi = 300
)

########################
### PRIOR UPDATING #####
########################

update_specs = tribble(
    ~stage, ~wins, ~losses,
    "Before season", 0, 0,
    "After a 3-0 start", 3, 0,
    "After a 45-30 start", 45, 30
)

alpha_prior = 16
beta_prior = 16

posterior_curves = crossing(
    update_specs,
    p = seq(0.001, 0.999, length.out = 600)
) |>
    mutate(
        alpha_post = alpha_prior + wins,
        beta_post = beta_prior + losses,
        density = dbeta(p, alpha_post, beta_post),
        posterior_mean = alpha_post / (alpha_post + beta_post)
    )

update_plot = ggplot(posterior_curves, aes(p, density, color = stage)) +
    geom_line(linewidth = 1.05) +
    geom_vline(
        data = distinct(posterior_curves, stage, posterior_mean),
        aes(xintercept = posterior_mean, color = stage),
        linetype = "dotted",
        linewidth = 0.8,
        show.legend = FALSE
    ) +
    scale_color_manual(
        values = c(
            "Before season" = "#4C78A8",
            "After a 3-0 start" = "#E0A458",
            "After a 45-30 start" = "#D95F02"
        )
    ) +
    labs(
        title = "Prior and Posterior Distributions",
        x = "Underlying win probability p",
        y = "Posterior density",
        color = "Information available"
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "11_prior-updating.png"),
    update_plot,
    width = 9,
    height = 5.5,
    dpi = 300
)

################################
### ESTIMATOR COMPARISON #######
################################

games = 0:162
comparison = crossing(
    games_played = games,
    true_p = c(0.45, 0.50, 0.60)
) |>
    group_by(games_played, true_p) |>
    mutate(wins = if_else(games_played == 0, 0, rbinom(1, games_played, true_p))) |>
    ungroup() |>
    mutate(
        mle = if_else(games_played == 0, NA_real_, wins / games_played),
        posterior_mean = (wins + alpha_prior) /
            (games_played + alpha_prior + beta_prior)
    )

trajectory_plot = comparison |>
    select(games_played, true_p, MLE = mle, `Posterior mean` = posterior_mean) |>
    pivot_longer(c(MLE, `Posterior mean`), names_to = "estimator", values_to = "estimate") |>
    ggplot(aes(games_played, estimate, color = estimator)) +
    geom_hline(aes(yintercept = true_p), linetype = "dashed", color = "gray45") +
    geom_line(linewidth = 0.85, na.rm = TRUE) +
    facet_wrap(~ true_p, nrow = 1, labeller = label_both) +
    scale_color_manual(values = c("MLE" = "#B22222", "Posterior mean" = "#1F78B4")) +
    coord_cartesian(ylim = c(0.25, 0.75)) +
    labs(
        title = "MLE and Posterior Mean over a Simulated Season",
        x = "Games played",
        y = "Estimated win probability",
        color = "Estimator"
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "11_estimator-trajectories.png"),
    trajectory_plot,
    width = 10,
    height = 5.2,
    dpi = 300
)
