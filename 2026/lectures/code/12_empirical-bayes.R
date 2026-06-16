#######################
### Authors: JP, RB ###
#######################

########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "readr", "tidyr"))

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

################
### SETTINGS ###
################

figure_dir = "2026/lectures/figures"
data_path = "2026/lectures/data/12_ba-2020-2021.csv"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

theme_week = theme_minimal(base_size = 12) +
    theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
    )

########################
### EMPIRICAL BAYES ####
########################

batting = read_csv(data_path, show_col_types = FALSE) |>
    filter(AB_2020 >= 20, AB_2021 >= 100)

mu_hat = with(batting, sum(H_2020) / sum(AB_2020))
C_hat = mu_hat * (1 - mu_hat)
tau2_hat = max(var(batting$BA_2020) - mean(C_hat / batting$AB_2020), 0)

batting = batting |>
    mutate(
        sigma2 = C_hat / AB_2020,
        lambda = tau2_hat / (tau2_hat + sigma2),
        EB_2020 = mu_hat + lambda * (BA_2020 - mu_hat),
        shrinkage = BA_2020 - EB_2020
    )

########################
### SHRINKAGE ARROWS ###
########################

arrow_data = batting |>
    arrange(desc(abs(shrinkage))) |>
    slice_head(n = 35) |>
    arrange(BA_2020) |>
    mutate(rank = row_number())

arrow_plot = ggplot(arrow_data) +
    geom_segment(
        aes(x = BA_2020, xend = EB_2020, y = rank, yend = rank),
        arrow = arrow(length = unit(0.10, "inches"), type = "closed"),
        color = "#4C78A8",
        linewidth = 0.7
    ) +
    geom_point(aes(BA_2020, rank), color = "#B22222", size = 2.1) +
    geom_point(aes(EB_2020, rank), color = "#1F78B4", size = 2.1) +
    geom_vline(xintercept = mu_hat, linetype = "dashed", color = "gray40") +
    labs(
        title = "Raw and Empirical-Bayes Batting Averages",
        x = "Batting-average estimate",
        y = "Players ordered by raw batting average"
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "12_shrinkage-arrows.png"),
    arrow_plot,
    width = 8.5,
    height = 6.2,
    dpi = 300
)

#############################
### SHRINKAGE VS SAMPLE N ###
#############################

lambda_plot = ggplot(batting, aes(AB_2020, lambda)) +
    geom_point(alpha = 0.65, color = "#4C78A8") +
    geom_smooth(
        method = "loess",
        formula = y ~ x,
        se = FALSE,
        color = "#D95F02",
        linewidth = 1
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
        title = "Shrinkage Weight by At-Bats",
        x = "2020 at-bats",
        y = "Data weight lambda"
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "12_shrinkage-weight.png"),
    lambda_plot,
    width = 8,
    height = 5.2,
    dpi = 300
)

################################
### OUT-OF-SAMPLE COMPARISON ###
################################

projection_data = batting |>
    select(playerID, BA_2021, MLE = BA_2020, `Empirical Bayes` = EB_2020) |>
    pivot_longer(c(MLE, `Empirical Bayes`), names_to = "estimator", values_to = "prediction")

projection_plot = ggplot(projection_data, aes(prediction, BA_2021, color = estimator)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
    geom_point(alpha = 0.55) +
    facet_wrap(~ estimator) +
    scale_color_manual(values = c("MLE" = "#B22222", "Empirical Bayes" = "#1F78B4")) +
    coord_equal(xlim = c(0.08, 0.42), ylim = c(0.08, 0.42)) +
    labs(
        title = "2020 Estimates and 2021 Batting Average",
        x = "2020 estimate",
        y = "2021 batting average"
    ) +
    theme_week +
    theme(legend.position = "none")

ggsave(
    file.path(figure_dir, "12_projection-comparison.png"),
    projection_plot,
    width = 9,
    height = 5.3,
    dpi = 300
)

rmse = projection_data |>
    group_by(estimator) |>
    summarise(RMSE = sqrt(mean((prediction - BA_2021)^2)), .groups = "drop")

rmse_plot = ggplot(rmse, aes(estimator, RMSE, fill = estimator)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = sprintf("%.4f", RMSE)), vjust = -0.4, size = 4) +
    scale_fill_manual(values = c("MLE" = "#B22222", "Empirical Bayes" = "#1F78B4")) +
    coord_cartesian(ylim = c(0, max(rmse$RMSE) * 1.15)) +
    labs(
        title = "Out-of-Sample RMSE",
        x = NULL,
        y = "Root mean squared error"
    ) +
    theme_week +
    theme(legend.position = "none")

ggsave(
    file.path(figure_dir, "12_projection-rmse.png"),
    rmse_plot,
    width = 6.5,
    height = 4.8,
    dpi = 300
)
