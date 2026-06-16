#######################
### Authors: JP, RB ###
#######################

########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "readr", "tidyr"))

suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(readr)
    library(tidyr)
})

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

league_mean = with(batting, sum(H_2020) / sum(AB_2020))
C_hat = league_mean * (1 - league_mean)
tau2_hat = max(var(batting$BA_2020) - mean(C_hat / batting$AB_2020), 1e-8)
starting_strength = C_hat / tau2_hat - 1

neg_log_beta_binomial = function(log_par) {
    alpha = exp(log_par[1])
    beta = exp(log_par[2])

    -sum(
        lchoose(batting$AB_2020, batting$H_2020) +
            lbeta(batting$H_2020 + alpha, batting$AB_2020 - batting$H_2020 + beta) -
            lbeta(alpha, beta)
    )
}

beta_fit = optim(
    par = log(c(league_mean * starting_strength, (1 - league_mean) * starting_strength)),
    fn = neg_log_beta_binomial,
    method = "BFGS"
)

alpha_hat = exp(beta_fit$par[1])
beta_hat = exp(beta_fit$par[2])
prior_strength = alpha_hat + beta_hat
prior_center = alpha_hat / prior_strength

batting = batting |>
    mutate(
        sigma2 = C_hat / AB_2020,
        lambda = tau2_hat / (tau2_hat + sigma2),
        fake_data_weight = AB_2020 / (AB_2020 + prior_strength),
        EB_2020 = (H_2020 + alpha_hat) / (AB_2020 + alpha_hat + beta_hat),
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

label_lookup = c(
    "vanmejo01" = "Josh VanMeter",
    "mercaos01" = "Oscar Mercado",
    "hayeske01" = "Ke'Bryan Hayes",
    "leonsa01" = "Sandy Leon",
    "santada01" = "Danny Santana",
    "iglesjo01" = "Jose Iglesias",
    "plaweke01" = "Kevin Plawecki",
    "lemahdj01" = "DJ LeMahieu"
)

label_data = arrow_data |>
    filter(playerID %in% names(label_lookup)) |>
    mutate(
        label = unname(label_lookup[playerID]),
        label_x = pmax(BA_2020, EB_2020) + 0.006
    )

arrow_plot = ggplot(arrow_data) +
    geom_segment(
        aes(x = BA_2020, xend = EB_2020, y = rank, yend = rank),
        arrow = arrow(length = unit(0.10, "inches"), type = "closed"),
        color = "#4C78A8",
        linewidth = 0.7
    ) +
    geom_point(aes(BA_2020, rank), color = "#B22222", size = 2.1) +
    geom_point(aes(EB_2020, rank), color = "#1F78B4", size = 2.1) +
    geom_text(
        data = label_data,
        aes(label_x, rank, label = label),
        hjust = 0,
        size = 3.1,
        color = "gray20",
        check_overlap = TRUE
    ) +
    geom_vline(xintercept = prior_center, linetype = "dashed", color = "gray40") +
    coord_cartesian(xlim = c(0.11, 0.42), clip = "off") +
    labs(
        title = "Raw and Empirical-Bayes Batting Averages",
        x = "Batting-average estimate",
        y = "Players ordered by raw batting average"
    ) +
    theme_week +
    theme(plot.margin = margin(5.5, 70, 5.5, 5.5))

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

lambda_plot = ggplot(batting, aes(AB_2020, fake_data_weight)) +
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
        title = "Data Weight by At-Bats",
        x = "2020 at-bats",
        y = "Data weight N / (N + learned fake at-bats)"
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
