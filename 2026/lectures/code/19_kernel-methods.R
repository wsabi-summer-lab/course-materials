#######################
### Authors: JP, RB ###
#######################

########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "MASS", "readr", "scales", "tidyr"))

library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(tidyr)

################
### SETTINGS ###
################

set.seed(19)

data_path = "2026/lectures/data/19_shots.csv.gz"
figure_dir = "2026/lectures/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

theme_week = theme_minimal(base_size = 12) +
    theme(
        legend.position = "bottom",
        legend.justification = "center",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", color = "gray12", margin = margin(b = 7)),
        legend.margin = margin(1, 0, 0, 0),
        legend.box.margin = margin(2, 0, 0, 0),
        plot.margin = margin(3, 3, 2, 3)
    )

theme_pitch = theme_void(base_size = 12) +
    theme(
        legend.position = "bottom",
        legend.justification = "center",
        plot.title.position = "plot",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#2D7D32", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", color = "gray12", margin = margin(b = 7)),
        legend.margin = margin(1, 0, 0, 0),
        legend.box.margin = margin(2, 0, 0, 0),
        plot.margin = margin(2, 2, 2, 2)
    )

################
### HELPERS ####
################

clip_probability = function(p, eps = 1e-5) {
    pmin(pmax(p, eps), 1 - eps)
}

log_loss = function(actual, predicted) {
    predicted = clip_probability(predicted)
    -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

pitch_background = function() {
    list(
        annotate(
            "rect",
            xmin = 0,
            xmax = 68,
            ymin = 0,
            ymax = 105,
            fill = "#2D7D32",
            color = "white",
            linewidth = 0.45
        )
    )
}

pitch_lines = function() {
    list(
        annotate("segment", x = 0, xend = 68, y = 52.5, yend = 52.5, color = "white", linewidth = 0.45),
        annotate("rect", xmin = 13.84, xmax = 54.16, ymin = 88.5, ymax = 105, fill = NA, color = "white", linewidth = 0.55),
        annotate("rect", xmin = 24.84, xmax = 43.16, ymin = 99.5, ymax = 105, fill = NA, color = "white", linewidth = 0.55),
        annotate("segment", x = 30.34, xend = 37.66, y = 105, yend = 105, color = "white", linewidth = 1.5),
        annotate("point", x = 34, y = 94, color = "white", size = 1.2),
        annotate("path", x = 34 + 9.15 * cos(seq(-2.50, -0.64, length.out = 80)),
                 y = 94 + 9.15 * sin(seq(-2.50, -0.64, length.out = 80)),
                 color = "white", linewidth = 0.45)
    )
}

nw_predict_scaled = function(train_x, train_y, new_x, bandwidth = 1, chunk_size = 400) {
    train_x = as.matrix(train_x)
    new_x = as.matrix(new_x)
    train_y = as.numeric(train_y)

    if (length(bandwidth) == 1) {
        bandwidth = rep(bandwidth, ncol(train_x))
    }
    train_scaled = sweep(train_x, 2, bandwidth, "/")
    new_scaled = sweep(new_x, 2, bandwidth, "/")

    pred = numeric(nrow(new_scaled))
    starts = seq(1, nrow(new_scaled), by = chunk_size)
    for (start in starts) {
        idx = start:min(start + chunk_size - 1, nrow(new_scaled))
        d2 = matrix(0, length(idx), nrow(train_scaled))
        for (j in seq_len(ncol(train_scaled))) {
            d2 = d2 + outer(new_scaled[idx, j], train_scaled[, j], "-")^2
        }
        w = exp(-0.5 * d2)
        denom = rowSums(w)
        pred[idx] = as.vector(w %*% train_y) / denom
    }
    clip_probability(pred)
}

standardize_features = function(train_data, new_data, cols) {
    train_x = train_data |> select(all_of(cols)) |> as.data.frame()
    new_x = new_data |> select(all_of(cols)) |> as.data.frame()

    medians = vapply(cols, \(col) median(train_x[[col]], na.rm = TRUE), numeric(1))
    means = vapply(cols, \(col) {
        x = train_x[[col]]
        x[is.na(x)] = medians[[col]]
        mean(x)
    }, numeric(1))
    sds = vapply(cols, \(col) {
        x = train_x[[col]]
        x[is.na(x)] = medians[[col]]
        sd(x)
    }, numeric(1))
    sds[sds == 0 | is.na(sds)] = 1

    for (col in cols) {
        train_x[[col]][is.na(train_x[[col]])] = medians[[col]]
        new_x[[col]][is.na(new_x[[col]])] = medians[[col]]
    }

    list(
        train = sweep(sweep(as.matrix(train_x), 2, means, "-"), 2, sds, "/"),
        new = sweep(sweep(as.matrix(new_x), 2, means, "-"), 2, sds, "/")
    )
}

################
### LOAD DATA ##
################

shots = read_csv(data_path, show_col_types = FALSE) |>
    mutate(
        goal = as.integer(goal),
        split = factor(split, levels = c("train", "validation", "test")),
        shot_x_plot = pmin(pmax(shot_x, 0), 68),
        shot_y_plot = pmin(pmax(shot_y, 70), 105),
        log_ball_speed = log1p(pmin(ball_speed, quantile(ball_speed, 0.995, na.rm = TRUE)))
    )

train = shots |> filter(split == "train")
validation = shots |> filter(split == "validation")
test = shots |> filter(split == "test")
train_validation = shots |> filter(split %in% c("train", "validation"))

cat("Shots / goals:", nrow(shots), "/", sum(shots$goal), "\n")
print(shots |> count(split, wt = goal, name = "goals") |> left_join(count(shots, split), by = "split"))

#######################
### SHOT LOCATIONS ####
#######################

shot_location_plot = ggplot() +
    pitch_background() +
    geom_point(
        data = shots,
        aes(shot_x_plot, shot_y_plot, color = factor(goal)),
        alpha = 0.46,
        size = 0.9
    ) +
    pitch_lines() +
    coord_fixed(xlim = c(0, 68), ylim = c(70, 105.8), expand = FALSE, clip = "on") +
    scale_color_manual(
        values = c("0" = "white", "1" = "#D95D39"),
        labels = c("No goal", "Goal"),
        name = NULL
    ) +
    labs(
        title = "Premier League Shots, 2024-2025",
        x = NULL,
        y = NULL
    ) +
    theme_pitch

ggsave(
    file.path(figure_dir, "19_kernel-methods-shot-locations.png"),
    shot_location_plot,
    width = 7.2,
    height = 4.55,
    dpi = 300,
    bg = "white"
)

###########################
### KERNEL DENSITY EST. ###
###########################

kde_to_df = function(data, h, label) {
    kde = MASS::kde2d(
        data$shot_x_plot,
        data$shot_y_plot,
        h = c(h, h),
        n = 140,
        lims = c(0, 68, 70, 105)
    )
    expand_grid(shot_x = kde$x, shot_y = kde$y) |>
        mutate(
            density = as.vector(kde$z),
            bandwidth = label
        )
}

kde_data = bind_rows(
    kde_to_df(shots, 1.6, "Small bandwidth"),
    kde_to_df(shots, 3.6, "Medium bandwidth"),
    kde_to_df(shots, 7.2, "Large bandwidth")
) |>
    mutate(bandwidth = factor(bandwidth, levels = c("Small bandwidth", "Medium bandwidth", "Large bandwidth")))

kde_plot = ggplot() +
    pitch_background() +
    geom_raster(
        data = kde_data,
        aes(shot_x, shot_y, fill = density),
        alpha = 0.9,
        interpolate = TRUE
    ) +
    geom_contour(
        data = kde_data,
        aes(shot_x, shot_y, z = density),
        color = "white",
        alpha = 0.42,
        linewidth = 0.2,
        bins = 7
    ) +
    pitch_lines() +
    facet_wrap(vars(bandwidth), ncol = 1) +
    coord_fixed(xlim = c(0, 68), ylim = c(70, 105.8), expand = FALSE, clip = "on") +
    scale_fill_gradient(low = "#20502B", high = "#F9C74F", guide = "none") +
    labs(
        title = "Kernel Density Estimates by Bandwidth",
        x = NULL,
        y = NULL
    ) +
    theme_pitch +
    theme(
        strip.text = element_text(face = "bold", color = "gray12", margin = margin(b = 3)),
        panel.spacing = grid::unit(4, "pt"),
        plot.margin = margin(2, 2, 2, 2)
    )

ggsave(
    file.path(figure_dir, "19_kernel-methods-kde-bandwidths.png"),
    kde_plot,
    width = 7.2,
    height = 11.2,
    dpi = 300,
    bg = "white"
)

###########################
### LOCAL KERNEL WEIGHTS ##
###########################

target_shot = shots |>
    mutate(target_score = abs(distance_to_goal - 13) + abs(lateral_from_center) / 4) |>
    arrange(target_score) |>
    slice(1)

weight_data = shots |>
    mutate(
        location_distance = sqrt((shot_x - target_shot$shot_x)^2 + (shot_y - target_shot$shot_y)^2),
        kernel_weight = exp(-0.5 * (location_distance / 5)^2)
    ) |>
    arrange(desc(kernel_weight)) |>
    slice_head(n = 700)

weight_plot = ggplot() +
    pitch_background() +
    geom_point(
        data = weight_data,
        aes(shot_x_plot, shot_y_plot, color = kernel_weight, size = kernel_weight),
        alpha = 0.72
    ) +
    geom_point(
        data = target_shot,
        aes(shot_x_plot, shot_y_plot),
        shape = 4,
        stroke = 1.4,
        size = 4.0,
        color = "#2A7F9E"
    ) +
    pitch_lines() +
    coord_fixed(xlim = c(0, 68), ylim = c(70, 105.8), expand = FALSE, clip = "on") +
    scale_color_gradient(low = "white", high = "#F94144", labels = number_format(accuracy = 0.01)) +
    scale_size(range = c(0.4, 3.2), guide = "none") +
    labs(
        title = "Kernel Weights Around One Target Shot",
        color = "Weight",
        x = NULL,
        y = NULL
    ) +
    theme_pitch

ggsave(
    file.path(figure_dir, "19_kernel-methods-kernel-weights.png"),
    weight_plot,
    width = 7.2,
    height = 4.55,
    dpi = 300,
    bg = "white"
)

#########################
### BANDWIDTH TUNING ####
#########################

bandwidth_grid = c(1.5, 2, 2.8, 3.6, 4.5, 5.5, 7, 9, 12)

location_train = train |> select(shot_x, shot_y)
location_validation = validation |> select(shot_x, shot_y)
location_test = test |> select(shot_x, shot_y)

validation_results = tibble(bandwidth = bandwidth_grid) |>
    mutate(
        validation_log_loss = vapply(
            bandwidth,
            \(h) {
                pred = nw_predict_scaled(
                    location_train,
                    train$goal,
                    location_validation,
                    bandwidth = c(h, h)
                )
                log_loss(validation$goal, pred)
            },
            numeric(1)
        )
    )

best_location_h = validation_results |>
    arrange(validation_log_loss) |>
    slice(1) |>
    pull(bandwidth)

bandwidth_plot = ggplot(validation_results, aes(bandwidth, validation_log_loss)) +
    geom_line(color = "#277DA1", linewidth = 0.9) +
    geom_point(color = "#277DA1", size = 2.2) +
    geom_vline(xintercept = best_location_h, color = "#D95D39", linetype = "dashed", linewidth = 0.7) +
    scale_x_continuous(breaks = bandwidth_grid) +
    labs(
        title = "Validation Log Loss by Bandwidth",
        x = "Location bandwidth (meters)",
        y = "Validation log loss"
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "19_kernel-methods-bandwidth-validation.png"),
    bandwidth_plot,
    width = 7.2,
    height = 4.2,
    dpi = 300,
    bg = "white"
)

##########################
### LOCATION ONLY XG #####
##########################

xg_grid = expand_grid(
    shot_x = seq(0, 68, length.out = 95),
    shot_y = seq(70, 105, length.out = 85)
)

xg_grid$xg_hat = nw_predict_scaled(
    train_validation |> select(shot_x, shot_y),
    train_validation$goal,
    xg_grid |> select(shot_x, shot_y),
    bandwidth = c(best_location_h, best_location_h),
    chunk_size = 300
)

xg_grid = xg_grid |>
    mutate(
        grid_distance_to_goal = sqrt((shot_x - 34)^2 + (105 - shot_y)^2),
        xg_hat = if_else(grid_distance_to_goal <= 36, xg_hat, NA_real_)
    )

xg_surface_plot = ggplot() +
    pitch_background() +
    geom_raster(
        data = xg_grid,
        aes(shot_x, shot_y, fill = xg_hat),
        alpha = 0.92,
        interpolate = TRUE
    ) +
    pitch_lines() +
    coord_fixed(xlim = c(0, 68), ylim = c(70, 105.8), expand = FALSE, clip = "on") +
    scale_fill_gradientn(
        colors = c("#245A32", "#F9C74F", "#F8961E", "#D95D39"),
        labels = percent_format(accuracy = 1),
        limits = c(0, max(xg_grid$xg_hat, na.rm = TRUE)),
        na.value = NA,
        name = "xG"
    ) +
    labs(
        title = "Location Only Kernel Regression xG",
        x = NULL,
        y = NULL
    ) +
    theme_pitch

ggsave(
    file.path(figure_dir, "19_kernel-methods-xg-location-only.png"),
    xg_surface_plot,
    width = 7.2,
    height = 4.55,
    dpi = 300,
    bg = "white"
)

##################################
### ADD TRACKING FEATURE SIMILARITY
##################################

rich_cols = c(
    "distance_to_goal",
    "abs_angle_to_goal",
    "ball_height",
    "log_ball_speed",
    "goalkeeper_distance",
    "goalkeeper_abs_angle",
    "open_goal_share",
    "nearest_defender_distance"
)

rich_train_validation = train_validation |>
    mutate(log_ball_speed = log1p(pmin(ball_speed, quantile(shots$ball_speed, 0.995, na.rm = TRUE))))
rich_validation = validation |>
    mutate(log_ball_speed = log1p(pmin(ball_speed, quantile(shots$ball_speed, 0.995, na.rm = TRUE))))
rich_test = test |>
    mutate(log_ball_speed = log1p(pmin(ball_speed, quantile(shots$ball_speed, 0.995, na.rm = TRUE))))

rich_train_scaled = standardize_features(train, validation, rich_cols)
rich_bandwidth_grid = c(0.6, 0.8, 1.0, 1.25, 1.5, 2.0)

rich_validation_results = tibble(bandwidth = rich_bandwidth_grid) |>
    mutate(
        validation_log_loss = vapply(
            bandwidth,
            \(h) {
                pred = nw_predict_scaled(
                    rich_train_scaled$train,
                    train$goal,
                    rich_train_scaled$new,
                    bandwidth = h
                )
                log_loss(validation$goal, pred)
            },
            numeric(1)
        )
    )

best_rich_h = rich_validation_results |>
    arrange(validation_log_loss) |>
    slice(1) |>
    pull(bandwidth)

location_test_pred = nw_predict_scaled(
    train_validation |> select(shot_x, shot_y),
    train_validation$goal,
    location_test,
    bandwidth = c(best_location_h, best_location_h)
)

rich_final_scaled = standardize_features(rich_train_validation, rich_test, rich_cols)
rich_test_pred = nw_predict_scaled(
    rich_final_scaled$train,
    rich_train_validation$goal,
    rich_final_scaled$new,
    bandwidth = best_rich_h
)

model_comparison = tibble(
    model = c("Location only", "Location + tracking features"),
    test_log_loss = c(
        log_loss(test$goal, location_test_pred),
        log_loss(test$goal, rich_test_pred)
    )
)

print(model_comparison)

comparison_plot_data = test |>
    mutate(
        xg_location = location_test_pred,
        xg_rich = rich_test_pred
    )

comparison_plot = ggplot(comparison_plot_data, aes(xg_location, xg_rich, color = open_goal_share)) +
    geom_abline(slope = 1, intercept = 0, color = "gray60", linewidth = 0.55) +
    geom_point(alpha = 0.72, size = 1.35) +
    scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
    scale_color_gradient(low = "#277DA1", high = "#D95D39", labels = percent_format(accuracy = 1)) +
    labs(
        title = "Location Only vs. Tracking Feature Kernel xG",
        x = "Location only kernel xG",
        y = "Location + tracking feature kernel xG",
        color = "Open goal"
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "19_kernel-methods-location-vs-rich-similarity.png"),
    comparison_plot,
    width = 7.2,
    height = 4.85,
    dpi = 300,
    bg = "white"
)
