#######################
### Authors: JP, RB ###
#######################

#############
### SETUP ###
#############

#install.packages(c("MASS", "tidyverse"))

library(tidyverse)

set.seed(19)

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

theme_pitch = theme_void(base_size = 12) +
    theme(
        legend.position = "bottom",
        legend.justification = "center",
        plot.title.position = "plot",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#2D7D32", color = NA),
        plot.title = element_text(face = "bold", color = "gray12", margin = margin(b = 7))
    )

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
        pred[idx] = as.vector(w %*% train_y) / rowSums(w)
    }
    clip_probability(pred)
}

standardize_train_new = function(train_data, new_data, cols) {
    train_x = train_data |> select(all_of(cols)) |> as.data.frame()
    new_x = new_data |> select(all_of(cols)) |> as.data.frame()

    medians = map_dbl(train_x, \(x) median(x, na.rm = TRUE))
    means = map_dbl(names(train_x), \(col) {
        x = train_x[[col]]
        x[is.na(x)] = medians[[col]]
        mean(x)
    })
    sds = map_dbl(names(train_x), \(col) {
        x = train_x[[col]]
        x[is.na(x)] = medians[[col]]
        sd(x)
    })
    sds[sds == 0 | is.na(sds)] = 1

    for (col in cols) {
        train_x[[col]][is.na(train_x[[col]])] = medians[[col]]
        new_x[[col]][is.na(new_x[[col]])] = medians[[col]]
    }

    list(
        train = sweep(sweep(as.matrix(train_x), 2, means, "-"), 2, sds, "/"),
        new = sweep(sweep(as.matrix(new_x), 2, means, "-"), 2, sds, "/"),
        means = means,
        sds = sds,
        medians = medians
    )
}

#################
### LOAD DATA ###
#################

data_path = "../data/19_shots.csv.gz"
if (!file.exists(data_path)) {
    data_path = "2026/labs/data/19_shots.csv.gz"
}

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

shots |>
    group_by(split) |>
    summarize(shots = n(), goals = sum(goal), goal_rate = mean(goal), .groups = "drop")

############################
### TASK 1: EXPLORATION ####
############################

# TODO: Make a pitch plot of all shot locations, coloring goals differently.

ggplot() +
    pitch_background() +
    geom_point(
        data = shots,
        aes(shot_x_plot, shot_y_plot, color = factor(goal)),
        alpha = 0.45,
        size = 0.8
    ) +
    pitch_lines() +
    coord_fixed(xlim = c(0, 68), ylim = c(70, 105.8), expand = FALSE) +
    scale_color_manual(values = c("0" = "white", "1" = "#D95D39"), name = "Goal") +
    labs(title = "Premier League Shot Locations, 2024-2025") +
    theme_pitch

# Summarize the distributions of the four key shot-quality features and assess
# which appear most related to goal probability.

task1_features = c(
    "distance_to_goal",
    "abs_angle_to_goal",
    "open_goal_share",
    "nearest_defender_distance"
)

task1_feature_summary = shots |>
    select(goal, all_of(task1_features)) |>
    pivot_longer(-goal, names_to = "feature", values_to = "value") |>
    group_by(feature) |>
    mutate(value_z = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)) |>
    group_by(feature, goal) |>
    summarize(mean_value = mean(value, na.rm = TRUE),
              mean_z = mean(value_z, na.rm = TRUE), .groups = "drop")

task1_feature_gap = task1_feature_summary |>
    select(feature, goal, mean_z) |>
    pivot_wider(names_from = goal, values_from = mean_z,
                names_prefix = "goal_") |>
    mutate(std_gap = goal_1 - goal_0) |>
    arrange(desc(abs(std_gap)))

print(task1_feature_gap)

task1_corr = shots |>
    summarize(across(all_of(task1_features),
                     \(x) cor(x, goal, use = "complete.obs"))) |>
    pivot_longer(everything(), names_to = "feature", values_to = "correlation") |>
    arrange(desc(abs(correlation)))

print(task1_corr)

task1_goal_rate_by_quintile = shots |>
    select(goal, all_of(task1_features)) |>
    pivot_longer(-goal, names_to = "feature", values_to = "value") |>
    drop_na(value) |>
    group_by(feature) |>
    mutate(quintile = ntile(value, 5)) |>
    group_by(feature, quintile) |>
    summarize(shots = n(), goal_rate = mean(goal), .groups = "drop")

print(task1_goal_rate_by_quintile, n = 25)

ggplot(
    shots |>
        select(goal, all_of(task1_features)) |>
        pivot_longer(-goal, names_to = "feature", values_to = "value"),
    aes(value, fill = factor(goal))
) +
    geom_density(alpha = 0.45, color = NA) +
    facet_wrap(~ feature, scales = "free", ncol = 2) +
    scale_fill_manual(values = c("0" = "#5B8DEF", "1" = "#D95D39"), name = "Goal") +
    labs(title = "Shot-Quality Feature Distributions by Goal Outcome",
         x = NULL, y = "Density") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

ggplot(task1_goal_rate_by_quintile, aes(quintile, goal_rate)) +
    geom_line(color = "gray40") +
    geom_point(size = 2, color = "#D95D39") +
    facet_wrap(~ feature, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = 1:5) +
    labs(title = "Goal Rate by Feature Quintile (1 = lowest, 5 = highest)",
         x = "Quintile of feature value", y = "Goal rate") +
    theme_minimal(base_size = 12)

# distance_to_goal is by far the strongest signal,then nearest_defender_distance is the next most informative. Goal rate declines


############################
### TASK 2: SHOT DENSITY ###
############################

# Use MASS::kde2d() to estimate the 2-D shot-density surface at several
# bandwidths, then draw each as a pitch heatmap.

make_kde = function(data, h, label) {
    kde = MASS::kde2d(
        data$shot_x_plot,
        data$shot_y_plot,
        h = c(h, h),
        n = 120,
        lims = c(0, 68, 70, 105)
    )
    expand_grid(shot_x = kde$x, shot_y = kde$y) |>
        mutate(density = as.vector(kde$z), bandwidth = label)
}

# Three bandwidths: small (under-smoothed), medium, large (over-smoothed).
kde_bandwidths = c("h = 2 (small)" = 2,
                   "h = 5 (medium)" = 5,
                   "h = 12 (large)" = 12)

shot_density = imap_dfr(
    kde_bandwidths,
    \(h, label) make_kde(shots, h, label)
) |>
    mutate(bandwidth = factor(bandwidth, levels = names(kde_bandwidths)))

ggplot(shot_density, aes(shot_x, shot_y, fill = density)) +
    geom_raster(interpolate = TRUE) +
    pitch_lines() +
    facet_wrap(~ bandwidth, ncol = 3) +
    coord_fixed(xlim = c(0, 68), ylim = c(70, 105.8), expand = FALSE) +
    scale_fill_viridis_c(option = "magma", name = "Shot density") +
    labs(title = "Premier League Shot Density by KDE Bandwidth, 2024-2025") +
    theme_pitch +
    theme(legend.key.width = unit(2, "lines"))
###########################################
### TASK 3: LOCATION ONLY KERNEL XG #######
###########################################

location_train = train |> select(shot_x, shot_y)
location_validation = validation |> select(shot_x, shot_y)
location_test = test |> select(shot_x, shot_y)

location_bandwidth_grid = c(1.5, 2, 2.8, 3.6, 4.5, 5.5, 7, 9, 12)

location_validation_results = tibble(bandwidth = location_bandwidth_grid) |>
    mutate(
        validation_log_loss = map_dbl(
            bandwidth,
            \(h) {
                pred = nw_predict_scaled(
                    location_train,
                    train$goal,
                    location_validation,
                    bandwidth = c(h, h)
                )
                log_loss(validation$goal, pred)
            }
        )
    )

print(location_validation_results)

ggplot(location_validation_results, aes(bandwidth, validation_log_loss)) +
    geom_line(color = "gray50") +
    geom_point(size = 2, color = "#D95D39") +
    labs(title = "Location-Only xG: Validation Log Loss vs Bandwidth",
         x = "Bandwidth h (pitch units)", y = "Validation log loss") +
    theme_minimal(base_size = 12)

best_location_h = location_validation_results |>
    arrange(validation_log_loss) |>
    slice(1) |>
    pull(bandwidth)

location_test_pred = nw_predict_scaled(
    train_validation |> select(shot_x, shot_y),
    train_validation$goal,
    location_test,
    bandwidth = c(best_location_h, best_location_h)
)
location_test_log_loss = log_loss(test$goal, location_test_pred)

location_baseline_log_loss = log_loss(test$goal, rep(mean(train$goal), nrow(test)))

cat("Best location bandwidth:", best_location_h, "\n")
cat("Test log loss (location-only):", round(location_test_log_loss, 5), "\n")
cat("Test log loss (constant base-rate baseline):",
    round(location_baseline_log_loss, 5), "\n")

xg_grid = expand_grid(
    shot_x = seq(0, 68, length.out = 95),
    shot_y = seq(70, 105, length.out = 85)
) |>
    mutate(
        xg = nw_predict_scaled(
            train_validation |> select(shot_x, shot_y),
            train_validation$goal,
            pick(shot_x, shot_y),
            bandwidth = c(best_location_h, best_location_h)
        )
    )

ggplot(xg_grid, aes(shot_x, shot_y, fill = xg)) +
    geom_raster(interpolate = TRUE) +
    pitch_lines() +
    coord_fixed(xlim = c(0, 68), ylim = c(70, 105.8), expand = FALSE) +
    scale_fill_viridis_c(option = "inferno", name = "xG", labels = scales::percent) +
    labs(title = "Location-Only Expected Goals Surface",
         subtitle = paste0("Nadaraya-Watson, h = ", best_location_h)) +
    theme_pitch +
    theme(legend.key.width = unit(2, "lines"))

##############################################
### TASK 4: TRACKING FEATURE KERNEL XG #######
##############################################

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

rich_bandwidth_grid = c(0.6, 0.8, 1.0, 1.25, 1.5, 2.0)

rich_validation_scaled = standardize_train_new(train, validation, rich_cols)

rich_validation_results = tibble(bandwidth = rich_bandwidth_grid) |>
    mutate(
        validation_log_loss = map_dbl(
            bandwidth,
            \(h) {
                pred = nw_predict_scaled(
                    rich_validation_scaled$train,
                    train$goal,
                    rich_validation_scaled$new,
                    bandwidth = h
                )
                log_loss(validation$goal, pred)
            }
        )
    )

print(rich_validation_results)

best_rich_h = rich_validation_results |>
    arrange(validation_log_loss) |>
    slice(1) |>
    pull(bandwidth)

rich_test_scaled = standardize_train_new(train_validation, test, rich_cols)

rich_test_pred = nw_predict_scaled(
    rich_test_scaled$train,
    train_validation$goal,
    rich_test_scaled$new,
    bandwidth = best_rich_h
)

rich_test_log_loss = log_loss(test$goal, rich_test_pred)

model_comparison = tibble(
    model = c("Location only", "Location + tracking features"),
    test_log_loss = c(location_test_log_loss, rich_test_log_loss)
)

print(model_comparison)
cat("Best rich bandwidth:", best_rich_h, "\n")
cat("Improvement in test log loss:",
    round(location_test_log_loss - rich_test_log_loss, 5), "\n")

test_predictions = test |>
    mutate(
        xg_location = location_test_pred,
        xg_rich = rich_test_pred,
        delta = xg_rich - xg_location
    )

ggplot(test_predictions, aes(xg_location, xg_rich, color = delta)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.5, size = 1) +
    scale_color_gradient2(
        low = "#2166AC", mid = "gray80", high = "#B2182B",
        midpoint = 0, name = "Rich - location",
        labels = scales::percent
    ) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    coord_equal() +
    labs(
        title = "Test-Set xG: Location-Only vs Tracking-Feature Model",
        subtitle = "Points above the dashed line gain xG when tracking features are added",
        x = "Location-only xG", y = "Location + tracking xG"
    ) +
    theme_minimal(base_size = 12)

#Shots with high open goal shares or factors other than distance have improved xG in the new model, which makes sense since they aren't included
