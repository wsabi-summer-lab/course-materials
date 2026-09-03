#######################
### Authors: JP, RB ###
#######################

#############
### SETUP ###
#############

# install.packages(c("MASS", "tidyverse"))

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

data_path = "C:/Users/sundw/Downloads/19_shots.csv"
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

# TODO: Summarize or plot the distributions of important shot quality features.
# Ideas:
#   * distance_to_goal by goal outcome
#   * open_goal_share by goal outcome
#   * nearest_defender_distance by goal outcome

ggplot(shots, aes(x = factor(goal), y = distance_to_goal, fill = factor(goal))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.12, fill = "white") +
  scale_x_discrete(labels = c("No Goal", "Goal")) +
  scale_fill_manual(
    values = c("0" = "#4E79A7", "1" = "#D95D39"),
    guide = "none"
  ) +
  labs(
    title = "Distance to Goal by Shot Outcome",
    x = "Shot Outcome",
    y = "Distance to Goal (m)"
  ) +
  theme_minimal()

############################
### TASK 2: SHOT DENSITY ###
############################

# TODO: Use MASS::kde2d() to create KDE plots for at least three bandwidths.

kde_bandwidths = c(2, 4, 6, 9)  # tight -> oversmoothed, to show the bias/variance tradeoff

kde_results = map(
  kde_bandwidths,
  \(h) make_kde(shots, h, label = paste0("h = ", h))
) |>
  bind_rows() |>
  mutate(bandwidth = factor(bandwidth, levels = paste0("h = ", kde_bandwidths)))

ggplot() +
  pitch_background() +
  geom_raster(
    data = kde_results,
    aes(shot_x, shot_y, fill = density),
    interpolate = TRUE,
    alpha = 0.9
  ) +
  pitch_lines() +
  facet_wrap(~bandwidth) +
  coord_fixed(xlim = c(0, 68), ylim = c(70, 105.8), expand = FALSE) +
  scale_fill_viridis_c(option = "inferno", name = "Density") +
  labs(title = "Shot Density KDE by Bandwidth") +
  theme_pitch +
  theme(strip.text = element_text(face = "bold", color = "gray12"))

###########################################
### TASK 3: LOCATION ONLY KERNEL XG #######
###########################################

location_train = train |> select(shot_x, shot_y)
location_validation = validation |> select(shot_x, shot_y)
location_test = test |> select(shot_x, shot_y)

location_bandwidth_grid = c(1.5, 2, 2.8, 3.6, 4.5, 5.5, 7, 9, 12)

# Compute validation log loss for each location bandwidth.
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

best_location_h = location_validation_results |>
  arrange(validation_log_loss) |>
  slice(1) |>
  pull(bandwidth)

cat("Best location bandwidth:", best_location_h, "\n")

# Fit on train + validation, then evaluate test log loss.
location_test_pred = nw_predict_scaled(
  train_validation |> select(shot_x, shot_y),
  train_validation$goal,
  location_test,
  bandwidth = c(best_location_h, best_location_h)
)
location_test_log_loss = log_loss(test$goal, location_test_pred)

cat("Location-only test log loss:", location_test_log_loss, "\n")

# Make a pitch heatmap of location only xG.
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

ggplot() +
  pitch_background() +
  geom_raster(data = xg_grid, aes(shot_x, shot_y, fill = xg), interpolate = TRUE, alpha = 0.9) +
  pitch_lines() +
  coord_fixed(xlim = c(0, 68), ylim = c(70, 105.8), expand = FALSE) +
  scale_fill_viridis_c(option = "inferno", name = "xG") +
  labs(title = paste0("Location-Only Kernel xG (h = ", best_location_h, ")")) +
  theme_pitch

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

# Standardize rich features using train, then tune bandwidth on validation.
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

cat("Best rich-feature bandwidth:", best_rich_h, "\n")

# Refit using train + validation, evaluate on test, and compare with
# the location only model.
rich_test_scaled = standardize_train_new(train_validation, test, rich_cols)

rich_test_pred = nw_predict_scaled(
  rich_test_scaled$train,
  train_validation$goal,
  rich_test_scaled$new,
  bandwidth = best_rich_h
)

rich_test_log_loss = log_loss(test$goal, rich_test_pred)

cat("Rich-feature test log loss:", rich_test_log_loss, "\n")

# Compare with the location-only model
model_comparison = tibble(
  model = c("Location only", "Rich tracking features"),
  bandwidth = c(best_location_h, best_rich_h),
  test_log_loss = c(location_test_log_loss, rich_test_log_loss)
) |>
  mutate(improvement_pct = round(100 * (test_log_loss[1] - test_log_loss) / test_log_loss[1], 2))

print(model_comparison)