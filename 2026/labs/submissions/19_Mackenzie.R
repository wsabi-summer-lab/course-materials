#############
### SETUP ###
#############

# Install packages only if they are not already installed
needed_packages = c("MASS", "tidyverse")
missing_packages = needed_packages[!needed_packages %in% rownames(installed.packages())]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(MASS)
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
    annotate(
      "segment",
      x = 0,
      xend = 68,
      y = 52.5,
      yend = 52.5,
      color = "white",
      linewidth = 0.45
    ),
    annotate(
      "rect",
      xmin = 13.84,
      xmax = 54.16,
      ymin = 88.5,
      ymax = 105,
      fill = NA,
      color = "white",
      linewidth = 0.55
    ),
    annotate(
      "rect",
      xmin = 24.84,
      xmax = 43.16,
      ymin = 99.5,
      ymax = 105,
      fill = NA,
      color = "white",
      linewidth = 0.55
    ),
    annotate(
      "segment",
      x = 30.34,
      xend = 37.66,
      y = 105,
      yend = 105,
      color = "white",
      linewidth = 1.5
    ),
    annotate(
      "point",
      x = 34,
      y = 94,
      color = "white",
      size = 1.2
    ),
    annotate(
      "path",
      x = 34 + 9.15 * cos(seq(-2.50, -0.64, length.out = 80)),
      y = 94 + 9.15 * sin(seq(-2.50, -0.64, length.out = 80)),
      color = "white",
      linewidth = 0.45
    )
  )
}

theme_pitch = theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#2D7D32", color = NA),
    plot.title = element_text(
      face = "bold",
      color = "gray12",
      margin = margin(b = 7)
    )
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
  train_x = train_data |>
    dplyr::select(all_of(cols)) |>
    as.data.frame()
  
  new_x = new_data |>
    dplyr::select(all_of(cols)) |>
    as.data.frame()
  
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

data_path = "/Users/mackenziebuckner/Desktop/lab-materials/2026/labs/data/19_shots.csv"

if (!file.exists(data_path)) {
  data_path = "2026/labs/data/19_shots.csv"
}

if (!file.exists(data_path)) {
  data_path = "19_shots.csv"
}

if (!file.exists(data_path)) {
  stop("Could not find 19_shots.csv. Move the file into your working directory or update data_path.")
}

shots = read_csv(data_path, show_col_types = FALSE) |>
  mutate(
    goal = as.integer(goal),
    split = factor(split, levels = c("train", "validation", "test")),
    shot_x_plot = pmin(pmax(shot_x, 0), 68),
    shot_y_plot = pmin(pmax(shot_y, 70), 105),
    log_ball_speed = log1p(
      pmin(ball_speed, quantile(ball_speed, 0.995, na.rm = TRUE))
    )
  )

train = shots |> filter(split == "train")
validation = shots |> filter(split == "validation")
test = shots |> filter(split == "test")

train_validation = shots |>
  filter(split %in% c("train", "validation"))

split_summary = shots |>
  group_by(split) |>
  summarize(
    shots = n(),
    goals = sum(goal),
    goal_rate = mean(goal),
    .groups = "drop"
  )

print(split_summary)

############################
### TASK 1: EXPLORATION ####
############################

# Task 1.1:
# Report the number of shots and goals in each split.
# This is printed above as split_summary.

# Task 1.2:
# Make a pitch plot of all shot locations.
# Goals are shown in orange and non-goals are shown in white.

shot_location_plot = ggplot() +
  pitch_background() +
  geom_point(
    data = shots,
    aes(shot_x_plot, shot_y_plot, color = factor(goal)),
    alpha = 0.45,
    size = 0.8
  ) +
  pitch_lines() +
  coord_fixed(xlim = c(0, 68), ylim = c(70, 105.8), expand = FALSE) +
  scale_color_manual(
    values = c("0" = "white", "1" = "#D95D39"),
    labels = c("0" = "No goal", "1" = "Goal"),
    name = "Outcome"
  ) +
  labs(title = "Premier League Shot Locations, 2024-2025") +
  theme_pitch

print(shot_location_plot)

# Task 1.3:
# Summarize the distributions of important shot quality features.

feature_summary = shots |>
  mutate(goal_label = if_else(goal == 1, "Goal", "No goal")) |>
  group_by(goal_label) |>
  summarize(
    shots = n(),
    avg_distance_to_goal = mean(distance_to_goal, na.rm = TRUE),
    median_distance_to_goal = median(distance_to_goal, na.rm = TRUE),
    avg_abs_angle_to_goal = mean(abs_angle_to_goal, na.rm = TRUE),
    median_abs_angle_to_goal = median(abs_angle_to_goal, na.rm = TRUE),
    avg_open_goal_share = mean(open_goal_share, na.rm = TRUE),
    median_open_goal_share = median(open_goal_share, na.rm = TRUE),
    avg_nearest_defender_distance = mean(nearest_defender_distance, na.rm = TRUE),
    median_nearest_defender_distance = median(nearest_defender_distance, na.rm = TRUE),
    .groups = "drop"
  )

print(feature_summary)

feature_long = shots |>
  mutate(goal_label = if_else(goal == 1, "Goal", "No goal")) |>
  dplyr::select(
    goal_label,
    distance_to_goal,
    abs_angle_to_goal,
    open_goal_share,
    nearest_defender_distance
  ) |>
  pivot_longer(
    cols = -goal_label,
    names_to = "feature",
    values_to = "value"
  )

feature_distribution_plot = ggplot(
  feature_long,
  aes(x = goal_label, y = value, fill = goal_label)
) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.25) +
  facet_wrap(~ feature, scales = "free_y") +
  labs(
    title = "Shot Quality Feature Distributions by Goal Outcome",
    x = "",
    y = "Feature value",
    fill = "Outcome"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(feature_distribution_plot)

cat("\nTASK 1 INTERPRETATION:\n")
cat("The features that appear most related to goal probability are distance_to_goal,\n")
cat("abs_angle_to_goal, open_goal_share, and nearest_defender_distance.\n")
cat("Goals usually come from shorter distance, better angle, more open goal space,\n")
cat("and more space from nearby defenders.\n\n")

############################
### TASK 2: SHOT DENSITY ###
############################

# Task 2:
# Use MASS::kde2d() to create KDE shot density plots with different bandwidths.

make_kde = function(data, h, label) {
  kde = MASS::kde2d(
    data$shot_x_plot,
    data$shot_y_plot,
    h = c(h, h),
    n = 120,
    lims = c(0, 68, 70, 105)
  )
  
  expand_grid(
    shot_x = kde$x,
    shot_y = kde$y
  ) |>
    mutate(
      density = as.vector(t(kde$z)),
      bandwidth = label
    )
}

kde_results = bind_rows(
  make_kde(shots, h = 1.5, label = "Small bandwidth: h = 1.5"),
  make_kde(shots, h = 4.0, label = "Medium bandwidth: h = 4.0"),
  make_kde(shots, h = 8.0, label = "Large bandwidth: h = 8.0")
)

kde_plot = ggplot() +
  pitch_background() +
  geom_raster(
    data = kde_results,
    aes(x = shot_x, y = shot_y, fill = density),
    alpha = 0.9
  ) +
  pitch_lines() +
  coord_fixed(xlim = c(0, 68), ylim = c(70, 105.8), expand = FALSE) +
  facet_wrap(~ bandwidth) +
  scale_fill_viridis_c(option = "magma", name = "Shot density") +
  labs(title = "Kernel Density Estimates of Shot Locations") +
  theme_pitch +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(kde_plot)

cat("\nTASK 2 INTERPRETATION:\n")
cat("A small bandwidth gives a detailed density surface, but it can be noisy.\n")
cat("A medium bandwidth balances detail and smoothness.\n")
cat("A large bandwidth gives a smoother surface, but it can hide local patterns.\n")
cat("The small bandwidth may look noisy in low-volume wide-angle regions.\n")
cat("The large bandwidth may be too smooth near the middle of the penalty area.\n\n")

###########################################
### TASK 3: LOCATION ONLY KERNEL XG #######
###########################################

location_train = train |>
  dplyr::select(shot_x, shot_y)

location_validation = validation |>
  dplyr::select(shot_x, shot_y)

location_test = test |>
  dplyr::select(shot_x, shot_y)

location_bandwidth_grid = c(1.5, 2, 2.8, 3.6, 4.5, 5.5, 7, 9, 12)

# Task 3.1-3.3:
# Compute validation log loss for each bandwidth and choose the best one.

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
  ) |>
  arrange(validation_log_loss)

print(location_validation_results)

best_location_h = location_validation_results |>
  slice(1) |>
  pull(bandwidth)

cat("\nBest location-only bandwidth:", best_location_h, "\n")

# Task 3.4:
# Refit using train + validation, then evaluate on test.

location_test_pred = nw_predict_scaled(
  train_validation |>
    dplyr::select(shot_x, shot_y),
  train_validation$goal,
  location_test,
  bandwidth = c(best_location_h, best_location_h)
)

location_test_log_loss = log_loss(test$goal, location_test_pred)

cat("Location-only test log loss:", location_test_log_loss, "\n")

# Plot bandwidth tuning result.

location_bandwidth_plot = ggplot(
  location_validation_results,
  aes(x = bandwidth, y = validation_log_loss)
) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = best_location_h, linetype = "dashed") +
  labs(
    title = "Location-Only Kernel xG: Bandwidth Tuning",
    x = "Bandwidth",
    y = "Validation log loss"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(location_bandwidth_plot)

# Task 3.5:
# Create a pitch heatmap of the location-only xG surface.

xg_grid = expand_grid(
  shot_x = seq(0, 68, length.out = 95),
  shot_y = seq(70, 105, length.out = 85)
)

xg_grid = xg_grid |>
  mutate(
    location_xg = nw_predict_scaled(
      train_validation |>
        dplyr::select(shot_x, shot_y),
      train_validation$goal,
      xg_grid |>
        dplyr::select(shot_x, shot_y),
      bandwidth = c(best_location_h, best_location_h)
    )
  )

location_xg_heatmap = ggplot() +
  pitch_background() +
  geom_raster(
    data = xg_grid,
    aes(x = shot_x, y = shot_y, fill = location_xg),
    alpha = 0.9
  ) +
  pitch_lines() +
  coord_fixed(xlim = c(0, 68), ylim = c(70, 105.8), expand = FALSE) +
  scale_fill_viridis_c(option = "magma", name = "Location xG") +
  labs(title = "Location-Only Kernel Expected Goals Surface") +
  theme_pitch

print(location_xg_heatmap)

cat("\nTASK 3 INTERPRETATION:\n")
cat("The location-only xG model uses only shot_x and shot_y.\n")
cat("The highest xG areas should be central and close to the goal.\n")
cat("Farther shots and wide-angle shots should generally have lower predicted xG.\n\n")

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

# Task 4.1-4.2:
# Standardize the features using train only, then tune bandwidth on validation.

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
  ) |>
  arrange(validation_log_loss)

print(rich_validation_results)

best_rich_h = rich_validation_results |>
  slice(1) |>
  pull(bandwidth)

cat("\nBest rich-feature bandwidth:", best_rich_h, "\n")

# Task 4.3:
# Refit using train + validation, then evaluate on test.

rich_test_scaled = standardize_train_new(train_validation, test, rich_cols)

rich_test_pred = nw_predict_scaled(
  rich_test_scaled$train,
  train_validation$goal,
  rich_test_scaled$new,
  bandwidth = best_rich_h
)

rich_test_log_loss = log_loss(test$goal, rich_test_pred)

cat("Rich-feature test log loss:", rich_test_log_loss, "\n")

# Plot rich-feature bandwidth tuning result.

rich_bandwidth_plot = ggplot(
  rich_validation_results,
  aes(x = bandwidth, y = validation_log_loss)
) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = best_rich_h, linetype = "dashed") +
  labs(
    title = "Rich Tracking-Feature Kernel xG: Bandwidth Tuning",
    x = "Bandwidth",
    y = "Validation log loss"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(rich_bandwidth_plot)

# Task 4.4:
# Compare the location-only model and rich-feature model using test log loss.

model_comparison = tibble(
  model = c("Location only kernel xG", "Rich tracking-feature kernel xG"),
  test_log_loss = c(location_test_log_loss, rich_test_log_loss)
) |>
  arrange(test_log_loss)

print(model_comparison)

cat("\nMODEL COMPARISON:\n")

if (rich_test_log_loss < location_test_log_loss) {
  cat("The rich tracking-feature model has lower test log loss.\n")
  cat("That means it predicts test shots better than the location-only model.\n\n")
} else {
  cat("The location-only model has lower test log loss.\n")
  cat("That means the richer tracking features did not improve test prediction here.\n\n")
}

# Task 4.5:
# Make a scatter plot comparing the two models' test predictions.

test_predictions = test |>
  mutate(
    location_xg = location_test_pred,
    rich_xg = rich_test_pred,
    xg_change = rich_xg - location_xg,
    goal_label = if_else(goal == 1, "Goal", "No goal")
  )

prediction_scatter = ggplot(
  test_predictions,
  aes(x = location_xg, y = rich_xg, color = goal_label)
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.55, size = 1.4) +
  labs(
    title = "Location-Only xG vs Rich Tracking-Feature xG",
    x = "Location-only predicted xG",
    y = "Rich-feature predicted xG",
    color = "Outcome"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(prediction_scatter)

# Show the shots that changed the most after adding tracking features.

largest_increases = test_predictions |>
  arrange(desc(xg_change)) |>
  dplyr::select(
    goal,
    location_xg,
    rich_xg,
    xg_change,
    distance_to_goal,
    abs_angle_to_goal,
    open_goal_share,
    nearest_defender_distance
  ) |>
  slice(1:10)

largest_decreases = test_predictions |>
  arrange(xg_change) |>
  dplyr::select(
    goal,
    location_xg,
    rich_xg,
    xg_change,
    distance_to_goal,
    abs_angle_to_goal,
    open_goal_share,
    nearest_defender_distance
  ) |>
  slice(1:10)

cat("\nShots moved up the most by rich tracking features:\n")
print(largest_increases)

cat("\nShots moved down the most by rich tracking features:\n")
print(largest_decreases)

cat("\nTASK 4 INTERPRETATION:\n")
cat("Points above the dashed line have higher xG under the rich tracking-feature model.\n")
cat("These shots likely have helpful context beyond location, such as more open goal space,\n")
cat("better goalkeeper positioning, or more distance from defenders.\n")
cat("Points below the dashed line have lower xG under the rich model.\n")
cat("That means the tracking features make those shots look less dangerous than their location alone suggests.\n\n")