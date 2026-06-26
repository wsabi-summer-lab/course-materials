#############
### SETUP ###
#############

# Install these only if needed:
# install.packages(c("tidyverse"))

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

# This tries several common file locations.
# If your CSV is in the same folder as this R script, "19_shots.csv" should work.
possible_paths = c(
  "19_shots.csv",
  "../data/19_shots.csv",
  "2026/labs/data/19_shots.csv",
  "/mnt/data/19_shots.csv"
)

data_path = possible_paths[file.exists(possible_paths)][1]

if (is.na(data_path)) {
  stop("Could not find 19_shots.csv. Put it in your working directory or update data_path manually.")
}

shots = read_csv(data_path, show_col_types = FALSE) |>
  mutate(
    goal = as.integer(goal),
    split = factor(split, levels = c("train", "validation", "test")),
    
    # Keep plotted shot locations inside the attacking half/pitch region.
    shot_x_plot = pmin(pmax(shot_x, 0), 68),
    shot_y_plot = pmin(pmax(shot_y, 70), 105),
    
    # Useful later for richer feature models.
    log_ball_speed = log1p(
      pmin(ball_speed, quantile(ball_speed, 0.995, na.rm = TRUE))
    )
  )

train = shots |> filter(split == "train")
validation = shots |> filter(split == "validation")
test = shots |> filter(split == "test")
train_validation = shots |> filter(split %in% c("train", "validation"))

############################
### TASK 1: EXPLORATION ####
############################

#######################################################
### 1. Report number of shots and goals in each split ###
#######################################################

split_summary = shots |>
  group_by(split) |>
  summarize(
    shots = n(),
    goals = sum(goal),
    goal_rate = mean(goal),
    .groups = "drop"
  )

print(split_summary)

#######################################################
### 2. Pitch plot of all shot locations              ###
#######################################################

shot_location_plot = ggplot() +
  pitch_background() +
  geom_point(
    data = shots |> filter(goal == 0),
    aes(x = shot_x_plot, y = shot_y_plot),
    color = "white",
    alpha = 0.35,
    size = 0.75
  ) +
  geom_point(
    data = shots |> filter(goal == 1),
    aes(x = shot_x_plot, y = shot_y_plot),
    color = "#D95D39",
    alpha = 0.75,
    size = 1.05
  ) +
  pitch_lines() +
  coord_fixed(
    xlim = c(0, 68),
    ylim = c(70, 105.8),
    expand = FALSE
  ) +
  labs(
    title = "Premier League Shot Locations, 2024-2025",
    subtitle = "White = no goal; orange = goal"
  ) +
  theme_pitch +
  theme(
    plot.subtitle = element_text(
      color = "gray25",
      margin = margin(b = 6)
    )
  )

print(shot_location_plot)

###################################################################
### 3. Summarize distributions of important shot quality features ###
###################################################################

task1_features = c(
  "distance_to_goal",
  "abs_angle_to_goal",
  "open_goal_share",
  "nearest_defender_distance"
)

# Summary table by goal outcome
feature_summary_by_goal = shots |>
  select(goal, all_of(task1_features)) |>
  pivot_longer(
    cols = all_of(task1_features),
    names_to = "feature",
    values_to = "value"
  ) |>
  group_by(feature, goal) |>
  summarize(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

print(feature_summary_by_goal)

# Cleaner feature names for plotting
feature_labels = c(
  distance_to_goal = "Distance to goal",
  abs_angle_to_goal = "Absolute angle to goal",
  open_goal_share = "Open goal share",
  nearest_defender_distance = "Nearest defender distance"
)

feature_long = shots |>
  select(goal, all_of(task1_features)) |>
  mutate(
    goal_label = if_else(goal == 1, "Goal", "No goal")
  ) |>
  pivot_longer(
    cols = all_of(task1_features),
    names_to = "feature",
    values_to = "value"
  ) |>
  mutate(
    feature = recode(feature, !!!feature_labels)
  )

# Distribution plots by goal outcome
feature_distribution_plot = ggplot(
  feature_long,
  aes(x = value, fill = goal_label)
) +
  geom_density(alpha = 0.40, na.rm = TRUE) +
  facet_wrap(~ feature, scales = "free", ncol = 2) +
  labs(
    title = "Shot Quality Feature Distributions by Outcome",
    x = NULL,
    y = "Density",
    fill = "Outcome"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(feature_distribution_plot)

##############################################################
### 4. Goal rate by feature bins                            ###
### This helps identify which features relate to goal chance ###
##############################################################

feature_bin_goal_rates = shots |>
  select(goal, all_of(task1_features)) |>
  pivot_longer(
    cols = all_of(task1_features),
    names_to = "feature",
    values_to = "value"
  ) |>
  group_by(feature) |>
  mutate(
    feature_bin = ntile(value, 4)
  ) |>
  ungroup() |>
  group_by(feature, feature_bin) |>
  summarize(
    n = n(),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    avg_value = mean(value, na.rm = TRUE),
    goal_rate = mean(goal, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    feature = recode(feature, !!!feature_labels)
  )

print(feature_bin_goal_rates)

feature_bin_plot = ggplot(
  feature_bin_goal_rates,
  aes(x = avg_value, y = goal_rate)
) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ feature, scales = "free_x", ncol = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Goal Rate Across Feature Quartiles",
    subtitle = "Each point shows the goal rate within one quartile of the feature",
    x = "Average feature value within quartile",
    y = "Goal rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30")
  )

print(feature_bin_plot)

##############################################################
### 5. Quick written-style interpretation table             ###
##############################################################

interpretation_table = feature_bin_goal_rates |>
  group_by(feature) |>
  summarize(
    lowest_bin_goal_rate = first(goal_rate),
    highest_bin_goal_rate = last(goal_rate),
    difference_high_minus_low = last(goal_rate) - first(goal_rate),
    .groups = "drop"
  )

print(interpretation_table)
