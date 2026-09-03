#######################
### Authors: JP, RB ###
#######################

#############
### SETUP ###
#############

# install.packages(c("MASS", "tidyverse"))
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

# Local regression with adaptive bandwidth based on data size
adaptive_span = function(n_train, n_features, base_factor = 0.5) {
  # Adaptive span formula: decreases as training size and dimensionality increase
  # span = base_factor / n^(1/(1 + n_features))
  # Constrained to reasonable [0.15, 0.95] range
  pmax(0.15, pmin(0.95, base_factor / (n_train ^ (1 / (1 + n_features)))))
}

loess_predict = function(train_x, train_y, new_x, base_factor = 0.5) {
  # Convert to data frames and ensure numeric
  train_x = as.data.frame(train_x)
  new_x = as.data.frame(new_x)
  train_y = as.numeric(train_y)
  
  # Standardize column names
  col_names = paste0("x", seq_len(ncol(train_x)))
  names(train_x) = col_names
  names(new_x) = col_names
  
  n_features = ncol(train_x)
  n_train = nrow(train_x)
  
  # Calculate adaptive span
  span = adaptive_span(n_train, n_features, base_factor)
  
  # Build formula
  formula_str = paste("y ~", paste(col_names, collapse = " + "))
  formula_obj = as.formula(formula_str)
  
  # Fit loess model with error handling
  train_df = cbind(train_x, y = train_y)
  
  tryCatch({
    # Try loess first
    model = loess(formula_obj, data = train_df, span = span, degree = 1, 
                  control = loess.control(surface = "direct"))
    pred = predict(model, new_x)
    
    # Check if prediction failed
    if (all(is.na(pred))) {
      warning("loess prediction returned all NAs, using fallback kernel method")
      return(nw_predict_scaled_fallback(train_x, train_y, new_x, base_factor))
    }
    
    clip_probability(pmin(pmax(pred, 0), 1))
  }, error = function(e) {
    warning(paste("loess failed:", e$message, "- using fallback kernel method"))
    nw_predict_scaled_fallback(train_x, train_y, new_x, base_factor)
  })
}

# Fallback: simple kernel regression with adaptive bandwidth
nw_predict_scaled_fallback = function(train_x, train_y, new_x, base_factor = 0.5) {
  train_x = as.matrix(train_x)
  new_x = as.matrix(new_x)
  train_y = as.numeric(train_y)
  
  n_train = nrow(train_x)
  n_features = ncol(train_x)
  
  # Scale bandwidth adaptively
  h = base_factor / (n_train ^ (1 / (1 + n_features)))
  bandwidth = rep(h, n_features)
  
  # Standardize
  train_scaled = sweep(train_x, 2, bandwidth, "/")
  new_scaled = sweep(new_x, 2, bandwidth, "/")
  
  # Kernel regression
  pred = numeric(nrow(new_scaled))
  chunk_size = 400
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

# Pitch plot of all shot locations, coloring goals differently
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

# Summarize distributions of important shot quality features
feature_summary = shots |>
  pivot_longer(
    cols = c(distance_to_goal, open_goal_share, nearest_defender_distance),
    names_to = "feature",
    values_to = "value"
  ) |>
  ggplot(aes(x = value, fill = factor(goal))) +
  geom_density(alpha = 0.6) +
  facet_wrap(~feature, scales = "free") +
  scale_fill_manual(values = c("0" = "white", "1" = "#D95D39"), name = "Goal") +
  theme_minimal() +
  labs(title = "Shot Quality Features by Goal Outcome")

print(feature_summary)

############################
### TASK 2: SHOT DENSITY ###
############################

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

kde_data = bind_rows(
  make_kde(train, 1.5, "h = 1.5"),
  make_kde(train, 3.0, "h = 3.0"),
  make_kde(train, 6.0, "h = 6.0")
)

# Simple version without pitch background (more visible density)
kde_plot_simple = kde_data |>
  ggplot(aes(shot_x, shot_y, fill = density)) +
  facet_wrap(~bandwidth, nrow = 1) +
  geom_raster(interpolate = TRUE) +
  coord_fixed(xlim = c(0, 68), ylim = c(70, 105), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Kernel Density Estimation: Shot Locations",
    x = "X Position",
    y = "Y Position"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 10)),
    legend.position = "bottom"
  )

print(kde_plot_simple)

# Alternative: overlay on pitch
kde_plot_pitch = kde_data |>
  ggplot(aes(shot_x, shot_y, fill = density)) +
  facet_wrap(~bandwidth, nrow = 1) +
  pitch_background() +
  geom_raster(interpolate = TRUE, alpha = 0.8) +
  pitch_lines() +
  coord_fixed(xlim = c(0, 68), ylim = c(70, 105.8), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", alpha = 0.9) +
  labs(title = "KDE Density on Pitch") +
  theme_pitch

print(kde_plot_pitch)

###########################################
### TASK 3: LOCATION ONLY LOCAL REGRESSION XG #######
###########################################

location_train = train |> select(shot_x, shot_y)
location_validation = validation |> select(shot_x, shot_y)
location_test = test |> select(shot_x, shot_y)

# Compute validation log loss for different base_factor values (controls adaptive span)
location_base_factor_grid = c(0.3, 0.5, 0.7, 1.0, 1.3, 1.7)

location_validation_results = tibble(base_factor = location_base_factor_grid) |>
  mutate(
    validation_log_loss = map_dbl(
      base_factor,
      \(bf) {
        pred = loess_predict(
          location_train,
          train$goal,
          location_validation,
          base_factor = bf
        )
        log_loss(validation$goal, pred)
      }
    )
  )

print(location_validation_results)

best_location_bf = location_validation_results |>
  arrange(validation_log_loss) |>
  slice(1) |>
  pull(base_factor)

# Fit on train + validation, then evaluate test log loss
location_test_pred = loess_predict(
  train_validation |> select(shot_x, shot_y),
  train_validation$goal,
  location_test,
  base_factor = best_location_bf
)
location_test_log_loss = log_loss(test$goal, location_test_pred)

cat("Location-only model test log loss:", location_test_log_loss, "\n")

# Make a pitch heatmap of location only xG
xg_grid = expand_grid(
  shot_x = seq(0, 68, length.out = 95),
  shot_y = seq(70, 105, length.out = 85)
)

location_xg_pred = loess_predict(
  train_validation |> select(shot_x, shot_y),
  train_validation$goal,
  xg_grid,
  base_factor = best_location_bf
)

# Simple version (clear and visible)
location_xg_plot = xg_grid |>
  mutate(xg = location_xg_pred) |>
  ggplot(aes(shot_x, shot_y, fill = xg)) +
  geom_raster(interpolate = TRUE) +
  coord_fixed(xlim = c(0, 68), ylim = c(70, 105), expand = FALSE) +
  scale_fill_viridis_c(limits = c(0, 0.5), name = "xG") +
  labs(
    title = "Location-Only Local Regression xG Model",
    x = "X Position",
    y = "Y Position"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 10))
  )

print(location_xg_plot)

# Alternative: overlay on pitch with transparency
location_xg_plot_pitch = xg_grid |>
  mutate(xg = location_xg_pred) |>
  ggplot(aes(shot_x, shot_y, z = xg)) +
  pitch_background() +
  geom_contour_filled(bins = 12, alpha = 0.85) +
  geom_contour(color = "white", bins = 12, linewidth = 0.3) +
  pitch_lines() +
  coord_fixed(
    xlim = c(0, 68),
    ylim = c(70, 105.8),
    expand = FALSE
  ) +
  scale_fill_viridis_d(name = "xG") +
  labs(title = "Location-Only xG (on Pitch)") +
  theme_pitch

print(location_xg_plot_pitch)

##############################################
### TASK 4: TRACKING FEATURE LOCAL REGRESSION XG #######
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

rich_base_factor_grid = c(0.3, 0.5, 0.7, 1.0, 1.3, 1.7, 2.0)

# Standardize rich features using train, then tune base_factor on validation
rich_validation_scaled = standardize_train_new(train, validation, rich_cols)

rich_validation_results = tibble(base_factor = rich_base_factor_grid) |>
  mutate(
    validation_log_loss = map_dbl(
      base_factor,
      \(bf) {
        pred = loess_predict(
          rich_validation_scaled$train,
          train$goal,
          rich_validation_scaled$new,
          base_factor = bf
        )
        log_loss(validation$goal, pred)
      }
    )
  )

print(rich_validation_results)

best_rich_bf = rich_validation_results |>
  arrange(validation_log_loss) |>
  slice(1) |>
  pull(base_factor)

# Refit using train + validation, evaluate on test, and compare with location only model
rich_test_scaled = standardize_train_new(train_validation, test, rich_cols)

rich_test_pred = loess_predict(
  rich_test_scaled$train,
  train_validation$goal,
  rich_test_scaled$new,
  base_factor = best_rich_bf
)

rich_test_log_loss = log_loss(test$goal, rich_test_pred)

# Model comparison
cat("\n=== MODEL COMPARISON ===\n")
cat("Location-only test log loss:  ", location_test_log_loss, "\n")
cat("Rich features test log loss:  ", rich_test_log_loss, "\n")
cat("Improvement:                  ", location_test_log_loss - rich_test_log_loss, "\n")
cat("Percent improvement:          ", 100 * (location_test_log_loss - rich_test_log_loss) / location_test_log_loss, "%\n")
