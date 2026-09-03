#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "splines", "tidyverse"))
library(ggplot2)
library(nnet)
library(splines)
library(tidyverse)
library(plotly)
library(dplyr)

#######################
### EXPECTED POINTS ###
#######################

# load data
nfl_data = read_csv("data/06_expected-points.csv.gz")
glimpse(nfl_data)
# create histograms for ydstogo yardline_100 and a bar graph for pts_next_score
ggplot(nfl_data, aes(x = yardline_100)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Histogram of Yardline 100", x = "Yardline 100", y = "Frequency")
ggplot(nfl_data, aes(x = ydstogo)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Yards to Go", x = "Yards to Go", y = "Frequency")
ggplot(nfl_data, aes(x = pts_next_score)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Bar Graph of Points to Next Score", x = "Points to Next Score", y = "Frequency")
#groupby yardline_100 and calculate the mean of pts_next_score
nfl_data %>%
  group_by(yardline_100) %>%
  summarise(mean_pts_next_score = mean(pts_next_score, na.rm = TRUE)) %>%
  ggplot(aes(x = yardline_100, y = mean_pts_next_score)) +
  geom_line(color = "blue") +
  labs(title = "Mean Points to Next Score by Yardline 100", x = "Yardline 100", y = "Mean Points to Next Score")
# Fit the multinomial logistic regression
simple_model <- multinom(pts_next_score ~ yardline_100, data = nfl_data)

# Get predicted probabilities for each class (value of pts_next_score)
prob_matrix <- predict(simple_model, type = "probs")  # matrix: rows = observations, cols = class probabilities
prob_matrix
# Extract the actual score values from column names (they're used as class labels)
score_values <- as.numeric(colnames(prob_matrix))
score_values
# Compute expected points for each observation
expected_values_simple <- prob_matrix %*% score_values  # matrix multiplication: gives a vector of expected points
expected_values_simple
# Add the expected values to the data
nfl_data$expected_pts_simple <- as.numeric(expected_values_simple)
# Plot the expected points against yardline_100
ggplot(nfl_data, aes(x = yardline_100, y = expected_pts_simple)) +
  geom_line(color = "blue") +
  labs(title = "Expected Points by Yardline 100", x = "Yardline 100", y = "Expected Points")
unique(colnames(nfl_data))

# Summarize both values by yardline_100
summary_data <- nfl_data %>%
  group_by(yardline_100) %>%
  summarise(
    mean_pts_next_score = mean(pts_next_score, na.rm = TRUE),
    expected_pts_simple = mean(expected_pts_simple, na.rm = TRUE)
  )

# Reshape to long format for cleaner plotting
plot_data <- summary_data %>%
  pivot_longer(cols = c(mean_pts_next_score, expected_pts_simple),
               names_to = "type", values_to = "points")

# Plot both with color legend
ggplot(plot_data, aes(x = yardline_100, y = points, color = type)) +
  geom_line(size = 1.1) +
  labs(
    title = "Expected vs Actual Mean Points by Yardline 100",
    x = "Yardline 100",
    y = "Points",
    color = "Metric"
  ) +
  scale_color_manual(
    values = c("mean_pts_next_score" = "blue", "expected_pts_simple" = "red"),
    labels = c("Actual Mean", "Model Expected")
  ) +
  theme_minimal()

##########################

##including spline into model
# Fit the multinomial logistic regression with a spline term
spline_model <- multinom(pts_next_score ~ bs(yardline_100, df=5), data = nfl_data)
# Predict probabilities for each outcome
prob_matrix_spline <- predict(spline_model, type = "probs")
prob_matrix_spline
# Extract class labels
score_values <- as.numeric(colnames(prob_matrix_spline))
score_values
# Compute expected points
expected_values_spline <- prob_matrix_spline %*% score_values
nfl_data$expected_pts_spline <- as.numeric(expected_values_spline)
summary_data <- nfl_data %>%
  group_by(yardline_100) %>%
  summarise(
    mean_pts_next_score = mean(pts_next_score, na.rm = TRUE),
    expected_pts_simple = mean(expected_pts_simple, na.rm = TRUE),
    expected_pts_spline = mean(expected_pts_spline, na.rm = TRUE)
  )
plot_data <- summary_data %>%
  pivot_longer(
    cols = c(mean_pts_next_score, expected_pts_simple, expected_pts_spline),
    names_to = "type",
    values_to = "points"
  )
ggplot(plot_data, aes(x = yardline_100, y = points, color = type)) +
  geom_line(size = 1.1) +
  labs(
    title = "Expected vs Actual Mean Points by Yardline 100",
    x = "Yardline 100",
    y = "Points",
    color = "Metric"
  ) +
  scale_color_manual(
    values = c(
      "mean_pts_next_score" = "black",
      "expected_pts_simple" = "lightgray",
      "expected_pts_spline" = "steelblue"
    ),
    labels = c("Actual Mean", "Model Expected (Linear)", "Model Expected (Spline)")
  ) +
  theme_minimal()
# do this again with down as a confounder
nfl_data$down <- as.factor(nfl_data$down)
# Fit the multinomial logistic regression with a spline term and down as a confounder
spline_model_down <- multinom(pts_next_score ~ bs(yardline_100, df=5) + down, data = nfl_data)
# Predict probabilities for each outcome
prob_matrix_spline_down <- predict(spline_model_down, type = "probs")
# Extract class labels
score_values <- as.numeric(colnames(prob_matrix_spline_down))
# Compute expected points
expected_values_spline_down <- prob_matrix_spline_down %*% score_values
nfl_data$expected_pts_spline_down <- as.numeric(expected_values_spline_down)
# plot the expected points in the model vs observed for each down
summary_data <- nfl_data %>%
  group_by(yardline_100, down) %>%
  summarise(
    mean_pts_next_score = mean(pts_next_score, na.rm = TRUE),
    expected_pts_spline_down = mean(expected_pts_spline_down, na.rm = TRUE)
  )
plot_data <- summary_data %>%
  pivot_longer(
    cols = c(mean_pts_next_score, expected_pts_spline_down),
    names_to = "type",
    values_to = "points"
  )
ggplot(plot_data, aes(x = yardline_100, y = points, color = type)) +
  geom_line(size = 1.1) +
  facet_wrap(~ down) +
  labs(
    title = "Expected vs Actual Mean Points by Yardline 100 and Down",
    x = "Yardline 100",
    y = "Points",
    color = "Metric"
  ) +
  scale_color_manual(
    values = c("mean_pts_next_score" = "mediumseagreen", "expected_pts_spline_down" = "black"),
    labels = c("Actual Mean", "Model Expected (Spline with Down)")
  ) +
  theme_minimal()
#run a new model including ydstogo
spline_model_ydstogo <- multinom(pts_next_score ~ bs(yardline_100, df=5) + down + ydstogo, data = nfl_data)
# Predict probabilities for each outcome
prob_matrix_spline_ydstogo <- predict(spline_model_ydstogo, type = "probs")
# Extract class labels
score_values <- as.numeric(colnames(prob_matrix_spline_ydstogo))
# Compute expected points
expected_values_spline_ydstogo <- prob_matrix_spline_ydstogo %*% score_values
nfl_data$expected_pts_spline_ydstogo <- as.numeric(expected_values_spline_ydstogo)
# plot the expected points in the model vs observed for each down
summary_data <- nfl_data %>%
  group_by(yardline_100, down) %>%
  summarise(
    mean_pts_next_score = mean(pts_next_score, na.rm = TRUE),
    expected_pts_spline_ydstogo = mean(expected_pts_spline_ydstogo, na.rm = TRUE)
  )
plot_data <- summary_data %>%
  pivot_longer(
    cols = c(mean_pts_next_score, expected_pts_spline_ydstogo),
    names_to = "type",
    values_to = "points"
  )
# Create a grid of values
yard_vals <- seq(1, 99, by = 1)
togo_vals <- seq(1, 20, by = 1)
downs <- factor(1:4)

# Expand to full grid
prediction_grid <- expand.grid(
  yardline_100 = yard_vals,
  ydstogo = togo_vals,
  down = downs
)
# Predict probabilities and expected points
prob_matrix_grid <- predict(spline_model_ydstogo, newdata = prediction_grid, type = "probs")
score_values <- as.numeric(colnames(prob_matrix_grid))
prediction_grid$expected_pts <- as.numeric(prob_matrix_grid %*% score_values)
# Reshape for plotting
prediction_grid_long <- prediction_grid %>%
  select(yardline_100, down, expected_pts) %>%
  pivot_wider(names_from = down, values_from = expected_pts, names_prefix = "down_")

# Optionally bin ydstogo for cleaner coloring
prediction_grid <- prediction_grid %>%
  mutate(ydstogo_bin = cut(ydstogo, breaks = c(0, 5, 10, 15, 20), include.lowest = TRUE))

library(viridis)

ggplot(prediction_grid, aes(x = yardline_100, y = expected_pts, color = ydstogo, group = ydstogo)) +
  geom_line(size = 0.8) +
  facet_wrap(~ down) +
  scale_color_gradient(low = "#D0F0FD", high = "#1B365D") +
  labs(color = "Yards to Go")

# Fit model with spline on yardline_100 and half_seconds_remaining, plus down and ydstogo
spline_model_full <- multinom(
  pts_next_score ~ bs(yardline_100, df = 5) + down + ydstogo + bs(half_seconds_remaining, df = 5),
  data = nfl_data
)

# Predict probabilities on the full data
prob_matrix_full <- predict(spline_model_full, type = "probs")

# Extract class labels (score values)
score_values <- as.numeric(colnames(prob_matrix_full))

# Compute expected points per observation
expected_values_full <- prob_matrix_full %*% score_values

# Add expected points to nfl_data
nfl_data$expected_pts_full <- as.numeric(expected_values_full)

# Summarize actual and predicted points by yardline_100 and down
summary_data_full <- nfl_data %>%
  group_by(yardline_100, down) %>%
  summarise(
    mean_pts_next_score = mean(pts_next_score, na.rm = TRUE),
    expected_pts_full = mean(expected_pts_full, na.rm = TRUE)
  )

# Reshape data for plotting
plot_data_full <- summary_data_full %>%
  pivot_longer(
    cols = c(mean_pts_next_score, expected_pts_full),
    names_to = "type",
    values_to = "points"
  )

# Plot actual vs expected points faceted by down
ggplot(plot_data_full, aes(x = yardline_100, y = points, color = type)) +
  geom_line(size = 1.1) +
  facet_wrap(~ down) +
  labs(
    title = "Expected vs Actual Mean Points by Yardline 100 and Down (Full Model)",
    x = "Yardline 100",
    y = "Points",
    color = "Metric"
  ) +
  scale_color_manual(
    values = c("mean_pts_next_score" = "mediumseagreen", "expected_pts_full" = "black"),
    labels = c("Actual Mean", "Model Expected (Spline + Down + Ydstogo + Time)")
  ) +
  theme_minimal()

# Use only actual observed rows from nfl_data for prediction
prediction_data <- nfl_data %>%
  dplyr::select(yardline_100, ydstogo, down, half_seconds_remaining) %>%
  distinct()

# Predict probabilities on actual observed data
prob_matrix_actual <- predict(spline_model_full, newdata = prediction_data, type = "probs")

# Extract score values (class labels)
score_values <- as.numeric(colnames(prob_matrix_actual))

# Compute expected points
prediction_data$expected_pts <- as.numeric(prob_matrix_actual %*% score_values)

# Plot expected points by yardline_100 and down, colored by half_seconds_remaining
ggplot(prediction_data, aes(x = yardline_100, y = expected_pts, color = half_seconds_remaining, group = half_seconds_remaining)) +
  geom_line(size = 1, alpha = 0.7) +
  facet_wrap(~ down) +
  labs(
    title = "Expected Points by Yardline 100 and Down, Colored by Time Remaining",
    x = "Yardline 100",
    y = "Expected Points",
    color = "Seconds Remaining\nin Half"
  ) +
  scale_color_gradient(low = "#D0F0FD", high = "#1B365D") +
  theme_minimal()
unique(colnames(nfl_data))
"posteam_spread" %in% colnames(nfl_data)
if(!is.factor(nfl_data$posteam_spread)) {
  nfl_data$posteam_spread <- as.factor(nfl_data$posteam_spread)
}
# 1. Convert posteam_spread from factor to numeric safely
nfl_data <- nfl_data %>%
  mutate(posteam_spread_num = as.numeric(as.character(posteam_spread)))

# 2. Fit the multinomial logistic regression model with numeric spread
spline_model_with_spread <- multinom(
  pts_next_score ~ bs(yardline_100, df = 5) + down + ydstogo + bs(half_seconds_remaining, df = 5) + posteam_spread_num,
  data = nfl_data
)

# 3. Create counterfactual dataset with spread = 0
nfl_data_0spread <- nfl_data %>%
  mutate(posteam_spread_num = 0)

# 4. Predict probabilities with spread = 0 data
prob_matrix_0 <- predict(
  spline_model_with_spread,
  newdata = nfl_data_0spread,
  type = "probs"
)

# 5. Predict probabilities with actual spread data
prob_matrix_actual <- predict(
  spline_model_with_spread,
  newdata = nfl_data,
  type = "probs"
)

# 6. Extract score values (class labels) as numeric
score_values <- as.numeric(colnames(prob_matrix_0))

# 7. Compute expected points for both datasets
expected_pts_0 <- prob_matrix_0 %*% score_values
expected_pts_actual <- prob_matrix_actual %*% score_values

# 8. Add expected points to respective dataframes
nfl_data_0spread$expected_pts <- as.numeric(expected_pts_0)
nfl_data$expected_pts <- as.numeric(expected_pts_actual)

# 9. Define bin width for smoothing
bin_width <- 5

# 10. Bin and average expected points for spread=0 data
binned_0spread <- nfl_data_0spread %>%
  mutate(yardline_bin = cut(yardline_100, breaks = seq(0, 100, by = bin_width), include.lowest = TRUE)) %>%
  group_by(yardline_bin, down) %>%
  summarise(
    expected_pts = mean(expected_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(type = "Expected Points (Spread = 0)")

# 11. Bin and average expected points for actual spread data
binned_actualspread <- nfl_data %>%
  mutate(yardline_bin = cut(yardline_100, breaks = seq(0, 100, by = bin_width), include.lowest = TRUE)) %>%
  group_by(yardline_bin, down) %>%
  summarise(
    expected_pts = mean(expected_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(type = "Expected Points (Actual Spread)")

# 12. Combine datasets for plotting
plot_data <- bind_rows(binned_0spread, binned_actualspread)

# 13. Compute numeric midpoints of bins for plotting on x-axis
plot_data <- plot_data %>%
  mutate(
    yardline_mid = as.numeric(sub("\\((.+),(.+)\\]", "\\1", yardline_bin)) + bin_width / 2
  )

# 14. Plot combined smoothed expected points with facet by down and color by type
ggplot(plot_data, aes(x = yardline_mid, y = expected_pts, color = type)) +
  geom_line(size = 1.1) +
  facet_wrap(~ down) +
  labs(
    title = "Smoothed Expected Points by Yardline and Down",
    x = "Yardline (binned)",
    y = "Expected Points",
    color = "Model"
  ) +
  scale_color_manual(values = c("Expected Points (Spread = 0)" = "red",
                                "Expected Points (Actual Spread)" = "blue")) +
  theme_minimal()


##No, nba players that are good at making 3s shoot more, so percentage of all 3-point attempts should be higher, we could use player skill and other covariates as a confounder