library(ggplot2)
library(nnet)
library(splines)
library(tidyverse)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

nfl_data = read_csv("06_expected-points.csv.gz")

# Model 1: Just Yard Line

linear_function_yard_line <- multinom(
  pts_next_score ~ yardline_100,
  data = nfl_data
)

summary(linear_function_yard_line)

yardline_seq <- data.frame(yardline_100 = 1:99)

predicted_probs <- predict(
  linear_function_yard_line,
  newdata = yardline_seq,
  type = "probs"
)

outcomes <- as.numeric(colnames(predicted_probs))

prob_matrix <- as.matrix(predicted_probs)

weighted_probs <- sweep(prob_matrix, 2, outcomes, `*`)

expected_values <- rowSums(weighted_probs)

plot(expected_values, type = "l", 
     main = "Expected Values Over Observations",
     xlab = "Observation Index",
     ylab = "Expected Value",
     col = "blue", lwd = 2)


# Model 2: Spline

spline_function_yard_line <- multinom(
  pts_next_score ~ bs(yardline_100, df = 5, degree = 3),
  data = nfl_data
)

summary(spline_function_yard_line)

spline_predicted_probs <- predict(
  spline_function_yard_line,
  newdata = yardline_seq,
  type = "probs"
)

spline_prob_matrix <- as.matrix(spline_predicted_probs)

spline_weighted_probs <- sweep(spline_prob_matrix, 2, outcomes, `*`)

spline_expected_values <- rowSums(spline_weighted_probs)

plot(spline_expected_values, type = "l", 
     main = "Spline Expected Values Over Observations",
     xlab = "Observation Index",
     ylab = "Expected Value",
     col = "red", lwd = 2)

# Model 3: yard Line and Down

linear_yard_line_and_down <- multinom(
  pts_next_score ~ yardline_100 + as.factor(down),
  data = nfl_data
)

summary(linear_yard_line_and_down)

newdata_all <- expand.grid(
  yardline_100 = 1:99,
  down = 1:4
)

ydl_downs_predicted_probs <- predict(
  linear_yard_line_and_down,
  newdata = newdata_all,
  type = "probs"
)

ydl_downs_prob_matrix <- as.matrix(ydl_downs_predicted_probs)

ydl_downs_weighted_probs <- sweep(ydl_downs_prob_matrix, 2, outcomes, `*`)

ydl_downs_expected_values <- rowSums(ydl_downs_weighted_probs)

expected_matrix <- matrix(ydl_downs_expected_values, nrow = 99, ncol = 4, byrow = FALSE)

colors <- c("red", "blue", "green", "purple")

plot(expected_matrix[,1], type = "l", 
     main = "Expected Values by Down",
     xlab = "Observation Index",
     ylab = "Expected Value",
     col = colors[1], lwd = 2,
     ylim = range(expected_matrix))

for (i in 2:4) {
  lines(expected_matrix[, i], col = colors[i], lwd = 2)
}

legend("topright", legend = paste("Down", 1:4), col = colors, lwd = 1)

# Model 4: Yard Line, Down, and Yards to Go

linear_yard_line_down_yards_to_go <- multinom(
  pts_next_score ~ yardline_100 + as.factor(down) + ydstogo,
  data = nfl_data
)

model_fits <- as.data.frame(linear_yard_line_down_yards_to_go$fitted.values)

model_ep <- model_fits %>%
  mutate(expected_points = `-2` * -2 + `-3` * -3 + `-7` * -7 + `0` * 0 + `2` * 2 + `3` * 3 + `7` * 7) %>%
  select(expected_points)

model_results <- cbind(model_ep$expected_points, nfl_data$yardline_100, nfl_data$down, nfl_data$ydstogo) %>%
  as.data.frame()

colnames(model_results) <- c("expected_points", "yardline_100", "down", "yards_to_go")
model_results$down <- as.factor(model_results$down)

ggplot(model_results,aes(x=yardline_100, y=expected_points)) +
  geom_point(aes(color=yards_to_go), alpha=0.1) +
  labs(x = "Yardline", y = "Expected Points", title = "Expected Points by Yardline and Down") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  facet_wrap(~down)

# Model 5: Including Time left in Half

model_with_time_left <- multinom(
  pts_next_score ~ yardline_100 + as.factor(down) + ydstogo + half_seconds_remaining,
  data = nfl_data
)

summary(model_with_time_left)

model_time_left_fits <- as.data.frame(model_with_time_left$fitted.values)

neautral_model_ep <- model_time_left_fits %>%
  mutate(expected_points = `-2` * -2 + `-3` * -3 + `-7` * -7 + `0` * 0 + `2` * 2 + `3` * 3 + `7` * 7) %>%
  select(expected_points)

model_results <- cbind(neautral_model_ep$expected_points, nfl_data$yardline_100, nfl_data$down, nfl_data$ydstogo, nfl_data$half_seconds_remaining) %>%
  as.data.frame()

colnames(model_results) <- c("expected_points", "yardline_100", "down", "yards_to_go", "half_seconds_remaining")
model_results$down <- as.factor(model_results$down)

model_results <- model_results %>%
  filter(yards_to_go %in% c(1, 10))

ggplot(model_results,aes(x=yardline_100, y=expected_points)) +
  geom_point(aes(color=half_seconds_remaining), alpha=0.1) +
  labs(x = "Yardline", y = "Expected Points", title = "Expected Points by Yardline and Down") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  facet_wrap(~down)

# Task 2

model_with_team_quality <- multinom(
  pts_next_score ~ yardline_100 + as.factor(down) + ydstogo + half_seconds_remaining + posteam_spread,
  data = nfl_data
)

summary(model_with_team_quality)

nfl_data_neutral <- nfl_data
nfl_data_neutral$posteam_spread <- 0

model_fits_quality <- as.data.frame(model_with_team_quality$fitted.values)

model_ep_quality <- model_fits_quality %>%
  mutate(expected_points = `-2` * -2 + `-3` * -3 + `-7` * -7 + `0` * 0 + `2` * 2 + `3` * 3 + `7` * 7) %>%
  select(expected_points)

model_results_quality <- cbind(
  model_ep_quality$expected_points,
  nfl_data$yardline_100,
  nfl_data$down,
  nfl_data$ydstogo,
  nfl_data$half_seconds_remaining,
  nfl_data$posteam_spread
) %>%
  as.data.frame()

colnames(model_results_quality) <- c("expected_points", "yardline_100", "down", "yards_to_go", "half_seconds_remaining", "posteam_spread")
model_results_quality$down <- as.factor(model_results_quality$down)

model_results_quality <- model_results_quality %>%
  filter(yards_to_go %in% c(1, 10))

ggplot(model_results_quality,aes(x=yardline_100, y=expected_points)) +
  geom_point(aes(color=half_seconds_remaining), alpha=0.1) +
  labs(x = "Yardline", y = "Expected Points", title = "Expected Points by Yardline and Down") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  facet_wrap(~down)

# They are different, the second one has more noise due to the extra parameter
