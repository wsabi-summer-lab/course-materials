#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "splines", "tidyverse"))
library(ggplot2)
library(nnet)
library(splines)
library(tidyverse)

#######################
### EXPECTED POINTS ###
#######################

# load data
nfl_data = read_csv("../data/06_expected-points.csv.gz")

# print 50 rows of the dataset
print(nfl_data, n = 50)

# multinom model on yardline_100 and pts_next_score as a factor
model_multi = multinom(factor(pts_next_score) ~ yardline_100, data = nfl_data)

# Generate yardline's from 0 to 100
yardline_seq = seq(1, 100, by = 1)

# Get expected points
expected_points = predict(model_multi, newdata = data.frame(yardline_100 = yardline_seq), type = "prob")

expected_points = as.data.frame(expected_points)

expected_points

# Get the expected points for each yardline by doing 
expected_points_final = expected_points %>%
  mutate(
    yardline_100 = yardline_seq,
    expected_points = expected_points..7 * -7 + expected_points..3 * -3 + expected_points..2 * -2 + expected_points.0 * 0 + expected_points.2 * 2 + expected_points.3 * 3 + expected_points.7 * 7
  )

# Plot expected points against yardline_100 (do not do 100 - yardline)
ggplot(expected_points_final, aes(x = yardline_100, y = expected_points)) +
  geom_line() +
  labs(
    title = "Expected Points by Yardline",
    x = "Yardline (from opponent's end zone)",
    y = "Expected Points"
  ) +
  theme_minimal()

# Spline model on yardline_100 and pts_next_score as a factor
model_spline = multinom(factor(pts_next_score) ~ splines::bs(yardline_100, df = 5), data = nfl_data)

# Get expected points using the spline model and yardline_seq
expected_points_spline = predict(model_spline, newdata = data.frame(yardline_100 = yardline_seq), type = "prob")

# Convert to data frame
expected_points_spline = as.data.frame(expected_points_spline)

expected_points_spline

expected_points_spline_final = expected_points_spline %>%
  mutate(
    yardline_100 = yardline_seq,
    expected_points = `-7` * -7 + `-3` * -3 + `-2` * -2 + `0` * 0 + `2` * 2 + `3` * 3 + `7` * 7
  )

# Plot expected points against yardline_100 using the spline model
ggplot(expected_points_spline_final, aes(x = yardline_100, y = expected_points)) +
  geom_line() +
  labs(
    title = "Expected Points by Yardline (Spline Model)",
    x = "Yardline (from opponent's end zone)",
    y = "Expected Points"
  ) +
  theme_minimal()

# Model multinom as a function of yardline_100 and down
model_multi_down = multinom(factor(pts_next_score) ~ yardline_100 + factor(down), data = nfl_data)

# Generate sequence of downs 1 to 4
down_seq = 1:4 

down_seq

newdata_down = data.frame(yardline_100 = yardline_seq, down = factor(down_seq))

# Get expected points using the model with down_seq
expected_points_down = predict(model_multi_down, newdata = newdata_down, type = "prob")

# Convert to data frame
expected_points_down = as.data.frame(expected_points_down)

expected_points_down

expected_points_down_final = expected_points_down %>%
  mutate(
    yardline_100 = yardline_seq,
    down = newdata_down$down,
    expected_points = `-7` * -7 + `-3` * -3 + `-2` * -2 + `0` * 0 + `2` * 2 + `3` * 3 + `7` * 7
  )

expected_points_down_final = data.frame(yardline_100 =nfl_data$yardline_100,
                                        down = nfl_data$down,
                                        expected_points = predict(model_multi_down, type = "prob"))

expected_points_down_final

expected_points_down_final = expected_points_down_final %>%
  mutate(
    expected_points = expected_points..7 * -7 + expected_points..3 * -3 + expected_points..2 * -2 + expected_points.0 * 0 + expected_points.2 * 2 + expected_points.3 * 3 + expected_points.7 * 7
  )

# Plot expected points against yardline_100 and factor(down)
ggplot(expected_points_down_final, aes(x = yardline_100, y = expected_points, color = factor(down))) +
  geom_line() +
  labs(
    title = "Expected Points by Yardline and Down",
    x = "Yardline (from opponent's end zone)",
    y = "Expected Points",
    color = "Down"
  ) +
  theme_minimal()

# Model expected points as a function of yardline_100, down, and yards to go using a splines::bs
model_multi_yards = multinom(factor(pts_next_score) ~ splines::bs(yardline_100, df = 3) + factor(down) + splines::bs(ydstogo, df = 3), data = nfl_data)

# Convert to dataframe
expected_points_down_ydstogo_final = data.frame(yardline_100 =nfl_data$yardline_100,
                                        down = nfl_data$down,
                                        ydstogo = nfl_data$ydstogo,
                                        expected_points = predict(model_multi_yards, type = "prob"))

# Calculate expected points
expected_points_down_ydstogo_final = expected_points_down_ydstogo_final %>%
  mutate(
    expected_points = expected_points..7 * -7 + expected_points..3 * -3 + expected_points..2 * -2 + expected_points.0 * 0 + expected_points.2 * 2 + expected_points.3 * 3 + expected_points.7 * 7
  )

# Plot expected points against yardline_100, factor(down), and ydstogo coloring by ydstogo, faceting by down
ggplot(expected_points_down_ydstogo_final, aes(x = yardline_100, y = expected_points, color = factor(ydstogo))) +
  geom_line() +
  labs(
    title = "Expected Points by Yardline, Down, and Yards to Go",
    x = "Yardline (from opponent's end zone)",
    y = "Expected Points",
    color = "Yards to Go"
  ) +
  facet_wrap(~ factor(down)) +
  theme_minimal()


# Model expected points as a function of yardline_100, down, yards to go, and time remaining using a linear term for time remaining
model_multi_time_linear = multinom(factor(pts_next_score) ~ splines::bs(yardline_100, df = 3) + factor(down) + splines::bs(ydstogo, df = 3) + half_seconds_remaining, data = nfl_data)

# Same but with a spline term for half_seconds_remaining
model_multi_time_spline = multinom(factor(pts_next_score) ~ splines::bs(yardline_100, df = 3) + factor(down) + splines::bs(ydstogo, df = 3) + splines::bs(half_seconds_remaining, df = 3), data = nfl_data)

# Convert to dataframe (linear)
expected_points_down_ydstogo_time_linear = data.frame(yardline_100 = nfl_data$yardline_100,
                                        down = nfl_data$down,
                                        ydstogo = nfl_data$ydstogo,
                                        half_seconds_remaining = nfl_data$half_seconds_remaining,
                                        expected_points = predict(model_multi_time_linear, type = "prob"))

# Convert to dataframe (spline)
expected_points_down_ydstogo_time_spline = data.frame(yardline_100 = nfl_data$yardline_100,
                                        down = nfl_data$down,
                                        ydstogo = nfl_data$ydstogo,
                                        half_seconds_remaining = nfl_data$half_seconds_remaining,
                                        expected_points = predict(model_multi_time_spline, type = "prob"))

# Calculate expected points (linear)
expected_points_down_ydstogo_time_linear = expected_points_down_ydstogo_time_linear %>%
  mutate(
    expected_points = expected_points..7 * -7 + expected_points..3 * -3 + expected_points..2 * -2 + expected_points.0 * 0 + expected_points.2 * 2 + expected_points.3 * 3 + expected_points.7 * 7
  )

# Calculate expected points (spline)
expected_points_down_ydstogo_time_spline = expected_points_down_ydstogo_time_spline %>%
  mutate(
    expected_points = expected_points..7 * -7 + expected_points..3 * -3 + expected_points..2 * -2 + expected_points.0 * 0 + expected_points.2 * 2 + expected_points.3 * 3 + expected_points.7 * 7
  )

# Filter for only 1st down and 10 yards to go
expected_points_1st_down_10 = expected_points_down_ydstogo_time_linear %>%
  filter(down == 1, ydstogo == 10)

# Plot expected points against yardline_100 on 1st down and 10 yards to go colored by time remaining
ggplot(expected_points_1st_down_10, aes(x = yardline_100, y = expected_points, color = half_seconds_remaining)) +
  geom_line() +
  labs(
    title = "Expected Points by Yardline on 1st Down and 10 Yards to Go",
    x = "Yardline (from opponent's end zone)",
    y = "Expected Points",
    color = "Half Seconds Remaining"
  ) +
  theme_minimal()

# Filter for only 1st down and 10 yards to go using spline model
expected_points_1st_down_10_spline = expected_points_down_ydstogo_time_spline %>%
  filter(down == 1, ydstogo == 10)

# Plot expected points against yardline_100 on 1st down and 10 yards to go using spline model colored by time remaining
ggplot(expected_points_1st_down_10_spline, aes(x = yardline_100, y = expected_points, color = half_seconds_remaining)) +
  geom_line() +
  labs(
    title = "Expected Points by Yardline on 1st Down and 10 Yards to Go (Spline Model)",
    x = "Yardline (from opponent's end zone)",
    y = "Expected Points",
    color = "Half Seconds Remaining"
  ) +
  theme_minimal()

# M' Model

# Model adjusted for pre-game point spread
model_adj = multinom(factor(pts_next_score) ~ splines::bs(yardline_100, df = 3) + factor(down) + splines::bs(ydstogo, df = 3) + splines::bs(half_seconds_remaining, df = 3) + factor(posteam_spread), data = nfl_data)

# Convert to dataframe
expected_points_adj = data.frame(yardline_100 = nfl_data$yardline_100,
                                        down = nfl_data$down,
                                        ydstogo = nfl_data$ydstogo,
                                        half_seconds_remaining = nfl_data$half_seconds_remaining,
                                        posteam_spread = 0,
                                        expected_points = predict(model_adj, type = "prob"))

# Calculate expected points
expected_points_adj = expected_points_adj %>%
  mutate(
    expected_points = expected_points..7 * -7 + expected_points..3 * -3 + expected_points..2 * -2 + expected_points.0 * 0 + expected_points.2 * 2 + expected_points.3 * 3 + expected_points.7 * 7
  )

expected_points_adj

# Plot expected points from expected_points_adj and expected_points_down_ydstogo_time_spline
ggplot(expected_points_adj, aes(x = yardline_100, y = expected_points, color = "model_adj")) +
  geom_line() +
  geom_line(data = expected_points_down_ydstogo_time_spline, aes(x = yardline_100, y = expected_points, color = "model_multi_time_spline", linetype = "dashed")) +
  labs(
    title = "Expected Points by Yardline with Adjusted Model",
    x = "Yardline (from opponent's end zone)",
    y = "Expected Points",
    color = "Model"
  ) +
  theme_minimal()

# Conclusion: The adjusted model is slightly different to the non-adjusted model

# The true NBA 3-point make percentage of an average NBA player is not the 
# same as the percentage of all 3-point attempts made in the NBA in a given 
# year. Good 3 point shooters will take more 3 point shots, influencing the 
# overall percentage. To adjust for this, we can incorporate player 
# 3-pt quality into our model as a potential confounder

