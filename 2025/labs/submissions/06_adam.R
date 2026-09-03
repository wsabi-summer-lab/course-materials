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
setwd("filepath")
nfl_data = read_csv("2025/labs/data/06_expected-points.csv.gz")


model1 = multinom(pts_next_score ~ yardline_100, nfl_data)

# make a chart of yardline to expected point
yardline_chart = nfl_data %>%
  group_by(yardline_100) %>%
  summarise(pts_next_score = mean(pts_next_score)) %>%
  ggplot(aes(x = yardline_100, y = pts_next_score)) +
  geom_point() +
  labs(
    title = "Expected Points by Yardline",
    x = "Yardline (100 - own endzone)",
    y = "Expected Points"
  )
print(yardline_chart)

# i want to use predict with my model1 on a sequence of small points to get a smooth line to put on the grah
yardline_seq <- seq(1, 100, by = 1)
predicted_points <- predict(model1, newdata = data.frame(yardline_100 = yardline_seq), type = "probs")
# Convert predicted points to a data frame for plotting
predicted_df <- data.frame(
  yardline_100 = yardline_seq,
  expected_points = predicted_points
)

# make a new column in predicted_df for expected points, which can be -7,-3,-2,0,2,3, or 7
predicted_df = predicted_df %>%
  mutate(expected_points = 
           -7*expected_points..7 -3*expected_points..3 - 2*expected_points..2 +
           2*expected_points.2 + 3*expected_points.3 + 7*expected_points.7)




# Plot the expected points with the predicted line
yardline_chart + 
  geom_line(data = predicted_df, aes(x = yardline_100, y = expected_points), color = "blue", size = 1) +
  labs(title = "Expected Points by Yardline with Model Prediction")

### spline time!!! repeat above stuff but add in a 2 in places to rename
model2 = multinom(pts_next_score ~ splines::bs(yardline_100, df = 5, degree = 3), nfl_data)
predicted_points2 <- predict(model2, newdata = data.frame(yardline_100 = yardline_seq), type = "probs")
# Convert predicted points to a data frame for plotting
predicted_df2 <- data.frame(
  yardline_100 = yardline_seq,
  expected_points = predicted_points2
)

predicted_df2 = predicted_df2 %>%
  mutate(expected_points = 
           -7*expected_points..7 -3*expected_points..3 - 2*expected_points..2 +
           2*expected_points.2 + 3*expected_points.3 + 7*expected_points.7)
yardline_chart + 
  geom_line(data = predicted_df2, aes(x = yardline_100, y = expected_points), color = "blue", size = 1) +
  labs(title = "Expected Points by Yardline with Model Prediction")

# now we add in down
model3 = multinom(pts_next_score ~ splines::bs(yardline_100, df = 5, degree = 3) + factor(down), nfl_data)

#Plot expected points (y axis) vs yard line (x axis) and color your points by down.
yardline_down_grid <- expand_grid(
  yardline_100 = yardline_seq,
  down = factor(1:4)
)

predicted_df3 <- predict(model3, newdata = yardline_down_grid, type = "probs")
predicted_df3 <- cbind(yardline_down_grid, as.data.frame(predicted_df3))

predicted_df3 = predicted_df3 %>%
  mutate(expected_points = 
           -7*`-7` -3*`-3` - 2*`-2` +
           2*`2` + 3*`3` + 7*`7`)

#i want a graph that shows my expected points by yardline and down with down color coded
yardline_chart + 
  geom_line(data = predicted_df3, aes(x = yardline_100, y = expected_points, color = down), size = 1) +
  labs(title = "Expected Points by Yardline with Model Prediction and Down") +
  scale_color_manual(values = c("darkslategrey", "blue", "darkseagreen", "blueviolet"), name = "Down") +
  theme_minimal()

### add in yds to go
model4 = multinom(pts_next_score ~ splines::bs(yardline_100, df = 5, degree = 3) + factor(down) + splines::bs(ydstogo, df=5, degree=3), nfl_data)

## yards to go
yds_ydstogo_down_grid <- data.frame(
  yardline_100 = nfl_data$yardline_100,
  down = factor(nfl_data$down),
  ydstogo = nfl_data$ydstogo
)

predicted_df4 <- predict(model4, newdata = yds_ydstogo_down_grid, type = "probs")
predicted_df4 <- cbind(yds_ydstogo_down_grid, as.data.frame(predicted_df4))

predicted_df4 = predicted_df4 %>%
  mutate(expected_points = 
           -7*`-7` -3*`-3` - 2*`-2` +
           2*`2` + 3*`3` + 7*`7`)
#Plot expected points (y axis) vs yard line (x axis), coloring by yards to go and faceting by down.
yardline_chart + 
  geom_line(data = predicted_df4, aes(x = yardline_100, y = expected_points, color = ydstogo), size = 1) +
  labs(title = "Expected Points by Yardline with Model Prediction, Down, and Yards to Go") +
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "Yards to Go") +
  facet_wrap(~ down) +
  theme_minimal()

###time remaining in half
model5 = multinom(pts_next_score ~ splines::bs(yardline_100, df = 5, degree = 3) + 
                    factor(down) + splines::bs(ydstogo, df=5, degree=3) + 
                    half_seconds_remaining, nfl_data)
model5spline = multinom(pts_next_score ~ splines::bs(yardline_100, df = 5, degree = 3) + 
                    factor(down) + splines::bs(ydstogo, df=5, degree=3) + 
                    splines::bs(half_seconds_remaining, df = 5, degree = 3), nfl_data)

# create a grid to predict expected points only on 1st down with 10 yards to go
yardline_half_grid <- data.frame(
  yardline_100 = nfl_data$yardline_100,
  down = factor(1),
  ydstogo = 10,
  half_seconds_remaining = nfl_data$half_seconds_remaining
)
predicted_df5 <- predict(model5, newdata = yardline_half_grid, type = "probs")
predicted_df5 <- cbind(yardline_half_grid, as.data.frame(predicted_df5))
predicted_df5 = predicted_df5 %>%
  mutate(expected_points = 
           -7*`-7` -3*`-3` - 2*`-2` +
           2*`2` + 3*`3` + 7*`7`)
#Plot expected points (y axis) vs yard line (x axis) on 1st down and 10 yards to go, coloring by time remaining.
yardline_chart + 
  geom_line(data = predicted_df5, aes(x = yardline_100, y = expected_points, color = half_seconds_remaining), size = 1) +
  labs(title = "Expected Points by Yardline with Model Prediction, Down, Yards to Go, and Time Remaining") +
  scale_color_gradient(low = "lightgreen", high = "darkgreen", name = "Time Remaining (seconds)") +
  theme_minimal()

### redo with spline model
predicted_df5spline <- predict(model5spline, newdata = yardline_half_grid, type = "probs")
predicted_df5spline <- cbind(yardline_half_grid, as.data.frame(predicted_df5spline))
predicted_df5spline = predicted_df5spline %>%
  mutate(expected_points = 
           -7*`-7` -3*`-3` - 2*`-2` +
           2*`2` + 3*`3` + 7*`7`)

yardline_chart + 
  geom_line(data = predicted_df5spline, aes(x = yardline_100, y = expected_points, color = half_seconds_remaining), size = 1) +
  labs(title = "Expected Points by Yardline with Model Prediction, Down, Yards to Go, and Time Remaining") +
  scale_color_gradient(low = "lightgreen", high = "darkgreen", name = "Time Remaining (seconds)") +
  theme_minimal()


model6 = multinom(pts_next_score ~ splines::bs(yardline_100, df = 5, degree = 3) + 
                          factor(down) + splines::bs(ydstogo, df=5, degree=3) + 
                          splines::bs(half_seconds_remaining, df = 5, degree = 3) +
                    posteam_spread, nfl_data)


yardline_half_spread_grid <- data.frame(
  yardline_100 = nfl_data$yardline_100,
  down = factor(1),
  ydstogo = 10,
  half_seconds_remaining = nfl_data$half_seconds_remaining,
  posteam_spread = 0
)
predicted_df6 <- predict(model6, newdata = yardline_half_spread_grid, type = "probs")
predicted_df6 <- cbind(yardline_half_grid, as.data.frame(predicted_df6))
predicted_df6 = predicted_df6 %>%
  mutate(expected_points = 
           -7*`-7` -3*`-3` - 2*`-2` +
           2*`2` + 3*`3` + 7*`7`)


# make 2 graphs next to each other comparing the predicted_df5spline and predicted_df6
library(ggplot2)
library(gridExtra)
yardline_chart_spline <- yardline_chart + 
  geom_line(data = predicted_df5spline, aes(x = yardline_100, y = expected_points, color = half_seconds_remaining), size = 1) +
  labs(title = "Expected Points by Yardline with Spline Model Prediction, Down, Yards to Go, and Time Remaining") +
  scale_color_gradient(low = "lightgreen", high = "darkgreen", name = "Time Remaining (seconds)") +
  theme_minimal()
yardline_chart_spread <- yardline_chart +
  geom_line(data = predicted_df6, aes(x = yardline_100, y = expected_points, color = half_seconds_remaining), size = 1) +
  labs(title = "spread = 0") +
  scale_color_gradient(low = "lightgreen", high = "darkgreen", name = "Time Remaining (seconds)") +
  theme_minimal()
grid.arrange(yardline_chart_spline, yardline_chart_spread, ncol = 2)

# these are not the same, but slightly different

#DISCUSSION
#these are not the same since better players take more 3 pointers, 
#so the actual avg will be higher than the 'true' avg







