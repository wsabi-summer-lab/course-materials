#############
### SETUP ###
#############
rm(list=ls())

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
names(nfl_data)

## 6.1.4
## part 1: y = pts_next_score; x = yardline_100
model1 <- multinom(factor(pts_next_score) ~ yardline_100, data = nfl_data)
summary(model1)

# get expected points from model1
expected_points <- data.frame(yardline_100 = nfl_data$yardline_100,
                             expected_points = predict(model1, type = "probs"))
# calculate expected points as a weighted average
names(expected_points)
expected_points <- expected_points %>%
  mutate(expected_points = (-7)*expected_points..7 + (-3)*expected_points..3 + (-2)*expected_points..2+(2)*expected_points.2+(3)*expected_points.3+(7)*expected_points.7)

# graph expected points
ggplot(expected_points, aes(x = yardline_100, y = expected_points)) +
  geom_line() +
  labs(title = "Expected Points by Yardline",
       x = "Yardline (100 - own end zone)",
       y = "Expected Points") +
  theme_minimal()

## part 2: spline
model2 <- multinom(factor(pts_next_score) ~ splines::bs(yardline_100, degree = 3, df = 5), data = nfl_data)

expected_points <- data.frame(yardline_100 = nfl_data$yardline_100,
                              expected_points = predict(model2, type = "probs"))
# calculate expected points as a weighted average
names(expected_points)
expected_points <- expected_points %>%
  mutate(expected_points = (-7)*expected_points..7 + (-3)*expected_points..3 + (-2)*expected_points..2+(2)*expected_points.2+(3)*expected_points.3+(7)*expected_points.7)

# graph expected points
ggplot(expected_points, aes(x = yardline_100, y = expected_points)) +
  geom_line() +
  labs(title = "Expected Points by Yardline using Spline model",
       x = "Yardline (100 - own end zone)",
       y = "Expected Points") +
  theme_minimal()

## part 3
model3 <- multinom(factor(pts_next_score) ~ yardline_100 + factor(down), data = nfl_data)
summary(model3)

expected_points <- data.frame(yardline_100 = nfl_data$yardline_100,
                              down = nfl_data$down,
                              expected_points = predict(model3, type = "probs"))
# calculate expected points as a weighted average
expected_points <- expected_points %>%
  mutate(expected_points = (-7)*expected_points..7 + (-3)*expected_points..3 + (-2)*expected_points..2+(2)*expected_points.2+(3)*expected_points.3+(7)*expected_points.7)
# graph expected points
ggplot(expected_points, aes(x = yardline_100, y = expected_points, color = factor(down))) +
  geom_line() +
  labs(title = "Expected Points by Yardline and Down",
       x = "Yardline (100 - own end zone)",
       y = "Expected Points",
       color = "Down") +
  theme_minimal()

## part 4
model4 <- multinom(factor(pts_next_score) ~ splines::bs(yardline_100, degree = 3, df = 5) + factor(down) + splines::bs(ydstogo, degree = 3, df = 5), data = nfl_data)

expected_points <- data.frame(yardline_100 = nfl_data$yardline_100,
                              down = nfl_data$down,
                              ydstogo = nfl_data$ydstogo,
                              expected_points = predict(model4, type = "probs"))
# calculate expected points as a weighted average
expected_points <- expected_points %>%
  mutate(expected_points = (-7)*expected_points..7 + (-3)*expected_points..3 + (-2)*expected_points..2+(2)*expected_points.2+(3)*expected_points.3+(7)*expected_points.7)
# graph expected points vs. yardline_100, color by down, facet by ydstogo
ggplot(expected_points, aes(x = yardline_100, y = expected_points, color = factor(ydstogo))) +
  geom_line() +
  labs(title = "Expected Points by Yardline, Down, and Ydstogo",
       x = "Yardline (100 - own end zone)",
       y = "Expected Points",
       color = "ydstogo") +
  facet_wrap(~down) +
  theme_minimal()

## part 5
names(nfl_data)
model5 <- multinom(factor(pts_next_score) ~ splines::bs(yardline_100, degree = 3, df = 5) 
                   + factor(down) + splines::bs(ydstogo, degree = 3, df = 5)
                   + half_seconds_remaining + splines::bs(half_seconds_remaining, degree = 3, df = 5), data = nfl_data)

expected_points <- data.frame(yardline_100 = nfl_data$yardline_100,
                              down = nfl_data$down,
                              ydstogo = nfl_data$ydstogo,
                              half_seconds_remaining = nfl_data$half_seconds_remaining,
                              expected_points = predict(model5, type = "probs"))
# calculate expected points as a weighted average
expected_points <- expected_points %>%
  mutate(expected_points = (-7)*expected_points..7 + (-3)*expected_points..3 + (-2)*expected_points..2+(2)*expected_points.2+(3)*expected_points.3+(7)*expected_points.7)
# Plot expected points (y axis) vs yard line (x axis) on 1st down and 10 yards to go, coloring by time remaining
ggplot(expected_points %>% filter(down == 1, ydstogo == 10), aes(x = yardline_100, y = expected_points, color = half_seconds_remaining)) +
  geom_line() +
  labs(title = "Expected Points by Yardline on 1st Down and 10 Yards to Go",
       x = "Yardline (100 - own end zone)",
       y = "Expected Points",
       color = "Half Seconds Remaining") +
  theme_minimal()

#######################
### TEAM QUALITY ###
#######################

names(nfl_data)
model6 <- multinom(factor(pts_next_score) ~ splines::bs(yardline_100, degree = 3, df = 5) 
                   + factor(down) + splines::bs(ydstogo, degree = 3, df = 5)
                   + half_seconds_remaining + splines::bs(half_seconds_remaining, degree = 3, df = 5) + posteam_spread, data = nfl_data)

expected_points_model6 <- data.frame(yardline_100 = nfl_data$yardline_100,
                                     down = nfl_data$down,
                                     ydstogo = nfl_data$ydstogo,
                                     half_seconds_remaining = nfl_data$half_seconds_remaining,
                                     posteam_spread = 0,
                                     expected_points = predict(model6, type = "probs"))
# calculate expected points as a weighted average
expected_points_model6 <- expected_points_model6 %>%
  mutate(expected_points = (-7)*expected_points..7 + (-3)*expected_points..3 + (-2)*expected_points..2+(2)*expected_points.2+(3)*expected_points.3+(7)*expected_points.7)
expected_points$expected_points_new <- expected_points_model6$expected_points
# Plot the expected points from model6 with point spread = 0 vs the expected points from model5. Are these
# functions the same? Why or why not? Justify your answer with a visualization.
ggplot(expected_points, aes(x = yardline_100, y = expected_points, color = "Model 5")) +
  geom_line() +
  geom_line(data = expected_points_model6, aes(x = yardline_100, y = expected_points, color = "Model 6")) +
  labs(title = "Expected Points Comparison: Model 5 vs Model 6",
       x = "Yardline (100 - own end zone)",
       y = "Expected Points",
       color = "Model") +
  theme_minimal()
# The functions are not the same because Model 6 includes the team quality (posteam_spread) as a predictor:
# coefficients for model 6 are different from coefficients for model 5, so even though posteam_spread = 0 in model 6,
# our predictions are still different when using the two diff. models

#######################
### SELECTION BIAS ###
#######################

# The percentage of all 3-point attempts made in the NBA this year favors people who takes 3-point shots more often (they have heavier weight due to selection bias).
# The ”true” 3-point make percentage of an average NBA player takes the simple average of each player's 3-point average.
# So, no player is weighted more heavily than others in the "true" 3-point make percentage.
# therefore, we would expect the first one (percentage of all 3-point attempts made in the NBA) to be higher than the second.