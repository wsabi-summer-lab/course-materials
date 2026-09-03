#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "splines", "tidyverse"))
library(ggplot2)
library(nnet)
library(splines)
library(tidyverse)

# set seed
set.seed(6)

##############
### PART 1 ###
##############

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
nfl_data = read_csv("../data/05_expected-points.csv.gz")

# Task 1:
# - Fit a multinomial logistic regression for pts_next_score using yardline_100 only
# - Start with expected points modeled as a purely linear function of yardline_100
# - Convert fitted class probabilities into expected points
# - Plot estimated expected points against yardline_100
# - State briefly what implausible pattern this linear-yardline model produces
mlr <- multinom(pts_next_score~ yardline_100, data = nfl_data)

yard_grid <- data.frame(yardline_100 = 1:99)

probs <- predict(mlr, newdata = yard_grid, type = "probs")

point_values <- c("-7" = -7,
                  "-3" = -3,
                  "-2" = -2,
                  "0"  = 0,
                  "2"  = 2,
                  "3"  = 3,
                  "7"  = 7)

yard_grid$expected_points <- as.vector(
  probs %*% point_values[colnames(probs)]
)


ggplot(yard_grid, aes(x = yardline_100, y = expected_points)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Yardline 100",
    y = "Expected Points"
  )

#it makes sense in some places but seems to go into negative expected value much earlier than it should. 

# Task 2:
# - Replace the linear yardline term with a spline using splines::bs(...)
# - Refit the multinomial model and recompute expected points
# - Plot the revised expected-points curve against yardline_100
# - Briefly explain why the spline improves on the linear-yardline model

mlr1 <- multinom(pts_next_score~ splines::bs(yardline_100, degree = 3, knots = seq(0,100,10)), data = nfl_data)

probs1 <- predict(mlr1, newdata = yard_grid, type = "probs")

yard_grid$expected_points1 <- as.vector(
  probs1 %*% point_values[colnames(probs1)]
)


ggplot(yard_grid, aes(x = yardline_100, y = expected_points1)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Yardline 100",
    y = "Expected Points"
  )

#Near goal line probabilities should curve more than the general layout of a field, which is what this model does. 

# Task 3:
# - Extend the model to include yardline_100 and down
# - Decide whether down should be encoded as numeric or categorical
# - Plot expected points against yardline_100 and color by down
# - Briefly explain how down should be encoded and why
nfl_data$down <- factor(nfl_data$down)

mlr2 <- multinom(pts_next_score~ splines::bs(yardline_100, degree = 3, knots = seq(0,100,10)) + down, data = nfl_data)


yard_grid1 <- expand.grid(
  yardline_100 = 1:99,
  down = levels(nfl_data$down)
)

probs2 <- predict(mlr2, newdata = yard_grid1, type = "probs")

yard_grid1$expected_points2 <- as.vector(
  probs2 %*% point_values[colnames(probs2)]
)


ggplot(yard_grid1, aes(x = yardline_100, y = expected_points2, color = down)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Yardline 100",
    y = "Expected Points"
  )
#Down should be encoded as categorical because the relationship between downs is likely nonlinear


# Task 4:
# - Extend the model to include yardline_100, down, and ydstogo
# - Consider whether ydstogo should enter linearly, on a log scale, or through a spline
# - Plot expected points against yardline_100
# - Color by ydstogo and facet by down
# - Briefly describe how yards to go changes the expected-points surface across downs
#Use log for yards to go, since yards to go matters differently depending on field position. In turn that affects decision making and expected points. 

mlr3 <- multinom(
  pts_next_score ~ 
    splines::bs(yardline_100, degree = 3, knots = seq(10, 90, 10)) +
    down +
    log1p(ydstogo),
  data = nfl_data
)

yard_grid3 <- expand.grid(
  yardline_100 = 1:99,
  down = levels(nfl_data$down),
  ydstogo = sort(unique(nfl_data$ydstogo))
)

probs3 <- predict(mlr3, newdata = yard_grid3, type = "probs")

yard_grid3$expected_points <- as.vector(
  probs3 %*% point_values[colnames(probs3)]
)


ggplot(yard_grid3,
       aes(x = yardline_100,
           y = expected_points,
           color = factor(ydstogo))) +
  geom_line() +
  facet_wrap(~ down) +
  labs(
    x = "Yardline 100",
    y = "Expected Points",
    color = "Yards to go"
  )

# Task 5:
# - Add half_seconds_remaining to your model
# - Try both:
#   * a linear term in half_seconds_remaining
#   * a spline term in half_seconds_remaining
# - Restrict attention to 1st-and-10 when building the comparison plot
# - Plot expected points against yardline_100 and color by time remaining
# - Make one plot for the linear-time model and one for the spline-time model
# - Briefly compare the two time specifications

mlr4 <- multinom(
  pts_next_score ~ 
    bs(yardline_100, degree = 3, knots = seq(10, 90, 10)) +
    down +
    log1p(ydstogo) +
    half_seconds_remaining,
  data = nfl_data
)


mlr5 <- multinom(
  pts_next_score ~ 
    bs(yardline_100, degree = 3, knots = seq(10, 90, 10)) +
    down +
    log1p(ydstogo) +
    bs(half_seconds_remaining, degree = 3, knots = c(300, 600, 900, 1200, 1500)),
  data = nfl_data
)

time_values <- c(120, 300, 600, 900, 1200, 1500, 1800)

yard_grid_time <- expand.grid(
  yardline_100 = 1:99,
  down = factor("1", levels = levels(nfl_data$down)),
  ydstogo = 10,
  half_seconds_remaining = time_values
)


probs4 <- predict(mlr4, newdata = yard_grid_time, type = "probs")

yard_grid_time$expected_points_linear <- as.vector(
  probs4 %*% point_values[colnames(probs4)]
)


probs5 <- predict(mlr5, newdata = yard_grid_time, type = "probs")

yard_grid_time$expected_points_spline <- as.vector(
  probs5 %*% point_values[colnames(probs5)]
)

yard_grid_time$time_remaining <- factor(
  yard_grid_time$half_seconds_remaining,
  levels = time_values,
  labels = paste(time_values, "sec")
)

ggplot(yard_grid_time,
       aes(x = yardline_100,
           y = expected_points_linear,
           color = time_remaining)) +
  geom_line() +
  labs(
    title = "Expected Points: Linear Time Model",
    x = "Yardline 100",
    y = "Expected Points",
    color = "Time remaining"
  )


ggplot(yard_grid_time,
       aes(x = yardline_100,
           y = expected_points_spline,
           color = time_remaining)) +
  geom_line() +
  labs(
    title = "Expected Points: Spline Time Model",
    x = "Yardline 100",
    y = "Expected Points",
    color = "Time remaining"
  )

#I think the linear time model modeling both ends of the field. The spline one also extrapolates too much with negative values. 

##############
### PART 2 ###
##############

# Task 1:
# - Let M be your preferred expected-points model from Part 1
# - Fit an adjusted model M_prime that also includes posteam_spread
# - A linear spread term is a reasonable starting point
M<- mlr5

M_prime <- multinom(
  pts_next_score ~ 
    bs(yardline_100, degree = 3, knots = seq(10, 90, 10)) +
    down +
    log1p(ydstogo) +
    bs(half_seconds_remaining, degree = 3, knots = c(300, 600, 900, 1200, 1500))  + posteam_spread,
  data = nfl_data
)
# Task 2:
# - Compare expected points from M_prime at posteam_spread = 0 to expected points from M
# - Overlay the two curves as a function of yardline_100
# - Plot the difference M_prime(spread = 0) - M as a function of yardline_100
# - Briefly explain why conditioning on spread changes the target estimand

yard_grid_compare <- data.frame(
  yardline_100 = 1:99,
  down = factor("1", levels = levels(nfl_data$down)),
  ydstogo = 10,
  half_seconds_remaining = median(nfl_data$half_seconds_remaining),
  posteam_spread = 0
)

probs_M <- predict(
  M,
  newdata = yard_grid_compare[, c(
    "yardline_100",
    "down",
    "ydstogo",
    "half_seconds_remaining"
  )],
  type = "probs"
)

EP_M <- as.vector(
  probs_M %*% point_values[colnames(probs_M)]
)

probs_Mprime <- predict(
  M_prime,
  newdata = yard_grid_compare,
  type = "probs"
)

EP_Mprime <- as.vector(
  probs_Mprime %*% point_values[colnames(probs_Mprime)]
)


plot_df <- data.frame(
  yardline_100 = rep(1:99, 2),
  EP = c(EP_M, EP_Mprime),
  Model = rep(c("M", "M_prime (spread = 0)"),
              each = 99)
)

ggplot(plot_df,
       aes(x = yardline_100,
           y = EP,
           color = Model)) +
  geom_line(size = 1) +
  labs(
    x = "yardline_100",
    y = "Expected Points",
    title = "Expected Points: M vs M_prime"
  )

diff_df <- data.frame(
  yardline_100 = 1:99,
  Difference = EP_Mprime - EP_M
)

ggplot(diff_df,
       aes(x = yardline_100,
           y = Difference)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  labs(
    x = "yardline_100",
    y = "M_prime - M",
    title = "Effect of Adding posteam_spread"
  )
#Conditioning on spread means that team quality is now introduced into the equation. Often selection bias occurs because good teams are given expected point values at neutral rates. This tries to correct that. 
# Discussion:
# - Are these the same or different, and why?
#   * the percentage of all 3-point attempts made in the NBA this year
#   * the true 3-point make percentage of an average NBA player
# - If they differ, state which you expect to be higher
# - Briefly explain how you could adjust for this selection-bias problem
#The percent will probably be higher as high volume shooters will drive it up. The solution would be to adjust for player quality. 
