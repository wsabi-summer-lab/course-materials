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

model1 = multinom(factor(pts_next_score) ~ yardline_100, data = nfl_data)
summary(model1)
nfl_data$pred1 = predict(model1, type = "prob") %*% c(-7, -3, -2, 0, 2, 3, 7)
ggplot(nfl_data, aes(x = yardline_100, y = pred1)) +
  geom_line() +
  labs(title = "Expected Points Based on Yard Line",
       x = "Yard Line",
       y = "Expected Points") +
  theme_minimal()

# Add a spline term for yard line
model2 = multinom(factor(pts_next_score) ~ bs(yardline_100, df = 5), data = nfl_data)
summary(model2)
nfl_data$pred2 = predict(model2, type = "prob") %*% c(-7, -3, -2, 0, 2, 3, 7)
ggplot(nfl_data, aes(x = yardline_100, y = pred2)) +
  geom_line() +
  labs(title = "Expected Points Based on Yard Line with Spline",
       x = "Yard Line",
       y = "Expected Points") +
  theme_minimal()

# Add downs
model3 = multinom(factor(pts_next_score) ~ bs(yardline_100, df = 5) + factor(down), data = nfl_data)
summary(model3)
nfl_data$pred3 = predict(model3, type = "prob") %*% c(-7, -3, -2, 0, 2, 3, 7)
ggplot(nfl_data, aes(x = yardline_100, y = pred3, color = factor(down))) +
  geom_line() +
  labs(title = "Expected Points Based on Yard Line and Downs",
       x = "Yard Line",
       y = "Expected Points",
       color = "Down") +
  theme_minimal()

# Add yard to go
model4 = multinom(factor(pts_next_score) ~ bs(yardline_100, df = 5) + factor(down) + factor(ydstogo), data = nfl_data)
summary(model4)
nfl_data$pred4 = predict(model4, type = "prob") %*% c(-7, -3, -2, 0, 2, 3, 7)
# Plot color by yards to go and facet by downs
ggplot(nfl_data, aes(x = yardline_100, y = pred4, color = factor(ydstogo))) +
  geom_line() +
  facet_wrap(~ down) +
  labs(title = "Expected Points Based on Yard Line, Downs, and Yards to Go",
       x = "Yard Line",
       y = "Expected Points",
       color = "Yards to Go") +
  theme_minimal()

# Add time remaining in the half (half_seconds_remaining)
model5 = multinom(factor(pts_next_score) ~ bs(yardline_100, df = 5) + factor(down) + factor(ydstogo) + half_seconds_remaining, data = nfl_data)
nfl_data$pred5 = predict(model5, type = "prob") %*% c(-7, -3, -2, 0, 2, 3, 7)
# Plot 1 & 10 coloring by time remaining
ggplot(nfl_data %>% filter(down == 1 & ydstogo == 10), aes(x = yardline_100, y = pred5, color = half_seconds_remaining)) +
  geom_line() +
  labs(title = "Expected Points for 1st & 10 Based on Yard Line and Time Remaining",
       x = "Yard Line",
       y = "Expected Points",
       color = "Time Remaining (seconds)") +
  theme_minimal()

# Add spline to time remaining
model6 = multinom(factor(pts_next_score) ~ bs(yardline_100, df = 5) + factor(down) + factor(ydstogo) + bs(half_seconds_remaining, df = 5), data = nfl_data)
nfl_data$pred6 = predict(model6, type = "prob") %*% c(-7, -3, -2, 0, 2, 3, 7)
ggplot(nfl_data %>% filter(down == 1 & ydstogo == 10), aes(x = yardline_100, y = pred6, color = half_seconds_remaining)) +
  geom_line() +
  labs(title = "Expected Points for 1st & 10 Based on Yard Line and Time Remaining with Spline",
       x = "Yard Line",
       y = "Expected Points",
       color = "Time Remaining (seconds)") +
  theme_minimal()

# Add posteam_spread
model7 = multinom(factor(pts_next_score) ~ bs(yardline_100, df = 5) + factor(down) + factor(ydstogo) + bs(half_seconds_remaining, df = 5) + posteam_spread, data = nfl_data)
nfl_data$pred7 = nfl_data %>% mutate(posteam_spread = 0) %>% 
  predict(model7, type = "prob", newdata = .) %*% c(-7, -3, -2, 0, 2, 3, 7)
ggplot(nfl_data, aes(x = yardline_100)) +
  geom_line(aes(y = pred7), color = 'blue') +
  geom_line(aes(y = pred6), color = 'red') +
  labs(title = "Expected Points Based on Yard Line with/without Spread",
       x = "Yard Line",
       y = "Expected Points") +
  scale_color_manual(values = c("blue" = "Spread", "red" = "No Spread")) +
  guides(color = guide_legend(title = "Model")) +
  theme_minimal()

