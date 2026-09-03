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

names(nfl_data)


#LOGISTIC
logLR = multinom(factor(pts_next_score) ~ yardline_100, data = nfl_data)

yardlines = tibble(ydl = seq(1, 99, by = 1))

predictions = as_tibble(predict(logLR, yardlines, type = "probs"))

average_points = nfl_data %>% 
  group_by(yardline_100) %>% 
  summarise(avg_pts = mean(pts_next_score, na.rm = TRUE)) %>% 
  pull()

expected_points = predictions %>% 
  mutate(EP = -7 * `-7` -3 * `-3` - 2 * `-2` + 2 * `2` + 3 * `3` + 7 * `7`) %>% 
  pull()

plot_data = yardlines %>% 
  mutate(avg_points = average_points, exp_points = expected_points)

# Plotting the expected points based on yard line
plot = plot_data %>%
  ggplot(aes(x = ydl)) +
  geom_point(aes(y = avg_points)) +
  geom_line(aes(y = exp_points))
plot

#SPLINE
spline_logLR = multinom(factor(pts_next_score) ~ splines::bs(yardline_100, df = 4), data = nfl_data)

predictions2 = as_tibble(
  predict(spline_logLR, yardlines, type = "probs")
)

expected_points = predictions %>%
  mutate(
    EP = -7 * `-7` - 3 * `-3` - 2 * `-2` + 2 * `2`  + 3 * `3`  + 7 * `7`
  ) %>%
  pull()

plot_data = yardlines %>%
  mutate(
    avg_points = average_points,
    exp_points = expected_points
  )

plot = plot_data %>%
  ggplot(aes(x = ydl)) +
  geom_point(aes(y = avg_points)) +
  geom_line(aes(y = exp_points)) +
  labs(
    title = "Spline Multinomial Expected Points vs. Yard Line",
    x     = "Yard Line (1–99)",
    y     = "Expected Points"
  ) +
  theme_minimal()
plot


#NEXT VARIABLE
logLR_down = multinom(
  factor(pts_next_score) ~ yardline_100 + factor(down),
  data = nfl_data
)

down_levels   = levels(factor(nfl_data$down))
ydls           = seq(1, 99, by = 1)
yardlines_down = tibble(
  yardline_100 = rep(ydls, times   = length(down_levels)),
  down         = factor(
    rep(down_levels, each = length(ydls)),
    levels = down_levels
  )
)

predictions3 = as_tibble(
  predict(logLR_down, newdata = yardlines_down, type = "probs")
)


expected_points_down = predictions3 %>%
  mutate(
    EP = -7 * `-7` - 3 * `-3` - 2 * `-2`
    + 2 * `2`   + 3 * `3`   + 7 * `7`
  ) %>%
  pull()


average_points_down = nfl_data %>%
  group_by(yardline_100, down) %>%
  summarise(avg_pts = mean(pts_next_score, na.rm = TRUE), .groups = "drop") %>%
  pull()

down_plot_data = yardlines_down %>%
  mutate(
    avg_points = average_points_down,
    exp_points = expected_points_down
  )

plot = down_plot_data %>%
  ggplot(aes(x = yardline_100, color = down)) +
  geom_point(aes(y = avg_points)) +
  geom_line(aes(y = exp_points)) +
  labs(
    title = "Expected Points by Yard Line and Down",
    x     = "Yard Line (1–99)",
    y     = "Expected Points",
    color = "Down"
  ) +
  theme_minimal()
plot

#VARIABLE 4
logLR_down = multinom(factor(pts_next_score) ~ yardline_100 + factor(down),
  data = nfl_data)

down_levels   = levels(factor(nfl_data$down))
ydls           = seq(1, 99, by = 1)
yardlines_down = tibble(
  yardline_100 = rep(ydls, times   = length(down_levels)),
  down         = factor(
    rep(down_levels, each = length(ydls)),
    levels = down_levels
  )
)

predictions3 = as_tibble(
  predict(logLR_down, newdata = yardlines_down, type = "probs"))

expected_points_down = predictions3 %>%
  mutate(
    EP = -7 * `-7` - 3 * `-3` - 2 * `-2`
    + 2 * `2`   + 3 * `3`   + 7 * `7`
  ) %>%
  pull()

average_points_down = nfl_data %>%
  group_by(yardline_100, down) %>%
  summarise(avg_pts = mean(pts_next_score, na.rm = TRUE), .groups = "drop") %>%
  pull()

down_plot_data = yardlines_down %>%
  mutate(
    avg_points = average_points_down,
    exp_points = expected_points_down)

plot = down_plot_data %>%
  ggplot(aes(x = yardline_100, color = down)) +
  geom_point(aes(y = avg_points)) +
  geom_line(aes(y = exp_points)) +
  labs(
    title = "Expected Points by Yard Line and Down",
    x     = "Yard Line (1–99)",
    y     = "Expected Points",
    color = "Down"
  ) +
  theme_minimal()
print(plot)

#VARIABLE 5
logLR_time_lin = multinom(
  factor(pts_next_score) ~ yardline_100 + factor(down) + ydstogo + half_seconds_remaining,
  data = nfl_data
)
logLR_time_spline = multinom(
  factor(pts_next_score) ~ yardline_100 + factor(down) + ydstogo + splines::bs(half_seconds_remaining, df = 4),
  data = nfl_data
)


ydls      = seq(1, 99, by = 1)
time_vals = seq(0, max(nfl_data$half_seconds_remaining, na.rm = TRUE), length.out = 10)
gr_grid   = tibble(
  yardline_100           = rep(ydls, times = length(time_vals)),
  down                   = factor(rep(1, length(ydls) * length(time_vals)), levels = levels(factor(nfl_data$down))),
  ydstogo                = rep(10, length(ydls) * length(time_vals)),
  half_seconds_remaining = rep(time_vals, each = length(ydls))
)

pred_lin = predict(logLR_time_lin, newdata = gr_grid, type = "probs")
pred_spl = predict(logLR_time_spline, newdata = gr_grid, type = "probs")

gr_EP_lin = as.vector(pred_lin %*% as.numeric(levels(factor(nfl_data$pts_next_score))))
gr_EP_spl = as.vector(pred_spl %*% as.numeric(levels(factor(nfl_data$pts_next_score))))


gr_plot = gr_grid %>%
  mutate(
    EP_lin = gr_EP_lin,
    EP_spl = gr_EP_spl
  )

plot_lin = ggplot(gr_plot, aes(x = yardline_100, y = EP_lin, color = half_seconds_remaining)) +
  geom_line(size = 1) +
  labs(
    title = "EP vs Yard Line (1st & 10) – Linear Time", 
    x     = "Yard Line", 
    y     = "Expected Points"
  ) +
  theme_minimal()
print(plot_lin)

plot_spl = ggplot(gr_plot, aes(x = yardline_100, y = EP_spl, color = half_seconds_remaining)) +
  geom_line(size = 1) +
  labs(
    title = "EP vs Yard Line (1st & 10) – Spline Time", 
    x     = "Yard Line", 
    y     = "Expected Points"
  ) +
  theme_minimal()
print(plot_spl)



#TASK 2

logLR_base = multinom(factor(pts_next_score) ~ yardline_100, data = nfl_data)

logLR_spread = multinom(factor(pts_next_score) ~ yardline_100 + posteam_spread,data = nfl_data)

task2_grid = tibble(yardline_100  = seq(1, 99, by = 1),posteam_spread  = 0)

pred_base = as_tibble(predict(logLR_base, task2_grid, type = "probs"))
pred_spread = as_tibble(predict(logLR_spread, task2_grid, type = "probs"))

ep_base   = pred_base %>%   mutate(EP = -7*`-7` -3*`-3` -2*`-2` +2*`2` +3*`3` +7*`7`) %>% pull()
ep_spread = pred_spread %>% mutate(EP = -7*`-7` -3*`-3` -2*`-2` +2*`2` +3*`3` +7*`7`) %>% pull()

task2_plot = tibble(
  ydl      = seq(1, 99, by = 1),
  EP_base  = ep_base,
  EP_sp0   = ep_spread
) %>%
  ggplot(aes(x = ydl)) +
  geom_line(aes(y = EP_base), color = "steelblue") +
  geom_line(aes(y = EP_sp0),  color = "maroon", linetype = "dashed") +
  labs(
    title = "Task 2: EP Model M vs M' (Spread=0)",
    x     = "Yard Line",
    y     = "Expected Points"
  ) +
  theme_minimal()

task2_plot

#yay there are differences!
head(ep_base)
head(ep_spread)

#DISCUSSION:
#League wide 3pt percentage is weighted by the players who boost the percentage the most.
#The "true" 3pt percentage is unweighted. 
#I would expect the league wide one to be higher because it accounts for player strength.