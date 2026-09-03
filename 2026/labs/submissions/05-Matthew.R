#############
### SETUP ###
#############

# install.packages("nnet")
library(ggplot2)
library(nnet)
library(splines)
library(tidyverse)

# set seed
set.seed(6)

#######################
### EXPECTED POINTS ###
#######################

# load data
nfl_data = read_csv("../data/05_expected-points.csv.gz")
summary(nfl_data)
colnames(nfl_data)

model1=multinom(data=nfl_data, pts_next_score~yardline_100)
outcomes <- c(-7, -3, -2, 0, 2, 3, 7)

nfl_data <- nfl_data |>
  mutate(model1 = as.vector(predict(model1, type = "probs") %*% outcomes))
str(nfl_data[, c("yardline_100", "pts_next_score", "model1")])
ggplot(data = nfl_data) +
  geom_point(aes(x = yardline_100, y = pts_next_score), alpha = 0.05)+
  geom_line(aes(x= yardline_100, y = model1))

model2=multinom(data=nfl_data, pts_next_score~bs(yardline_100))

nfl_data <- nfl_data |>
  mutate(model2 = as.vector(predict(model2, type = "probs") %*% outcomes))
ggplot(data = nfl_data) +
  geom_point(aes(x = yardline_100, y = pts_next_score), alpha = 0.05)+
  geom_line(aes(x= yardline_100, y = model2))

# Make down categorical
nfl_data <- nfl_data |>
  mutate(down = factor(down))

# Fit multinomial model
model3 <- multinom(
  pts_next_score ~ (yardline_100) + down,
  data = nfl_data,
  trace = FALSE
)

# Compute expected points
nfl_data <- nfl_data |>
  mutate(
    model3 = as.vector(
      predict(model3, type = "probs") %*% outcomes
    )
  )

pred_grid <- expand_grid(
  yardline_100 = seq(1, 99, by = 1),
  down = factor(c(1, 2, 3, 4),
                levels = levels(nfl_data$down))
)

probs <- predict(model3, newdata = pred_grid, type = "probs")

pred_grid <- pred_grid |>
  mutate(
    expected_points = as.vector(probs %*% outcomes)
  )

ggplot(pred_grid,
       aes(x = yardline_100,
           y = expected_points,
           color = down)) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "Yard Line",
    y = "Expected Points",
    color = "Down",
    title = "Expected Points by Yard Line and Down"
  ) +
  theme_minimal()

model4 <- multinom(
  pts_next_score ~
    bs(yardline_100, df = 5) * down +
    bs(ydstogo, df = 4) * down,
  data = nfl_data,
  trace = FALSE
)

pred_grid <- expand_grid(
  yardline_100 = seq(1, 99, by = 1),
  down = factor(c(1, 2, 3, 4)),
  ydstogo = c(1, 3, 5, 10, 15, 20)
)

probs <- predict(model4, newdata = pred_grid, type = "probs")

pred_grid <- pred_grid |>
  mutate(
    expected_points = as.vector(probs %*% outcomes),
    ydstogo = factor(
      ydstogo,
      levels = c(1, 3, 5, 10, 15, 20),
      labels = c(
        "1 yd",
        "3 yds",
        "5 yds",
        "10 yds",
        "15 yds",
        "20 yds"
      )
    )
  )
ggplot(
  pred_grid,
  aes(
    x = yardline_100,
    y = expected_points,
    color = ydstogo
  )
) +
  geom_line(size = 1.1) +
  facet_wrap(~down, ncol = 2) +
  scale_color_viridis_d(
    name = "Yards to Go"
  ) +
  labs(
    title = "Expected Points by Field Position, Down, and Yards to Go",
    x = "Yard Line (distance from opponent goal line)",
    y = "Expected Points"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

model5_linear <- multinom(
  pts_next_score ~
    bs(yardline_100, df = 5) +
    down +
    bs(ydstogo, df = 4) +
    half_seconds_remaining,
  data = nfl_data,
  trace = FALSE
)
model5_spline <- multinom(
  pts_next_score ~
    bs(yardline_100, df = 5) +
    down +
    bs(ydstogo, df = 4) +
    bs(half_seconds_remaining, df = 5),
  data = nfl_data,
  trace = FALSE
)
times <- c(60, 300, 900, 1800)

pred_grid <- expand_grid(
  yardline_100 = seq(1, 99, by = 1),
  down = factor(1, levels = levels(nfl_data$down)),
  ydstogo = 10,
  half_seconds_remaining = times
)

probs_linear <- predict(
  model5_linear,
  newdata = pred_grid,
  type = "probs"
)

plot_linear <- pred_grid |>
  mutate(
    EP = as.vector(probs_linear %*% outcomes),
    Time = factor(
      half_seconds_remaining,
      levels = times,
      labels = c(
        "1 min",
        "5 min",
        "15 min",
        "30 min"
      )
    )
  )
ggplot(
  plot_linear,
  aes(
    x = yardline_100,
    y = EP,
    color = Time
  )
) +
  geom_line(size = 1.1) +
  scale_color_viridis_d(name = "Time Remaining") +
  labs(
    title = "Expected Points vs Yard Line (Linear Time Effect)",
    x = "Yard Line",
    y = "Expected Points"
  ) +
  theme_minimal()
probs_spline <- predict(
  model5_spline,
  newdata = pred_grid,
  type = "probs"
)

plot_spline <- pred_grid |>
  mutate(
    EP = as.vector(probs_spline %*% outcomes),
    Time = factor(
      half_seconds_remaining,
      levels = times,
      labels = c(
        "1 min",
        "5 min",
        "15 min",
        "30 min"
      )
    )
  )
ggplot(
  plot_spline,
  aes(
    x = yardline_100,
    y = EP,
    color = Time
  )
) +
  geom_line(size = 1.1) +
  scale_color_viridis_d(name = "Time Remaining") +
  labs(
    title = "Expected Points vs Yard Line (Spline Time Effect)",
    x = "Yard Line",
    y = "Expected Points"
  ) +
  theme_minimal()

# M' = best EP model + pregame spread
model_Mprime <- multinom(
  pts_next_score ~
    bs(yardline_100, df = 5) +
    down +
    bs(ydstogo, df = 4) +
    bs(half_seconds_remaining, df = 5) +
    posteam_spread,
  data = nfl_data,
  trace = FALSE
)

# Prediction grid: 1st-and-10, 15 minutes remaining, spread = 0
pred_grid <- tibble(
  yardline_100 = seq(1, 99, by = 1),
  down = factor(1, levels = levels(nfl_data$down)),
  ydstogo = 10,
  half_seconds_remaining = 900,
  posteam_spread = 0
)

# Predictions from original model M (your best model from Task 1)
ep_M <- as.vector(
  predict(
    model5_spline,
    newdata = pred_grid,
    type = "probs"
  ) %*% outcomes
)

# Predictions from spread-adjusted model M'
ep_Mprime <- as.vector(
  predict(
    model_Mprime,
    newdata = pred_grid,
    type = "probs"
  ) %*% outcomes
)

comparison_df <- pred_grid |>
  mutate(
    EP_M = ep_M,
    EP_Mprime = ep_Mprime,
    Difference = EP_Mprime - EP_M
  )

# Overlay plot
comparison_df |>
  select(yardline_100, EP_M, EP_Mprime) |>
  pivot_longer(
    cols = c(EP_M, EP_Mprime),
    names_to = "Model",
    values_to = "EP"
  ) |>
  ggplot(
    aes(
      x = yardline_100,
      y = EP,
      color = Model
    )
  ) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "EP_M" = "steelblue",
      "EP_Mprime" = "darkorange"
    ),
    labels = c(
      "M (Original)",
      "M' (Spread Adjusted)"
    )
  ) +
  labs(
    title = "Expected Points Curves",
    subtitle = "Comparison of M and M' at posteam_spread = 0",
    x = "Yard Line",
    y = "Expected Points",
    color = "Model"
  ) +
  theme_minimal()

# Difference plot
ggplot(
  comparison_df,
  aes(
    x = yardline_100,
    y = Difference
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 1.2,
    color = "firebrick"
  ) +
  labs(
    title = "Difference Between Models",
    subtitle = "M' − M at posteam_spread = 0",
    x = "Yard Line",
    y = "Difference in Expected Points"
  ) +
  theme_minimal()
