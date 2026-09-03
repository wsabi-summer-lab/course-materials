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

#######################
### EXPECTED POINTS ###
#######################

# load data
nfl_data = read_csv("../data/05_expected-points.csv.gz")

nfl_data <- nfl_data %>%
  mutate(pts_next_score = relevel(factor(pts_next_score), ref = "0"))

ep_linear <- multinom(pts_next_score ~ yardline_100, data = nfl_data, trace = FALSE)
grid <- tibble(yardline_100 = 1:99)
probs <- predict(ep_linear, newdata = grid, type = "probs")
grid$ep <- as.vector(probs %*% as.numeric(colnames(probs)))

ggplot(grid, aes(x = yardline_100, y = ep)) +
  geom_point() +
  labs(x = "Yards to opponent end zone (yardline_100)",
       y = "Expected points",
       title = "Linear-in-yardline multinomial EP model") +
  theme_minimal()

# EP doesn't appear to be linear in yardline

spline <- multinom(pts_next_score ~ bs(yardline_100, degree = 3, knots = c(25, 50, 75)),
                   data = nfl_data, trace = FALSE)

# collapse predicted probabilities to EP, exactly as for the linear model
probs_spline   <- predict(spline, newdata = grid, type = "probs")
grid$ep_spline <- as.vector(probs_spline %*% as.numeric(colnames(probs_spline)))

ggplot(grid, aes(x = yardline_100)) +
  geom_line(aes(y = ep,        color = "Linear")) +
  geom_line(aes(y = ep_spline, color = "Spline")) +
  labs(x = "Yards to opponent end zone (yardline_100)",
       y = "Expected points", color = "Model",
       title = "Linear vs spline EP model") +
  theme_minimal()
#spline better captures expected points at sm all distances being very high

nfl_data <- nfl_data %>% mutate(down = factor(down))
ep_down_distance = multinom(pts_next_score ~ bs(yardline_100, degree = 3, knots = c(25, 50, 75)) + down,
                            data = nfl_data, trace = FALSE)

grid_down <- expand_grid(yardline_100 = 1:99, down = factor(1:4))

probs_down   <- predict(ep_down_distance, newdata = grid_down, type = "probs")
grid_down$ep <- as.vector(probs_down %*% as.numeric(colnames(probs_down)))

ggplot(grid_down, aes(x = yardline_100, y = ep, color = down)) +
  geom_line() +
  labs(x = "Yards to opponent end zone (yardline_100)", y = "Expected points",
       color = "Down", title = "EP by yard line and down") +
  theme_minimal()
#Down should be encoded catergorically since there are 4 discrete options

ep_down_distance_to_go = multinom(pts_next_score ~ bs(yardline_100, degree = 3, knots = c(25, 50, 75)) + down + bs(ydstogo, degree = 3, knots = c(5,10,15)),
                            data = nfl_data, trace = FALSE)

# grid now crosses yardline x down x a few yards-to-go values
grid_ytg <- expand_grid(yardline_100 = 1:99,
                        down         = factor(1:4),
                        ydstogo      = c(2, 5, 10)) %>%
  filter(ydstogo <= yardline_100)

probs_ytg   <- predict(ep_down_distance_to_go, newdata = grid_ytg, type = "probs")
grid_ytg$ep <- as.vector(probs_ytg %*% as.numeric(colnames(probs_ytg)))

ggplot(grid_ytg, aes(x = yardline_100, y = ep, color = factor(ydstogo))) +
  geom_line() +
  facet_wrap(~ down, labeller = label_both) +
  labs(x = "Yards to opponent end zone (yardline_100)", y = "Expected points",
       color = "Yards to go", title = "EP by yard line, down, and distance") +
  theme_minimal()

ep_time_lin = multinom(pts_next_score ~ bs(yardline_100, degree = 3, knots = c(25, 50, 75)) + down + bs(ydstogo, degree = 3, knots = c(5,10,15)) + half_seconds_remaining,
                       data = nfl_data, trace = FALSE)

ep_time_spline = multinom(pts_next_score ~ bs(yardline_100, degree = 3, knots = c(25, 50, 75)) + down + bs(ydstogo, degree = 3, knots = c(5,10,15)) + bs(half_seconds_remaining, degree = 3, knots = c(100,300,1000)),
                          data = nfl_data, trace = FALSE)

grid_time <- expand_grid(yardline_100           = 1:99,
                         down                   = factor(1, levels = 1:4),
                         ydstogo                = 10,
                         half_seconds_remaining = c(30, 300, 900, 1800))

probs_time_lin   <- predict(ep_time_lin,    newdata = grid_time, type = "probs")
grid_time$ep_lin <- as.vector(probs_time_lin %*% as.numeric(colnames(probs_time_lin)))

probs_time_spl   <- predict(ep_time_spline, newdata = grid_time, type = "probs")
grid_time$ep_spl <- as.vector(probs_time_spl %*% as.numeric(colnames(probs_time_spl)))

ggplot(grid_time, aes(x = yardline_100, y = ep_lin, color = factor(half_seconds_remaining))) +
  geom_line() +
  labs(x = "Yards to opponent end zone (yardline_100)", y = "Expected points",
       color = "Sec. left in half", title = "Linear-time EP model (1st & 10)") +
  theme_minimal()

ggplot(grid_time, aes(x = yardline_100, y = ep_spl, color = factor(half_seconds_remaining))) +
  geom_line() +
  labs(x = "Yards to opponent end zone (yardline_100)", y = "Expected points",
       color = "Sec. left in half", title = "Spline-time EP model (1st & 10)") +
  theme_minimal()

# linear time shifts EP by an equal amount per second (lines stay parallel and evenly spaced)
# the 30s line drops near the goal line while 900s vs 1800s barely differ

ep_spread = multinom(pts_next_score ~ bs(yardline_100, degree = 3, knots = c(25, 50, 75)) + down + bs(ydstogo, degree = 3, knots = c(5,10,15)) + bs(half_seconds_remaining, degree = 3, knots = c(100,300,1000)) + posteam_spread,
                     data = nfl_data, trace = FALSE, maxit = 500)

grid_spread <- expand_grid(yardline_100           = 1:99,
                           down                   = factor(1, levels = 1:4),
                           ydstogo                = 10,
                           half_seconds_remaining = 1800,
                           posteam_spread         = 0)

probs_M           <- predict(ep_time_spline, newdata = grid_spread, type = "probs")  # M ignores spread
grid_spread$ep_M  <- as.vector(probs_M  %*% as.numeric(colnames(probs_M)))

probs_Mp          <- predict(ep_spread,     newdata = grid_spread, type = "probs")   # M' at spread = 0
grid_spread$ep_Mp <- as.vector(probs_Mp %*% as.numeric(colnames(probs_Mp)))

grid_spread$diff  <- grid_spread$ep_M - grid_spread$ep_Mp

ggplot(grid_spread, aes(x = yardline_100)) +
  geom_line(aes(y = ep_M,  color = "M (Task 1 best)")) +
  geom_line(aes(y = ep_Mp, color = "M' at spread = 0")) +
  labs(x = "Yards to opponent end zone (yardline_100)", y = "Expected points",
       color = "Model", title = "M vs M' (spread = 0), 1st & 10") +
  theme_minimal()

ggplot(grid_spread, aes(x = yardline_100, y = diff)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Yards to opponent end zone (yardline_100)",
       y = "EP(M) - EP(M' at spread = 0)",
       title = "How much M and M' disagree at a neutral matchup") +
  theme_minimal()

#Both models appear pretty similar, but selection bias occurs for M as better teams will more often find themselves in better field positions and vice versa. 
# Therefore, M will over/underestimate values at certain field positions when team strength is not adjusted for. 

#Discussion: the average of all NBA attampts would be higher since it gets pulled up by above average shooters who take more shots.
