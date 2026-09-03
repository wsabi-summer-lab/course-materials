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

##############
### TASK 1 ###
##############

# 1. Linear function of yardline only
model1 <- multinom(
  pts_next_score ~ yardline_100,
  data = nfl_data,
  trace = FALSE
)

# prediction grid
pred_grid <- data.frame(yardline_100 = 1:99)

# predicted probs — columns ordered by multinom's factor levels
probs <- predict(model1, newdata = pred_grid, type = "probs")

# check column order matches outcomes
outcomes <- as.numeric(colnames(probs))

# EP = weighted sum
pred_grid$EP <- as.matrix(probs) %*% outcomes

# plot
ggplot(pred_grid, aes(x = yardline_100, y = EP)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Expected Points vs Yard Line (Linear Model)",
    x = "Yard Line (distance to opponent end zone)",
    y = "Expected Points"
  ) +
  theme_minimal()

#This model is not great because EP on the 1 yard line seems lower. It does not account for down which is also a strong factor for EP. Also, towards your own yardline is very steep whereas there is not a significant difference being on your own 5 yard line and your own 1.    

# Task 2:
# 2. Spline on yardline
library(caret)

knot_rmse <- data.frame(knots = integer(), rmse = numeric())

for (k in 1:10) {
  model_k <- multinom(pts_next_score ~ bs(yardline_100, df = k + 3), 
                      data = nfl_data, trace = FALSE)
  preds <- as.matrix(predict(model_k, newdata = nfl_data, type = "probs")) %*% 
    as.numeric(colnames(predict(model_k, newdata = nfl_data, type = "probs")))
  rmse_k <- sqrt(mean((nfl_data$pts_next_score - preds)^2))
  knot_rmse <- rbind(knot_rmse, data.frame(knots = k, rmse = rmse_k))
}

ggplot(knot_rmse, aes(x = knots, y = rmse)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(x = "Number of Knots", y = "RMSE",
       title = "Elbow Plot: RMSE vs Number of Knots") +
  theme_minimal()

#^shows that 6 knots is ideal

model2 <- multinom(pts_next_score ~ bs(yardline_100, df = 9), data = nfl_data, trace = FALSE)

pred_grid$EP2 <- as.matrix(predict(model2, newdata = pred_grid, type = "probs")) %*% as.numeric(colnames(probs))

ggplot(pred_grid, aes(x = yardline_100, y = EP2)) +
  geom_line(linewidth = 1.2) +
  labs(x = "Yard Line (distance to opponent end zone)", y = "Expected Points",
       title = "EP vs Yard Line (Spline Model)") +
  theme_minimal()

#The splines improves the linear yardline model because it increase the expected points signficantly when close to the endzone.

# Task 3:
# 3. yardline + down
model3 <- multinom(pts_next_score ~ bs(yardline_100, df = 9) + factor(down), 
                   data = nfl_data, trace = FALSE)

pred_grid3 <- expand.grid(yardline_100 = 1:99, down = 1:4)
probs3 <- predict(model3, newdata = pred_grid3, type = "probs")
pred_grid3$EP <- as.matrix(probs3) %*% as.numeric(colnames(probs3))

ggplot(pred_grid3, aes(x = yardline_100, y = EP, color = factor(down))) +
  geom_line(linewidth = 1.2) +
  labs(x = "Yard Line (distance to opponent end zone)", y = "Expected Points",
       title = "EP vs Yard Line by Down", color = "Down") +
  theme_minimal()

#To encode this you need to use factor so R does not treat it as a numerical value.

# Task 4:
# 4. yardline + down + yards to go
model4 <- multinom(pts_next_score ~ bs(yardline_100, df = 9) + factor(down) + log(ydstogo), 
                   data = nfl_data, trace = FALSE)

pred_grid4 <- expand.grid(yardline_100 = 1:99, down = 1:4, ydstogo = c(1, 5, 10, 20))
probs4 <- predict(model4, newdata = pred_grid4, type = "probs")
pred_grid4$EP <- as.matrix(probs4) %*% as.numeric(colnames(probs4))

ggplot(pred_grid4, aes(x = yardline_100, y = EP, color = factor(ydstogo))) +
  geom_line(linewidth = 1) +
  facet_wrap(~ down, labeller = label_both) +
  labs(x = "Yard Line (distance to opponent end zone)", y = "Expected Points",
       title = "EP vs Yard Line by Down and Yards to Go", color = "Yards to Go") +
  theme_minimal()

#On downs 3 and 4, the EP goes down significantly.


# Task 5:
# 5a. linear time term
model5_linear <- multinom(pts_next_score ~ bs(yardline_100, df = 9) + factor(down) + 
                            log(ydstogo) + half_seconds_remaining, 
                          data = nfl_data, trace = FALSE)

# 5b. spline time term
model5_spline <- multinom(pts_next_score ~ bs(yardline_100, df = 9) + factor(down) + 
                            log(ydstogo) + bs(half_seconds_remaining), 
                          data = nfl_data, trace = FALSE)

# prediction grid: 1st down, 10 yards to go, vary time and yardline
pred_grid5 <- expand.grid(yardline_100 = 1:99, down = 1, ydstogo = 10,
                          half_seconds_remaining = c(60, 300, 900, 1800))

# linear model predictions
probs5l <- predict(model5_linear, newdata = pred_grid5, type = "probs")
pred_grid5$EP_linear <- as.matrix(probs5l) %*% as.numeric(colnames(probs5l))

# spline model predictions
probs5s <- predict(model5_spline, newdata = pred_grid5, type = "probs")
pred_grid5$EP_spline <- as.matrix(probs5s) %*% as.numeric(colnames(probs5s))

# plot linear
ggplot(pred_grid5, aes(x = yardline_100, y = EP_linear, color = factor(half_seconds_remaining))) +
  geom_line(linewidth = 1) +
  labs(x = "Yard Line", y = "Expected Points", color = "Seconds Remaining",
       title = "EP vs Yard Line by Time Remaining (Linear Time)") +
  theme_minimal()

# plot spline
ggplot(pred_grid5, aes(x = yardline_100, y = EP_spline, color = factor(half_seconds_remaining))) +
  geom_line(linewidth = 1) +
  labs(x = "Yard Line", y = "Expected Points", color = "Seconds Remaining",
       title = "EP vs Yard Line by Time Remaining (Spline Time)") +
  theme_minimal()

#they differ in the sense that the linear model chunks all the EP lines relatively at the same distance apart, but if you spline time the 300 s remaining gets pulled up towards 900 and 1800 seconds. 

#I learned that the models do what they're told in the sense that if you are seaching out capturing a trend, you can while neglecting the true trend.

##############
### PART 2 ###
##############

# Task 1:
# Part 2 Task 1: M' adjusts for spread
model_prime <- multinom(pts_next_score ~ bs(yardline_100, df = 9) + factor(down) + 
                          log(ydstogo) + bs(half_seconds_remaining) + posteam_spread,
                        data = nfl_data, trace = FALSE)

# Part 2 Task 2: compare M vs M' at spread = 0
pred_grid_p2 <- data.frame(yardline_100 = 1:99, down = 1, ydstogo = 10, 
                           half_seconds_remaining = 900)

# M predictions
probs_M <- predict(model5_spline, newdata = pred_grid_p2, type = "probs")
pred_grid_p2$EP_M <- as.matrix(probs_M) %*% as.numeric(colnames(probs_M))

# M' predictions at spread = 0
pred_grid_p2$posteam_spread <- 0
probs_Mp <- predict(model_prime, newdata = pred_grid_p2, type = "probs")
pred_grid_p2$EP_Mp <- as.matrix(probs_Mp) %*% as.numeric(colnames(probs_Mp))

# overlay plot
ggplot(pred_grid_p2, aes(x = yardline_100)) +
  geom_line(aes(y = EP_M, color = "M (no spread)"), linewidth = 1) +
  geom_line(aes(y = EP_Mp, color = "M' (spread = 0)"), linewidth = 1, linetype = "dashed") +
  labs(x = "Yard Line", y = "Expected Points", color = "Model",
       title = "M vs M' at Spread = 0") +
  theme_minimal()

# difference plot
pred_grid_p2$diff <- pred_grid_p2$EP_Mp - pred_grid_p2$EP_M

ggplot(pred_grid_p2, aes(x = yardline_100, y = diff)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Yard Line", y = "EP_M' - EP_M",
       title = "Difference in EP: M' (spread=0) vs M") +
  theme_minimal()

#The M' does not differ significantly from M, differing at most by 0.2 which is insignificant in a -2 to 6 scale. This show that point spread or relatively difference in team strength changes things minimally. 


# Discussion:
# The percent of all 3 point attempts made in NBA this year is not overestimated since it is just the true proportion of observed makes over the observed attempts. However, this is not the "true" 3- point make percentage of an average NBA player if you extracted this from the data would be an overestimated because better players tend to shoot more frequently and shoot better than the average player. To account for this you can weight each players average by a constant same number of shots rather than giving more weight to the better players. However, this woudl still be biased because the worst player don't play or do not get enough shots to be around their own true average. 