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
nfl_data


#Task 1
#Part 1: 

model_1 = multinom(
  pts_next_score ~ yardline_100,
  data = nfl_data,
  maxit = 1000,
  trace = FALSE
)

summary(model_1)

pred_grid <- data.frame(yardline_100 = 1:99)

# Get predicted probabilities for each outcome class
probs <- predict(model_1, newdata = pred_grid, type = "probs")

# Compute EP as weighted sum: sum over k of k * P(y = k | x)
point_values <- as.numeric(colnames(probs))
pred_grid$EP <- as.numeric(probs %*% point_values)

# Plot EP vs yard line
ggplot(pred_grid, aes(x = yardline_100, y = EP)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  scale_x_reverse() +
  labs(
    title = "Expected Points vs Yard Line (Linear Model)",
    x = "Yard Line (distance to opponent's end zone)",
    y = "Expected Points"
  ) +
  theme_minimal()

#The issue with this model is that the relationship is not linear. 
#As you get closer to the opposing team's endzone, your expected points should 
#go up more than when you are still close to your own


#Data plot: 




#Part 2: 

model_2 = multinom(
  pts_next_score ~ splines::bs(yardline_100, degree = 3, knots = 1),
  data = nfl_data,
  maxit = 1000,
  trace = FALSE
)


probs_2 <- predict(model_2, newdata = pred_grid, type = "probs")
point_values_2 <- as.numeric(colnames(probs_2))
pred_grid$EP_model2 <- as.numeric(probs_2 %*% point_values_2)

# Bucket data
bucket_data <- nfl_data %>%
  mutate(yardline_bucket = round(yardline_100 / 2) * 2) %>%
  group_by(yardline_bucket) %>%
  summarise(EP = mean(as.numeric(as.character(pts_next_score))))

# Overlay
ggplot() +
  geom_point(data = bucket_data, aes(x = yardline_bucket, y = EP), color = "steelblue") +
  geom_line(data = pred_grid, aes(x = yardline_100, y = EP_model2), color = "red", linewidth = 1.2) +
  scale_x_reverse() +
  labs(
    title = "Empirical EP (2-yard buckets) with Spline Model Fit",
    x = "Yard Line (distance to opponent's end zone)",
    y = "Expected Points"
  ) +
  theme_minimal()

#This works better because it accounts for the increase in probability
#as you get past the 50 yard line. 




#Part 3: 

model_3 = multinom(
  pts_next_score ~ splines::bs(yardline_100 , degree = 3, knots = 1) + as.factor(down),
  data = nfl_data,
  maxit = 1000,
  trace = FALSE
)




pred_grid_3 <- expand.grid(
  yardline_100 = 1:99,
  down = 1:4
)

probs_3 <- predict(model_3, newdata = pred_grid_3, type = "probs")
point_values_3 <- as.numeric(colnames(probs_3))
pred_grid_3$EP <- as.numeric(probs_3 %*% point_values_3)
pred_grid_3$down <- as.factor(pred_grid_3$down)

# Bucket data by yard line and down
bucket_data_3 <- nfl_data %>%
  mutate(
    yardline_bucket = round(yardline_100 / 2) * 2,
    down = as.factor(down)
  ) %>%
  group_by(yardline_bucket, down) %>%
  summarise(EP = mean(as.numeric(as.character(pts_next_score))), .groups = "drop")

# Plot
ggplot() +
  geom_point(data = bucket_data_3, aes(x = yardline_bucket, y = EP, color = down), alpha = 0.5) +
  geom_line(data = pred_grid_3, aes(x = yardline_100, y = EP, color = down), linewidth = 1.2) +
  scale_x_reverse() +
  labs(
    title = "Expected Points vs Yard Line by Down",
    x = "Yard Line (distance to opponent's end zone)",
    y = "Expected Points",
    color = "Down"
  ) +
  theme_minimal()

#It makes sense to encode downs as categorical because going from 
#third to fourth down is a lot different than going from first to 
#second down. 



#Part 4: 

model_4 = multinom(
  pts_next_score ~ splines::bs(yardline_100 , degree = 3, knots = 1) + as.factor(down) + 
    ydstogo,
  data = nfl_data,
  maxit = 1000,
  trace = FALSE
)


pred_grid_4 <- expand.grid(
  yardline_100 = 1:99,
  ydstogo = c(1, 5, 10, 20),
  down = 1:4
)





probs_4 <- predict(model_4, newdata = pred_grid_4, type = "probs")
point_values_4 <- as.numeric(colnames(probs_4))
pred_grid_4$EP <- as.numeric(probs_4 %*% point_values_4)
pred_grid_4$down <- as.factor(pred_grid_4$down)
pred_grid_4$ydstogo <- as.factor(pred_grid_4$ydstogo)

# Bucket data by yard line and down
bucket_data_4 <- nfl_data %>%
  mutate(
    yardline_bucket = round(yardline_100 / 2) * 2,
    down = as.factor(down),
    ydstogo_bucket = as.factor(case_when(
      ydstogo <= 2  ~ 1,
      ydstogo <= 7  ~ 5,
      ydstogo <= 15 ~ 10,
      TRUE ~ 20
    ))
  ) %>%
  group_by(yardline_bucket, down, ydstogo_bucket) %>%
  summarise(EP = mean(as.numeric(as.character(pts_next_score))), .groups = "drop")

# Plot
ggplot() +
  geom_point(data = bucket_data_4, 
             aes(x = yardline_bucket, y = EP, color = ydstogo_bucket), 
             alpha = 0.4, size = 0.8) +
  geom_line(data = pred_grid_4, 
            aes(x = yardline_100, y = EP, color = ydstogo), 
            linewidth = 1) +
  facet_wrap(~ down, labeller = label_both) +
  scale_x_reverse() +
  labs(
    title = "Expected Points vs Yard Line by Down and Yards to Go",
    x = "Yard Line (distance to opponent's end zone)",
    y = "Expected Points",
    color = "Yards to Go"
  ) +
  theme_minimal()






#Part 5: 

# Model 5a: linear time remaining
model_5a = multinom(
  pts_next_score ~ bs(yardline_100, degree = 3, knots = 1) + as.factor(down) + 
    ydstogo + half_seconds_remaining,
  data = nfl_data,
  maxit = 1000,
  trace = FALSE
)

# Model 5b: spline time remaining
model_5b = multinom(
  pts_next_score ~ bs(yardline_100, degree = 3, knots = 1) + as.factor(down) + 
    ydstogo + bs(half_seconds_remaining, degree = 3),
  data = nfl_data,
  maxit = 1000,
  trace = FALSE
)

# Prediction grid: 1st down, 10 yards to go, vary yard line and time remaining
pred_grid_5 <- expand.grid(
  yardline_100 = 1:99,
  down = 1,
  ydstogo = 10,
  half_seconds_remaining = c(60, 300, 600, 900, 1200, 1800)
)

# Model 5a predictions
probs_5a <- predict(model_5a, newdata = pred_grid_5, type = "probs")
pred_grid_5$EP_linear <- as.numeric(probs_5a %*% as.numeric(colnames(probs_5a)))

# Model 5b predictions
probs_5b <- predict(model_5b, newdata = pred_grid_5, type = "probs")
pred_grid_5$EP_spline <- as.numeric(probs_5b %*% as.numeric(colnames(probs_5b)))

# Convert time remaining to factor for color scale
pred_grid_5 <- pred_grid_5 %>%
  mutate(time_label = as.factor(half_seconds_remaining))

# Plot 5a: linear time
ggplot(pred_grid_5, aes(x = yardline_100, y = EP_linear, color = time_label)) +
  geom_line(linewidth = 1) +
  scale_x_reverse() +
  labs(
    title = "EP vs Yard Line by Time Remaining (Linear Time Term)",
    x = "Yard Line (distance to opponent's end zone)",
    y = "Expected Points",
    color = "Seconds Remaining"
  ) +
  theme_minimal()

# Plot 5b: spline time
ggplot(pred_grid_5, aes(x = yardline_100, y = EP_spline, color = time_label)) +
  geom_line(linewidth = 1) +
  scale_x_reverse() +
  labs(
    title = "EP vs Yard Line by Time Remaining (Spline Time Term)",
    x = "Yard Line (distance to opponent's end zone)",
    y = "Expected Points",
    color = "Seconds Remaining"
  ) +
  theme_minimal()

#The spline model seems to make more sense because it makes the difference
#when there is more than 10 minutes left in the half seem minimal, which it 
#likely is. It also shows the expected points with 60 second left and an entire
#field to go higher which makes sense since the team with the ball likely will try
#score at all costs and if the turn it over, the other team will just run the 
#clock out. 





#Task 2: We will use model from part 3


#Task 2:

# M' : best model from Task 1 (model_3) + posteam_spread
model_prime = multinom(
  pts_next_score ~ bs(yardline_100, degree = 3, knots = 1) + as.factor(down) +
    posteam_spread,
  data = nfl_data,
  maxit = 1000,
  trace = FALSE
)

# Prediction grid: all yard lines, 1st down, spread = 0
pred_grid_task2 <- expand.grid(
  yardline_100 = 1:99,
  down = 1,
  posteam_spread = 0
)

# M predictions (model_3, no spread term)
pred_grid_M <- data.frame(yardline_100 = 1:99, down = 1)
probs_M <- predict(model_3, newdata = pred_grid_M, type = "probs")
pred_grid_task2$EP_M <- as.numeric(probs_M %*% as.numeric(colnames(probs_M)))

# M' predictions at spread = 0
probs_Mprime <- predict(model_prime, newdata = pred_grid_task2, type = "probs")
pred_grid_task2$EP_Mprime <- as.numeric(probs_Mprime %*% as.numeric(colnames(probs_Mprime)))

# Difference
pred_grid_task2$EP_diff <- pred_grid_task2$EP_Mprime - pred_grid_task2$EP_M

# Plot 1: overlay of M and M' at spread = 0
pred_grid_task2 %>%
  pivot_longer(cols = c(EP_M, EP_Mprime), names_to = "model", values_to = "EP") %>%
  mutate(model = recode(model, EP_M = "M (no spread)", EP_Mprime = "M' (spread = 0)")) %>%
  ggplot(aes(x = yardline_100, y = EP, color = model)) +
  geom_line(linewidth = 1.2) +
  scale_x_reverse() +
  labs(
    title = "M vs M' at Spread = 0 (1st Down)",
    x = "Yard Line (distance to opponent's end zone)",
    y = "Expected Points",
    color = "Model"
  ) +
  theme_minimal()

# Plot 2: difference between M' and M as a function of yard line
ggplot(pred_grid_task2, aes(x = yardline_100, y = EP_diff)) +
  geom_line(color = "darkred", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_reverse() +
  labs(
    title = "Difference in EP: M' (spread=0) minus M",
    x = "Yard Line (distance to opponent's end zone)",
    y = "EP Difference"
  ) +
  theme_minimal()


#There is a small difference between M and M' at spread = 0
#due to the additional conditioning. Even though even spread
#is the average, it is not garunteed to be exactly the same. 






