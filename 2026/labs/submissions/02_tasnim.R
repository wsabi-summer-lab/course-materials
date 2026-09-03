#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(splines)

# set seed
set.seed(3)

##############
### PART 1 ###
##############

# load data
nba_four_factors <- read_csv("~/Desktop/02_nba-four-factors.csv.gz")

# Task 1:
# - Compute each variable's mean, standard deviation, minimum, and maximum
nba = nba_four_factors[, c(
  "W",
  "Shooting Factor",
  "Crashing Factor",
  "Protecting Factor",
  "Attacking Factor"
)]
data.frame(
  Mean = sapply(nba, mean),
  SD = sapply(nba, sd),
  Min = sapply(nba, min),
  Max = sapply(nba, max)
)
# - Plot the marginal distribution of each explanatory variable
ggplot(nba_four_factors,
       aes(x = `Shooting Factor`)) +
  geom_histogram()
ggplot(nba_four_factors,
       aes(x = `Crashing Factor`)) +
  geom_histogram()
ggplot(nba_four_factors,
       aes(x = `Protecting Factor`)) +
  geom_histogram()
ggplot(nba_four_factors,
       aes(x = `Attacking Factor`)) +
  geom_histogram()



# - Compute correlations between each pair of explanatory variables
cor(nba[, -1])
# - Identify which variables look most strongly related to wins before fitting a model
cor(nba)[, "W"]

# Task 2:
# - Fit the multivariable model: wins ~ x1 + x2 + x3 + x4
model <- lm(
  W ~ `Shooting Factor` +
    `Crashing Factor` +
    `Protecting Factor` +
    `Attacking Factor`,
  data = nba
)

summary(model)
# - Write down the fitted regression equation
W=−93.845+3.675(Shooting Factor)+1.340(Crashing Factor)−3.059(Protecting Factor)+0.771(Attacking Factor)

# - Interpret each coefficient in context
#Holding all other factors constant, a one unit increase in shooting factor corresponds to 3.675 increase in wins
#Holding all other factors constant, a one unit increase in crashing factor corresponds to 1.340 increase in wins
#Holding all other factors constant, a one unit increase in crashing factor corresponds to 3.059 decrease in wins
#Holding all other factors constant, a one unit increase in crashing factor corresponds to 0.771 increase in wins
# - Check whether the coefficient signs make sense given the variable definitions
#Yes(the signs match with the corr matrix we created earlier)
# - Identify which factors look strongest and weakest after adjustment
#shooting factor : strongest and attacking factor: weakest
# Task 3:
# - Standardize the four predictors
nba_std = nba
nba_std[, -1] = scale(nba[, -1])
# - Fit the standardized model
model_std = lm(
  W ~ `Shooting Factor`
  + `Crashing Factor`
  + `Protecting Factor`
  + `Attacking Factor`,
  data = nba_std
)

summary(model_std)
# - Rank the factors by absolute standardized coefficient size
coef(model_std)
# - Compare the original and standardized models for interpretability
#It's harder to interpret the effect of these coefficients as they're in sd units, but it's 
#easier to compare them dirctly. 
# - Compare fitted values from both models and explain why they match or differ
pred_original = fitted(model)
pred_standardized = fitted(model_std)
all.equal(pred_original, pred_standardized)
# Task 4:
# - Report the residual standard error and interpret it in wins
#Residual standard error: 3.977
# - Report coefficient standard errors and 95% confidence intervals
confint(model)
# - Identify which effects are clearly different from zero
#All four factors
# - Choose one team and compute a point prediction, confidence interval, and prediction interval
# - State which interval is wider and why
team2 = nba[2, ]
predict(model, newdata = team2)
predict(
  model,
  newdata = team2,
  interval = "confidence"
)
# Task 5:
# - Randomly split the data into training and test sets
n = nrow(nba)

train_index = sample(
  1:n,
  size = round(0.7 * n)
)

train = nba[train_index, ]
test = nba[-train_index, ]

# - Fit the original and standardized models on the training set
model_train = lm(
  W ~ `Shooting Factor` +
    `Crashing Factor` +
    `Protecting Factor` +
    `Attacking Factor`,
  data = train
)
train_std = train
test_std = test

means = sapply(train[, -1], mean)
sds = sapply(train[, -1], sd)

train_std[, -1] = scale(
  train[, -1],
  center = means,
  scale = sds
)

test_std[, -1] = scale(
  test[, -1],
  center = means,
  scale = sds
)


model_std_train = lm(
  W ~ `Shooting Factor` +
    `Crashing Factor` +
    `Protecting Factor` +
    `Attacking Factor`,
  data = train_std
)


pred_original = predict(model_train, newdata = test)
pred_std = predict(model_std_train, newdata = test_std)
# - Compute test-set RMSE for both models

rmse_original =
  sqrt(mean((test$W - pred_original)^2))

rmse_std =
  sqrt(mean((test$W - pred_std)^2))

rmse_original
rmse_std

# - Compare predictive performance
all.equal(pred_original, pred_std)
##############
### PART 2 ###
##############

# load data
punts <- read_csv("~/Desktop/02_punts.csv.gz")

# Task 1:
# - Plot post-punt yard line against starting yard line
ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(alpha = 0.2) +
  labs(
    x = "Starting Yard Line",
    y = "Post-Punt Yard Line",
    title = "Post-Punt Yard Line vs Starting Yard Line"
  )
# - Bin punts by starting field position and plot average post-punt yard line in each bin
punts$ydl_bin = cut(
  punts$ydl,
  breaks = seq(30, 100, by = 10),
  include.lowest = TRUE
)

bin_summary = punts %>%
  group_by(ydl_bin) %>%
  summarize(
    avg_next_ydl = mean(next_ydl)
  )

ggplot(bin_summary,
       aes(x = ydl_bin,
           y = avg_next_ydl,
           group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    x = "Starting Yard Line Bin",
    y = "Average Post-Punt Yard Line"
  )
# - Describe the shape of the relationship and where it bends: non linear/decreasing/ around 50.60 or 60.70
# is where the bend is 
# - Plot or summarize the distribution of punter quality
ggplot(punts, aes(x = pq)) +
  geom_histogram(bins = 30) +
  labs(
    x = "Punter Quality",
    y = "Count",
    title = "Distribution of Punter Quality"
  )

# Task 2:
# - Fit competing punt models: linear, quadratic, quadratic plus punter quality, and spline
# Linear
model_linear = lm(
  next_ydl ~ ydl,
  data = punts
)

# Quadratic
model_quad = lm(
  next_ydl ~ ydl + I(ydl^2),
  data = punts
)

# Quadratic + Punter Quality
model_quad_pq = lm(
  next_ydl ~ ydl + I(ydl^2) + pq,
  data = punts
)

# Spline
model_spline = lm(
  next_ydl ~ bs(ydl, df = 5),
  data = punts
)

# - Visualize the fitted curves from each model
grid = data.frame(
  ydl = seq(
    min(punts$ydl),
    max(punts$ydl),
    length.out = 200
  ),
  pq = mean(punts$pq)
)
grid$linear =
  predict(model_linear, newdata = grid)

grid$quadratic =
  predict(model_quad, newdata = grid)

grid$quad_pq =
  predict(model_quad_pq, newdata = grid)

grid$spline =
  predict(model_spline, newdata = grid)
ggplot(punts,
       aes(x = ydl,
           y = next_ydl)) +
  geom_point(alpha = 0.15) +
  
  geom_line(
    data = grid,
    aes(y = linear,
        color = "Linear"),
    linewidth = 1
  ) +
  
  geom_line(
    data = grid,
    aes(y = quadratic,
        color = "Quadratic"),
    linewidth = 1
  ) +
  
  geom_line(
    data = grid,
    aes(y = quad_pq,
        color = "Quadratic + PQ"),
    linewidth = 1
  ) +
  
  geom_line(
    data = grid,
    aes(y = spline,
        color = "Spline"),
    linewidth = 1
  ) +
  
  labs(
    x = "Starting Yard Line",
    y = "Post-Punt Yard Line",
    color = "Model"
  )
# - Use train/test RMSE or cross-validation to choose a preferred model
set.seed(3)

n = nrow(punts)

train_index = sample(
  1:n,
  size = round(0.7 * n)
)

train = punts[train_index, ]
test = punts[-train_index, ]
# - Compare the linear, quadratic, and spline tradeoffs
model_linear = lm(
  next_ydl ~ ydl,
  data = train
)

model_quad = lm(
  next_ydl ~ ydl + I(ydl^2),
  data = train
)

model_quad_pq = lm(
  next_ydl ~ ydl + I(ydl^2) + pq,
  data = train
)

model_spline = lm(
  next_ydl ~ bs(ydl, df = 5),
  data = train
)
rmse = function(actual, pred) {
  sqrt(mean((actual - pred)^2))
}
pred_linear =
  predict(model_linear, newdata = test)

pred_quad =
  predict(model_quad, newdata = test)

pred_quad_pq =
  predict(model_quad_pq, newdata = test)

pred_spline =
  predict(model_spline, newdata = test) 
rmse_linear =
  rmse(test$next_ydl, pred_linear)

rmse_quad =
  rmse(test$next_ydl, pred_quad)

rmse_quad_pq =
  rmse(test$next_ydl, pred_quad_pq)

rmse_spline =
  rmse(test$next_ydl, pred_spline)

data.frame(
  Model = c(
    "Linear",
    "Quadratic",
    "Quadratic + PQ",
    "Spline"
  ),
  RMSE = c(
    rmse_linear,
    rmse_quad,
    rmse_quad_pq,
    rmse_spline
  )
)
#.  The model with the smallest RSME -> Quadratic 

# - Assess whether punter quality improves out-of-sample prediction
# - Interpret the punter-quality coefficient if it is included in the selected model
summary(model_quad_pq)
rmse_quad
rmse_quad_pq
# Task 3:
# - Plot the fitted mean response for the selected punt model
grid = data.frame(
  ydl = seq(
    min(punts$ydl),
    max(punts$ydl),
    length.out = 200
  ),
  pq = mean(punts$pq)
)
# - Add a 95% confidence band for the expected response
conf = predict(
  model_quad_pq,
  newdata = grid,
  interval = "confidence",
  level = 0.95
)

head(conf)
grid$fit_conf = conf[, "fit"]
grid$lwr_conf = conf[, "lwr"]
grid$upr_conf = conf[, "upr"]
# - Add a 95% prediction band for one individual punt
pred = predict(
  model_quad_pq,
  newdata = grid,
  interval = "prediction",
  level = 0.95
)

grid$fit_pred = pred[, "fit"]
grid$lwr_pred = pred[, "lwr"]
grid$upr_pred = pred[, "upr"]
ggplot() +
  
  geom_point(
    data = punts,
    aes(x = ydl, y = next_ydl),
    alpha = 0.15
  ) +
  
  # prediction band
  geom_ribbon(
    data = grid,
    aes(
      x = ydl,
      ymin = lwr_pred,
      ymax = upr_pred
    ),
    alpha = 0.15,
    fill = "red"
  ) +
  
  # confidence band
  geom_ribbon(
    data = grid,
    aes(
      x = ydl,
      ymin = lwr_conf,
      ymax = upr_conf
    ),
    alpha = 0.3,
    fill = "blue"
  ) +
  
  geom_line(
    data = grid,
    aes(
      x = ydl,
      y = fit_conf
    ),
    linewidth = 1
  ) +
  
  labs(
    x = "Starting Yard Line",
    y = "Post-Punt Yard Line",
    title = "Quadratic + Punter Quality Model",
    subtitle = "95% Confidence and Prediction Bands"
  )
# - Explain why the prediction band is wider
#  The confidence band only accounts for uncertainty in the mean response while the prediction 
#  band accounts for both the mean response uncertainty and the variability of the individual punts.
# - Identify where the model is most uncertain -> the extreme starting yard lines

# Task 4:
# - Define punt yards over expected so that positive values are better punts
punts$expected_next_ydl =
  predict(model_quad_pq, newdata = punts)


# - Compute PYOE for each punt
punts$PYOE =
  punts$expected_next_ydl -
  punts$next_ydl
# - For each punter, compute average PYOE, number of punts, and standard error of average PYOE
punter_summary =
  punts %>%
  group_by(punter) %>%
  summarize(
    avg_PYOE = mean(PYOE),
    n_punts = n(),
    sd_PYOE = sd(PYOE),
    se_PYOE = sd_PYOE / sqrt(n_punts)
  )
# - Rank punters by average PYOE
punter_summary =
  punter_summary %>%
  arrange(desc(avg_PYOE))
# - Visualize punter rankings with uncertainty intervals
punter_summary =
  punter_summary %>%
  mutate(
    lower = avg_PYOE - 1.96 * se_PYOE,
    upper = avg_PYOE + 1.96 * se_PYOE
  )

ggplot(
  punter_summary,
  aes(
    x = avg_PYOE,
    y = reorder(punter, avg_PYOE)
  )
) +
  geom_point() +
  geom_errorbarh(
    aes(
      xmin = lower,
      xmax = upper
    ),
    height = 0.2
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    x = "Average PYOE",
    y = "Punter",
    title = "Punter Rankings with 95% Uncertainty Intervals"
  )
# - Identify which punters look clearly above average and which rankings are unstable
clearly_above_average =
  punter_summary %>%
  filter(lower > 0)

unstable_rankings =
  punter_summary %>%
  filter(lower < 0 & upper > 0)

clearly_above_average
unstable_rankings
# Final reflection:
# - Explain how adding columns changed what the model could fit
#Adding additional columns to the model increased its complexity and flexibility. In particular, including polynomial terms allowed the model to fit a quadratic relationship rather than being restricted to a linear relationship. This was useful because the visualizations suggested that the relationship between the variables was not perfectly linear.
# - Explain when flexibility helped and when it could hurt
#A complex model may perform well on the observed data, it may generalize poorly to new punts and can become harder to interpret. I’ve always had to deal with overfitting. 
# - Interpret the residual standard error in this setting
#The residual standard error represents the typical prediction error of the model. It can be interpreted as the typical distance, measured in yards, between an observed post-punt yard line and the value predicted by the model.
# - Explain why prediction intervals are wider than confidence intervals
#A prediction interval is wider because it includes both uncertainty about the mean prediction and the natural variability of an individual punt. As a result, prediction intervals intuitively will be wider. 
# - Note one coefficient, prediction, or ranking you would interpret cautiously
#One result I would interpret cautiously is the ranking of punters with very small sample sizes. For example, C. Catanzaro had a high average PYOE, but it was based on only two punts and had a very wide confidence interval. Because the interval crossed zero, there is substantial uncertainty about his true performance. More generally, I see that confidence intervals should be interpreted in the context of the amount of data available and the magnitude of uncertainty, rather than as a simple binary test of significance.

