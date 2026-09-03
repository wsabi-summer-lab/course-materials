#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(3)

##############
### PART 1 ###
##############

# load data
nba_four_factors = read_csv("../data/02_nba-four-factors.csv.gz")

# Task 1:
# - Compute each variable's mean, standard deviation, minimum, and maximum
# - Plot the marginal distribution of each explanatory variable
# - Make scatterplots of wins against each of the four factors
# - Compute correlations between each pair of explanatory variables
# - Identify which variables look most strongly related to wins before fitting a model

print(nba_four_factors)

nba_four_factors = nba_four_factors %>%
  mutate(
    `eFG%` = (FGM + (0.5 * `3PM`)) / (FGA),
    `opp_eFG%` = (OPP_FGM + (0.5 * OPP_3PM)) / (OPP_FGA)
  )

key_vars = nba_four_factors %>%
  mutate(
    x1 = (`eFG%` - `opp_eFG%`) * 100,
    x2 = `OREB%` + `DREB%` - 100,
    x3 = `OPP TOV %` - `TOV%`,
    x4 = (`FT Rate` - `OPP FT Rate`) * 100
  )

summary(key_vars)

#Distributions:

ggplot(key_vars, aes(x = x1)) + geom_histogram(binwidth = 0.01) + labs(title = "Distribution of x1 (eFG% differential)")
ggplot(key_vars, aes(x = x2)) + geom_histogram(binwidth = 1) + labs(title = "Distribution of x2 (REB% differential)")
ggplot(key_vars, aes(x = x3)) + geom_histogram(binwidth = 0.5) + labs(title = "Distribution of x3 (TOV% differential)")
ggplot(key_vars, aes(x = x4)) + geom_histogram(binwidth = 0.5) + labs(title = "Distribution of x4 (FT Rate differential)")


#Scatterplots: 

ggplot(key_vars, aes(x = x1, y = W)) + geom_point() + geom_smooth(method = "lm") + labs(title = "Wins vs x1 (eFG% differential)")
ggplot(key_vars, aes(x = x2, y = W)) + geom_point() + geom_smooth(method = "lm") + labs(title = "Wins vs x2 (REB% differential)")
ggplot(key_vars, aes(x = x3, y = W)) + geom_point() + geom_smooth(method = "lm") + labs(title = "Wins vs x3 (TOV% differential)")
ggplot(key_vars, aes(x = x4, y = W)) + geom_point() + geom_smooth(method = "lm") + labs(title = "Wins vs x4 (FT Rate differential)")


#Correlation between pairs of explanatory variables and with wins:

cor(key_vars[, c("x1", "x2", "x3", "x4", "W")])



# Task 2:
# - Fit the multivariable model: wins ~ x1 + x2 + x3 + x4
# - Write down the fitted regression equation
# - Interpret each coefficient in context
# - Check whether the coefficient signs make sense given the variable definitions
# - Identify which factors look strongest and weakest after adjustment

model = lm(W ~ x1 + x2 + x3 + x4, data = key_vars)

summary(model)


# Equation is above, each coefficient represents the expected number of additional
# wins from a unit increase in the corresponding variable. All coefficients being 
# positive makes sense since a positive increase in the vaariable values should 
# increase the expected number of wins for a team. Since the scoring factor has the
# highest t value, it is the most associated with winning. The attacking factor has
# the lowest t value and seems weakly associated with winning





# Task 3:
# - Standardize the four predictors
# - Fit the standardized model
# - Rank the factors by absolute standardized coefficient size
# - Compare the original and standardized models for interpretability
# - Compare fitted values from both models and explain why they match or differ


key_vars = key_vars %>%
  mutate(
    z1 = as.numeric(scale(x1)),
    z2 = as.numeric(scale(x2)),
    z3 = as.numeric(scale(x3)),
    z4 = as.numeric(scale(x4))
  )

st_model = lm(W ~ z1 + z2 + z3 + z4, data = key_vars)

summary(st_model)

#From highest to lowest absolute values: Scoring, Protecting, Crashing, Attacking.
#The standardized model is better for comparing relative importance because it 
#gives coefficients that are adjusted for the variance of their variable. 
#The coefficients are not the same but the t values are the same. 
#In terms of the geometry, the adjusted model makes fitting more accurate. 




# Task 4:
# - Report the residual standard error and interpret it in wins
# - Report coefficient standard errors and 95% confidence intervals
# - Identify which effects are clearly different from zero
# - Choose one team and compute a point prediction, confidence interval, and prediction interval
# - State which interval is wider and why

#From the summary, the RSE is 3.974 on 475 degrees of freedom. 
confint(st_model)

#beta0: [39.785, 40.498]
#beta1: [9.760, 10.504]
#beta2: [3.274, 4.022]
#beta3: [4.064, 4.817]
#beta4: [1.929, 2.678]

#All factors have effects clearly distinguishable from 0 since none have 0 within
#their confidence intervals. 

Hawks = key_vars %>% filter(TEAM == "Atlanta Hawks")

predict(st_model, newdata = Hawks)

predict(model, newdata = Hawks, interval = "confidence", level = 0.95)

predict(model, newdata = Hawks, interval = "prediction", level = 0.95)

#This gives the data for a bunch of years. The first year (2005) has a prediction
# of 14.9 wins with a confidence interval of [13.73, 16.07] and a prediction 
#interval of [7.01, 22.80]. The prediction interval is wider because it accounts 
#for team by team variation. 



# Task 5:
# - Randomly split the data into training and test sets
# - Fit the original and standardized models on the training set
# - Compute test-set RMSE for both models
# - Compare predictive performance

#These lines split the data into training and test sets
n = nrow(key_vars)
train_idx = sample(1:n, size = 0.8 * n)
train = key_vars[train_idx, ]
test  = key_vars[-train_idx, ]

#original and scaled models
model_train = lm(W ~ x1 + x2 + x3 + x4, data = train)
train_scaled = train %>% mutate(across(c(x1, x2, x3, x4), scale))
test_scaled  = test %>% mutate(across(c(x1, x2, x3, x4), scale))
model_train_scaled = lm(W ~ x1 + x2 + x3 + x4, data = train_scaled)

#helper function
rmse = function(actual, predicted) sqrt(mean((actual - predicted)^2))

rmse_original = rmse(test$W, predict(model_train, newdata = test))
rmse_scaled = rmse(test_scaled$W, predict(model_train_scaled, newdata = test_scaled))


rmse_original #4.094
rmse_scaled #4.144

#Since the two values are very similar, the predict with a relatively 
#similar level of accuracy. 


##############
### PART 2 ###
##############

# load data
punts = read_csv("../data/02_punts.csv.gz")

print(punts)

# Task 1:
# - Plot post-punt yard line against starting yard line
# - Bin punts by starting field position and plot average post-punt yard line in each bin
# - Describe the shape of the relationship and where it bends
# - Plot or summarize the distribution of punter quality

#Normal Plot
ggplot(punts, aes(x = ydl, y = next_ydl)) + geom_point()

#Binned version
punts_binned = punts %>%
  mutate(bin = cut(ydl, breaks = seq(0, 100, by = 1))) %>%
  group_by(bin) %>%
  summarise(avg_post_punt = mean(next_ydl, na.rm = TRUE))

ggplot(punts_binned, aes(x = bin, y = avg_post_punt)) +
  geom_point(fill = "steelblue") +
  scale_x_discrete(breaks = levels(punts_binned$bin)[seq(1, 100, by = 10)]) +
  labs(title = "Average Post-Punt Yard Line by Starting Field Position",
       x = "Starting Yard Line (binned)",
       y = "Average Post-Punt Yard Line") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#It looks somewhat linear but seems to be more quadratic than linear

ggplot(punts, aes(x = pq)) + geom_histogram()


# Task 2:
# - Fit competing punt models: linear, quadratic, quadratic plus punter quality, and spline
# - Visualize the fitted curves from each model
# - Use train/test RMSE or cross-validation to choose a preferred model
# - Compare the linear, quadratic, and spline tradeoffs
# - Assess whether punter quality improves out-of-sample prediction
# - Interpret the punter-quality coefficient if it is included in the selected model


#Numerical models
model1 = lm(next_ydl ~ ydl, data = punts)
model2 = lm(next_ydl ~ ydl + I(ydl^2), data = punts)
model3 = lm(next_ydl ~ ydl + I(ydl^2) + pq, data = punts)

summary(model1)
summary(model2)
summary(model3)


punts$pred3 = predict(model3)

ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(alpha = 0.3, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ x, aes(color = "Linear"), se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), aes(color = "Quadratic"), se = FALSE) +
  geom_line(aes(y = pred3, color = "Quadratic + PQI")) +
  scale_color_manual(values = c("Linear" = "blue", "Quadratic" = "red", "Quadratic + PQI" = "green" )) +
  labs(title = "Fitted Punt Models",
       x = "Starting Yard Line",
       y = "Next Yard Line",
       color = "Model")

#Test for quadratic
n = nrow(punts)
train_idx = sample(1:n, size = 0.8 * n)
train = punts[train_idx, ]
test  = punts[-train_idx, ]

model_train = lm(next_ydl ~ ydl + I(ydl^2), data = train)
rmse_test = rmse(test$next_ydl, predict(model_train, newdata = test))

rmse_test #rmse = 10.497


#From these graphs, it is clear that the quadratic model is better than the linear, 
#but it seems like there is little benefit to adding punter quality to the model



# Task 3:
# - Plot the fitted mean response for the selected punt model
# - Add a 95% confidence band for the expected response
# - Add a 95% prediction band for one individual punt
# - Explain why the prediction band is wider
# - Identify where the model is most uncertain

ydl_grid = data.frame(ydl = seq(min(punts$ydl), max(punts$ydl), length.out = 300))

conf_band = as.data.frame(predict(model2, newdata = ydl_grid, interval = "confidence", level = 0.95))
pred_band = as.data.frame(predict(model2, newdata = ydl_grid, interval = "prediction", level = 0.95))

plot_data = ydl_grid %>%
  mutate(fit    = conf_band$fit,
         ci_lwr = conf_band$lwr,
         ci_upr = conf_band$upr,
         pi_lwr = pred_band$lwr,
         pi_upr = pred_band$upr)

# Plot
ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(alpha = 0.2, color = "gray") +
  geom_ribbon(data = plot_data, aes(x = ydl, ymin = pi_lwr, ymax = pi_upr), 
              inherit.aes = FALSE, fill = "red", alpha = 0.2) +
  geom_ribbon(data = plot_data, aes(x = ydl, ymin = ci_lwr, ymax = ci_upr), 
              inherit.aes = FALSE, fill = "blue", alpha = 0.4) +
  geom_line(data = plot_data, aes(x = ydl, y = fit), color = "black", linewidth = 1) +
  labs(title = "Quadratic Punt Model with Confidence and Prediction Bands",
       x = "Starting Yard Line",
       y = "Opponent Yard Line After Punt")


#The prediction band is wider because it accounts for punter by punter variation.
#The model is most uncertain at the left and right extremes of the x-axis
#because there is less data to pull from

# Task 4:
# - Define punt yards over expected so that positive values are better punts
# - Compute PYOE for each punt
# - For each punter, compute average PYOE, number of punts, and standard error of average PYOE
# - Rank punters by average PYOE
# - Visualize punter rankings with uncertainty intervals
# - Identify which punters look clearly above average and which rankings are unstable

punts_pyoe <- punts %>%
  mutate(
    expected_next_ydl = predict(model2, newdata = punts),
    pyoe = expected_next_ydl - next_ydl
  )

punter_rankings = punts_pyoe %>%
  group_by(punter) %>%
  summarise(
    avg_pyoe = mean(pyoe, na.rm = TRUE),
    n_punts  = n(),
    se_pyoe  = sd(pyoe, na.rm = TRUE) / sqrt(n_punts)
  ) %>%
  arrange(desc(avg_pyoe)) %>%
  mutate(punter = factor(punter, levels = punter))

ggplot(punter_rankings, aes(x = avg_pyoe, y = punter)) +
  geom_point() +
  geom_errorbarh(aes(xmin = avg_pyoe - 1.96 * se_pyoe,
                     xmax = avg_pyoe + 1.96 * se_pyoe),
                 height = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Punter Rankings by PYOE with 95% Uncertainty Intervals",
       x = "Average Punt Yards Over Expected (PYOE)",
       y = "Punter")


#The punters with their entire CI to the right of the red line are clearly above
#average, those who have the prediction to the right are likely above average
#The punters with large confidence intervals are the unstable ones


# Final reflection:
# - Explain how adding columns changed what the model could fit
# - Explain when flexibility helped and when it could hurt
# - Interpret the residual standard error in this setting
# - Explain why prediction intervals are wider than confidence intervals
# - Note one coefficient, prediction, or ranking you would interpret cautiously


#Adding columns meant that there are now more variables we can consider, 
#allowing us to make more accurate models. The flexibility helped in the first 
#part, but made things a bit weird in the second when considering punter ratings.
#Sigma hat tells us the typical size of prediction errors. Prediction intervals 
#are wider because confidence intervals only captures uncertainty about the 
#average response. I would be cautious about interpreting the coefficients in front
#of punter rankings too seriously because of variation in number of punts. 




