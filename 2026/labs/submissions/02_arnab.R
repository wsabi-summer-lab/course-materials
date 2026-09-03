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

nba_four_factors = nba_four_factors %>%
  mutate(
    eFGdiff = `EFG%`- `OPP EFG%`,
    x2Rebs= `OREB%`+ `DREB%`-100,
    TOVDiff = `OPP TOV %`- `TOV%`,
    ftdiff =  `FT Rate` - `OPP FT Rate`
  )
summary(nba_four_factors['eFGdiff'])
summary(nba_four_factors['x2Rebs'])
summary(nba_four_factors['TOVDiff'])
summary(nba_four_factors['ftdiff'])

nba_four_factors %>%
  select(eFGdiff, x2Rebs, TOVDiff, ftdiff) %>%
  pivot_longer(everything(),
               names_to = "Variable",
               values_to = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Density Plots of Four Factors")

ggplot(nba_four_factors, aes(x = eFGdiff, y = W)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(nba_four_factors, aes(x = x2Rebs, y = W)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(nba_four_factors, aes(x = TOVDiff, y = W)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(nba_four_factors, aes(x = ftdiff, y = W)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

print(cor(nba_four_factors$W, nba_four_factors$eFGdiff))
print(cor(nba_four_factors$W, nba_four_factors$x2Rebs))
print(cor(nba_four_factors$W, nba_four_factors$TOVDiff))
print(cor(nba_four_factors$W, nba_four_factors$ftdiff))

#####eFGdiff seems to be most strongly correlated with wins. 


# Task 2:
# - Fit the multivariable model: wins ~ x1 + x2 + x3 + x4
# - Write down the fitted regression equation
# - Interpret each coefficient in context
# - Check whether the coefficient signs make sense given the variable definitions
# - Identify which factors look strongest and weakest after adjustment

WinModel = lm(W~ eFGdiff + x2Rebs + TOVDiff + ftdiff, data = nba_four_factors)
coef(WinModel)
##The fitted equation is wins = 40.186725 + 3.674911x1 + 1.340316x2 + 3.058813x3 + 77.069048x4
#Provides a baseline of 40 wins with the intercept, with an approximate of 4 wins added in an increase of 1% in eFG differential.
# approx 1 win is added per 1% increase in either defensive or offensive rebounding percentage. 
#approx 3 wins are added per 1% increase in turnover differential.
#approx .7 wins are added with a 1% increase in free throw rate differential

# Task 3:
# - Standardize the four predictors
# - Fit the standardized model
# - Rank the factors by absolute standardized coefficient size
# - Compare the original and standardized models for interpretability
# - Compare fitted values from both models and explain why they match or differ

nba_four_factors <- nba_four_factors %>%
  mutate(
    eFGdiff_z = as.numeric(scale(eFGdiff)),
    x2Rebs_z = as.numeric(scale(x2Rebs)),
    TOVDiff_z = as.numeric(scale(TOVDiff)),
    ftdiff_z = as.numeric(scale(ftdiff))
  )

model_std <- lm(
  W ~ eFGdiff_z + x2Rebs_z + TOVDiff_z + ftdiff_z,
  data = nba_four_factors
)

summary(model_std)
#eFG differential, turnover differential, rebounding, free throw rate differential
#for interpretability, the original model is probably better given that a direct 1% increase would indicate the effect on wins, whereas here it is a 1 standard deviation increase. Though for comparing between predictors, the standardized is more interpretable. 
#The baseline wins (intercept) stayed about the same, while the impact of eFG was much greater here, which I think makes sense given the correlations examined earlier
#The rebounding ability relatively seems to have the same impact on wins, as does turnover differential, while free throw rate had a greater one. 


# Task 4:
# - Report the residual standard error and interpret it in wins
# - Report coefficient standard errors and 95% confidence intervals
# - Identify which effects are clearly different from zero
# - Choose one team and compute a point prediction, confidence interval, and prediction interval
# - State which interval is wider and why

#Residual standard error is 3.977, indicating predictions are off on average by around 3.977 wins. 
#std error: x1 = .1896, x2 = .1905, x3 = .1919, x4 = .1908
confint(model_std, level = 0.95)
############### 2.5 %    97.5 %
#(Intercept) 39.784934 40.498399
#eFGdiff_z    9.762035 10.506983
#x2Rebs_z     3.266077  4.014824
#TOVDiff_z    4.063610  4.817913
#ftdiff_z     1.911685  2.661586

row_idx = 424
selected_row = nba_four_factors[row_idx, ]
fitted_value = predict(model_std, newdata = selected_row)
ci_model_b = predict(model_std, newdata = selected_row, interval = "confidence", level = 0.95)
pi_model_b = predict(model_std, newdata = selected_row, interval = "prediction", level = 0.95)
print(fitted_value)
print(ci_model_b)
print(pi_model_b)
#Prediction interval is wider, as it is predicting where any future value would land, not just the average. 

# Task 5:
# - Randomly split the data into training and test sets
# - Fit the original and standardized models on the training set
# - Compute test-set RMSE for both models
# - Compare predictive performance

n <- nrow(nba_four_factors)

train_indices <- sample(1:n, size = 0.8 * n)

train_data <- nba_four_factors[train_indices, ]
test_data  <- nba_four_factors[-train_indices, ]

WinModel1 <- lm(
  W ~ eFGdiff + x2Rebs + TOVDiff + ftdiff,
  data = train_data
)

model_std1 <- lm(
  W ~ eFGdiff_z + x2Rebs_z + TOVDiff_z + ftdiff_z,
  data = train_data
)

pred_win <- predict(WinModel1, newdata = test_data)
pred_std <- predict(model_std1, newdata = test_data)

rmse_win <- sqrt(mean((test_data$W - pred_win)^2))
rmse_std <- sqrt(mean((test_data$W - pred_std)^2))

rmse_win
rmse_std
#The RMSE's are equal. 


##############
### PART 2 ###
##############

# load data
punts = read_csv("../data/02_punts.csv.gz")

# Task 1:
# - Plot post-punt yard line against starting yard line
# - Bin punts by starting field position and plot average post-punt yard line in each bin
# - Describe the shape of the relationship and where it bends
# - Plot or summarize the distribution of punter quality

ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point() 

binned <- punts %>%
  filter(!is.na(ydl), !is.na(next_ydl)) %>%
  mutate(
    ydl_bin = cut(
      ydl,
      breaks = seq(0, 100, by = 10),
      include.lowest = TRUE,
      right = TRUE
    )
  ) %>%
  filter(!is.na(ydl_bin)) %>%
  group_by(ydl_bin) %>%
  summarize(
    avg_next_ydl = mean(next_ydl),
    n = n(),
    .groups = "drop"
  )
binned
ggplot(binned, aes(x = ydl_bin, y = avg_next_ydl, group = 1)) +
  geom_point(size = 2) +
  geom_line() +
  labs(
    x = "Yard line bin",
    y = "Average next yard line",
    title = "Average Next YDL by Current YDL Bin"
  ) +
  theme_minimal()
##it is more parabolic,and really starts to bend after the 40,50 bin. 

summary(punts$pq)

# Task 2:
# - Fit competing punt models: linear, quadratic, quadratic plus punter quality 
lm_model = lm(next_ydl ~ ydl, data = punts)

quad_model = lm(next_ydl ~ ydl + I(ydl^2), data = punts)

quad2_model = lm(next_ydl ~ ydl + I(ydl^2) + pq, data = punts)
punts$fit_lm <- fitted(lm_model)
punts$fit_quad <- fitted(quad_model)
punts$fit_quad2 <- fitted(quad2_model)


# - Visualize the fitted curves from each model

ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(y = fit_lm, color = "Linear")) +
  geom_line(aes(y = fit_quad, color = "Quadratic")) +
  geom_line(aes(y = fit_quad2, color = "Quadratic + PQ")) +
  labs(
    title = "Fitted Punt Models",
    x = "ydl",
    y = "next_ydl",
    color = "Model"
  ) +
  theme_minimal()

# - Use train/test RMSE or cross-validation to choose a preferred model
n <- nrow(punts)

train_indices <- sample(1:n, size = 0.8 * n)

train_data <- punts[train_indices, ]
test_data  <- punts[-train_indices, ]

lm_model_train = lm(next_ydl ~ ydl, data = train_data)

quad_model_train = lm(next_ydl ~ ydl + I(ydl^2), data = train_data)

quad2_model_train = lm(next_ydl ~ ydl + I(ydl^2) + pq, data = train_data)

pred_lm <- predict(lm_model_train, newdata = test_data)
pred_quad <- predict(quad_model_train, newdata = test_data)
pred_quad2 <- predict(quad2_model_train, newdata = test_data)

rmse_lm <- sqrt(mean((test_data$next_ydl - pred_lm)^2))
rmse_quad <- sqrt(mean((test_data$next_ydl - pred_quad)^2))
rmse_quad2 <- sqrt(mean((test_data$next_ydl - pred_quad2)^2))

rmse_lm
rmse_quad
rmse_quad2

# the model using quadratic plus punter quality has the lowest RMSE.

# - Compare the linear, quadratic, tradeoffs
#The tradeoff here is that you're adding a more flexible model in return for reduced basic interpretability. 

# - Assess whether punter quality improves out-of-sample prediction

# Based on a lower RMSE with punter quality, there is an argument that it does improve prediction. 

# - Interpret the punter-quality coefficient if it is included in the selected model

summary(quad2_model)

#an increase of .1 in punter quality leads to an increase of .1258 yards for the punt landing position.  

# Task 3:
# - Plot the fitted mean response for the selected punt model
# - Add a 95% confidence band for the expected response
# - Add a 95% prediction band for one individual punt
# - Explain why the prediction band is wider
# - Identify where the model is most uncertain

ggplot(punts, aes(x = ydl, y = fit_quad2)) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "ydl",
    y = "Fitted mean response",
    title = "Fitted Mean Response from Quadratic + Punter Quality Model"
  ) +
  theme_minimal()

row_idx = 216
selected_row = punts[row_idx, ]
ci_model_quad = predict(quad2_model, newdata = punts, interval = "confidence", level = 0.95)
pi_model_quad = predict(quad2_model, newdata = selected_row, interval = "prediction", level = 0.95)
print(ci_model_quad)
print(pi_model_quad)
#prediction intervals are wider because it isn't determining with 95% confidence where the average will be, but rather an interval where any future value will land.
#Model seems most uncertain around the middle of the field, where the most difference is seen on the plot. 

# Task 4:
# - Define punt yards over expected so that positive values are better punts
# the expected landing position of a punt minus the actual.

# - Compute PYOE for each punt
punts <- punts %>%
  mutate(
    expected_next_ydl = fitted(quad2_model),
    PYOE = expected_next_ydl - next_ydl
  )

# - For each punter, compute average PYOE, number of punts, and standard error of average PYOE
# - Rank punters by average PYOE
punter_summary <- punts %>%
  group_by(punter) %>%
  summarize(
    avg_PYOE = mean(PYOE, na.rm = TRUE),
    n = n(),
    sd_PYOE = sd(PYOE, na.rm = TRUE),
    se_avg_PYOE = sd_PYOE / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_PYOE))

punter_summary

# - Visualize punter rankings with uncertainty intervals
ggplot(
  punter_summary %>%
    mutate(punter = reorder(punter, avg_PYOE)),
  aes(x = avg_PYOE, y = punter)
) +
  geom_point(size = 2) +
  geom_errorbarh(
    aes(xmin = avg_PYOE - 1.96 * se_avg_PYOE, xmax = avg_PYOE + 1.96 * se_avg_PYOE),
    height = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Punter Rankings with 95% Confidence Intervals",
    x = "Average PYOE",
    y = "Punter"
  ) +
  theme_minimal()

# - Identify which punters look clearly above average and which rankings are unstable
#ben roethlisberger had one punt, so unlikely to be stable. There are many punters with a higher average PYOE that have huge confidence intervals that are likely unstable.


# Final reflection:
# - Explain how adding columns changed what the model could fit
#it offered more opportunities to modify data that could be better used for a model. 
# - Explain when flexibility helped and when it could hurt
#flexibility helped in being able to modify a linear model of a line, to a better fit in a quadratic. This made it harder to conceptualize though, and interpret the effects of a predictor. It improves predictive accuracy. Also, flexbility could lead to overfitting to data that isn't representative of the population.
# - Interpret the residual standard error in this setting
# Measures how far off actual punts were from the prediction, on average.  
# - Explain why prediction intervals are wider than confidence intervals
# Prediction intervals are always wider given that they provide a range where most random future values would fit within it. The confidence interval seeks out a range for where the average of more sets would fit in.
# - Note one coefficient, prediction, or ranking you would interpret cautiously
# I would be cautious about interpreting the intercept, as it's not a indication or a guarantee of punt yards, rather something that happens to be implemented in the model.
