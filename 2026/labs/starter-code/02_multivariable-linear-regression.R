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

# Task 2:
# - Fit the multivariable model: wins ~ x1 + x2 + x3 + x4
# - Write down the fitted regression equation
# - Interpret each coefficient in context
# - Check whether the coefficient signs make sense given the variable definitions
# - Identify which factors look strongest and weakest after adjustment

# Task 3:
# - Standardize the four predictors
# - Fit the standardized model
# - Rank the factors by absolute standardized coefficient size
# - Compare the original and standardized models for interpretability
# - Compare fitted values from both models and explain why they match or differ

# Task 4:
# - Report the residual standard error and interpret it in wins
# - Report coefficient standard errors and 95% confidence intervals
# - Identify which effects are clearly different from zero
# - Choose one team and compute a point prediction, confidence interval, and prediction interval
# - State which interval is wider and why

# Task 5:
# - Randomly split the data into training and test sets
# - Fit the original and standardized models on the training set
# - Compute test-set RMSE for both models
# - Compare predictive performance

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

# Task 2:
# - Fit competing punt models: linear, quadratic, quadratic plus punter quality, and spline
# - Visualize the fitted curves from each model
# - Use train/test RMSE or cross-validation to choose a preferred model
# - Compare the linear, quadratic, and spline tradeoffs
# - Assess whether punter quality improves out-of-sample prediction
# - Interpret the punter-quality coefficient if it is included in the selected model

# Task 3:
# - Plot the fitted mean response for the selected punt model
# - Add a 95% confidence band for the expected response
# - Add a 95% prediction band for one individual punt
# - Explain why the prediction band is wider
# - Identify where the model is most uncertain

# Task 4:
# - Define punt yards over expected so that positive values are better punts
# - Use PYOE = y - expected
# - Compute PYOE for each punt
# - For each punter, compute average PYOE, number of punts, and standard error of average PYOE
# - Rank punters by average PYOE
# - Visualize punter rankings with uncertainty intervals
# - Identify which punters look clearly above average and which rankings are unstable

# Final reflection:
# - Explain how adding columns changed what the model could fit
# - Explain when flexibility helped and when it could hurt
# - Interpret the residual standard error in this setting
# - Explain why prediction intervals are wider than confidence intervals
# - Note one coefficient, prediction, or ranking you would interpret cautiously
