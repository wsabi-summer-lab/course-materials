#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

##############
### PART 1 ###
##############

# load team-season data
mlb_team_seasons = read_csv("../data/01_mlb-team-seasons.csv.gz")

# transform variables used in the Pythagorean exponent model
pythag_data = mlb_team_seasons %>%
  mutate(
    logit_wp = log(WP / (1 - WP)),
    log_rs_ra = log(RS / RA)
  )

# Task 1:
# - Fit the no-intercept regression: logit_wp ~ 0 + log_rs_ra
# - Extract the fitted exponent alpha_hat

# Task 2:
# - Build predictions for:
#   1) your fitted alpha model
#   2) Bill James model with alpha = 2
# - Compute RMSE for both models
# - Report which model has smaller RMSE
# - Make a plot of actual WP vs predicted WP for both models
# - Add a 45-degree line and compare model accuracy visually

# Task 3:
# - Report alpha_hat, its standard error, and a 95% confidence interval
# - Interpret the confidence interval in words
# - State whether alpha = 2 is plausible from your interval

# Task 4:
# - Make a residuals-vs-fitted plot for your fitted Pythagorean model
# - Briefly discuss curvature, outliers, and changing spread

##############
### PART 2 ###
##############

# load payroll data
mlb_payrolls = read_csv("../data/01_mlb-payrolls.csv.gz")

# remove 2020 covid-shortened season
payroll_data = mlb_payrolls %>%
  filter(year_id != 2020)

# fit both models used throughout Part 2
model_a = lm(wp ~ payroll_median_ratio, data = payroll_data)
model_b = lm(wp ~ log_payroll_median_ratio, data = payroll_data)

# Task 1:
# - Plot wp vs payroll_median_ratio
# - Highlight Oakland A's and New York Yankees
# - Add Model A and Model B regression lines on the same figure
# - Compare and explain which fit looks better

# Task 2:
# - Add fitted values and residual columns for Model A and Model B
# - Make residuals-vs-fitted plots for both models
# - Briefly discuss curvature, outliers, and changing spread
# - Compute average residual by team for each model
# - Make two ordered graphs (highest to lowest average residual), one per model
# - Convert y-axis to wins by multiplying residuals by 162
# - Add a legend to the graphs

# Task 3:
# - Pick one team-season
# - Compute fitted wp, confidence interval for mean wp, and prediction interval
# - State which interval is wider and why
