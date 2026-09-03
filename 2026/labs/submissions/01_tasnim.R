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
mlb_team_seasons <- read_csv("~/Desktop/01_mlb-team-seasons.csv.gz")

# transform variables used in the Pythagorean exponent model
pythag_data = mlb_team_seasons %>%
  mutate(
    logit_wp = log(WP / (1 - WP)),
    log_rs_ra = log(RS / RA)
  )

# Task 1:
# - Fit the no-intercept regression: logit_wp ~ 0 + log_rs_ra
model_pythag = lm(logit_wp ~ 0 + log_rs_ra, data = pythag_data)

# - Extract the fitted exponent alpha_hat
alpha_hat = coef(model_pythag)[["log_rs_ra"]]

# Task 2:
# - Build predictions for:
#   1) your fitted alpha model
#   2) Bill James model with alpha = 2
pythag_data = pythag_data %>%
  mutate(
    pred_alpha = exp(alpha_hat * log_rs_ra) /
      (1 + exp(alpha_hat * log_rs_ra)),
    
    pred_james = RS^2 / (RS^2 + RA^2)
  )
# - Compute RMSE for both models
rmse_alpha = sqrt(mean((pythag_data$WP - pythag_data$pred_alpha)^2))
rmse_james = sqrt(mean((pythag_data$WP - pythag_data$pred_james)^2))

rmse_alpha
rmse_james
# - Report which model has smaller RMSE
if (rmse_alpha < rmse_james) {
  print("The alpha model has smaller RMSE.")
} else {
  print("The Bill James model has smaller RMSE.")
}

# - Make a plot of actual WP vs predicted WP for both models
# - Add a 45-degree line and compare model accuracy visually
plot_data = pythag_data %>%
  select(WP, pred_alpha, pred_james) %>%
  pivot_longer(
    cols = c(pred_alpha, pred_james),
    names_to = "model",
    values_to = "predicted_WP"
  )

ggplot(plot_data, aes(x = predicted_WP, y = WP, color = model)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Predicted Winning Percentage",
    y = "Actual Winning Percentage",
    color = "Model",
    title = "Actual WP vs Predicted WP"
  )
# Task 3:
# - Report alpha_hat, its standard error, and a 95% confidence interval
# - Interpret the confidence interval in words
# - State whether alpha = 2 is plausible from your interval
summary(model_pythag)

alpha_se = summary(model_pythag)$coefficients["log_rs_ra", "Std. Error"]
alpha_ci = confint(model_pythag)

alpha_hat
alpha_se
alpha_ci

# Task 4:
# - Make a residuals-vs-fitted plot for your fitted Pythagorean model
# - Briefly discuss curvature, outliers, and changing spread
pythag_data = pythag_data %>%
  mutate(
    fitted_logit = fitted(model_pythag),
    residual = resid(model_pythag)
  )

ggplot(pythag_data, aes(x = fitted_logit, y = residual)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = "Residuals vs Fitted: Pythagorean Model"
  )
##############
### PART 2 ###
##############

# load payroll data
mlb_payrolls <- read_csv("~/Desktop/01_mlb-payrolls.csv.gz")

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
payroll_plot_data = payroll_data %>%
  mutate(
    highlight_team = case_when(
      team_id == "OAK" ~ "Oakland A's",
      team_id == "NYA" ~ "New York Yankees",
      TRUE ~ "Other"
    )
  )

grid_data = tibble(
  payroll_median_ratio = seq(
    min(payroll_data$payroll_median_ratio),
    max(payroll_data$payroll_median_ratio),
    length.out = 200
  )
) %>%
  mutate(
    log_payroll_median_ratio = log(payroll_median_ratio)
  ) %>%
  mutate(
    pred_model_a = predict(model_a, newdata = .),
    pred_model_b = predict(model_b, newdata = .)
  )

ggplot(payroll_plot_data, aes(x = payroll_median_ratio, y = wp)) +
  geom_point(aes(color = highlight_team), alpha = 0.7) +
  geom_line(
    data = grid_data,
    aes(y = pred_model_a, linetype = "Model A: raw payroll"),
    color = "black"
  ) +
  geom_line(
    data = grid_data,
    aes(y = pred_model_b, linetype = "Model B: log payroll"),
    color = "black"
  ) +
  labs(
    x = "Payroll / Median Payroll",
    y = "Winning Percentage",
    color = "Team",
    linetype = "Model",
    title = "Winning Percentage vs Payroll"
  )
# Task 2:
# - Add fitted values and residual columns for Model A and Model B
payroll_data = payroll_data %>%
  mutate(
    fitted_a = fitted(model_a),
    residual_a = resid(model_a),
    fitted_b = fitted(model_b),
    residual_b = resid(model_b)
  )

# - Make residuals-vs-fitted plots for both models
ggplot(payroll_data, aes(x = fitted_a, y = residual_a)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Fitted WP",
    y = "Residual",
    title = "Residuals vs Fitted: Model A"
  )

ggplot(payroll_data, aes(x = fitted_b, y = residual_b)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Fitted WP",
    y = "Residual",
    title = "Residuals vs Fitted: Model B"
# - Briefly discuss curvature, outliers, and changing spread

# - Compute average residual by team for each model
# - Make two ordered graphs (highest to lowest average residual), one per model
# - Convert y-axis to wins by multiplying residuals by 162
# - Add a legend to the graphs
avg_resid_a = payroll_data %>%
  group_by(team_id) %>%
  summarize(avg_resid_wins = mean(residual_a) * 162)

ggplot(avg_resid_a, aes(
  x = reorder(team_id, avg_resid_wins),
  y = avg_resid_wins,
  fill = "Model A"
)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Team",
    y = "Average Residual in Wins",
    fill = "Model",
    title = "Average Team Residuals: Model A"
  )

avg_resid_b = payroll_data %>%
  group_by(team_id) %>%
  summarize(avg_resid_wins = mean(residual_b) * 162)

ggplot(avg_resid_b, aes(
  x = reorder(team_id, avg_resid_wins),
  y = avg_resid_wins,
  fill = "Model B"
)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Team",
    y = "Average Residual in Wins",
    fill = "Model",
    title = "Average Team Residuals: Model B"
  )
# Task 3:
# - Pick one team-season
team_season = payroll_data[1, ]
# - Compute fitted wp, confidence interval for mean wp, and prediction interval
# - State which interval is wider and why
predict(model_b, newdata = team_season)

predict(
  model_b,
  newdata = team_season,
  interval = "confidence"
)

predict(
  model_b,
  newdata = team_season,
  interval = "prediction"
)

