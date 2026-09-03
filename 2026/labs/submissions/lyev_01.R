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

# TODO:
# 1) Fit no-intercept model: logit_wp ~ 0 + log_rs_ra
# 2) Extract alpha estimate and a 95% confidence interval
# 3) Build helper to convert RS/RA + alpha -> predicted WP
# 4) Compare fitted-alpha predictions vs Bill James alpha = 2

#Lyev to-do

# 1) Fit no-intercept model to model alpha

model1 = lm(data = pythag_data, logit_wp ~ 0 + log_rs_ra)
summary(model1)
alpha = model1$coefficients["log_rs_ra"]


# 2) Compare fitted Model to alpha = 2

#Compare numbers
pythag_data = pythag_data %>%
  mutate(WP_Pythag_modeled = (RS^alpha)/(RS^alpha+RA^alpha))

RMSE_james <- sqrt(mean((pythag_data$WP - pythag_data$WP_Pythag_2)^2))
RMSE_james # = 0.0292717
RMSE_lyev <- sqrt(mean((pythag_data$WP - pythag_data$WP_Pythag_modeled)^2))
RMSE_lyev # = 0.02781082
# The new model has a smaller RMSE


#Create plots

james_plot = ggplot(pythag_data, aes(x = WP, y = WP_Pythag_2)) +
  geom_point(color = "dodgerblue", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.2) +
  labs(
    x = "Actual WP",
    y = "Predicted WP (Pythag 2)",
    title = "Actual vs Predicted WP (Bill James)"
  ) +
  theme_minimal()
james_plot

model_plot = ggplot(pythag_data, aes(x = WP, y = WP_Pythag_modeled)) +
  geom_point(color = "dodgerblue", size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.2) +
  labs(
    x = "Actual WP",
    y = "Predicted WP (Model Pythag)",
    title = "Actual vs Predicted WP (Model)"
  ) +
  theme_minimal()
model_plot

grid.arrange(james_plot, model_plot, ncol = 2)


# 3) Report uncertainty for alpha estimate

# alpha hat = 1.79972 =~ 1.8
# standard error = 0.04987 =~ 0.05
confint(model1, level = 0.95)
# confidence interval is [1.701175 , 1.898261]
# Based on this interval, alpha = 2 is NOT plausible


# 4) Make residual diagnostic plot for fitted Pythagorean model

residual_model = ggplot(model1, aes(.fitted, .resid)) +
  geom_point(color = "dodgerblue", size = 2) +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()
residual_model

# If the model is reasonable, you'd expect a cloud with no particular patterns, which is what we see.




##############
### PART 2 ###
##############

# load payroll data
mlb_payrolls = read_csv("../data/01_mlb-payrolls.csv.gz")

# remove 2020 covid-shortened season
payroll_data = mlb_payrolls %>%
  filter(year_id != 2020)

# TODO:
# 1) Fit model A: wp ~ payroll_median_ratio
# 2) Fit model B: wp ~ log_payroll_median_ratio
# 3) Compare fits (residual behavior, RMSE, and interpretation)
# 4) Compute confidence and prediction intervals for a selected team-season

#Lyev to-do

#Task 1

plot1 = payroll_data %>%
  mutate(
    highlight = ifelse(
      name %in% c("Oakland Athletics", "New York Yankees"),
      name,
      "Other"
    )
  ) %>%
  ggplot(aes(x = payroll_median_ratio, y = wp)) +
  geom_point(aes(color = highlight), size = 3) +
  scale_color_manual(
    values = c(
      "Oakland Athletics" = "green3",
      "New York Yankees" = "navy",
      "Other" = "gray70"
    )
  ) +
  labs(
    x = "Payroll Median Ratio",
    y = "Winning Percentage (WP)",
    color = "Team",
    title = "WP vs Payroll Median Ratio with A’s and Yankees Highlighted"
  ) +
  theme_minimal()
plot1


lin_model = lm(data = payroll_data, wp ~ payroll_median_ratio)
log_model = lm(data = payroll_data, wp ~ log_payroll_median_ratio)

line_plot = payroll_data %>%
  mutate(
    highlight = ifelse(
      name %in% c("Oakland Athletics", "New York Yankees"),
      name,
      "Other"
    )
  ) %>%
  ggplot(aes(x = payroll_median_ratio, y = wp)) +
  geom_point(aes(color = highlight), size = 3) +
  scale_color_manual(
    values = c(
      "Oakland Athletics" = "green3",
      "New York Yankees" = "navy",
      "Other" = "gray70"
    )
  ) +
  
  # Regression line: WP ~ payroll_median_ratio
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
  
  # Regression line: WP ~ log(payroll_median_ratio)
  geom_smooth(
    method = "lm",
    formula = y ~ log(x),
    se = FALSE,
    color = "red",
    linewidth = 1
  ) +
  
  labs(
    x = "Payroll Median Ratio",
    y = "Winning Percentage (WP)",
    color = "Team",
    title = "WP vs Payroll Median Ratio with Highlighted Teams and Regression Lines"
  ) +
  theme_minimal()
line_plot

#the log plot seems to fit the data better


#Task 2

payroll_data <- payroll_data %>%
  mutate(
    pred_lin = predict(lin_model),
    pred_log = predict(log_model),
    lin_diff = wp - pred_lin,
    log_diff = wp - pred_log
  )

lin_plot = ggplot(payroll_data, aes(x = pred_lin, y = lin_diff)) +
  geom_point(color = "dodgerblue", size = 2) +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  labs(
    x = "Fitted Values (Linear Model)",
    y = "Residuals",
    title = "Residuals vs Fitted Values — Linear Model"
  ) +
  theme_minimal()
lin_plot
#The spread seems slightly more bottom-heavy but otherwise pretty random


log_plot = ggplot(payroll_data, aes(x = pred_log, y = log_diff)) +
  geom_point(color = "purple", size = 2) +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  labs(
    x = "Fitted Values (Log Model)",
    y = "Residuals",
    title = "Residuals vs Fitted Values — Log Model"
  ) +
  theme_minimal()
log_plot
#The spread also seems slightly more bottom-heavy but otherwise pretty random


#Class by team
lin_team_distance_summary <- payroll_data %>%
  group_by(team_id) %>%
  summarize(avg_lin_diff = 162 * mean(lin_diff, na.rm = TRUE)) %>%
  arrange(desc(avg_lin_diff))

lin_bar_graph = ggplot(lin_team_distance_summary,
                       aes(y = reorder(team_id, avg_lin_diff), x = avg_lin_diff)) +
  geom_col(fill = "dodgerblue") +
  coord_flip() +
  labs(
    x = "Linear Distance",
    y = "Team",
    title = "Average Distance per Team (Ordered by Distance)"
  ) +
  theme_minimal()
lin_bar_graph


log_team_distance_summary <- payroll_data %>%
  group_by(team_id) %>%
  summarize(avg_log_diff = 162 * mean(log_diff, na.rm = TRUE)) %>%
  arrange(desc(avg_log_diff))

log_bar_graph = ggplot(log_team_distance_summary,
                       aes(y = reorder(team_id, avg_log_diff), x = avg_log_diff)) +
  geom_col(fill = "dodgerblue") +
  coord_flip() +
  labs(
    x = "Log Distance",
    y = "Team",
    title = "Average Distance per Team (Ordered by Distance)"
  ) +
  theme_minimal()
log_bar_graph


# Task 3: I picked 2017 Los Angeles Dodgers

lad_2017 <- payroll_data %>%
  filter(year_id == 2017, team_id == "LAN")

lad_2017_log_prediction = predict(log_model, newdata = lad_2017)
lad_2017_log_prediction # = 0.5469089 (their actual was 0.6419753)

#Confidence interval
predict(log_model, newdata = lad_2017, interval = "confidence")
# [0.5384459 , 0.5553719]

#Prediction interval
predict(log_model, newdata = lad_2017, interval = "prediction")
# [0.4139004 , 0.6799174]

#The prediction interval is much wider, because for a single data point there are many more factors of variance that can affect it.
