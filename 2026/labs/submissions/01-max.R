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
library(here)
mlb_team_seasons = read_csv(here("2026/labs/data/01_mlb-team-seasons.csv.gz"))

# transform variables used in the Pythagorean exponent model
pythag_data = mlb_team_seasons %>%
  mutate(
    logit_wp = log(WP / (1 - WP)),
    log_rs_ra = log(RS / RA)
  )

# fit the no-intercept regression that estimates alpha
model = lm(logit_wp ~ 0 + log_rs_ra, data = pythag_data)
model

#fitted alpha coefficient ~ 1.8
alpha = coef(model)[["log_rs_ra"]]

# pythagorean win-percentage fucntion
pythag_wp = function(rs, ra, alpha) {
  rs^alpha / (rs^alpha + ra^alpha)
}

pythag_data = pythag_data %>%
  mutate(
    wp_hat_alpha = pythag_wp(RS, RA, alpha),
    wp_hat_2 = pythag_wp(RS, RA, 2)
  )

#RMSE - computed alpha gives a lower value (0.028 vs. 0.029)
RMSE_alpha = sqrt(mean((pythag_data$WP - pythag_data$wp_hat_alpha)^2))
RMSE_2 = sqrt(mean((pythag_data$WP - pythag_data$wp_hat_2)^2))
RMSE_alpha
RMSE_2

# actual vs. predicted win percentage
ggplot(data = pythag_data, aes(x = WP, y = wp_hat_alpha)) + 
  geom_point(alpha = 0.4) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") + 
  labs(
    title = "Actual vs. Predicted Win Percentage (computed alpha)",
    x = "Actual win percentage",
    y = "Predicted win percentage"
    )
ggplot(data = pythag_data, aes(x = WP, y = wp_hat_2)) + 
  geom_point(alpha = 0.4) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") + 
  labs(
    title = "Actual vs. Predicted Win Percentage (alpha = 2)",
    x = "Actual win percentage",
    y = "Predicted win percentage"
  )

# confidence interval is [1.7, 1.9], so 2 is outside range
alpha_se = summary(model)$coefficients["log_rs_ra", "Std. Error"]
alpha_ci_95 = alpha + c(-1, 1) * qt(0.975, df = df.residual(model)) * alpha_se
alpha_se
alpha_ci_95

pythag_data = pythag_data %>%
  mutate(
    resid_alpha = WP-wp_hat_alpha
  )

#would expect to see flat, patternless band around zero, curve looks relatively flat with no clear curvature
ggplot(data = pythag_data, aes(x = wp_hat_alpha, y = resid_alpha)) + 
  geom_point(alpha = 0.4) + 
  geom_abline(slope = 0, intercept = 0, linetype = "dashed", color = "red") + 
  labs(
      title = "Residual Diagnostic Plot of Fitted Pythagorean Model",
      x = "Fitted win percentage",
      y = "Residual (actual - predicted)"
    )

##############
### PART 2 ###
##############

# load payroll data
mlb_payrolls = read_csv(here("2026/labs/data/01_mlb-payrolls.csv.gz"))

# remove 2020 covid-shortened season
payroll_data = mlb_payrolls %>%
  filter(year_id != 2020)

#regress on payroll and log payroll
wp_pay_reg = lm(wp ~ payroll_median_ratio, data = payroll_data)
wp_log_pay_reg = lm(wp ~ log_payroll_median_ratio, data = payroll_data)

#plot wp vs. payroll, red = yankees, athletics
ggplot(data = payroll_data, aes(x = payroll_median_ratio, y = wp)) +
  geom_point(data = filter(payroll_data, !team_id %in% c("NYA", "OAK")), alpha = 0.4) +
  geom_point(data = filter(payroll_data, team_id %in% c("NYA", "OAK")), alpha = 0.4, color = "red") +
  geom_abline(slope = coef(wp_pay_reg)[["payroll_median_ratio"]],
              intercept = coef(wp_pay_reg)[["(Intercept)"]], color = "blue") + 
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "green")+
  labs(
    title = "Win Percentage vs. Payroll",
    x = "median payroll",
    y = "Win percentage"
  )

#Log curve seems to match the shape of the data better to me, capturing steep curve on left and diminishing returns on right

# add fitted values and residuals from both payroll models
payroll_data = payroll_data %>%
  mutate(
    fitted_pay = fitted(wp_pay_reg),
    fitted_log_pay = fitted(wp_log_pay_reg),
    resid_pay = wp-fitted(wp_pay_reg),
    resid_log_pay = wp-fitted(wp_log_pay_reg)
  )
# residuals vs. fitted, linear-payroll model
ggplot(data = payroll_data, aes(x = fitted_pay, y = resid_pay)) + 
  geom_point(alpha = 0.4) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
      title = "Residuals vs. Fitted: Linear Payroll Model",
      x = "Fitted win percentage",
      y = "Residual"
    )
# residuals vs. fitted, log-payroll model
ggplot(data = payroll_data, aes(x = fitted_log_pay, y = resid_log_pay)) + 
  geom_point(alpha = 0.4) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    title = "Residuals vs. Fitted: Log Payroll Model",
    x = "Fitted win percentage",
    y = "Residual"
  )
# both clouds are centered near zero with fairly even spread

# average residual per franchise across all seasons
team_diff = payroll_data %>%
  group_by(team_id) %>%
    summarise(
      avg_diff_pay = mean(resid_pay),
      avg_diff_log_pay = mean(resid_log_pay)
  )
# average wins above/below payroll expectation per team, linear model
ggplot(data = team_diff, aes(x = reorder(team_id, avg_diff_pay), y = avg_diff_pay * 162)) + 
  geom_col() + 
  coord_flip() +
  labs(x = "Team", y = "Wins") +
  labs(
    title = "Wins Above/Below Payroll Expectation: Linear Model",
    x = "Team",
    y = "Average wins vs. expectation (per 162 games)"
  )
# average wins above/below payroll expectation per team, log model
ggplot(data = team_diff, aes(x = reorder(team_id, avg_diff_log_pay), y = avg_diff_log_pay * 162)) +
  geom_col() + 
  coord_flip() +
  labs(x = "Team", y = "Wins") + 
  labs(
    title = "Wins Above/Below Payroll Expectation: Log Model",
    x = "Team",
    y = "Average wins vs. expectation (per 162 games)"
  )

#picked team: NY yankees, 2015
nyy_15 = payroll_data %>%
  filter(year_id == 2015, team_id == "NYA")
nyy_15
predict(wp_log_pay_reg, newdata = nyy_15)
# = 0.55
predict(wp_log_pay_reg, newdata = nyy_15, interval = "confidence")
# mean CI = [0.54, 0.56]
predict(wp_log_pay_reg, newdata = nyy_15, interval = "prediction")
# team PI= [0.42, 0.68]
#prediction interval is wider because there is additional residual variance for predicting individual team performance on top of uncertainty in the mean

