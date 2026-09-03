#############
### SETUP ###
#############

#install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

##############
### PART 1 ###
##############

# load data
mlb_team_seasons = read_csv("../data/01_mlb-team-seasons.csv.gz")

# transformed regression data for estimating alpha
pythag_data = mlb_team_seasons %>%
  mutate(
    logit_wp = log(WP / (1 - WP)),
    log_rs_ra = log(RS / RA)
  )

# no-intercept model: log(WP / (1 - WP)) = alpha * log(RS / RA)
alpha_model = lm(logit_wp ~ 0 + log_rs_ra, data = pythag_data)
summary(alpha_model)

# alpha estimate + uncertainty
alpha_hat = coef(alpha_model)[["log_rs_ra"]]
alpha_se = summary(alpha_model)$coefficients["log_rs_ra", "Std. Error"]
alpha_ci_95 = alpha_hat + c(-1, 1) * qt(0.975, df = df.residual(alpha_model)) * alpha_se

# helper for converting exponent alpha back to predicted WP
pythag_wp = function(rs, ra, alpha) {
  rs^alpha / (rs^alpha + ra^alpha)
}

# compare fitted-alpha model vs Bill James alpha = 2
pythag_data = pythag_data %>%
  mutate(
    wp_hat_alpha = pythag_wp(RS, RA, alpha_hat),
    wp_hat_bj = pythag_wp(RS, RA, 2),
    resid_alpha = WP - wp_hat_alpha,
    resid_bj = WP - wp_hat_bj
  )

rmse = function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
rmse_alpha = rmse(pythag_data$WP, pythag_data$wp_hat_alpha)
rmse_bj = rmse(pythag_data$WP, pythag_data$wp_hat_bj)

ggplot(data=pythag_data)+
  geom_point(aes(x=wp_hat_bj, y=WP))+
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth=1)+
  labs(title="Predicted vs Actual Win Probability BJ")+
  geom_smooth(aes(x=wp_hat_bj, y=WP))
ggplot(data=pythag_data)+
  geom_point(aes(x=wp_hat_alpha, y=WP))+
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth=1)+
  labs(title="Predicted vs Actual Win Probability Us")+
  geom_smooth(aes(x=wp_hat_alpha, y=WP))
ggplot(data=pythag_data)+
  geom_point(aes(x=wp_hat_bj, y=resid_bj))+
  geom_abline(intercept = 0, slope = 0, color = "red", linewidth=1)+
  labs(title="Residuals BJ")+
  geom_smooth(aes(x=wp_hat_bj, y=resid_bj))
ggplot(data=pythag_data)+
  geom_point(aes(x=wp_hat_alpha, y=resid_alpha))+
  geom_abline(intercept = 0, slope = 0, color = "red", linewidth=1)+
  labs(title="Residuals Us")+
  geom_smooth(aes(x=wp_hat_alpha, y=resid_alpha))
#THESE ALL HAVE CURVATURE THE LOESS PROVES IT
#1.8 IS WAY BETER THAN 2
##############
### PART 2 ###
##############

# load data
mlb_payrolls = read_csv("../data/01_mlb-payrolls.csv.gz")

# remove 2020 covid-shortened season
payroll_data = mlb_payrolls %>%
  filter(year_id != 2020)

# model A: WP ~ payroll / median
model_a = lm(wp ~ payroll_median_ratio, data = payroll_data)

# model B: WP ~ log(payroll / median)
model_b = lm(wp ~ log_payroll_median_ratio, data = payroll_data)

# fitted values + residuals for both models
payroll_data = payroll_data %>%
  mutate(
    fitted_a = fitted(model_a),
    resid_a = resid(model_a),
    fitted_b = fitted(model_b),
    resid_b = resid(model_b)
  )
ggplot(payroll_data,
       aes(y = wp, x = payroll_median_ratio,
           color = factor(case_when(
             team_id == "OAK" ~ "Oakland",
             team_id == "NYA" ~ "Yankees",
             TRUE ~ "Other"
           )))) +
  geom_point() +
#  geom_smooth(method = "loess",
#              formula = y ~ x,
#              span = 0.05,
#              color = "hotpink") +
  scale_color_manual(
    values = c(
      "Oakland" = "darkgreen",
      "Yankees" = "black",
      "Other" = "gray70"
    ),
    breaks = c("Yankees", "Oakland"),
    name = NULL
  )

ggplot(payroll_data, aes(x = payroll_median_ratio, y = wp)) +
  geom_point(color = "gray40") +
  geom_line(aes(y = fitted_a, color = "Linear Model"),
            linewidth = 1) +
  geom_line(aes(y = fitted_b, color = "Log Model"),
            linewidth = 1) +
  scale_color_manual(
    values = c(
      "Linear Model" = "steelblue",
      "Log Model" = "skyblue"
    ),
    name = "Fitted Models"
  ) +
  labs(
    title = "Relationship Between Payroll and Winning Percentage",
    subtitle = "Observed data with linear and logarithmic fitted models",
    x = "Payroll Relative to League Median",
    y = "Winning Percentage"
  ) +
  theme_minimal()
               
ggplot(payroll_data, aes(x = payroll_median_ratio, alpha=.1)) +
  geom_point(aes(y = resid_a, color = "Linear Model")) +
  geom_point(aes(y = resid_b, color = "Log Model")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "Linear Model" = "steelblue",
      "Log Model" = "skyblue"
    ),
    name = "Model"
  ) +
  labs(
    title = "Residuals vs. Relative Payroll",
    subtitle = "Comparison of linear and logarithmic models",
    x = "Payroll Relative to League Median",
    y = "Residual"
  ) +
  theme_minimal()
    
# example: CI and PI for one selected team-season
# replace row_idx with your selected row index
row_idx = 67
selected_row = payroll_data[row_idx, ]

ci_model_b = predict(model_b, newdata = selected_row, interval = "confidence", level = 0.95)
pi_model_b = predict(model_b, newdata = selected_row, interval = "prediction", level = 0.95)

#PI is higher, as we are estimatingthe mean and the error, not just the error.
