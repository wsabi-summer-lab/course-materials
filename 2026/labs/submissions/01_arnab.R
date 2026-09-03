#############
### SETUP ###
#############

install.packages(c("ggplot2", "tidyverse"))
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

#The confidence interval indicates that we are 95% confident that the true alpha value would fall within 
#the values 1.701175 and 1.898261. Alpha = 2 is not plausible based on this interval

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
print(rmse_alpha)
print(rmse_bj)

###the alpha chosen by the linear model has a smaller RMSE, of .02781082
plot_alpha <- ggplot(pythag_data, aes(x = WP, y = wp_hat_alpha)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Actual vs Predicted WP (Estimated Alpha)",
    x = "Actual WP",
    y = "Predicted WP"
  )


plot_bj <- ggplot(pythag_data, aes(x = WP, y = wp_hat_bj)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Actual vs Predicted WP (Bill James α = 2)",
    x = "Actual WP",
    y = "Predicted WP"
  )
### both are quite similar, the 45 degree line seems to fit the estimate alpha slightly better. 

plot_alpha
plot_bj

print(alpha_hat)
print(alpha_se)
print(alpha_ci_95)

pythag_data$residual_alpha =
  pythag_data$WP - pythag_data$wp_hat_alpha

residuals <- ggplot(pythag_data, aes(x = wp_hat_alpha, y = residual_alpha)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residual Plot for Fitted Pythagorean Model",
    x = "Predicted Win Percentage",
    y = "Residuals"
  ) 
residuals
### You expect to see points randomly spread around, pretty evenly, which is what is seen here. There's not much curvature, a couple outliers, and the spread doesn't really change.
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

ggplot(payroll_data,
       aes(x = payroll_median_ratio, y = wp)) +
  
  # scatterplot
  geom_point(alpha = 0.7) +
  
  geom_point(
    data = payroll_data %>%
      filter(team_id %in% c("OAK", "NYA")),
    color = "red",
    size = 1
  ) +
  
  geom_text(
    data = payroll_data %>%
      filter(team_id %in% c("OAK", "NYA")),
    aes(label = team_id),
    vjust = -1
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  
  geom_smooth(
    aes(x = exp(log_payroll_median_ratio)),
    method = "lm",
    formula = y ~ log(x),
    se = FALSE,
    color = "darkgreen"
  ) +
  
  labs(
    title = "WP vs Payroll Median Ratio",
    x = "Payroll / Median Payroll Ratio",
    y = "Winning Percentage"
  ) + 
  theme_minimal()
#the log payroll regression line (dark green) seems to fit the shape of payroll vs winning percentage better

# fitted values + residuals for both models
payroll_data = payroll_data %>%
  mutate(
    fitted_a = fitted(model_a),
    resid_a = resid(model_a),
    fitted_b = fitted(model_b),
    resid_b = resid(model_b)
  )

ggplot(payroll_data,
       aes(x = fitted_a, y = resid_a)) +
  geom_point() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(
    title = "Residual Plot: Model A",
    x = "Fitted Values",
    y = "Residuals"
  ) 



ggplot(payroll_data,
       aes(x = fitted_b, y = resid_b)) +
  geom_point() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(
    title = "Residual Plot: Model B",
    x = "Fitted Values",
    y = "Residuals"
  ) 

#Model A seems to change spread into a funnel a bit, with a couple outliers, while model B has more random points placed around everywhere.  

team_resids <- payroll_data %>%
  group_by(team_id) %>%
  summarize(
    avg_resid_a = mean(resid_a, na.rm = TRUE),
    avg_resid_b = mean(resid_b, na.rm = TRUE)
  ) %>%
  mutate(
    # convert from WP to wins
    avg_resid_a_wins = 162 * avg_resid_a,
    avg_resid_b_wins = 162 * avg_resid_b
  )


team_resids %>%
  arrange(desc(avg_resid_a_wins)) %>%
  mutate(team_id = factor(team_id, levels = team_id)) %>%
  
  ggplot(aes(x = team_id,
             y = avg_resid_a_wins,
             fill = "Model A")) +
  
  geom_col() +
  
  labs(
    title = "Average Residual by Team (Model A)",
    x = "Team",
    y = "Average Difference (Wins)",
    fill = "Model"
  ) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))




team_resids %>%
  arrange(desc(avg_resid_b_wins)) %>%
  mutate(team_id = factor(team_id, levels = team_id)) %>%
  
  ggplot(aes(x = team_id,
             y = avg_resid_b_wins,
             fill = "Model B")) +
  
  geom_col() +
  
  labs(
    title = "Average Residual by Team (Model B)",
    x = "Team",
    y = "Average Difference (Wins)",
    fill = "Model"
  ) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

# example: CI and PI for one selected team-season
# replace row_idx with your selected row index
row_idx = 502
selected_row = payroll_data[row_idx, ]
fitted_value = payroll_data$fitted_b[row_idx]
ci_model_b = predict(model_b, newdata = selected_row, interval = "confidence", level = 0.95)
pi_model_b = predict(model_b, newdata = selected_row, interval = "prediction", level = 0.95)
print(fitted_value)
print(ci_model_b)
print(pi_model_b)
#The prediction interval is a lot wider because it is predicting the interval in which a future predicted value will fall, whereas the CI predicts where the average is likely to fall within. 


##Part 1.2.3
#Model A is better from a intuitive perspective, given a constant increase in the predictor leads to a constant increase in win percentage, whereas Model B, with the conversion between variables is harder to think of intuitively.