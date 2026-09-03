#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

##############
### PART 1 ###
##############

# load data
mlb_team_seasons = read_csv("C:/Users/sundw/OneDrive/文档/GitHub/lab-materials/2026/labs/data/01_mlb-team-seasons.csv")

# transformed regression data for estimating alpha
pythag_data = mlb_team_seasons %>%
  mutate(
    logit_wp = log(WP / (1 - WP)),
    log_rs_ra = log(RS / RA)
  )

# no-intercept model: log(WP / (1 - WP)) = alpha * log(RS / RA)
alpha_model = lm(logit_wp ~ 0 + log_rs_ra, data = pythag_data)
summary(alpha_model)

# Task 3:
# alpha_hat = 1.7997
# standard error = 0.0499
# 95% confidence interval = [1.7012, 1.8983]
#
# Using the 95% confidence interval, we are 95% confident that the true value for alpha is 
# between 1.7012 and 1.8993, so we reject the exponent of 2

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
rmse_alpha
rmse_bj

# Task 2: Our model has a smaller RMSE

ggplot(pythag_data, aes(x = wp_hat_alpha, y = WP)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Actual vs Predicted Win Percentage (Estimated α)",
    x = "Predicted WP",
    y = "Actual WP"
  ) +
  theme_minimal()

ggplot(pythag_data, aes(x = wp_hat_bj, y = WP)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Actual vs Predicted Win Percentage (Bill James α = 2)",
    x = "Predicted WP",
    y = "Actual WP"
  ) +
  theme_minimal()

# Task 4: There's no obvious curvature, outliers, or changing spread

ggplot(pythag_data,
       aes(x = wp_hat_alpha, y = resid_alpha)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Win Percentage",
    y = "Residual"
  ) +
  theme_minimal()

##############
### PART 2 ###
##############

# load data
mlb_payrolls = read_csv("C:/Users/sundw/OneDrive/文档/GitHub/lab-materials/2026/labs/data/01_mlb-payrolls.csv")

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

payroll_data = payroll_data %>%
  mutate(
    highlight = case_when(
      team_id == "OAK" ~ "Oakland A's",
      team_id == "NYA" ~ "New York Yankees",
      TRUE             ~ "Other"
    )
  )

payroll_grid = data.frame(
  payroll_median_ratio = seq(min(payroll_data$payroll_median_ratio),
                             max(payroll_data$payroll_median_ratio),
                             length.out = 300)
) %>%
  mutate(log_payroll_median_ratio = log(payroll_median_ratio))

payroll_grid = payroll_grid %>%
  mutate(
    pred_a = predict(model_a, newdata = payroll_grid),
    pred_b = predict(model_b, newdata = payroll_grid)
  )

ggplot() +
  geom_point(data = filter(payroll_data, highlight == "Other"),
             aes(x = payroll_median_ratio, y = wp),
             color = "grey70", alpha = 0.5, size = 1.5) +
  geom_point(data = filter(payroll_data, highlight != "Other"),
             aes(x = payroll_median_ratio, y = wp,
                 color = highlight, shape = highlight),
             size = 2.5) +
  geom_line(data = payroll_grid,
            aes(x = payroll_median_ratio, y = pred_a, linetype = "Model A (linear)"),
            color = "steelblue", linewidth = 1) +
  geom_line(data = payroll_grid,
            aes(x = payroll_median_ratio, y = pred_b, linetype = "Model B (log)"),
            color = "tomato", linewidth = 1) +
  scale_color_manual(name = "Team",
                     values = c("Oakland A's" = "#003831",
                                "New York Yankees" = "#003087")) +
  scale_shape_manual(name = "Team",
                     values = c("Oakland A's" = 17, "New York Yankees" = 15)) +
  scale_linetype_manual(name = "Model",
                        values = c("Model A (linear)" = "solid",
                                   "Model B (log)" = "dashed")) +
  labs(title = "Win Percentage vs. Payroll (1998–2023, excl. 2020)",
       x = "Payroll / Median Payroll",
       y = "Win Percentage") +
  theme_bw()

#The log model fits better both visually and intuitively due to diminishing returns.

# example: CI and PI for one selected team-season
# replace row_idx with your selected row index
row_idx = 1
selected_row = payroll_data[row_idx, ]

ci_model_b = predict(model_b, newdata = selected_row, interval = "confidence", level = 0.95)
pi_model_b = predict(model_b, newdata = selected_row, interval = "prediction", level = 0.95)


# task 2
# In model A, because of the diminishing return, the curve tapers off towards the end
# Both models have about the same number of outliers that are about the same distance from 0
# For model A, the spread again reduces as win percentage increases, most likely again due to 
# diminishing return

# Model A
ggplot(payroll_data, aes(x = fitted_a, y = resid_a)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "tomato", linewidth = 0.8) +
  labs(title = "Model A Residuals vs. Fitted",
       subtitle = "WP ~ payroll_median_ratio",
       x = "Fitted WP", y = "Residual") +
  theme_bw()

# Model B
ggplot(payroll_data, aes(x = fitted_b, y = resid_b)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "tomato", linewidth = 0.8) +
  labs(title = "Model B Residuals vs. Fitted",
       subtitle = "WP ~ log(payroll_median_ratio)",
       x = "Fitted WP", y = "Residual") +
  theme_bw()

team_resids = payroll_data %>%
  group_by(team_id) %>%
  summarise(
    avg_wins_a = mean(resid_a) * 162,
    avg_wins_b = mean(resid_b) * 162,
    .groups = "drop"
  )

# Model A
ggplot(team_resids, aes(x = reorder(team_id, avg_wins_a), y = avg_wins_a,
                        fill = avg_wins_a > 0)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(name = "vs. Expected",
                    values = c("TRUE" = "#2a9d8f", "FALSE" = "#e76f51"),
                    labels = c("TRUE" = "Over-performed", "FALSE" = "Under-performed")) +
  coord_flip() +
  labs(title = "Avg. Wins Above/Below Expected by Team (Model A)",
       x = NULL, y = "Wins per 162-game season") +
  theme_bw()

# Model B
ggplot(team_resids, aes(x = reorder(team_id, avg_wins_b), y = avg_wins_b,
                        fill = avg_wins_b > 0)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(name = "vs. Expected",
                    values = c("TRUE" = "#2a9d8f", "FALSE" = "#e76f51"),
                    labels = c("TRUE" = "Over-performed", "FALSE" = "Under-performed")) +
  coord_flip() +
  labs(title = "Avg. Wins Above/Below Expected by Team (Model B)",
       x = NULL, y = "Wins per 162-game season") +
  theme_bw()

row_idx = which(payroll_data$team_id == "SEA" & payroll_data$year_id == 2004)
selected_row = payroll_data[row_idx, ]

fitted_wp = predict(model_b, newdata = selected_row)
ci = predict(model_b, newdata = selected_row, interval = "confidence", level = 0.95)
pi = predict(model_b, newdata = selected_row, interval = "prediction", level = 0.95)

cat("Team-season:       SEA 2004\n")
cat("Actual WP:        ", round(selected_row$wp, 4), "\n")
cat("Fitted WP:        ", round(fitted_wp, 4), "\n\n")
cat("95% Confidence Interval:\n")
cat(" [", round(ci[,"lwr"], 4), ",", round(ci[,"upr"], 4), "]\n\n")
cat("95% Prediction Interval:\n")
cat(" [", round(pi[,"lwr"], 4), ",", round(pi[,"upr"], 4), "]\n")

# > cat("Team-season:       SEA 2004\n")
# Team-season:       SEA 2004
# > cat("Actual WP:        ", round(selected_row$wp, 4), "\n")
# Actual WP:         0.3889 
# > cat("Fitted WP:        ", round(fitted_wp, 4), "\n\n")
# Fitted WP:         0.5198 

# > cat("95% Confidence Interval:\n")
# 95% Confidence Interval:
#   > cat(" [", round(ci[,"lwr"], 4), ",", round(ci[,"upr"], 4), "]\n\n")
# [ 0.5141 , 0.5255 ]

# > cat("95% Prediction Interval:\n")
# 95% Prediction Interval:
#   > cat(" [", round(pi[,"lwr"], 4), ",", round(pi[,"upr"], 4), "]\n")
# [ 0.3869 , 0.6526 ]
# 
# The prediction interval is wider because it deals with an individual season of an individual
# team (one data point), whereas the confidence interval deals with the mean, so the PI has higher
# variance and is thus wider
#
# Intuitively, Model B is better because of diminishing returns of investing money into payroll.
# Intuitively, every unit of payroll added doesn't linearly add to the number of wins, so the
# logarithmic model makes more sense.
