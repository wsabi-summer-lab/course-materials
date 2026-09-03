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



#Task 1:

model = lm(logit_wp ~ 0 + log_rs_ra, data = pythag_data)
model
alpha_hat = coef(model)[1]
alpha_hat
#This gives me an alpha value of 1.8


#Task 2:

helper = function(rs, ra, alpha) {
  rs^alpha / (rs^alpha + ra^alpha)
}

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}


pythag_data = pythag_data %>%
  mutate(
    wp_hat_fitted = helper(RS, RA, alpha_hat),
    wp_hat_james = helper(RS, RA, 2)
  )

rmse_fitted = rmse(pythag_data$WP,
                   pythag_data$wp_hat_fitted)



rmse_james = rmse(pythag_data$WP,
                  pythag_data$wp_hat_james)

rmse_fitted
rmse_james

#Based on this, the fitted rmse is 0.0278 while the non-fitted is 0.0293

#Code for the plots:
plot_data = pythag_data %>%
  select(WP,
         wp_hat_fitted,
         wp_hat_james) %>%
  pivot_longer(
    cols = c(wp_hat_fitted, wp_hat_james),
    names_to = "model",
    values_to = "predicted_wp"
  )

ggplot(plot_data,
       aes(x = predicted_wp,
           y = WP,
           color = model)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1,
              intercept = 0,
              linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Win Percentage",
    x = "Predicted WP",
    y = "Actual WP",
    color = "Model"
  ) +
  theme_minimal()

#Task 3:
#As mentioned before, the estimated value of alpha is 1.8

alpha_se = summary(model)$coefficients[1, 2]
alpha_se
#This gives a standard error of 0.0499

confint(model, level = 0.95)
#This gives me a confidence interval of [1.701, 1.898]

#This confidence interval means that the true exponent value for this formula
#is between 1.701 and 1.898 with a 95% likelihood. This means that an alpha 
#value of 2 is close but not plausible since it does not fall in the range. 


#Task 4:
pythag_data = pythag_data %>%
  mutate(
    fitted_wp = wp_hat_fitted,
    residuals = WP - fitted_wp
  )

ggplot(pythag_data,
       aes(x = fitted_wp,
           y = residuals)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Win Percentage",
    y = "Residual"
  ) +
  theme_minimal()

#If the linear model is reasonable, the residuals should be randomly scattered.
#There seems to be a tighter spread of residuals as the fitted win percentage
#increases, suggesting there might be some slight issue with the model. 




##############
### PART 2 ###
##############

# load payroll data
mlb_payrolls = read_csv("../data/01_mlb-payrolls.csv.gz")

# remove 2020 covid-shortened season
payroll_data = mlb_payrolls %>%
  filter(year_id != 2020)

print(payroll_data)



#Task 1:
# Fit model A: wp ~ payroll_median_ratio (linear)
model_A = lm(wp ~ payroll_median_ratio, data = payroll_data)

# Fit model B: wp ~ log_payroll_median_ratio (log-linear)
model_B = lm(wp ~ log_payroll_median_ratio, data = payroll_data)

# Build smooth prediction curves for plotting
x_seq   = seq(min(payroll_data$payroll_median_ratio),
              max(payroll_data$payroll_median_ratio), length.out = 300)
pred_df = tibble(
  payroll_median_ratio     = x_seq,
  log_payroll_median_ratio = log(x_seq),
  fit_A = predict(model_A, newdata = tibble(payroll_median_ratio = x_seq)),
  fit_B = predict(model_B, newdata = tibble(log_payroll_median_ratio = log(x_seq)))
)

# Label Oakland A's and Yankees for highlighting
highlight = payroll_data %>%
  filter(team_id %in% c("OAK", "NYY"))

p1 = ggplot(payroll_data, aes(x = payroll_median_ratio, y = wp)) +
  geom_point(alpha = 0.3, color = "gray50", size = 1.5) +
  geom_point(data = highlight,
             aes(color = team_id), size = 2, alpha = 0.8) +
  geom_line(data = pred_df, aes(y = fit_A, linetype = "Model A (linear)"),
            color = "steelblue", linewidth = 1) +
  geom_line(data = pred_df, aes(y = fit_B, linetype = "Model B (log)"),
            color = "firebrick", linewidth = 1) +
  scale_color_manual(values = c("OAK" = "forestgreen", "NYY" = "navy"),
                     name = "Team") +
  scale_linetype_manual(values = c("Model A (linear)" = "solid",
                                   "Model B (log)"    = "dashed"),
                        name = "Model") +
  labs(title = "Win Percentage vs. Payroll Median Ratio (1998–2023, excl. 2020)",
       x     = "Payroll / Median Payroll",
       y     = "Win Percentage") +
  theme_bw()

p1





#Task 2: 
payroll_data = payroll_data %>%
  mutate(
    pred_A   = predict(model_A),
    pred_B   = predict(model_B),
    resid_A  = wp - pred_A,
    resid_B  = wp - pred_B
  )
#This gives all of the differences between predicted and actual
#model B is the one with the logs



# -- Residual plot: Model A --
p2a = ggplot(payroll_data, aes(x = pred_A, y = resid_A)) +
  geom_point(alpha = 0.3, size = 1.5, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "firebrick", linewidth = 0.8) +
  labs(title = "Residuals vs. Fitted — Model A (linear payroll)",
       x = "Fitted Win Percentage", y = "Residual") +
  theme_bw()

p2a



# -- Residual plot: Model B --
p2b = ggplot(payroll_data, aes(x = pred_B, y = resid_B)) +
  geom_point(alpha = 0.3, size = 1.5, color = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 0.8) +
  labs(title = "Residuals vs. Fitted — Model B (log payroll)",
       x = "Fitted Win Percentage", y = "Residual") +
  theme_bw()

p2b

#The graphs for both of these models have curvatures, suggesting that
#neither model is perfectly accurate



# -- Per-team average residuals (converted to wins out of 162) --
team_avg = payroll_data %>%
  group_by(team_id) %>%
  summarise(
    avg_resid_A = mean(resid_A) * 162,
    avg_resid_B = mean(resid_B) * 162,
    .groups = "drop"
  )

# Model A: ordered bar chart
p2c = team_avg %>%
  mutate(team_id = fct_reorder(team_id, avg_resid_A)) %>%
  ggplot(aes(x = team_id, y = avg_resid_A,
             fill = avg_resid_A > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
  coord_flip() +
  labs(title = "Average Residual Wins by Team — Model A (linear)",
       x = NULL, y = "Avg Wins Above/Below Prediction (out of 162)") +
  theme_bw(base_size = 9)

p2c




# Model B: ordered bar chart
p2d = team_avg %>%
  mutate(team_id = fct_reorder(team_id, avg_resid_B)) %>%
  ggplot(aes(x = team_id, y = avg_resid_B,
             fill = avg_resid_B > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
  coord_flip() +
  labs(title = "Average Residual Wins by Team — Model B (log)",
       x = NULL, y = "Avg Wins Above/Below Prediction (out of 162)") +
  theme_bw(base_size = 9)

p2d





#Task 3: 
selected = payroll_data %>%
  filter(team_id == "ATL", year_id == 2002)

# Use Model B (log) — the better-fitting model — for interval estimates
new_obs = tibble(log_payroll_median_ratio = selected$log_payroll_median_ratio)

fitted_val = predict(model_B, newdata = new_obs)
ci         = predict(model_B, newdata = new_obs, interval = "confidence", level = 0.95)
pi         = predict(model_B, newdata = new_obs, interval = "prediction", level = 0.95)

cat("Fitted WP (Model B):", round(fitted_val, 4), "\n\n")

#Confidence interval
cat("  [", round(ci[, "lwr"], 4), ",", round(ci[, "upr"], 4), "]\n\n")

#Prediction interval:
cat("  [", round(pi[, "lwr"], 4), ",", round(pi[, "upr"], 4), "]\n\n")

#The prediction interval is wider because it accounts for any individual
#team's variation, which is greater than the variation for an arbitrary
#team of that payroll level





