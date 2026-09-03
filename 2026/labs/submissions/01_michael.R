#############
### SETUP ###
#############

#install.packages(c("ggplot2", "tidyverse"))
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
pythag_model = lm(logit_wp ~ 0 + log_rs_ra, data = pythag_data)
summary(pythag_model)

# 2) Extract alpha estimate and a 95% confidence interval
alpha_hat = coef(pythag_model)["log_rs_ra"]
alpha_ci = confint(pythag_model)["log_rs_ra",]

# 3) Build helper to convert RS/RA + alpha -> predicted WP
pythag_wp = function(RS, RA, alpha) RS^alpha / (RS^alpha + RA^alpha)
# 4) Compare fitted-alpha predictions vs Bill James alpha = 2
pythag_data = pythag_data %>%
  mutate(
    wp_fitted = pythag_wp(RS, RA, alpha_hat),
    wp_james  = pythag_wp(RS, RA, 2)
  )

#RMSE
rmse = function(actual, pred) sqrt(mean((actual - pred)^2))
cat("Fitted RMSE:", rmse(pythag_data$WP, pythag_data$wp_fitted), "\n")
cat("James RMSE:",  rmse(pythag_data$WP, pythag_data$wp_james), "\n")
#Fitted alpha model has smaller RMSE (0.0278 vs 0.0293), so it fits better.

# plot actual vs predicted with 45-degree line
ggplot(pythag_data) +
  geom_point(aes(wp_fitted, WP, color = "Fitted α")) +
  geom_point(aes(wp_james,  WP, color = "Bill James α=2")) +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  labs(x="Predicted WP", y="Actual WP", color="Model") +
  theme_minimal()

#Task 3
cat("Alpha:", alpha_hat, "\n")
cat("SE:", summary(pythag_model)$coefficients["log_rs_ra", "Std. Error"], "\n")
cat("95% CI:", alpha_ci, "\n")
#a = 1.80, Se, = 0.04986, 95% CI = [1.701, 1.898]. Since a=2 is outside this interval, it is not plausible based on our data. 

#Task 4
ggplot(pythag_data, aes(wp_fitted, WP - wp_fitted)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Fitted WP", y="Residuals") +
  theme_minimal()

#what would you expect to see if the linear model is reasonable? It should be scattered
#Do you see curvature, outliers, or changing spread? No it seems pretty scatter and random.
#

##############
### PART 2 ###
##############

#Task 1

# load payroll data
mlb_payrolls = read_csv("../data/01_mlb-payrolls.csv.gz")

# remove 2020 covid-shortened season
payroll_data = mlb_payrolls %>%
  filter(year_id != 2020)

ggplot(payroll_data, aes(x = payroll_median_ratio, y = wp)) +
  geom_point(aes(color = case_when(
    team_id == "OAK" ~ "Oakland A's",
    team_id == "NYA" ~ "NY Yankees",
    TRUE ~ "Other"
  ))) +
  scale_color_manual(values = c("Oakland A's" = "green", "NY Yankees" = "navy", "Other" = "gray")) +
  geom_smooth(method = "lm", formula = y ~ x, aes(linetype = "Model A"), se = FALSE, color = "red") +
  geom_smooth(method = "lm", formula = y ~ log(x), aes(linetype = "Model B"), se = FALSE, color = "blue") +
  labs(x = "Payroll Median Ratio", y = "Win Percentage", color = "Team", linetype = "Model") +
  theme_minimal()
#Model B seems to fit the data better because it fits the data better around median ratio of 2 and above.

#Task 2
# fit both models
model_a = lm(wp ~ payroll_median_ratio, data = payroll_data)
model_b = lm(wp ~ log_payroll_median_ratio, data = payroll_data)

# add residuals
payroll_data = payroll_data %>%
  mutate(
    resid_a = wp - fitted(model_a),
    resid_b = wp - fitted(model_b)
  )

# residual plots
ggplot(payroll_data, aes(fitted(model_a), resid_a)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted WP", y = "Residuals", title = "Model A Residuals") +
  theme_minimal()

ggplot(payroll_data, aes(fitted(model_b), resid_b)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted WP", y = "Residuals", title = "Model B Residuals") +
  theme_minimal()
# average residual per team, converted to wins
avg_resid_a = payroll_data %>%
  group_by(team_id) %>%
  summarise(avg_wins = mean(resid_a) * 162) %>%
  arrange(desc(avg_wins))

avg_resid_b = payroll_data %>%
  group_by(team_id) %>%
  summarise(avg_wins = mean(resid_b) * 162) %>%
  arrange(desc(avg_wins))

# Model A graph
ggplot(avg_resid_a, aes(x = reorder(team_id, avg_wins), y = avg_wins, fill = avg_wins > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Team", y = "Avg Wins Above Expected", title = "Model A", fill = "Above Expected") +
  theme_minimal()

# Model B graph
ggplot(avg_resid_b, aes(x = reorder(team_id, avg_wins), y = avg_wins, fill = avg_wins > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Team", y = "Avg Wins Above Expected", title = "Model B", fill = "Above Expected") +
  theme_minimal()

# Both residual plots look randomly scattered around 0 with no clear curvature.
# Model A has slight funnel shape suggesting changing spread at high payrolls.

#Task 3
# pick a Red Sox season
red_sox = payroll_data %>% filter(team_id == "BOS", year_id == 2010)

# fitted WP
predict(model_b, newdata = red_sox)

# confidence interval (mean WP at that payroll level)
predict(model_b, newdata = red_sox, interval = "confidence")

# prediction interval (individual team-season)
predict(model_b, newdata = red_sox, interval = "prediction")

## Fitted WP: 0.550
# Confidence interval: [0.541, 0.559] - uncertainty in mean WP at this payroll level
# Prediction interval: [0.417, 0.683] - uncertainty for one specific team-season
# Prediction interval is wider because it accounts for individual team randomness on top of model uncertainty.

# Model B is better intuitively because spending has diminishing returns. Similar to how the athletes practicing skills has diminishing returns. 