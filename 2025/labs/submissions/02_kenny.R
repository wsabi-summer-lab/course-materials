# 02 simple linear regression

# 2.1 Pythagorean Win Percentage

library(ggplot2)
library(dplyr)

setwd("/Users/kennywatts/Downloads")

mlb_seasons <- read.csv("02_mlb-team-seasons.csv.gz")

mlb_seasons <- mlb_seasons %>%
  mutate(x = (log(RS)-log(RA)))

model_pythag <- lm(WP ~ WP_Pythag_2, data = mlb_seasons)
summary(model_pythag)

model_regression <- lm(log(WP/(1-WP)) ~ x, data = mlb_seasons)
summary(model_regression)

mlb_seasons <- mlb_seasons %>%
  mutate(
    new_WP = (RS^1.8)/(RA^1.8+RS^1.8),
  ) %>%
  na.omit()

model_new_pythag <- lm(WP ~ new_WP, data = mlb_seasons)
summary(model_new_pythag)

ggplot(mlb_seasons, aes(y = WP)) +
  geom_point(aes(x = WP_Pythag_2, color = "WP_Pythag_2")) +
  geom_point(aes(x = new_WP, color = "new_WP")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(x = "Predicted WP", y = "Actual WP", color = "Prediction")


# Reduction in Error

# RE with 2

RE_2 = 1 - sd(mlb_seasons$WP - mlb_seasons$WP_Pythag_2)/sd(mlb_seasons$WP)
RE_2

# RE with 1.8

RE_1.8 = 1 - sd(residuals(model_new_pythag))/sd(mlb_seasons$WP)
RE_1.8


# 2.2 Evaluating MLB General Managers

mlb_payrolls <- read.csv("02_mlb-payrolls.csv.gz")

mlb_payrolls <- subset(mlb_payrolls, yearID != 2020)

ggplot(mlb_payrolls, aes(x = Payroll.Median, y = WP)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "red", linetype = "dashed") +
  geom_point(
    data = filter(mlb_payrolls, name %in% c("Oakland Athletics", "New York Yankees")),
    aes(color = name)
)


model <- lm(WP ~ Payroll.Median, data = mlb_payrolls)

mlb_payrolls$predicted_WP <- predict(model, newdata = mlb_payrolls)

mlb_payrolls$diff_pred_actual <- mlb_payrolls$WP -mlb_payrolls$predicted_WP

mlb_payrolls$log_payroll <- log(mlb_payrolls$Payroll.Median)

model_log <- lm(WP ~ log_payroll, data = mlb_payrolls)

mlb_payrolls$predicted_WP_log <- predict(model_log)

mlb_payrolls$diff_pred_actual_log <- mlb_payrolls$WP - mlb_payrolls$predicted_WP_log

# Finding Average for Each team

team_diffs <- mlb_payrolls %>%
  group_by(name) %>%
  summarize(
    avg_diff_linear = mean(diff_pred_actual, na.rm = TRUE),
    avg_diff_log = mean(diff_pred_actual_log, na.rm = TRUE)
  ) %>%
  mutate(
    rank_linear = rank(avg_diff_linear, ties.method = "first"),
    rank_log = rank(avg_diff_log, ties.method = "first")
  ) %>%
  arrange(rank_linear)

team_diffs %>%
  arrange(desc(avg_diff_linear)) %>%
  mutate(name = factor(name, levels = name)) %>%
  ggplot(aes(x = name, y = avg_diff_linear)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(avg_diff_linear, 3)), vjust = -0.5, size = 3) +
  labs(
    x = "Team",
    y = "Average Difference (Linear Model)",
    title = "Average Difference Between Predicted and Actual WP by Team"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

team_diffs %>%
  arrange(desc(avg_diff_log)) %>%
  mutate(name = factor(name, levels = name)) %>%
  ggplot(aes(x = name, y = avg_diff_log)) +
  geom_col(fill = "tomato") +
  geom_text(aes(label = round(avg_diff_log, 3)), vjust = -0.5, size = 3) +
  labs(
    x = "Team",
    y = "Average Difference (Log Model)",
    title = "Average Difference Between Predicted and Actual WP by Team (Log Model)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Y in Wins

team_diffs %>%
  arrange(desc(avg_diff_linear)) %>%
  mutate(name = factor(name, levels = name)) %>%
  ggplot(aes(x = name, y = avg_diff_linear * 162)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(avg_diff_linear * 162, 1)), vjust = -0.5, size = 3) +
  labs(
    x = "Team",
    y = "Average Difference in Wins (Linear Model)",
    title = "Average Difference Between Predicted and Actual Wins by Team"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


team_diffs %>%
  arrange(desc(avg_diff_log)) %>%
  mutate(name = factor(name, levels = name)) %>%
  ggplot(aes(x = name, y = avg_diff_log * 162)) +
  geom_col(fill = "tomato") +
  geom_text(aes(label = round(avg_diff_log * 162, 1)), vjust = -0.5, size = 3) +
  labs(
    x = "Team",
    y = "Average Difference in Wins (Log Model)",
    title = "Average Difference Between Predicted and Actual Wins by Team (Log Model)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
