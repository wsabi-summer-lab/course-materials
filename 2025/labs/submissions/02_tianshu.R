library(tidyverse)
setwd("/Users/tianshufeng/Desktop/UPenn/2025 Summer")

# Part 1
team = read.csv("02_mlb-team-seasons.csv.gz")

model = lm(log(1 / WP - 1) ~ log(RA / RS) + 0, data = team)

summary(model)

alpha = model$coefficients[1]
team$WP_2 = team$RS ^ alpha / (team$RS ^ alpha + team$RA ^ alpha)

ss_res <- sum((team$WP - team$WP_Pythag_2)^2)
ss_tot <- sum((team$WP - mean(team$WP))^2)

r2_pythag <- 1 - (ss_res / ss_tot)
r2_2 <- summary(model)$r.squared

sd_WP <- sd(team$WP)
sd_resid_2 <- sd(team$WP - team$WP_2)
sd_resid_pythag <- sd(team$WP - team$WP_Pythag_2)
re_2 <- 1 - (sd_resid_2 / sd_WP)
re_pythag <- 1 - (sd_resid_pythag / sd_WP)

team %>% ggplot(aes(x = WP)) +
  geom_point(aes(y = WP_2, color = "Fitted"), alpha = 0.6) +
  geom_point(aes(y = WP_Pythag_2, color = "Pythag"), alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Predicted vs Actual WP with RE and R²",
       x = "Actual WP",
       y = "Predicted WP",
       color = "Model") +
  annotate("text", x = Inf, y = -Inf, label = paste0("R² (Fitted): ", round(r2_2, 3)),
           hjust = 1.1, vjust = -4, color = "red", size = 4) +
  annotate("text", x = Inf, y = -Inf, label = paste0("R² (Pythag): ", round(r2_pythag, 3)),
           hjust = 1.1, vjust = -2.5, color = "blue", size = 4) +
  annotate("text", x = Inf, y = -Inf, label = paste0("RE (Fitted): ", round(re_2, 3)),
           hjust = 1.1, vjust = -8, color = "red", size = 4) +
  annotate("text", x = Inf, y = -Inf, label = paste0("RE (Pythag): ", round(re_pythag, 3)),
           hjust = 1.1, vjust = -6.5, color = "blue", size = 4) +
  theme_minimal()

# Part 2
payroll = read.csv("02_mlb-payrolls.csv.gz")
payroll = payroll %>% filter(yearID != 2020)

payroll %>%
  ggplot(aes(x = Payroll.Median, y = WP)) +
  geom_point() +
  geom_point(data = filter(payroll, teamID == "NYA"), 
             aes(x = Payroll.Median, y = WP), 
             color = "blue") +
  geom_point(data = filter(payroll, teamID == "OAK"), 
             aes(x = Payroll.Median, y = WP), 
             color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Win Percentage vs Payroll Ratio",
       y = "Win Percentage",
       x = "Payroll Ratio") +
  theme_minimal()
  
payroll %>%
  ggplot(aes(x = Log..Payroll.Median., y = WP)) +
  geom_point() +
  geom_point(data = filter(payroll, teamID == "NYA"), 
             aes(x = Log..Payroll.Median., y = WP), 
             color = "blue") +
  geom_point(data = filter(payroll, teamID == "OAK"), 
             aes(x = Log..Payroll.Median., y = WP), 
             color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Win Percentage vs Log Payroll Ratio",
       y = "Win Percentage",
       x = "Log Payroll Ratio") +
  theme_minimal()

model2 = lm(WP ~ Payroll.Median, data = payroll)
payroll$WP2.diff = payroll$WP - predict(model2)
model3 = lm(WP ~ Log..Payroll.Median., data = payroll)
payroll$WP3.diff = payroll$WP - predict(model3)

payroll %>% group_by(teamID) %>%
  summarise(WP2.diff = mean(WP2.diff, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(teamID, WP2.diff), y = WP2.diff * 162)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Difference in Wins from Payroll Model",
       x = "Team",
       y = "Average Wins Difference") +
  theme_minimal()

payroll %>% group_by(teamID) %>%
  summarise(WP3.diff = mean(WP3.diff, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(teamID, WP3.diff), y = WP3.diff * 162)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Difference in Wins from Log Payroll Model",
       x = "Team",
       y = "Average Wins Difference") +
  theme_minimal()