rm(list=ls())
setwd("C:/UPenn/research/WSABI summer 2025/Research/lectures and labs/day 2")

#-----------------------------------------------------------------------------------------------------
# regression from morning lecture

# # install.packages("Lahman")
# library(Lahman)
# library(dplyr)
# library(tidyverse)
# 
# data(Batting)
# 
# batting <- Batting %>% 
#   filter(yearID == 2020 | yearID == 2021) %>% 
#   group_by(playerID, yearID) %>% 
#   summarise(hits = sum(H, na.rm = T),
#             at_bats = sum(AB, na.rm = T)
#             ) %>% 
#   ungroup() %>% 
#   filter(at_bats != 0) %>% 
#   mutate(batting_average = hits / at_bats) %>% 
#   pivot_wider(
#     names_from = yearID,
#     values_from = batting_average,
#     names_prefix = "ba_"
#   ) %>% 
#   filter(!is.na(ba_2020) & !is.na(ba_2021))

#-----------------------------------------------------------------------------------------------------
# afternoon lab

library(rio)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)

team_seasons <- as.data.table(read.csv("data/02_mlb-team-seasons.csv.gz"))

team_seasons$log_ratio_RS_RA <- log(team_seasons$RS / team_seasons$RA)
team_seasons$log_odds_WP <- log((team_seasons$WP) / (1 - team_seasons$WP))

team_seasons <- subset(team_seasons, is.finite(log_ratio_RS_RA) & is.finite(log_odds_WP))

model <- lm(log_odds_WP ~ log_ratio_RS_RA, data = team_seasons)
summary(model) # y= -0.004672 + 1.800026x

old_model <- lm(WP ~ WP_Pythag_2 +0, data = team_seasons)
summary(old_model) # y= 0.04859 + 0.90093x

alpha <- coef(model)["log_ratio_RS_RA"]
cat("Estimated alpha:", round(alpha, 4), "\n")

ggplot(team_seasons, aes(x = log_ratio_RS_RA, y = log_odds_WP)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.2) +
  labs(title = paste("Estimated alpha =", round(alpha, 4)),
       x = "log(RS / RA)",
       y = "log(WP / (1 - WP))") +
  theme_minimal()

# original regression with alpha = 2
ggplot(team_seasons, aes(x = WP_Pythag_2, y = WP)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.2) +
  labs(title = "Alpha = 2",
       x = "WP_Pythag_2",
       y = "WP") +
  theme_minimal()

# RE
sd(team_seasons$WP) # 0.08762479
# team_seasons <- team_seasons %>% 
#   mutate(res_log = team_seasons$log_odds_WP - (-0.004672 + 1.800026*team_seasons$log_ratio_RS_RA),
#          res = team_seasons$WP - (0.04859 + 0.90093*team_seasons$WP_Pythag_2),
#          res_log_sq = res_log**2,
#          res_sq = res**2)

sd(model$residuals)
re_log_new <- 1 - (sd(model$residuals) / sd(team_seasons$log_odds_WP))
re_old <- 1 - (sd(old_model$residuals) / sd(team_seasons$WP))
# 
# res_log_sd <- sqrt((1/(dim(team_seasons)[1]-1))*sum(team_seasons$res_log_sq))
# res_sd <- sqrt((1/(dim(team_seasons)[1]-1))*sum(team_seasons$res_sq))
# re <- 1 - (res_sd) / sd(team_seasons$WP)
# re_log <- 1 - (res_log_sd) / sd(team_seasons$WP)

#-----------------------------------------------------------------------------------------------------
# afternoon lab part 2 (payrolls)

payrolls <- as.data.table(read.csv("data/02_mlb-payrolls.csv.gz"))
names(payrolls)

payrolls <- payrolls %>% filter(yearID != 2020)
# median_payroll <- median(payrolls$Payroll.Median)
# payrolls <- payrolls %>% mutate(payroll_over_median = Payroll.Median/median_payroll)

ggplot(payrolls, aes(x = Payroll.Median, y = WP)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.2) +
  labs(title = "WP vs. median payroll",
       x = "Payroll over Median",
       y = "WP") +
  theme_minimal()+
  geom_smooth(aes(x = Log..Payroll.Median.), method = "lm", se = FALSE, color = "blue") +  # Log-linear line
  geom_point(data = subset(payrolls, name == "Oakland Athletics"), color = "green", size = 3) +
  geom_point(data = subset(payrolls, name == "New York Yankees"), color = "purple", size = 3) 

ggplot(payrolls, aes(x = Log..Payroll.Median., y = WP)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1.2) +
  labs(title = "WP vs. log median payroll",
       x = "Log Payroll over Median",
       y = "WP") +
  theme_minimal()+
  geom_point(data = subset(payrolls, name == "Oakland Athletics"), color = "green", size = 3) +
  geom_point(data = subset(payrolls, name == "New York Yankees"), color = "purple", size = 3)

## part 2
new_model <- lm(WP ~ Payroll.Median, data = payrolls)
summary(new_model) # WP = 0.421843 + 0.074124(Payroll.Median)

payrolls <- payrolls %>% 
  mutate(pred_WP = 0.421843 + 0.074124 * Payroll.Median)

payrolls$residual_WP <- payrolls$WP - payrolls$pred_WP

team_averages_og <- payrolls %>%
  group_by(teamID) %>%
  summarise(avg_residual = mean(residual_WP, na.rm = TRUE)) %>%
  arrange(desc(avg_residual)) %>%
  mutate(teamID = factor(teamID, levels = unique(teamID)))

team_averages_og <- team_averages_og %>% 
  mutate(wins = avg_residual*162)

ggplot(team_averages_og, aes(x = teamID, y = wins, fill = "Average Wins")) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = "steelblue") +
labs(title = "Average Wins by Team (vs. payroll/median)",
     x = "Team",
     y = "Average Wins",
     fill = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# log
new_log_model <- lm(WP ~ Log..Payroll.Median., data = payrolls)
summary(new_log_model) # WP = 0.50267 + 0.07666(Log..Payroll.Median.)

payrolls <- payrolls %>% 
  mutate(pred_WP_log = 0.50267 + 0.07666 * Log..Payroll.Median.)

payrolls$residual_log_WP <- payrolls$WP - payrolls$pred_WP_log

team_averages_log <- payrolls %>%
  group_by(teamID) %>%
  summarise(avg_residual = mean(residual_log_WP, na.rm = TRUE)) %>%
  arrange(desc(avg_residual)) %>%
  mutate(teamID = factor(teamID, levels = unique(teamID)))

team_averages_log <- team_averages_log %>% 
  mutate(wins_new = avg_residual*162)

ggplot(team_averages_log, aes(x = teamID, y = wins_new, fill = "Average Wins")) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = "steelblue") +
  labs(title = "Average Wins by Team (vs. log payroll/median)",
       x = "Team",
       y = "Average Wins",
       fill = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
