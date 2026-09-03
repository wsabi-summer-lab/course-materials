#task 1.1
library(tidyverse)
setwd("/Users/amritabalajee/downloads")
mlb_team_seasons = read.csv("02_mlb-team-seasons.csv.gz")
mlb_team_seasons$X <- log(mlb_team_seasons$RS / mlb_team_seasons$RA)
mlb_team_seasons$Y <- log(mlb_team_seasons$WP / (1 - mlb_team_seasons$WP))
model <- lm(Y ~ X, data = mlb_team_seasons)
model

#task 1.2
resid = model$residuals
re_model = 1-(sd(model$residuals)/sd(Y))

resid_null = mlb_team_seasons$WP - mlb_team_seasons$WP_Pythag_2
re_null = 1 - (sd(resid_null) / sd(mlb_team_seasons$WP))

data = tibble(model_name = c("null model", "our model"), re = c(re_null, re_model))

ggplot(data, aes(x = model_name, y = re)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Payroll/Median",
       x = NULL, y = "Avg. WP Error (in Wins)") +
  coord_cartesian(ylim = c(0.6, 0.7)) +
  theme_minimal()

#task 2.1
mlb_payroll = read.csv("02_mlb-payrolls.csv.gz")
payroll_no_2020 = subset(mlb_payroll, yearID != 2020)
ggplot(mlb_payroll, aes(x = Payroll.Median, y = WP)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "red", linetype = "dashed") +
  geom_point(
    data = filter(mlb_payroll, name %in% c("Oakland Athletics", "New York Yankees")),
    aes(color = name)
  )

#task 2.2
model_raw <- lm(WP ~ Payroll.Median, data = mlb_payroll)
mlb_payroll$pred_WP <- predict(model_raw, newdata = mlb_payroll)
mlb_payroll$raw_diff <- mlb_payroll$WP - mlb_payroll$pred_WP

model_log <- lm(WP ~ Log..Payroll.Median., data = mlb_payroll)
mlb_payroll$pred_WP_log <- predict(model_log, newdata = mlb_payroll)
mlb_payroll$diff_log <- mlb_payroll$WP - mlb_payroll$pred_WP_log

mlb_payroll <- mlb_payroll %>%
  mutate(diff_raw_wins = raw_diff * 162,
         diff_log_wins = diff_log * 162)

avg_diffs <- mlb_payroll %>%
  group_by(teamID) %>%
  summarise(diff_raw_wins = mean(diff_raw_wins),
            diff_log_wins = mean(diff_log_wins)) %>%
  ungroup()

avg_diffs_raw <- avg_diffs %>% arrange(desc(diff_raw_wins))

p1 <- ggplot(avg_diffs_raw, aes(x = reorder(teamID, -diff_raw_wins), y = diff_raw_wins)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Payroll/Median",
       x = NULL, y = "Avg. WP Error (in Wins)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed")

avg_diffs_log <- avg_diffs %>% arrange(desc(diff_log_wins))

p2 <- ggplot(avg_diffs_log, aes(x = reorder(teamID, -diff_log_wins), y = diff_log_wins)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Log Payroll/Median",
       x = NULL, y = "Avg. WP Error (in Wins)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed")


# JP
model_data = mlb_team_seasons %>% 
  # add columns
  mutate(
    X = log(RS / RA),
    Y = log(WP / (1-WP))
  )
model = lm(Y ~ X, data = model_data)
model
plot(mlb_payroll$WP,mlb_payroll$Payroll.Median)