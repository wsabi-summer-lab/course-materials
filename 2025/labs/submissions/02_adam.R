library(Lahman)
library(dplyr)
library(tidyverse)
library(ggplot2)

### class example before lab
batting = Batting |>
  filter(yearID==2020 | yearID==2021) |>
  group_by(playerID, yearID) |>
  summarise(
     hits=sum(H, na.rm=TRUE),
    at_bats=sum(AB, na.rm=TRUE),
    ba=hits/at_bats
  ) |>
  ungroup() |>
  filter(at_bats != 0) |>
  mutate (
    ba=hits/at_bats)|>
  pivot_wider(
    names_from = yearID,
    values_from = ba,
    names_prefix = "ba_"
  ) |>
  filter()


batting

#### 2.1.3
### Task 1

data = read.csv('02_mlb-team-seasons.csv.gz')


data = data |>
  mutate(
    log_ratio = log(RA/RS),
    log_win_odds = log((1-WP)/WP),
    fits = 1/(1+exp(model$fitted.values))
  )

model = lm(log_win_odds ~ log_ratio, data = data)
summary(model)


### Task 2
model_RE = 1 - sd(model$residuals)/sd(data$log_win_odds)
model_RE

pytha_RE = 1 - sd(data$WP_Pythag_2-data$WP)/sd(data$WP)
pytha_RE

ggplot(data = data, aes(x = fits, y = WP)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") 

ggplot(data = data, aes(x = WP_Pythag_2, y = WP)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") 


ggplot(data = data, aes(abs(fits-WP))) +
  geom_histogram()
ggplot(data = data, aes(abs(WP_Pythag_2 - WP))) +
  geom_histogram()

pred_df <- data %>%
  select(WP, fits, WP_Pythag_2) %>%
  pivot_longer(
    cols = c(fits, WP_Pythag_2),
    names_to = "model",
    values_to = "WP_pred"
  ) %>%
  mutate(model = recode(model,
                        fits = "Linear Model",
                        WP_Pythag_2 = "Pythagorean"))

q1_graph = ggplot(pred_df, aes(x = WP_pred, y = WP, color = model)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Actual vs. Predicted Winning Percentage",
    x = "Predicted WP",
    y = "Actual WP",
    color = "Model"
  ) +
  theme_minimal()

#### 2.2.2
### Task 1
data = read.csv('02_mlb-payrolls.csv.gz')

data = data |>
  filter(yearID != 2020)


data$highlight <- ifelse(data$teamID == "NYA", "Yankees",
                         ifelse(data$teamID == "OAK", "Athletics", "Other"))

ggplot(data = data, aes(x = Payroll.Median, y = WP, color = highlight)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c(
    "Yankees" = "blue",
    "Athletics" = "green",
    "Other" = "gray"
  )) +
  labs(color = "Team Highlight") +
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black") 
  
ggplot(data = data, aes(x = Log..Payroll.Median., y = WP, color = highlight)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c(
    "Yankees" = "blue",
    "Athletics" = "green",
    "Other" = "gray"
  )) +
  labs(color = "Team Highlight") +
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "black") 


### Task 2


model = lm(WP ~ Payroll.Median, data)
model_log = lm(WP ~ Log..Payroll.Median., data)

data = data |> 
  mutate(
    norm_resids = model$residuals,
    log_resids = model_log$residuals
  )


team_avg_resids <- data |>
  group_by(teamID) |>
  summarise(
    norm_wins = mean(norm_resids) * 162,
    log_wins = mean(log_resids) * 162
  )


resid_long <- team_avg_resids |>
  pivot_longer(
    cols = c(norm_wins, log_wins),
    names_to = "model",
    values_to = "resid_wins"
  ) |>
  mutate(model = recode(model,
                        norm_wins = "Linear Model",
                        log_wins = "Log Model"))
ggplot(team_avg_resids, aes(x = reorder(teamID, norm_wins), y = norm_wins)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Average Residuals by Team (Linear Model)",
    x = "Team",
    y = "Avg Residual (Wins)"
  ) +
  theme_minimal()

ggplot(team_avg_resids, aes(x = reorder(teamID, log_wins), y = log_wins)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Average Residuals by Team (Log Model)",
    x = "Team",
    y = "Avg Residual (Wins)"
  ) +
  theme_minimal()
DF