# 04 Logistic Regression

library(ggplot2)
library(tidyverse)
library(Metrics)
library(dplyr)

# 4.1 Field Goal Success Probability

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

field_goals = read_csv("04_field-goals.csv.gz")

head(field_goals)

train_index <- createDataPartition(field_goals$fg_made, p = 0.8, list = FALSE)
train_data <- field_goals[train_index, ]
test_data <- field_goals[-train_index, ]

# 4.1.2

# Linear model with just yard line

linear_model <- lm(fg_made ~ ydl, data = train_data)

# Multivariate Linear model with yard line and kicker quality

multi_linear_model <- lm(fg_made ~ ydl + kq, data = train_data)

# Logistic Model

logistic_model <- glm(fg_made ~ ydl + kq, data = train_data)

# Predictions

pred_linear <- predict(linear_model, newdata = test_data)
pred_multi <- predict(multi_linear_model, newdata = test_data)
pred_logistic <- predict(logistic_model, newdata = test_data, type = "response")

clip_probs <- function(x) {
  pmin(pmax(x, 1e-15), 1 - 1e-15)  # avoid log(0)
}

pred_linear_clipped <- clip_probs(pred_linear)
pred_multi_clipped <- clip_probs(pred_multi)
pred_logistic_clipped <- clip_probs(pred_logistic)

mean((test_data$fg_made - pred_linear)^2)
mean((test_data$fg_made - pred_multi_clipped)^2)
mean((test_data$fg_made - pred_logistic_clipped)^2)

# Interpretation of Coefficients for Multi Linear

coef(multi_linear_model)

# Holding kicker quality constant, each additional yard decreases probability of success by 1.2%
# Holding yard line constant, each additional point of kicker quality increases probability of success by 3.4%

# Plot of Predictions vs Actual outcomes

ggplot(test_data, aes(x = pred_multi, y = fg_made)) +
  geom_jitter(height = 0.1, width = 0, alpha = 0.4, color = "darkblue") +
  labs(
    title = "Predicted vs Actual FG Made (Test Data)",
    x = "Predicted Value",
    y = "Actual FG Made (0 = Miss, 1 = Make)"
  ) +
  theme_minimal()

# 4.2 NCAA Basketball Power Scores

ncaab_results = read_csv("04_ncaab-results.csv.gz")

head(ncaab_results)

ncaab_2023_24 <- ncaab_results %>%
  filter(Season == 2023)

bt_df <- ncaab_2023_24 %>%
  mutate(
    HomeTeam = ifelse(WLoc == "H", WTeamID,
                      ifelse(WLoc == "A", LTeamID, NA)),
    AwayTeam = ifelse(WLoc == "H", LTeamID,
                      ifelse(WLoc == "A", WTeamID, NA)),
    HomeWin  = ifelse(WLoc == "H", 1,
                      ifelse(WLoc == "A", 0, NA))
  ) %>%
  filter(!is.na(HomeTeam)) %>%
  select(HomeTeam, AwayTeam, HomeWin)

team_ids <- sort(unique(c(bt_df$HomeTeam, bt_df$AwayTeam)))
team_ids <- as.character(team_ids)

bt_matrix <- matrix(0, nrow = nrow(bt_df), ncol = length(team_ids))
colnames(bt_matrix) <- team_ids

for (i in 1:nrow(bt_df)) {
  bt_matrix[i, as.character(bt_df$HomeTeam[i])] <- 1
  bt_matrix[i, as.character(bt_df$AwayTeam[i])] <- -1
}

bt_design <- as.data.frame(bt_matrix)
bt_design$home_field <- 1
bt_design$result <- bt_df$HomeWin

bt_model <- glm(result ~ . - result, data = bt_design, family = binomial())
summary(bt_model)

ncaa_logistic_model <- glm( ~ ydl + kq, data = train_data)

# The intercept is 0.62 which indicates the probability of winning for a home team of equal quality to the away team

coeffs <- coef(bt_model)

team_strengths <- coeffs[!names(coeffs) %in% c("(Intercept)", "home_field")]

team_strengths_df <- data.frame(
  TeamID = names(team_strengths),
  Strength = as.numeric(team_strengths)
) %>%
  arrange(desc(Strength)) %>%
  mutate(Rank = row_number())

ggplot(team_strengths_df, aes(x = reorder(TeamID, Strength), y = Strength)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "NCAAB Team Strength Rankings (Bradley-Terry Model)",
    x = "Team ID (higher = stronger)",
    y = "Estimated Strength (β)"
  ) +
  theme_minimal()

# The coefficients show the increase in the log odds for each team compared to the baseline (intercept)

ncaab_2023_24 <- ncaab_results %>%
  filter(Season == 2023)

ncaab_team_info = read_csv("04_ncaab-teams.csv.gz")

ncaab_team_info <- ncaab_team_info %>%
  filter(TeamName %in% c("Purdue", "Connecticut"))

bt_coeffs <- coef(bt_model)

names(bt_coeffs)

beta_1163 <- bt_coeffs["`1163`"]
beta_1345 <- bt_coeffs["`1345`"]

logit_diff <- beta_1163 - beta_1345
p_1163_beats_1345 <- plogis(logit_diff)
p_1345_beats_1163 <- 1 - p_1163_beats_1345

# Probability of UConn beating Purdue = 0.415
# Probability of Purdue beating UConn = 0.585

# Moneyline odds (no vig): UConn +141, Purdue -141
# Moneyline odds (5% vig): UConn +127, Purdue -156
