#############
### SETUP ###
#############

rm(list=ls())

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

##############
### PART 1 ###
##############

# load data
field_goals = read_csv("../data/04_field-goals.csv.gz")
names(field_goals)

# EDA scatterplots
ggplot(field_goals, aes(x = fg_made, y = ydl)) +
  geom_point() # neg relationship?
ggplot(field_goals, aes(x = fg_made, y = kq)) +
  geom_point() # positive relationship?

## model 1: multiple regression with ydl and kq (fg_made = B0 + B1(ydl) + B2(kq))
model_1 <- lm(fg_made ~ ydl + kq, data = field_goals)
summary(model_1)

## model 2: spline?
spline_model <- lm(fg_made ~ splines::bs(ydl, degree = 3, df = 5) + splines::bs(kq, degree = 3, df = 5),
                  data = field_goals)
summary(spline_model)

## model 3: logistic regression
logit_model <- glm(fg_made ~ ydl + kq, data = field_goals, family = "binomial")
summary(logit_model)

## find best model
set.seed(3)
n <- dim(field_goals)[1]
train_indices <- sample(1:n, size = floor(0.8 * n))

train <- field_goals[train_indices, ]
test <- field_goals[-train_indices, ]

model_1_train <- lm(fg_made ~ ydl + kq, data = train)
spline_train <- lm(fg_made ~ splines::bs(ydl, degree = 3, df = 5) + splines::bs(kq, degree = 3, df = 5),
                   data = train)
logit_train <- glm(fg_made ~ ydl + kq, data = train, family = "binomial")

pred_1 <- predict(model_1_train, newdata = test)
pred_spline <- predict(spline_train, newdata = test)
pred_logit <- predict(logit_train, newdata = test, type = "response")

# MSE
mse_1 <- mean((test$fg_made - pred_1)^2)
mse_spline <- mean((test$fg_made - pred_spline)^2)
mse_logit <- mean((test$fg_made - pred_logit)^2)

print(c("Linear regression model MSE" = mse_1, "Spline model MSE" = mse_spline, "Logistic regression model MSE" = mse_logit))
# logit MSE is lower

## interpretation of logistic regression coefs:
# increase in 1 yard away from opponent's end zone --> decrease in 0.1ish in log odds on average; 
# i.e., odds decrease by 10% for each 1 yard we move further back from end zone.
# increase in 1 unit of kicker quality --> increase in 0.277 ish in log odds on average;
# i.e., odds decrease by 27.7% ish for a unit increase in kicker quality.

# new col for expected fg probabilities
field_goals <- field_goals %>%
  mutate(exp_fg_prob = predict(logit_model, newdata = field_goals, type = "response"))

# predicted vs. actual outcomes
ggplot(field_goals, aes(x = fg_made, y = exp_fg_prob, color = kq)) +
  geom_point(alpha = 0.5) +
  labs(title = "Logistic Regression: Expected vs. Actual Probability of Field Goal",
       x = "If Field Goal was actually made",
       y = "Probability of Field Goal") +
  theme_minimal()

##############
### PART 2 ###
##############

# load data
ncaab_results = read_csv("../data/04_ncaab-results.csv.gz")
ncaab_team_info = read_csv("../data/04_ncaab-teams.csv.gz")

names(ncaab_results)
names(ncaab_team_info)
ncaab_2023 <- ncaab_results %>% 
  filter(Season == 2023)

# rename based on team ID instead of team number
ncaab_2023 <- ncaab_2023 %>%
  left_join(ncaab_team_info, by = c("WTeamID" = "TeamID")) %>%
  rename(WTeamName = TeamName)
ncaab_2023 <- ncaab_2023 %>%
  left_join(ncaab_team_info, by = c("LTeamID" = "TeamID")) %>%
  rename(LTeamName = TeamName)

# team IDs
team_ids <- sort(unique(c(ncaab_2023$WTeamName, ncaab_2023$LTeamName)))

# Create a design matrix with columns: B0 (home field), then one column per team
X <- matrix(0, nrow = nrow(ncaab_2023), ncol = 1 + length(team_ids))
colnames(X) <- c("B0", paste0("B", team_ids))
X[, "B0"] <- 1  # Home field advantage

# Loop through each game to fill in the matrix
for (i in 1:nrow(ncaab_2023)) {
  row <- ncaab_2023[i, ]
  winner <- row$WTeamName
  loser <- row$LTeamName
  loc <- row$WLoc
  
  if (loc == "H") {
    home <- winner
    away <- loser
    outcome <- 1
  } else if (loc == "A") {
    home <- loser
    away <- winner
    outcome <- 0
  } else {
    next  # skip neutral site games
  }
  
  # Set +1 for home team, -1 for away team
  X[i, paste0("B", home)] <- 1
  X[i, paste0("B", away)] <- -1
  
  # Store outcome (1 if home team won, 0 if away team won)
  ncaab_2023$outcome[i] <- outcome
}

# Convert X to a data frame and add the outcome
X_df <- as.data.frame(X)
X_df$outcome <- ncaab_2023$outcome

# Remove rows with NA outcome (i.e., neutral-site games)
X_df <- X_df %>% filter(!is.na(outcome))

# Fit the Bradley-Terry model using logistic regression
bradley_terry_model <- glm(outcome ~ . , data = X_df, family = "binomial")
summary(bradley_terry_model)

coeffs <- coef(bradley_terry_model)
coeffs_df <- data.frame(
  term = names(coeffs),
  beta = as.numeric(coeffs)
)
coeffs_df <- coeffs_df %>% filter(term != "B0")

ggplot(coeffs_df, aes(x = reorder(term, beta), y = beta)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Estimated Team Strengths (Beta)", x = "Team Name", y = "Beta") +
  theme_minimal()

# top 20 teams
top_20 <- coeffs_df %>% 
  arrange(desc(beta)) %>%
  slice_head(n = 20)

ggplot(top_20, aes(x = reorder(term, beta), y = beta)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Teams by Estimated Strength (Beta)", x = "Team Name", y = "Beta") +
  theme_minimal()

## coef interpretation:
# each coefficient is the power rating (or strength) of each team. 
# B0 (intercept) is home field advantage.

## Purdue Boilermakers ("BPurdue") vs. UConn Huskies ("BConnecticut")
purdue_beta <- coeffs_df %>% filter(term == "BPurdue") %>% pull(beta)
uconn_beta  <- coeffs_df %>% filter(term == "BConnecticut") %>% pull(beta)
log_odds_diff <- purdue_beta - uconn_beta + 0.5 # neutral site so no home field adv

# Using rule of thumb (1 log-odds unit ≈ 7 points)
spread_estimate <- log_odds_diff * 7
cat("Estimated Vegas spread (Purdue - UConn):", round(spread_estimate, 1), "points\n")
# so Purdue is favored by this many points

# probability
prob <- 1 / (1 + exp(-(0.5 + purdue_beta - uconn_beta)))
prob
# why 0.68?