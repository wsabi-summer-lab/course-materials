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
field_goals = read_csv("../data/04_field-goals.csv.gz")

set.seed(42)
n <- nrow(nba_factors)
train_indices <- sample(seq_len(n), size = 0.8 * n)
field_goals_train = field_goals[train_indices, ]
field_goals_test = field_goals[-train_indices, ]

model_fg1 = lm(fg_made ~ ydl + kq, data = field_goals_train)
model_fg2 = glm(fg_made ~ ydl + kq, data = field_goals_train, family = "binomial")
model_fg3 = glm(fg_made ~ ydl + I(ydl ^ 2) + I(ydl ^ 3) + kq,
                data = field_goals_train, family = "binomial")

fg1_pred = (predict(model_fg1, newdata=field_goals_test) >= 0.5)
fg2_pred = (predict(model_fg2, newdata=field_goals_test, type = "response") >= 0.5)
fg3_pred = (predict(model_fg3, newdata=field_goals_test, type = "response") >= 0.5)

# Compute accuracy
fg1_accuracy = mean(fg1_pred == field_goals_test$fg_made)
fg2_accuracy = mean(fg2_pred == field_goals_test$fg_made)
fg3_accuracy = mean(fg3_pred == field_goals_test$fg_made)
print(c(fg1_accuracy, fg2_accuracy, fg3_accuracy))
# The third model (kq + cubic on ydl logistic regression model) has the highest accuracy

model_fg3$coefficients
# The log odds decrease by 1 when the yard line increases by 0.445
# and increases by 1 when the kicker quality increases by 0.254
# (Squared and cubic coefficients are very small)

field_goals_test$fg_made_pred = predict(model_fg3, newdata=field_goals_test, type = "response")
field_goals_test %>% 
  ggplot(aes(x = ydl, y = fg_made_pred)) +
  geom_point(aes(color = as.factor(fg_made)), alpha = 0.6) +
  labs(title = "Predicted vs Actual FG Made",
       x = "Yard Line",
       y = "Predicted FG Probability",
       color = "Actual FG Result") +
  scale_color_manual(values = c("red", "blue"), label=c('Missed','Made')) +
  theme_minimal()


##############
### PART 2 ###
##############

# load data
ncaab_results = read_csv("../data/04_ncaab-results.csv.gz")
ncaab_team_info = read_csv("../data/04_ncaab-teams.csv.gz") %>% 
  filter(LastD1Season >= 2023 & FirstD1Season <= 2023)

ncaab_results = ncaab_results %>% filter(Season == 2023)
n_games = nrow(ncaab_results)
n_teams = nrow(ncaab_team_info)
X = matrix(0, nrow = n_games, ncol = n_teams + 1) 
for (i in 1:n_games) {
  win_id = which(ncaab_team_info$TeamID == ncaab_results$WTeamID[i])
  lose_id = which(ncaab_team_info$TeamID == ncaab_results$LTeamID[i])
  if (ncaab_results$WLoc[i] == 'H') {
    X[i, 1] = 1
    X[i, win_id + 1] = 1
    X[i, lose_id + 1] = -1
  }
  else if (ncaab_results$WLoc[i] == 'A') {
    X[i, 1] = 1
    X[i, win_id + 1] = -1
    X[i, lose_id + 1] = 1
  }
  else {
    X[i, 1] = 0
    X[i, win_id + 1] = 0.5
    X[i, lose_id + 1] = -0.5
  }
}
ncaab_results$HomeW = ncaab_results$WLoc != 'A'

X = X[, -ncol(X)]

model = glm(HomeW ~ X + 0, data = ncaab_results, family = "binomial")
summary(model)

ncaab_team_info$power = c(model$coefficients[2:n_teams], 0)
ncaab_team_info %>%
  .[sample(n_teams, 25),] %>%
  ggplot(aes(x = reorder(TeamName, power), y = power)) +
  geom_point() +
  coord_flip() +
  labs(title = "NCAAB Team Power Ratings",
       x = "Team",
       y = "Power Rating") +
  theme_minimal()


uconn_id = which(ncaab_team_info$TeamName == "Connecticut")
purdue_id = which(ncaab_team_info$TeamName == "Purdue")
# UConn is the underdog against Purdue
as.numeric(exp(-ncaab_team_info$power[uconn_id] + ncaab_team_info$power[purdue_id]) * 100)