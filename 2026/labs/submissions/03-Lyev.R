#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(4)

##############
### PART 1 ###
##############

# load data
field_goals = read_csv("../data/03_field-goals.csv.gz")

#Initial plot

fg_binned <- field_goals %>%
  group_by(ydl) %>%                 # one row per yard line
  summarize(prob_make = mean(fg_made), .groups = "drop")

plot1 = ggplot(fg_binned, aes(x = ydl, y = prob_make)) +
  geom_point(color = "dodgerblue", size = 2) +
  labs(
    x = "Field Goal Distance (ydl)",
    y = "Probability of Make",
    title = "FG Make Probability by Yard Line"
  ) +
  theme_minimal()
plot1

#Model 1: linear model with kicker quality
model1 = lm(data = field_goals, fg_made ~ ydl + kq)
summary(model1)

#Model 2: logistic model with kicker quality
model2 = glm(data = field_goals, fg_made ~ ydl + kq, family = "binomial")
summary(model2)
confint(model2)
#ydl coefficient: estimate = -0.106, se = 0.003188, confint = [ -0.1122534 , -0.09975375 ]
#interpretation, increase of 1 yard decreases log odds by -0.106, multiplying odds by e^(-0.106) or ~0.9

#kq coefficient: estimate = 0.276526, se = 0.053498, confint = [ 0.1721175 ,  0.38185379 ]
#interpretation: increase of 1 kq increases log odds by 0.05, multiplying odds by 1.05 (e^(0.05))


#Model 3: logistic with kicker quality and quadratic
model3 = glm(data = field_goals, fg_made ~ ydl + I(ydl^2) + kq, family = "binomial")
summary(model3)


### OUT OF SAMPLE PREDICTIVE PERFORMANCE TEST

n = nrow(field_goals)
train_index = sample(1:n, size = n * 0.8)

fg_train = field_goals[train_index, ]
fg_test  = field_goals[-train_index, ]

#Remake models for training data
model1_train = lm(data = fg_train, fg_made ~ ydl + kq)
model2_train = glm(data = fg_train, fg_made ~ ydl + kq, family = "binomial")
model3_train = glm(data = fg_train, fg_made ~ ydl + I(ydl^2) + kq, family = "binomial")

#Predict test data
fg_test = fg_test %>%
  mutate(
    m1_pred = predict(model1_train, .),
    m1_pred = case_when(m1_pred > 0.9999 ~ 0.9999 , m1_pred <=0.9999 ~ m1_pred),
    m2_pred = predict(model2_train, type = "response", .),
    m3_pred = predict(model3_train, type = "response", .),
    m1_logloss = fg_made*log(m1_pred) + (1-fg_made)*log(1-m1_pred),
    m2_logloss = fg_made*log(m2_pred) + (1-fg_made)*log(1-m2_pred),
    m3_logloss = fg_made*log(m3_pred) + (1-fg_made)*log(1-m3_pred)
    )

#Find average log loss
m1_avg_logloss = (-1)*mean(fg_test$m1_logloss) # = 0.3499157
m2_avg_logloss = (-1)*mean(fg_test$m2_logloss) # = 0.3459867
m3_avg_logloss = (-1)*mean(fg_test$m3_logloss) # = 0.3455214
n = nrow(fg_test)
m1_se_logloss = sd(fg_test$m1_logloss) / sqrt(n-1) # 0.01067847
m2_se_logloss = sd(fg_test$m2_logloss) / sqrt(n-1) # 0.01076823
m3_se_logloss = sd(fg_test$m3_logloss) / sqrt(n-1) # 0.01067689

#All three could be feasible, but we will select model2
#Because if you look at model 3, the quadratic coefficient is almost zero, meaning the models are the same

summary(model2_train)
#increase of 1 yard decreases log odds by -0.104, multiplying odds by e^(-0.104) or ~0.9
#increase of 1 kq decreases log odds by 0.278, multiplying odds by e^(0.278) or ~1.32

#Plot (have to use no kq)
fg_binned = fg_binned %>%
  mutate(
    kq = mean(field_goals$kq),
    m2_pred = predict(model2_train, type = "response", .)
  )

model2_plot = ggplot(data = fg_binned) +
  geom_point(aes(x = ydl, y = prob_make), color = "dodgerblue", size = 2) +
  geom_line(aes(x = ydl, y = m2_pred), color = "salmon", linewidth = 1) +
  geom_ribbon(aes(x=ydl, ymin = m2_pred - 1.96 * m2_se_logloss,
                     ymax = m2_pred + 1.96 * m2_se_logloss),
                 alpha = 0.25,
                 fill = "gray50") +
  labs(
    x = "Field Goal Distance (ydl)",
    y = "Probability of Make",
    title = "FG Make Probability by Yard Line"
  ) +
  theme_minimal()
model2_plot

#The uncertainty of a single kick is much larger, since the kick takes on bernoulli values, whereas the uncertainty of the probability is much smaller



##############
### PART 2 ###
##############

# load data
ncaab_results = read_csv("../data/03_ncaab-results.csv.gz")
ncaab_team_info = read_csv("../data/03_ncaab-teams.csv.gz")

ncaab_results = ncaab_results %>%
  filter(Season == 2023)

new_ncaab = ncaab_results %>%
  mutate(
    Home_ID = case_when(
      WLoc == "H" ~ WTeamID,
      WLoc == "N" ~ WTeamID,
      WLoc == "A" ~ LTeamID
    ),
    Away_ID = case_when(
      WLoc == "H" ~ LTeamID,
      WLoc == "N" ~ LTeamID,
      WLoc == "A" ~ WTeamID
    ),
    Home_Score = case_when(
      WLoc == "H" ~ WScore,
      WLoc == "N" ~ WScore,
      WLoc == "A" ~ LScore
    ),
    Away_Score = case_when(
      WLoc == "H" ~ LScore,
      WLoc == "N" ~ LScore,
      WLoc == "A" ~ WScore
    ),
    score_diff = Home_Score - Away_Score,
    is_neutral = case_when(
      WLoc == "H" ~ 0,
      WLoc == "N" ~ 1,
      WLoc == "A" ~ 0
    ),
    home_win = case_when(
      score_diff > 0 ~ 1,
      score_diff < 0 ~ 0
    )
  ) %>%
  select(Home_ID, Away_ID, Home_Score, Away_Score, score_diff, home_win, is_neutral)

# Make team factor
teams_2023 <- sort(unique(c(new_ncaab$Home_ID, new_ncaab$Away_ID)))
new_ncaab$home_f <- factor(new_ncaab$Home_ID, levels = teams_2023)
new_ncaab$away_f <- factor(new_ncaab$Away_ID, levels = teams_2023)

# Model matrix for home and away teams
X_home <- model.matrix(~ home_f - 1, new_ncaab)
X_away <- model.matrix(~ away_f - 1, new_ncaab)

# Difference in abilities: home - away
X_diff <- X_home - X_away

#Make model
bt_data <- data.frame(
  home_win = new_ncaab$home_win,
  is_neutral = new_ncaab$is_neutral,
  X_diff
)

bt_model <- glm(
  home_win ~ is_neutral + .,
  data = bt_data,
  family = binomial(link = "logit")
)

summary(bt_model) #Centered around one reference team that was dropped

# Extract all team ability coefficients
team_coefs <- coef(bt_model)[grep("^home_f", names(coef(bt_model)))]

# Compute confidence intervals
team_ci <- confint.default(bt_model)[grep("^home_f", names(coef(bt_model))), ] #used default to save time

# Build table
team_table <- data.frame(
  team = gsub("home_f", "", names(team_coefs)),
  estimate = team_coefs,
  ci_lower = team_ci[, 1],
  ci_upper = team_ci[, 2],
  row.names = NULL
)

ncaab_team_info$TeamID = as.character(ncaab_team_info$TeamID)

team_table = team_table %>%
  rename(TeamID = team) %>%
  left_join(ncaab_team_info) %>%
  arrange(estimate) %>%
  mutate(TeamName = factor(TeamName, levels = TeamName))


ggplot(team_table, aes(x = estimate, y = TeamName)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.25) +
  labs(
    title = "Team Strength Estimates (Bradley–Terry Model)",
    x = "Estimated Ability (log-odds scale)",
    y = "Team"
  ) +
  theme_minimal(base_size = 14)


###Make final moneyline

purdue_score = team_table$estimate[team_table$TeamName == "Purdue"]
uconn_score = team_table$estimate[team_table$TeamName == "Connecticut"]

purdue_upper = team_table$ci_upper[team_table$TeamName == "Purdue"]
purdue_lower = team_table$ci_lower[team_table$TeamName == "Purdue"]
uconn_upper = team_table$ci_upper[team_table$TeamName == "Connecticut"]
uconn_lower = team_table$ci_lower[team_table$TeamName == "Connecticut"]

prob_purdue = 1/(1+exp(-(purdue_score-uconn_score)))
# 0.584872 purdue wins

diff_low = purdue_lower - uconn_upper
diff_hi = purdue_upper - uconn_lower

#Confidence interval
prob_low = 1/(1+exp(-diff_low))
# 0.04461338 purdue wins

prob_hi = 1/(1+exp(-diff_hi))
# 0.9770158 purdue wins


moneyline_purdue = (-100)*prob_purdue / (1-prob_purdue) # = - 140.8895

#Estimated win probability for a difference has less uncertainty because it is an estimate of the expected value, whereas the estimate for a single game probability can have much more variance

