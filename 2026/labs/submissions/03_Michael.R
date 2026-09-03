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

# Model 1: Multiple Linear Regression
lm_model = lm(fg_made ~ ydl + kq, data = field_goals)
summary(lm_model)

# Model 2: Simple Logistic Regression
logit_model1 = glm(fg_made ~ ydl + kq, data = field_goals, family = binomial)
summary(logit_model1)
confint(logit_model1)

# Model 3: Multiple Logistic Regression with Quadratic Log-Yardline and Kicker Quality
logit_model2 = glm(fg_made ~ log(ydl) + I(log(ydl)^2) + kq, data = field_goals, family = binomial)
summary(logit_model2)
confint(logit_model2)

# log loss function
log_loss = function(actual, predicted) {
  predicted = pmin(pmax(predicted, 1e-10), 1 - 1e-10)
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

# train/test split (80/20)
n = nrow(field_goals)
train_idx = sample(1:n, size = 0.8 * n)
train = field_goals[train_idx, ]
test  = field_goals[-train_idx, ]

# Model 1: RMSE
m1 = lm(fg_made ~ ydl + kq, data = train)
p1 = predict(m1, newdata = test)
lm_rmse = sqrt(mean((test$fg_made - p1)^2))
cat("Linear RMSE:", lm_rmse, "\n")

# Model 2: Log Loss 
m2 = glm(fg_made ~ ydl + kq, data = train, family = binomial)
p2 = predict(m2, newdata = test, type = "response")
cat("Logistic Log Loss:", log_loss(test$fg_made, p2), "\n")

# Model 3: Log Loss 
m3 = glm(fg_made ~ log(ydl) + I(log(ydl)^2) + kq, data = train, family = binomial)
p3 = predict(m3, newdata = test, type = "response")
cat("Quadratic Log-Yardline Logistic Log Loss:", log_loss(test$fg_made, p3), "\n")

#Third model is the best since the log loss is the lowest
# Coefficient estimates, SEs, and 95% CIs for selected model (logit_model2)
summary(logit_model2)
confint(logit_model2)

# Exponentiate coefficients for odds ratio interpretation
exp(coef(logit_model2))
exp(confint(logit_model2))

#Intepretation: the coeffients of log(ydl) (coeff = -0.41) and log(ydl)^2 (coeff = -0.56) there is some uncertiany in the CI for log(ydl) becuase it spans over 0, but the log(ydl)^2 does not so we can syay it is accelerating negativitly. The coeff for kq of 0.264 (CI:  (0.16, 0.369)) we are confident in an positive significant effect of better kickers

# Plot predicted make probability vs yardline with confidence band

# create prediction grid
ydl_grid = data.frame(
  ydl = seq(min(field_goals$ydl), max(field_goals$ydl), length.out = 200),
  kq = mean(field_goals$kq)  # hold kicker quality at mean
)

# get predictions on log-odds scale with SE
pred = predict(logit_model2, newdata = ydl_grid, type = "link", se.fit = TRUE)

# convert to probability scale with 95% CI
ydl_grid$fit    = plogis(pred$fit)
ydl_grid$lower  = plogis(pred$fit - 1.96 * pred$se.fit)
ydl_grid$upper  = plogis(pred$fit + 1.96 * pred$se.fit)

# bin observed make rates by yardline
field_goals$ydl_bin = cut(field_goals$ydl, breaks = seq(0, 70, by = 5))
binned = field_goals %>%
  group_by(ydl_bin) %>%
  summarise(
    make_rate = mean(fg_made),
    ydl_mid   = mean(ydl)
  )

# plot
ggplot() +
  geom_ribbon(data = ydl_grid, aes(x = ydl, ymin = lower, ymax = upper), 
              alpha = 0.2, fill = "blue") +
  geom_line(data = ydl_grid, aes(x = ydl, y = fit), color = "blue") +
  geom_point(data = binned, aes(x = ydl_mid, y = make_rate), 
             color = "red", size = 3) +
  labs(title = "Predicted Field Goal Make Probability vs Yardline",
       x = "Yardline (yards from opponent's end zone)",
       y = "Make Probability") +
  theme_minimal()

#The 95% confidence band shows the uncertainty in the model's estimated probability which is the avergae make rate at a given yard. However, a kick is still random and is either a make or a miss and that has is own CI of uncertainty.


##############
### PART 2 ###
##############

# Task 1: Filter to 2023-2024 season
ncaab_2023 = ncaab_results %>% filter(Season == 2023)

# Task 2: Recode into Bradley-Terry model dataset
ncaab_model = ncaab_2023 %>%
  mutate(
    home_team  = ifelse(WLoc == "H", WTeamID, LTeamID),
    away_team  = ifelse(WLoc == "H", LTeamID, WTeamID),
    home_win   = ifelse(WLoc == "N", 1, ifelse(WLoc == "H", 1, 0)),
    is_neutral = ifelse(WLoc == "N", 1, 0)
  )

# Task 3: Fit Bradley-Terry model
all_teams = sort(unique(c(ncaab_model$home_team, ncaab_model$away_team)))
n_teams = length(all_teams)
n_games = nrow(ncaab_model)

# build design matrix: +1 home, -1 away
X = matrix(0, nrow = n_games, ncol = n_teams)
colnames(X) = all_teams

for (i in 1:n_games) {
  h = as.character(ncaab_model$home_team[i])
  a = as.character(ncaab_model$away_team[i])
  X[i, h] =  1
  X[i, a] = -1
}

# drop first team for identifiability, add neutral offset column
X_fit = cbind(X[, -1], neutral_offset = -ncaab_model$is_neutral)

bt_df = as.data.frame(X_fit)
bt_df$home_win = ncaab_model$home_win

bt_model = glm(home_win ~ ., data = bt_df, family = binomial)

# extract ratings, reference team gets 0
ratings = c(0, coef(bt_model)[2:n_teams])
names(ratings) = all_teams
ratings = ratings - mean(ratings)

team_ratings = data.frame(
  team_id = as.integer(names(ratings)),
  rating  = ratings
) %>%
  left_join(ncaab_team_info, by = c("team_id" = "TeamID")) %>%
  arrange(desc(rating))

# Task 4: Visualize team ratings with uncertainty intervals

# extract SEs for team coefficients
coef_summary = summary(bt_model)$coefficients
coef_names = rownames(coef_summary)

# build rating + SE table
rating_se = data.frame(
  team_id = as.integer(all_teams),
  rating  = ratings
)

# get SEs from model (reference team gets SE = 0)
ses = c(0, coef_summary[2:n_teams, 2])
rating_se$se = ses
rating_se$lower = rating_se$rating - 1.96 * rating_se$se
rating_se$upper = rating_se$rating + 1.96 * rating_se$se

# join team names
rating_se = rating_se %>%
  left_join(ncaab_team_info, by = c("team_id" = "TeamID")) %>%
  arrange(desc(rating))

# plot top 25 teams
rating_se %>%
  slice(1:25) %>%
  ggplot(aes(x = reorder(TeamName, rating), y = rating, 
             ymin = lower, ymax = upper)) +
  geom_pointrange() +
  coord_flip() +
  labs(title = "Bradley-Terry Team Ratings with 95% CI",
       x = "Team", y = "Rating") +
  theme_minimal()

# Task 5: Purdue vs UConn neutral site win probability

# get ratings for Purdue and Connecticut
purdue_rating = team_ratings %>% filter(TeamName == "Purdue") %>% pull(rating)
uconn_rating  = team_ratings %>% filter(TeamName == "Connecticut") %>% pull(rating)

# rating difference
delta = purdue_rating - uconn_rating

# get SE of delta from vcov matrix
purdue_id = as.character(team_ratings %>% filter(TeamName == "Purdue") %>% pull(team_id))
uconn_id  = as.character(team_ratings %>% filter(TeamName == "Connecticut") %>% pull(team_id))

vcov_mat = vcov(bt_model)
purdue_coef = paste0("`", purdue_id, "`")
uconn_coef  = paste0("`", uconn_id, "`")

se_delta = sqrt(
  vcov_mat[purdue_coef, purdue_coef] + 
    vcov_mat[uconn_coef, uconn_coef] - 
    2 * vcov_mat[purdue_coef, uconn_coef]
)

# 95% CI for delta
delta_lower = delta - 1.96 * se_delta
delta_upper = delta + 1.96 * se_delta

# convert to win probabilities (beta_0 = 0 for neutral site)
p_purdue = plogis(delta)
p_purdue_lower = plogis(delta_lower)
p_purdue_upper = plogis(delta_upper)

# moneyline function
moneyline = function(p) {
  ifelse(p >= 0.5, -100 * p / (1 - p), 100 * (1 - p) / p)
}

cat("Purdue win probability:", round(p_purdue, 3), "\n")
cat("95% CI: [", round(p_purdue_lower, 3), ",", round(p_purdue_upper, 3), "]\n")
cat("Purdue moneyline:", round(moneyline(p_purdue), 1), "\n")
cat("CI moneyline: [", round(moneyline(p_purdue_lower), 1), ",", round(moneyline(p_purdue_upper), 1), "]\n\n")

cat("Connecticut win probability:", round(1 - p_purdue, 3), "\n")
cat("Connecticut moneyline:", round(moneyline(1 - p_purdue), 1), "\n")

#CI for the win probabilty reflects the uncertianty in the estimated ratings, and hten even if we knew the true win probability exactly, the actual game outcome is still a single Bernoulli draw so the game still has randomness in itself. 