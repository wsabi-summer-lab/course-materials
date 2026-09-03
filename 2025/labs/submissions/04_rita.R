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

##############
### PART 2 ###
##############

# load data
ncaab_results = read_csv("../data/04_ncaab-results.csv.gz")
ncaab_team_info = read_csv("../data/04_ncaab-teams.csv.gz")

#task 1
names(field_goals)
head(field_goals)
str(field_goals)

set.seed(100)                                 
n = nrow(field_goals)
train_indices = sample(seq_len(n), size = 0.7 * n)
train_fg = field_goals[train_indices, ]
test_fg  = field_goals[-train_indices, ]

model_SLR = lm(fg_made ~ ydl + kq, data = train_fg)
model_logR = glm(fg_made ~ ydl + kq, data = train_fg, family = "binomial")  
model_quadLR = glm(fg_made ~ ydl + I(ydl^2) + kq, data = train_fg, family = "binomial")

pred1 = predict(model_SLR, newdata = test_fg, type = "response")
classify_pred1 = ifelse(pred1 > 0.5, 1, 0)
accuracy_1 = mean(classify_pred == test_fg$fg_made)

pred2 = predict(model_logR, newdata = test_fg, type = "response")
classify_pred2 = ifelse(pred2 > 0.5, 1, 0)
accuracy_2 = mean(classify_pred2 == test_fg$fg_made)

pred3 = predict(model_quadLR, newdata = test_fg, type = "response")
classify_pred3 = ifelse(pred3 > 0.5, 1, 0)
accuracy_3 = mean(classify_pred3 == test_fg$fg_made)

summary(model_logR)
summary(pred1)

test_fg$pred_prob_model2 = pred2
ggplot(test_fg, aes(x = ydl)) +
  geom_point(aes(y = pred_prob_model2, color = factor(fg_made)), alpha = 0.6) +
  geom_smooth(
    aes(y = fg_made),
    method       = "glm",
    method.args = list(family = "binomial"),
    se           = FALSE,
    color        = "black",
    formula      = y ~ x
  ) +
  labs(
    x     = "Yardline (ydl)",
    y     = "Predicted Probability (Model 2)",
    color = "Actual Outcome\n(fg_made = 1 made, 0 missed)"
  ) +
  theme_minimal() +
  ggtitle("Field Goal: Predicted Probability vs. Yardline\n(Logistic Fit in Black)")


#task 2
names(ncaab_results)
names(ncaab_team_info)

ncaab_2023 = ncaab_results %>% filter(Season == 2023)
ncaab_2023 = subset(ncaab_2023, WLoc != "N")

ncaab_2023 = ncaab_2023 %>%
  mutate(
   home_win = ifelse(WLoc == "H", 1, 0),
  )
beta_zero = 1

ncaab_2023 = ncaab_results %>% 
  left_join(ncaab_team_info, by = c("WTeamID" = "TeamID")) %>%
  rename(WTeamName = TeamName)

ncaab_2023 = ncaab_2023 %>% 
  left_join(ncaab_team_info, by = c("LTeamID" = "TeamID")) %>%
  rename(LTeamName = TeamName)

all_teams = sort(unique(c(ncaab_2023$WTeamName, ncaab_2023$LTeamName)))


ncaab_2023 = ncaab_2023 %>%
  mutate(
    home_id = if_else(WLoc == "H", WTeamID, LTeamID),
    away_id = if_else(WLoc == "H", LTeamID, WTeamID)
  )
all_team_ids = sort(unique(c(ncaab_2023$home_id, ncaab_2023$away_id)))

HA_matrix = matrix(0, nrow = nrow(ncaab_2023), ncol = length(all_teams))
colnames(HA_matrix) = all_teams

for (i in seq_len(nrow(ncaab_2023))) {
  w = as.character(ncaab_2023$WTeamName[i])
  l = as.character(ncaab_2023$LTeamName[i])
  loc = ncaab_results$WLoc[i]
  
  if (loc == "H") {
    HA_matrix[i,w] = 1
    HA_matrix[i, l] = -1
  } else if (loc == "A") {
    HA_matrix[i,w] = -1
    HA_matrix[i, l] = 1
  } else {
    HA_matrix[i,w] = 0
    HA_matrix[i, l] = 0
  }
}

sum(HA_matrix == -1)

model_HA = glm(ncaab_2023$home_win ~ HA_matrix, family = "binomial")


coefs <- coef(model_HA)
team_coefs <- coeffs[!names(coefs) %in% "home_advantage"] 

team_coefs <- coefs[!names(coefs) %in% c("(Intercept)", "home_advantage")]
clean_names <- gsub("^home_away_matrix", "", names(coeffs))

keep_indices <- !(clean_names %in% c("home_advantage", "(Intercept)"))

team_coefs <- coefs[keep_indices]
team_names  <- clean_names[keep_indices]

coef_df <- data.frame(
  Team = team_names,
  Strength = team_coefs
)
  ggplot(coef_df, aes(x = reorder(Team, Coefficient), y = Coefficient)) +
    geom_col(fill = "blue") +
    coord_flip() +
    labs(
      x     = "Team",
      y     = "Estimated Log-Odds Strength",
      title = "Bradley–Terry Coefficients: 2023 NCAA Men’s Basketball"
    ) +
    theme_minimal()

purdue = coef_df %>% 
  filter(Team == "Purdue") %>% 
  pull(Strength)

uconn = coef_df %>% 
  filter(Team == "Connecticut") %>% 
  pull(Strength)

p = 1/(1+exp(-(purdue-uconn)))

p

