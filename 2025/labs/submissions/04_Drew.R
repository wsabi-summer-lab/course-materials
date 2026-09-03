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
linear_mod= lm(fg_made ~ ydl + kq, data = field_goals)
logistic_mod = glm(fg_made ~ ydl + kq, data = field_goals, family = "binomial")
polynomial_mod = lm(fg_made ~ poly(ydl, 2) + poly(kq, 2), data = field_goals)
#fix the code in 19-24 the match the data

n=nrow(field_goals)
train_index = sample(1:n,size = floor(.8*n))
train = slice(field_goals,train_index)
test = slice(field_goals,-train_index)
linear_mod= lm(fg_made ~ ydl + kq, data = train)
summary(linear_mod)

predictions1 = predict(linear_mod,test, type = "response")
MSE_linear=mean((test$fg_made-predictions)^2)

predictions2 = predict(logistic_mod,test, type = "response")
MSE_logistic=mean((test$fg_made-predictions)^2)

predictions3 = predict(polynomial_mod,test, type = "response")
MSE_polynomial=mean((test$fg_made-predictions)^2)

MSE_linear
MSE_logistic
MSE_polynomial

summary(logistic_mod)
##Question 1: The logistic regression model is a better fit than the linear regression model, as indicated by the lower MSE (Mean Squared Error) of the logistic model compared to the linear model.
 ## Question 2: if ydl = 0 and kq = 0, the model’s predicted probability of “success” is about 98.3 %. 
#That means each additional yard (increasing ydl by 1) multiplies the odds of success by about 0.90 (a 10 % reduction in the odds), holding kq fixed
#So every one-unit bump in kq multiplies the odds of success by about 1.318—i.e., a 31.8 % increase in odds—after controlling for yard line.

#plot predictions_log vs field_goals data
predictions_log= predict(logistic_mod, newdata = field_goals, type = "response")
test$predictions_log = predictions2
#plot predictions_log vs field_goals data
ggplot(field_goals, aes(x = ydl, y = fg_made)) +
  geom_point(aes(y=predictions_log,color=factor(fg_made)), alpha=.6) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, formula = y ~ x) +
  labs(title = "Logistic Regression Predictions vs Field Goals Data",
       x = "Yard Line (ydl)",
       y = "Field Goals Made (fg_made)") +
  theme_minimal()


### PART 2 ###
##############

# load data
library(tidyverse)

# Read data
ncaab_results   <- read_csv("../data/04_ncaab-results.csv.gz")
ncaab_team_info <- read_csv("../data/04_ncaab-teams.csv.gz")

# Filter for 2023 and remove neutrals
ncaab_2023 <- ncaab_results %>%
  filter(Season == 2023, WLoc != "N") %>%
  mutate(
    home_win = ifelse(WLoc == "H", 1, 0)
  ) %>%
  left_join(ncaab_team_info, by = c("WTeamID" = "TeamID")) %>%
  rename(WTeamName = TeamName) %>%
  left_join(ncaab_team_info, by = c("LTeamID" = "TeamID")) %>%
  rename(LTeamName = TeamName)

# Team lists
all_teams <- sort(unique(c(ncaab_2023$WTeamName, ncaab_2023$LTeamName)))

# Home/Away IDs
ncaab_2023 <- ncaab_2023 %>%
  mutate(
    home_id = if_else(WLoc == "H", WTeamID, LTeamID),
    away_id = if_else(WLoc == "H", LTeamID, WTeamID)
  )

# Home/Away matrix
HA_matrix <- matrix(0, nrow = nrow(ncaab_2023), ncol = length(all_teams))
colnames(HA_matrix) <- all_teams

for (i in seq_len(nrow(ncaab_2023))) {
  w   <- as.character(ncaab_2023$WTeamName[i])
  l   <- as.character(ncaab_2023$LTeamName[i])
  loc <- ncaab_2023$WLoc[i]
  
  if (loc == "H") {
    HA_matrix[i, w] <- 1
    HA_matrix[i, l] <- -1
  } else if (loc == "A") {
    HA_matrix[i, w] <- -1
    HA_matrix[i, l] <- 1
  } else {
    HA_matrix[i, w] <- 0
    HA_matrix[i, l] <- 0
  }
}

# Fit Bradley–Terry model
model_HA <- glm(ncaab_2023$home_win ~ HA_matrix, family = "binomial")

coefs_all  <- coef(model_HA)
team_coefs <- coefs_all[names(coefs_all) != "(Intercept)"]
team_names <- gsub("^HA_matrix", "", names(team_coefs))

coef_df <- data.frame(
  Team        = team_names,
  Coefficient = as.numeric(team_coefs),
  stringsAsFactors = FALSE
)

# Plot
library(ggplot2)
ggplot(coef_df, aes(x = reorder(Team, Coefficient), y = Coefficient)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    x     = "Team",
    y     = "Estimated Log-Odds Strength",
    title = "Bradley–Terry Coefficients: 2023 NCAA Men’s Basketball"
  ) +
  theme_minimal()

summary(model_HA)
#visualize the model the coefficients

#Vegas Spread




