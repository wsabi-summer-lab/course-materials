#############
### SETUP ###
#############
getwd()
# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
#install.packages("pROC")
##############
### PART 1 ###
##############

# load data
field_goals = read_csv("data/04_field-goals.csv.gz")

# inspect data
glimpse(field_goals)
unique(colnames(field_goals))

#training split
set.seed(123)
train_indices = sample(1:nrow(field_goals), size = 0.8 * nrow(field_goals))
field_goals_train = field_goals[train_indices, ]
field_goals_test = field_goals[-train_indices, ]

# fit logistic model of "fg_made" using "ydl" and "kq"
logistic_model = glm(fg_made ~ ydl + kq, 
                     data = field_goals_train, 
                     family = binomial(link = "logit"))
logistic_model
# make roc curve
library(pROC)
roc_curve = roc(field_goals_test$fg_made, 
                 predict(logistic_model, newdata = field_goals_test, type = "response"))
# plot roc curve
plot(roc_curve, 
     main = "ROC Curve for Field Goals Made Prediction",
     col = "blue", 
     lwd = 2,
     print.auc = TRUE
     )
# calculate AUC
auc_value = auc(roc_curve)
cat("AUC Value:", auc_value, "\n")
# make predictions
field_goals_test$predicted_prob = predict(logistic_model, 
                                           newdata = field_goals_test, 
                                           type = "response")


#plot ydl vs predicted probability observed and expected
ggplot(field_goals_test, aes(x = ydl, y = predicted_prob)) +
  geom_point(aes(color = factor(fg_made)), alpha = 0.1) +
  labs(title = "Predicted Probability of Field Goals Made vs YDL",
       x = "YDL (Yards Downfield)",
       y = "Predicted Probability of FG Made",
       color = "FG Made") +
  theme_minimal() +
  scale_color_manual(values = c("red", "green"))

#make a linear model of fg_made using ydl and kq
linear_model = lm(fg_made ~ ydl + kq, 
                  data = field_goals_train)
summary(linear_model)

#make predictions
field_goals_test$predicted_linear = predict(linear_model, 
                                             newdata = field_goals_test)
# plot ydl vs predicted linear probability
ggplot(field_goals_test, aes(x = ydl, y = predicted_linear)) +
  geom_point(aes(color = factor(fg_made)), alpha = 0.1) +
  labs(title = "Predicted Linear Probability of Field Goals Made vs YDL",
       x = "YDL (Yards Downfield)",
       y = "Predicted Linear Probability of FG Made",
       color = "FG Made") +
  theme_minimal() +
  scale_color_manual(values = c("red", "green"))

# KNN model using caret package
#install.packages("caret")  # if not already installed
library(caret)

# Train the model
knn_model_prob = knn3(fg_made ~ ydl + kq, data = field_goals_train, k = 100)

# Predict probabilities
field_goals_test$knn_prob_class1 = predict(knn_model_prob, field_goals_test, type = "prob")[, "1"]

# Predicted class (optional)
field_goals_test$knn_predicted = ifelse(field_goals_test$knn_prob_class1 >= 0.5, 1, 0)

# Accuracy
accuracy_knn = mean(field_goals_test$knn_predicted == as.numeric(as.character(field_goals_test$fg_made)))

# Make sure fg_made is numeric 0/1 for pROC
actuals <- as.numeric(as.character(field_goals_test$fg_made))

# ROC curve object
roc_obj <- roc(actuals, field_goals_test$knn_prob_class1)

# Plot ROC curve
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve for KNN Model")

# Optional: add AUC to plot
auc_knn <- auc(roc_obj)
legend("bottomright", legend = paste("AUC =", round(auc_knn, 3)), bty = "n")

# Convert actual fg_made labels to numeric 0/1
actuals <- as.numeric(as.character(field_goals_test$fg_made))

# Your predicted probabilities for class 1 (already computed)
pred_probs <- field_goals_test$knn_prob_class1

# Residuals = actual - predicted probability
knn_residuals <- actuals - pred_probs

# Calculate reduction in variation (similar to R-squared)

print(paste("KNN reduction metric:", round(reduction_knn, 4)))

#calculate reduction in error for both models (1-sd(residuals)/sd(y))
logistic_pred_probs <- predict(logistic_model, newdata = field_goals_test, type = "response")
logistic_residuals <- as.numeric(as.character(field_goals_test$fg_made)) - logistic_pred_probs

reduction_logistic <- 1 - sd(logistic_residuals) / sd(as.numeric(as.character(field_goals_test$fg_made)))
reduction_linear = 1 - sd(residuals(linear_model)) / sd(field_goals$fg_made)
reduction_knn <- 1 - sd(knn_residuals) / sd(actuals)

##############
### PART 2 ###
##############

library(dplyr)
library(ggplot2)
library(readr)

# Load data
ncaab_results = read_csv("data/04_ncaab-results.csv.gz")
ncaab_team_info = read_csv("data/04_ncaab-teams.csv.gz")
glimpse(ncaab_team_info)
# Filter season 2023 and create home/away scores and teams
ncaab_results_2023 <- ncaab_results %>%
  filter(Season == 2023, WLoc %in% c("H", "A")) %>%
  mutate(
    HScore = ifelse(WLoc == "H", WScore, LScore),
    AScore = ifelse(WLoc == "A", WScore, LScore),
    Diff = HScore - AScore,
    home_team = ifelse(WLoc == "H", WTeamID, LTeamID),
    away_team = ifelse(WLoc == "A", WTeamID, LTeamID),
    home_win = ifelse(WLoc == "H", 1, 0)
  )

# Plot distribution of score differences (optional)
ggplot(ncaab_results_2023, aes(x = Diff)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Score Differences (Home - Away) in 2023",
       x = "Score Difference",
       y = "Frequency") +
  theme_minimal()

# Create factor variables for home and away teams
ncaab_results_2023 <- ncaab_results_2023 %>%
  mutate(
    home_team = factor(home_team),
    away_team = factor(away_team)
  )

# Create list of all teams (levels)
teams <- levels(factor(c(ncaab_results_2023$home_team, ncaab_results_2023$away_team)))

# Build design matrix with +1 for home team, -1 for away team
X <- matrix(0, nrow = nrow(ncaab_results_2023), ncol = length(teams))
colnames(X) <- teams

for (i in seq_len(nrow(ncaab_results_2023))) {
  X[i, as.character(ncaab_results_2023$home_team[i])] <- 1
  X[i, as.character(ncaab_results_2023$away_team[i])] <- -1
}

X <- as.data.frame(X)
nrow(ncaab_results_2023)
nrow(X)
length(unique(c(ncaab_results_2023$WteamID, ncaab_results_2023$LTeamID)))
ncol(X)
# Combine design matrix with outcome variable (home_win)
logistic_data <- cbind(home_win = ncaab_results_2023$home_win, X)

# Fit logistic regression with intercept (home field advantage)
model <- glm(home_win ~ ., data = logistic_data, family = binomial())

# Summary to check home field advantage
summary(model)

# 1. Extract all coefficients
coef_all <- coef(model)

# 2. Separate intercept
home_field_advantage <- coef_all["(Intercept)"]

# 3. Extract team coefficients (ignore intercept)
team_strengths <- coef_all[names(coef_all) != "(Intercept)"]

# 4. Strip backticks from names
clean_names <- gsub("`", "", names(team_strengths))

# 5. Convert cleaned names to numeric, filter valid ones
team_ids_numeric <- suppressWarnings(as.numeric(clean_names))
valid_idx <- !is.na(team_ids_numeric)

# 6. Keep only valid team coefficients
team_strengths <- team_strengths[valid_idx]
team_ids_numeric <- team_ids_numeric[valid_idx]

# 7. Build dataframe
team_ranking_df <- data.frame(
  TeamID = team_ids_numeric,
  Strength = as.numeric(team_strengths),
  stringsAsFactors = FALSE
)


# 8. Merge with team info and arrange
team_ranking_named <- team_ranking_df %>%
  left_join(ncaab_team_info, by = "TeamID") %>%
  select(TeamName, Strength) %>%
  arrange(desc(Strength))

# 9. Check for missing TeamName
na_rows <- team_ranking_named %>% filter(is.na(TeamName))
cat("Rows with missing TeamName:", nrow(na_rows), "\n")

# 11. Print top 20
print(head(team_ranking_named, 20))
# 12. Plot team strengths
ggplot(team_ranking_named, aes(x = reorder(TeamName, Strength), y = Strength)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "NCAAB Team Strengths (2023 Season)",
       x = "Team",
       y = "Strength (Log-Odds of Home Win)") +
  theme_minimal()


# 2. Get strengths for Purdue and Connecticut
strength_purdue <- team_ranking_named %>% filter(TeamName == "Purdue") %>% pull(Strength)
strength_connecticut <- team_ranking_named %>% filter(TeamName == "Connecticut") %>% pull(Strength)
strength_purdue
strength_connecticut
# 3. Calculate logit (assuming Purdue is home)
logit_purdue_win <- strength_purdue - strength_connecticut

# 4. Convert to probability
prob_purdue_win <- 1 / (1 + exp(-logit_purdue_win))

prob_purdue_win
