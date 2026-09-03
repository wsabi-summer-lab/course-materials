#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)


# set seed
set.seed(3)

##############
### PART 1 ###
##############

# load data
nba_four_factors = read_csv("../data/02_nba-four-factors.csv.gz")

#Task 1
nba_four_factors$`Protecting Factor` <- nba_four_factors$`Protecting Factor` * -1
nba_four_factors %>%
  select(`Shooting Factor`, `Crashing Factor`, `Protecting Factor`, `Attacking Factor`) %>%
  summarise(across(everything(), list(
    mean = ~mean(., na.rm=TRUE),
    sd   = ~sd(., na.rm=TRUE),
    min  = ~min(., na.rm=TRUE),
    max  = ~max(., na.rm=TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)

nba_four_factors %>%
  select(`Shooting Factor`, `Crashing Factor`, `Protecting Factor`, `Attacking Factor`) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Marginal Distributions of Four Factors")

nba_four_factors %>%
  select(W, `Shooting Factor`, `Crashing Factor`, `Protecting Factor`, `Attacking Factor`) %>%
  pivot_longer(-W, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, y = W)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  facet_wrap(~variable, scales = "free_x") +
  labs(title = "Wins vs Four Factors", x = "Factor Value", y = "Wins")

nba_four_factors %>%
  select(`Shooting Factor`, `Crashing Factor`, `Protecting Factor`, `Attacking Factor`) %>%
  cor() %>%
  round(3)

#the variables that seem most strongly related to wins before fitting a multivariable model is most certainly shooting factor is the most importnat and protecting factor and attacking factor also seem to be important but more scattered.

#Task 2
model1 <- lm(W ~ `Shooting Factor` + `Crashing Factor` + `Protecting Factor` + `Attacking Factor`, 
             data = nba_four_factors)

summary(model1)

#Beta1_hat is 3.6749 which means for each increase in one shooting advantage unit, the team wins roughly 3.67 more games
#Beta2_hat (crashing) = 1.3403
#Beta3_hat (protecting) = 3.05
#Beta4_hat (attacking) = 0.77
#the signs all make sense except for protecting since its negative. 
#the factor that appears to be the strongest association to winning is shooting factor. 
#attacking factor now seems to be weakly correlated when considering other factors

#Task 3 
nba_four_factors <- nba_four_factors %>%
  mutate(
    z1 = (`Shooting Factor` - mean(`Shooting Factor`)) / sd(`Shooting Factor`),
    z2 = (`Crashing Factor` - mean(`Crashing Factor`)) / sd(`Crashing Factor`),
    z3 = (`Protecting Factor` - mean(`Protecting Factor`)) / sd(`Protecting Factor`),
    z4 = (`Attacking Factor` - mean(`Attacking Factor`)) / sd(`Attacking Factor`)
  )

# Fit standardized model
model2 <- lm(W ~ z1 + z2 + z3 + z4, data = nba_four_factors)

summary(model2)

#RANKING: z1, z3, z2, z4
#The standardize version it is easier to compare 
#They are the exaclty the same. since standardizing is just rescaling, the fitted values will still be the same.
#Since standardization basically just changes the units, but the plane in column space does not move so the projection of y onto it lands on the same spot. 

#Task 4
# Residual standard error
summary(model1)$sigma

# Standard errors and 95% CI for each coefficient
confint(model1, level = 0.95)

#All of the factors that are clearly distinguishable from zero. However, Attacking factor is not much away from 0. 
# Pick a team - let's use the first team in the dataset
team <- nba_four_factors[1, ]
team$TEAM  # see which team it is

# Point prediction + confidence interval + prediction interval
predict(model1, newdata = team, interval = "confidence", level = 0.95)
predict(model1, newdata = team, interval = "prediction", level = 0.95)
 #the prediction interval is wider because it accounts for more uncertainty (estimated mean and the randomness of games and a teams season)

#Task 5
# Split data 80/20
n <- nrow(nba_four_factors)
train_idx <- sample(1:n, 0.8 * n)
train <- nba_four_factors[train_idx, ]
test  <- nba_four_factors[-train_idx, ]

# Fit both models on training data
model1_train <- lm(W ~ `Shooting Factor` + `Crashing Factor` + `Protecting Factor` + `Attacking Factor`, 
                   data = train)
model2_train <- lm(W ~ z1 + z2 + z3 + z4, data = train)

# Compute test RMSE
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

rmse1 <- rmse(test$W, predict(model1_train, newdata = test))
rmse2 <- rmse(test$W, predict(model2_train, newdata = test))

rmse1
rmse2

#They predict equally well because the RMSE is idential. THe standardizationdoes not change the models predictions. 


##############
### PART 2 ###
##############

# load data
punts = read_csv("../data/02_punts.csv.gz")

#Task 1
# Plot yi against ydli
# Plot yi against ydli
ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "steelblue") +
  labs(title = "Post-Punt Yard Line vs Starting Yard Line", x = "Yard Line (ydl)", y = "Post-Punt Yard Line")
#The relationship looks none linear.

# Bin punts by starting field position
punts %>%
  mutate(ydl_bin = cut(ydl, breaks = 10)) %>%
  group_by(ydl_bin) %>%
  summarise(avg_y = mean(next_ydl)) %>%
  ggplot(aes(x = ydl_bin, y = avg_y)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Post-Punt Yard Line by Starting Position", x = "Starting Yard Line Bin", y = "Average Post-Punt Yard Line")

# Distribution of punter quality
ggplot(punts, aes(x = pq)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Punter Quality", x = "Punter Quality (pq)", y = "Count")

## Model 1: linear
punt_model1 <- lm(next_ydl ~ ydl, data = punts)

# Model 2: quadratic
punt_model2 <- lm(next_ydl ~ ydl + I(ydl^2), data = punts)

# Model 3: quadratic + punter quality
punt_model3 <- lm(next_ydl ~ ydl + I(ydl^2) + pq, data = punts)

# Visualize fitted curves
ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, aes(color = "Linear"), se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), aes(color = "Quadratic"), se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), aes(color = "Quadratic + PQ"), se = FALSE) +
  scale_color_manual(values = c("Linear" = "red", "Quadratic" = "blue", "Quadratic + PQ" = "green")) +
  labs(title = "Fitted Curves for Punt Models", x = "Starting Yard Line", y = "Post-Punt Yard Line", color = "Model")

# Train/test RMSE
set.seed(3)
n <- nrow(punts)
train_idx <- sample(1:n, 0.8 * n)
train_p <- punts[train_idx, ]
test_p  <- punts[-train_idx, ]

pm1_train <- lm(next_ydl ~ ydl, data = train_p)
pm2_train <- lm(next_ydl ~ ydl + I(ydl^2), data = train_p)
pm3_train <- lm(next_ydl ~ ydl + I(ydl^2) + pq, data = train_p)

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

rmse(test_p$next_ydl, predict(pm1_train, newdata = test_p))
rmse(test_p$next_ydl, predict(pm2_train, newdata = test_p))
rmse(test_p$next_ydl, predict(pm3_train, newdata = test_p))

#TRADE OFF: The model becomes stronger at predicting but it less applicable and easy to withdrawal information from, due to the nonlinearity.
#Punter quality does (barely) improve out of sample prediction (RMSE 10.65 to 10.63)

#Task 3
# Use quadratic + pq model as selected model
punt_preds <- predict(punt_model3, newdata = punts, interval = "confidence", level = 0.95)
punt_preds_pi <- predict(punt_model3, newdata = punts, interval = "prediction", level = 0.95)

# Plot with confidence and prediction bands
ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(alpha = 0.2) +
  geom_line(aes(y = punt_preds[,"fit"]), color = "steelblue", linewidth = 1) +
  geom_ribbon(aes(ymin = punt_preds[,"lwr"], ymax = punt_preds[,"upr"]), 
              fill = "steelblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = punt_preds_pi[,"lwr"], ymax = punt_preds_pi[,"upr"]), 
              fill = "red", alpha = 0.15) +
  labs(title = "Punt Model: Fitted Mean with Confidence and Prediction Bands",
       x = "Starting Yard Line", y = "Post-Punt Yard Line",
       caption = "Blue = 95% CI for mean, Red = 95% Prediction Interval")
#The prediction band is wider for the confidence band because the confidence band just captures the mean response whereas the prediciton band adds the punt to punt randomness of an individuals too. 
#The model is most uncertain where there is less data so where there are extremes (less data)

# Compute PYOE for each punt
punts <- punts %>%
  mutate(
    fitted = predict(punt_model3),
    PYOE = fitted - next_ydl
  )

# Summarize by punter
punter_summary <- punts %>%
  group_by(punter) %>%
  summarise(
    avg_PYOE = mean(PYOE),
    n_punts = n(),
    se_PYOE = sd(PYOE) / sqrt(n())
  ) %>%
  arrange(desc(avg_PYOE))

punter_summary

# Visualize rankings with uncertainty intervals
ggplot(punter_summary, aes(x = reorder(punter, avg_PYOE), y = avg_PYOE)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_PYOE - 1.96 * se_PYOE, 
                    ymax = avg_PYOE + 1.96 * se_PYOE), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Punter Rankings by PYOE with Uncertainty",
       x = "Punter", y = "Average PYOE")
#There are some putters that are clearly above average, but some of the clearly above average punters only had 1 or 2 punts. HOwever there are some like Redfern who had 10 and are clearly above average. 
#Roethlisberger and Novak only have 1 punt and an NA standard error and are unstable because of small sample size. 

#Final Reflection
#1 How did adding columns to the design matrix change what the model could fit?
#Adding columns to the design allows for the model to fit in more directions. 
#2 When did extra flexibility help?
#Extra flexibility helps when it is nonlinear when the true relationship is nonlinear.
#3 When might extra flexibility hurt?
#Extra flexibility can lead to over fitting, and is less interpretable unit wise compared to simple linear regression.
#4 what does sigma hat tell you about the typical size of model errors?
#it tells you the typical size of the model's error in the original units (wins for NBA model and yards for the punt model)
#5 Why is a prediction interval wider than a confidence interval for the expected response?
#prediction interval is wider than a confidence interval for the expected response because confidence interval considers the average whereas the prediction interval also considers the randomness of the outcome of an individual season for a specific team.
#6 What is one coefficient, prediction, or punter ranking from this lab that you would be cautious about interpreting too strongly?
#The punter rankings with small sample sizes like Roethlisberger. 

  