#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(gridExtra)

# set seed
set.seed(3)

##############
### PART 1 ###
##############

# load data
nba_four_factors = read_csv("../data/02_nba-four-factors.csv.gz")

#switch protecting factor
nba_four_factors = nba_four_factors %>%
  mutate(`Protecting Factor` = (-1)*`Protecting Factor`)


# Task 1: Get to know the data

summary(nba_four_factors$`Shooting Factor`)
sd(nba_four_factors$`Shooting Factor`)
# Scoring: mean = 0.0004167 , sd = 2.757756 , min = -7.3 , max = 8.4

summary(nba_four_factors$`Crashing Factor`)
sd(nba_four_factors$`Crashing Factor`)
# Crashing: mean = 99.97, sd = 2.716114, min = 91.1 , max = 107.3

summary(nba_four_factors$`Protecting Factor`)
sd(nba_four_factors$`Protecting Factor`)
# Protecting: mean = -0.001458, sd = 1.451792, min = -4.1 , max = 3.8

summary(nba_four_factors$`Attacking Factor`)
sd(nba_four_factors$`Attacking Factor`)
# Attacking: mean = -0.008649, sd = 2.966996, min = -8.557608 , max = 8.320346


### MARGINAL DISTRIBUTION PLOTS

# Shooting Factor
shooting_marg_dist = ggplot(nba_four_factors, aes(x = `Shooting Factor`)) +
  geom_histogram(binwidth = 0.25, fill = "dodgerblue") +
  labs(title = "Distribution of Shooting Factor",
       x = "Shooting Factor",
       y = "Count")

# Crashing Factor
crashing_marg_dist = ggplot(nba_four_factors, aes(x = `Crashing Factor`)) +
  geom_histogram(binwidth = 0.25, fill = "darkorange") +
  labs(title = "Distribution of Crashing Factor",
       x = "Crashing Factor",
       y = "Count")

# Protecting Factor
protecting_marg_dist = ggplot(nba_four_factors, aes(x = `Protecting Factor`)) +
  geom_histogram(binwidth = 0.25, fill = "seagreen") +
  labs(title = "Distribution of Protecting Factor",
       x = "Protecting Factor",
       y = "Count")

# Attacking Factor
attacking_marg_dist = ggplot(nba_four_factors, aes(x = `Attacking Factor`)) +
  geom_histogram(binwidth = 0.25, fill = "purple") +
  labs(title = "Distribution of Attacking Factor",
       x = "Attacking Factor",
       y = "Count")

grid.arrange(
  shooting_marg_dist,
  crashing_marg_dist,
  protecting_marg_dist,
  attacking_marg_dist,
  ncol = 2
)


### SCATTERPLOTS OF WINS VERSUS EACH FACTOR

#Shooting factor
shooting_plot = ggplot(nba_four_factors, aes(x = `Shooting Factor`, y = W)) +
  geom_point(color = "dodgerblue") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", linewidth = 1) +
  labs(title = "Wins vs Shooting Factor",
       x = "Shooting Factor",
       y = "Wins")

# Crashing Factor
crashing_plot = ggplot(nba_four_factors, aes(x = `Crashing Factor`, y = W)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", linewidth = 1) +
  labs(title = "Wins vs Crashing Factor",
       x = "Crashing Factor",
       y = "Wins")

# Protecting Factor
protecting_plot = ggplot(nba_four_factors, aes(x = `Protecting Factor`, y = W)) +
  geom_point(color = "seagreen") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", linewidth = 1) +
  labs(title = "Wins vs Protecting Factor",
       x = "Protecting Factor",
       y = "Wins")

# Attacking Factor
attacking_plot = ggplot(nba_four_factors, aes(x = `Attacking Factor`, y = W)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", linewidth = 1) +
  labs(title = "Wins vs Attacking Factor",
       x = "Attacking Factor",
       y = "Wins")

grid.arrange(
  shooting_plot,
  crashing_plot,
  protecting_plot,
  attacking_plot,
  ncol = 2
)

### FIND CORRELATION BETWEEN VARIABLES
shooting_model = lm(data = nba_four_factors, W ~ `Shooting Factor`)
summary(shooting_model)
cor(nba_four_factors$W, nba_four_factors$`Shooting Factor`) # = 0.8543934

crashing_model = lm(data = nba_four_factors, W ~ `Crashing Factor`)
summary(crashing_model)
cor(nba_four_factors$W, nba_four_factors$`Crashing Factor`) # = 0.1409524

protecting_model = lm(data = nba_four_factors, W ~ `Protecting Factor`)
summary(protecting_model)
cor(nba_four_factors$W, nba_four_factors$`Protecting Factor`) # = 0.2929491

attacking_model = lm(data = nba_four_factors, W ~ `Attacking Factor`)
summary(attacking_model)
cor(nba_four_factors$W, nba_four_factors$`Attacking Factor`) # = 0.3631507


#Task 2: Multivariable regression

multi_model = lm(data = nba_four_factors, W ~ `Shooting Factor` + `Crashing Factor` + `Protecting Factor` + `Attacking Factor`)
summary(multi_model)

#Interpretation
#Beta1: each % increase in eFG difference results in 3.67 more wins
#Beta2: each % increase in REB% results in 1.34 more wins
#Beta3: each % increase in turnover differential results in 3.06 more wins
#Beta4: each % increase in FT rate differential results in 0.77 more wins

#The signs all make sense: they should be positive because getting better at each factor increases wins

#Shooting seems most associated with winning

#Attacking seems least associated with winning



#Task 3: Standardize the predictors

nba_four_factors = nba_four_factors %>%
  mutate(
    z_shooting = ((`Shooting Factor` - mean(`Shooting Factor`))/sd(`Shooting Factor`)),
    z_crashing = ((`Crashing Factor` - mean(`Crashing Factor`))/sd(`Crashing Factor`)),
    z_protecting = ((`Protecting Factor` - mean(`Protecting Factor`))/sd(`Protecting Factor`)),
    z_attacking = ((`Attacking Factor` - mean(`Attacking Factor`))/sd(`Attacking Factor`))
  )

standardized_model = lm(data = nba_four_factors,
                        W ~ z_shooting + z_crashing + z_protecting + z_attacking)
summary(standardized_model)

#Most to least ranking: shooting, protecting, crashing, attacking (shooting significantly higher)

#This model is better for comparisons because it places all four factors within the same context

#The fitted values are not the same, although the rankings are. This time, the difference between are made much clearer

#Since the X matrix is different from the Z matrix after being standardized, the column space is different, which results in a different optimal vector



#Task 4: Quantify uncertainty

#Find residual standard error
#p = 5

nba_four_factors = nba_four_factors %>%
  mutate(
    predicted_W = predict(standardized_model, newdata = nba_four_factors),
    residual = W - predicted_W
  )

RSS = sum(residuals(standardized_model)^2)
n   = nrow(nba_four_factors)
p = 5

sigma_hat = sqrt(RSS / (n - p))
sigma_hat

#Find std error and confidence interval
summary(standardized_model)
confint(standardized_model)

#All four factors are clearly distinguishable from zero


#Choose one team: Los Angeles Lakers 2020

lal_2020 = nba_four_factors %>%
  filter(Team_Season == "Los Angeles Lakers2020")

lal_2020_prediction = predict(standardized_model, newdata = lal_2020)
lal_2020_prediction # = 54.88817

#Confidence interval given 4 factor profile
predict(standardized_model, newdata = lal_2020, interval = "confidence")
# [54.23298 , 55.54336]

#Confidence interval for specific team
predict(standardized_model, newdata = lal_2020, interval = "prediction")
# [47.04513 , 62.7312]


#Task 5: Prediction

#Randomly split into training and test data

n = nrow(nba_four_factors)
train_index = sample(1:n, size = n * 0.5)

nba_train = nba_four_factors[train_index, ]
nba_test  = nba_four_factors[-train_index, ]

pred_regular_model = lm(data = nba_train, W ~ `Shooting Factor` + `Crashing Factor` + `Protecting Factor` + `Attacking Factor`)
summary(pred_regular_model)

pred_standardized_model = lm(data = nba_train, W ~ z_shooting + z_crashing + z_protecting + z_attacking)
summary(pred_standardized_model)


#Compute test-set RMSE for both models

nba_test = nba_test %>%
  mutate(residual_pred_reg = W - predict(pred_regular_model, newdata = nba_test),
         residual_pred_stand = W - predict(pred_standardized_model, newdata = nba_test))

n = nrow(nba_test)
p = 5

rmse_reg_pred = sqrt(sum(nba_test$residual_pred_reg^2)/(n-p)) # = 3.95
rmse_stand_pred = sqrt(sum(nba_test$residual_pred_stand^2)/(n-p)) # = 3.95

#Both models predict equally well because standardizing doesn't change the rmse


##############
### PART 2 ###
##############

# load data
punts = read_csv("../data/02_punts.csv.gz")

#Task 1: explore the data

plot1 = ggplot(data = punts, aes(x = ydl, y = next_ydl)) +
  geom_point(color = "dodgerblue") +
  labs(title = "Punt end location v Punt start location",
       x = "Distance from endzone start",
       y = "Distance from endzone end (opponent)")
plot1

punts_binned <- punts %>%
  mutate(start_bin = cut(ydl,
                         breaks = seq(0, 100, by = 5),
                         include.lowest = TRUE,
                         right = FALSE)) %>%
  group_by(start_bin) %>%
  summarise(mean_next_ydl = mean(next_ydl, na.rm = TRUE),
            .groups = "drop")

plot2 = ggplot(punts_binned, aes(x = start_bin, y = mean_next_ydl, group = 1)) +
  geom_point(size = 3, color = "dodgerblue") +
  geom_line(color = "dodgerblue") +
  labs(
    title = "Average Post Punt Field Position by Starting Location",
    x = "Punt Starting Location (5 yard bins)",
    y = "Average Post‑Punt Location"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2
# the relationship is not linear, and moreso quadratic, and it bends at around the 55 yard starting line


plot3 = ggplot(data = punts, aes(x = pq)) +
  geom_histogram(fill = "dodgerblue", binwidth = 0.05)
plot3


#Task 2: Fit competing models

#basic linear model
lin_model = lm(data = punts, next_ydl ~ ydl)
summary(lin_model)

#quadratic model
quad_model = lm(data = punts, next_ydl ~ ydl + I(ydl^2))
summary(quad_model)

#quadratic model with pq
quadpq_model = lm(data = punts, next_ydl ~ ydl + I(ydl^2) + pq)
summary(quad_model)

#Visualize

# Predictions from each model
punts <- punts %>%
  mutate(
    lin_pred     = predict(lin_model, .),
    quad_pred    = predict(quad_model, .),
    quadpq_pred  = predict(quadpq_model, .)
  )

pred_plot = ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(color = "dodgerblue") +
  
  geom_line(aes(y = lin_pred), color = "red", size = 1.1) +
  geom_line(aes(y = quad_pred), color = "darkgreen", size = 1.1) +
  geom_line(aes(y = quadpq_pred), color = "purple", size = 1.1) +
  
  labs(
    title = "Punt End Location vs Start Location with Model Fits",
    subtitle = "Red = Linear, Green = Quadratic, Purple = Quadratic + pq",
    x = "Distance from endzone start",
    y = "Distance from opponent endzone"
  ) +
  theme_minimal()
pred_plot


#Find train/test RMSE

n = nrow(punts)
train_index = sample(1:n, size = n * 0.5)

punts_train = punts[train_index, ]
punts_test  = punts[-train_index, ]


lin_model_train = lm(data = punts_train, next_ydl ~ ydl)
summary(lin_model_train)

quad_model_train = lm(data = punts_train, next_ydl ~ ydl + I(ydl^2))
summary(quad_model_train)

quadpq_model_train = lm(data = punts_train, next_ydl ~ ydl + I(ydl^2) + pq)
summary(quadpq_model_train)


punts_test = punts_test %>%
  mutate(
    lin_pred     = predict(lin_model_train, .),
    quad_pred    = predict(quad_model_train, .),
    quadpq_pred  = predict(quadpq_model_train, .),
    lin_pred_reg = next_ydl - lin_pred,
    quad_pred_reg = next_ydl - quad_pred,
    quadpq_pred_reg = next_ydl - quadpq_pred
  )

n = nrow(punts_test)

rmse_lin = sqrt(sum(punts_test$lin_pred_reg^2)/(n-1)) # = 11.00945
rmse_quad = sqrt(sum(punts_test$quad_pred_reg^2)/(n-1)) # = 10.75742
rmse_quadpq = sqrt(sum(punts_test$quadpq_pred_reg^2)/(n-2)) # = 10.73742

#The quadratic model fits better, but is harder to create a convenient interpretation

#The pq coefficient in the original model is ~1.26, meaning that an increase of +1 in pq results in an additional 1.26 yards on the punt


#Task 3: Visualize uncertainty (preferred model: quadratic)

# 95% confidence interval
ci <- predict(quad_model, punts, interval = "confidence")

# 95% prediction interval
pi <- predict(quad_model, punts, interval = "prediction")

# Add to dataset
punts$ci_lwr <- ci[, "lwr"]
punts$ci_upr <- ci[, "upr"]

punts$pi_lwr <- pi[, "lwr"]
punts$pi_upr <- pi[, "upr"]

plot4 <- ggplot(punts, aes(x = ydl)) +
  
  # Prediction interval (widest band)
  geom_ribbon(aes(ymin = pi_lwr, ymax = pi_upr),
              fill = "orange", alpha = 0.15) +
  
  # Confidence interval (narrower band)
  geom_ribbon(aes(ymin = ci_lwr, ymax = ci_upr),
              fill = "navy", alpha = 0.25) +
  
  # Fitted quadratic line
  geom_line(aes(y = quad_pred), 
            color = "dodgerblue", linewidth = 1.2) +
  
  labs(
    title = "Quadratic Model Fit with 95% CI and 95% PI",
    x = "Punt Start Location (ydl)",
    y = "Predicted Punt End Location"
  ) +
  theme_minimal()

plot4

#The prediction band is much wider because more factors can go into creating uncertainty for a certain prediction.

#The model is most uncertain when the punt is closer to the endzone, since that is where fewer punts happen and they have more variance.




#Task 4: interpret punt yards over expected

punts = punts %>%
  mutate(
    pyoe = next_ydl - quad_pred
  )

#compute per punter
punters = punts %>%
  group_by(punter) %>%
  summarize(avg_pyoe = mean(pyoe), punt_n = n(),se_pyoe = sd(pyoe)/sqrt(n()-1)) %>%
  arrange(-avg_pyoe)

real_punter = punters %>%
  filter(punt_n >= 5)

real_punter <- real_punter %>%
  mutate(punter = reorder(punter, avg_pyoe))

ggplot(real_punter, aes(x = avg_pyoe, y = punter)) +
  
  # 95% confidence interval: avg ± 1.96 * SE
  geom_errorbarh(aes(xmin = avg_pyoe - 1.96 * se_pyoe,
                     xmax = avg_pyoe + 1.96 * se_pyoe),
                 height = 0.25,
                 color = "gray50") +
  
  # point for the average
  geom_point(size = 3, color = "dodgerblue") +
  
  labs(
    title = "Punter Performance (PYOE) with Uncertainty",
    subtitle = "Points = average PYOE; Bars = 95% confidence intervals",
    x = "Average PYOE",
    y = "Punter"
  ) +
  theme_minimal(base_size = 13)
#Everyone from Kern to higher are clearly above average
#Ammendola and some of the lower rankings are unstable due to high standard error



### FINAL REFLECTION

#1. Adding columns to the design matrix means the model can fit more variables, such as the quadratic and pq model, which can lower error but threatens to overfit
#2. Extra flexibility helps when the existing model doesn't accurately fit the data
#3. Extra flexibility hurts when there is a threat of overfitting training data to testing data.
#4. Sigma hat tells us that the error can usually be very wide, such as 3-4 wins in an NBA season
#5. A prediction interval is wider than a confidence interval because a prediction interval is for a signle observation, which has way more potential for variance compared to an expected value.
#6. I would be carefull with a lot of the punter rankings at the bottom, since the confidence intervals show that they can very likely be differently ranked, if not even above average.
