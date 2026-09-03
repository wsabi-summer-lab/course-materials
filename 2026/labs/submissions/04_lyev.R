########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "mgcv", "readr"))

library(dplyr)
library(ggplot2)
library(mgcv)
library(readr)

###############################
### PART 1: BATTING AVERAGE ###
###############################

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
ba_data = read_csv("../data/04_ba-2020-2021.csv.gz", show_col_types = FALSE)

#Task 1: Fit the 2 competing models

#Day 1 linear regression model
day1_model = lm(data = ba_data, BA_2021 ~ BA_2020)
summary(day1_model)
#BA_2021 = 0.16213 + 0.32473 * BA_2020 + error
#increase in 0.1 of 2020 BA increases 2021 BA by 0.032


#Binomial GLM
bin_model = glm(data = ba_data, cbind(H_2021, AB_2021 - H_2021) ~ BA_2020, family = binomial(link = "logit"))
summary(bin_model)
# logit(pi) = -1.463 + 1.4814 * BA_2020


#Task 2: Compare fitted mean curves

newdat <- data.frame(
  BA_2020 = seq(min(ba_data$BA_2020), max(ba_data$BA_2020), length.out = nrow(ba_data))
)

newdat$pred <- predict(bin_model, newdata = newdat, type = "response")

plot1 = ggplot(data = ba_data) +
  geom_point(aes(x = BA_2020, y = BA_2021, color = AB_2021)) +
  geom_line(aes(x = BA_2020, y = predict(day1_model, new_data = ba_data$BA_2020)), color = "salmon", linewidth = 1)+
  geom_line(data = newdat, aes(x = BA_2020, y = pred), color = "darkgreen", linewidth = 1)+
  scale_color_viridis_c(option = "plasma", name = "AB (2021)", direction = -1) +
  labs(title = "Batting averages compared")
plot1

#The linear model fits the scatterplot more, whereas the binomial model fits relative to the amount of at bats, which is more natural for the context of the problem


#Task 3: Interpret the Binomial GLM
summary(bin_model)
coef(bin_model) # BA_2020: 1.481364 , SE = 0.1445
# BA_2021 odds increase by e^(BA_2021)
# increase in 0.01 of 2020 BA multiplies probability of hit (BA) by e^0.0148 or 1.5% increase

confint(bin_model) #BA_2020:  [ 1.198216  ,  1.764642 ]



#Task 4: Show why the denominator matters

#Make a hyopthetical player with BA_2020 = 0.260

p = predict(bin_model, newdata = data.frame(BA_2020 = 0.260), type = "response")
#BA_2021 prediction = 0.2539101 ~ 0.254

# For 60 ABs: 15.23 ~ 15 hits
# For 600 ABs: 152.35 ~ 152 hits

# Confidence intervals:
# For 60 ABs: [ 0.1437775 , 0.3640427 ]
# For 600 ABs: [ 0.2190831 , 0.2887371 ]
#A low at bat player has a much wider interval because there's much wider variance in what can happen in 60 ABs due to randomness in the game






#######################################
### PART 2: FIELD-GOAL SUCCESS GAM ###
#######################################

fg_data = read_csv("../data/04_field-goals.csv.gz", show_col_types = FALSE)

#Task 1: Fit competing probability models

#Simple logistic
model1 = glm(data = fg_data, fg_made ~ ydl, family = binomial(link = "logit"))
summary(model1)

#Logistic with kq
model2 = glm(data = fg_data, fg_made ~ ydl + kq, family = binomial(link = "logit"))
summary(model2)

#Logistic GAM model
model3 = gam(data = fg_data, fg_made ~ s(ydl) + kq, family = binomial(link = "logit"))
summary(model3)


#Task 2: Compare models out of sample

n = nrow(fg_data)
train_index = sample(1:n, size = n * 0.8)

fg_train = fg_data[train_index, ]
fg_test  = fg_data[-train_index, ]

#Remake models for training data
model1_train = glm(data = fg_train, fg_made ~ ydl, family = binomial(link = "logit"))
model2_train = glm(data = fg_train, fg_made ~ ydl + kq, family = binomial(link = "logit"))
model3_train = gam(data = fg_train, fg_made ~ s(ydl) + kq, family = binomial(link = "logit"))

#Predict test data
fg_test = fg_test %>%
  mutate(
    m1_pred = predict(model1_train, type = "response", .),
    m2_pred = predict(model2_train, type = "response", .),
    m3_pred = predict(model3_train, type = "response", .),
    m1_logloss = fg_made*log(m1_pred) + (1-fg_made)*log(1-m1_pred),
    m2_logloss = fg_made*log(m2_pred) + (1-fg_made)*log(1-m2_pred),
    m3_logloss = fg_made*log(m3_pred) + (1-fg_made)*log(1-m3_pred)
  )

#Find average log loss
m1_avg_logloss = (-1)*mean(fg_test$m1_logloss) # = 0.370978
m2_avg_logloss = (-1)*mean(fg_test$m2_logloss) # = 0.3690923
m3_avg_logloss = (-1)*mean(fg_test$m3_logloss) # = 0.3680049
n = nrow(fg_test)
m1_se_logloss = sd(fg_test$m1_logloss) / sqrt(n-1) # 0.0119339
m2_se_logloss = sd(fg_test$m2_logloss) / sqrt(n-1) # 0.01188021
m3_se_logloss = sd(fg_test$m3_logloss) / sqrt(n-1) # 0.01181513

#The width of the standard errors make it difficult to determine which model is the best
#However, I prefer model 3 the best since it has the lowest estimated log_loss


#Task 3: Interpret the GAM fit

#kq coefficient: estimate = 0.26958    SE = 0.05342   
#ydl EDF = 4.837

#The EDF being noticeably above 1 means that it is more than a linear relation, and a relationship with a lot of curves


#Task 4: Visualize fitted curves

fg_data = fg_data %>%
  mutate(
    m2_pred = predict(model2, type = "response", .),
    m3_pred = predict(model3, type = "response", .)
  )

fg_binned <- fg_data %>%
  group_by(ydl) %>%         
  summarize(prob_make = mean(fg_made), m2_pred = m2_pred, m3_pred = m3_pred, .groups = "drop")
  

fg_plot1 = ggplot(data = fg_binned) +
  geom_point(aes(x = ydl, y = prob_make)) +
  geom_smooth(aes(x = ydl, y = m2_pred), color = "dodgerblue") +
  geom_smooth(aes(x = ydl, y = m3_pred), color = "salmon")
fg_plot1

#The two fits are similar for short field goals, but the game improves in the longer distances


#Task 5: Make concrete predictions
median_kq = median(fg_data$kq) # = 0.1184969

predict(model3, newdata = data.frame(ydl = 20, kq = median_kq), type = "response")
# p = 0.8668078

predict(model3, newdata = data.frame(ydl = 30, kq = median_kq), type = "response")
# p = 0.7189209 

predict(model3, newdata = data.frame(ydl = 50, kq = median_kq), type = "response")
# p = 0.1378078

p_50 = predict(model3, newdata = data.frame(ydl = 50, kq = median_kq), type = "link", se.fit = TRUE)


lower_link <- p_50$fit - 1.96 * p_50$se.fit
upper_link <- p_50$fit + 1.96 * p_50$se.fit

lower_prob <- plogis(lower_link)
upper_prob <- plogis(upper_link)

#Conf int : [0.06123282 , 0.2814356]


#Task 6: Reflection

#Choosing polynomial terms by hand in a GLM takes a lot more trial and error, as well as intuition for what a good model would be,
#while letting a GAM learn a smooth curve leaves that effort to the computer

#A reason a GAM can help is by helping save time and creating a more accurate model
#A reason a GAM can hurt is by threatening to overfit the data and take it out of its context