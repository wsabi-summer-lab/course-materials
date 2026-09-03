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
print(ba_data)

# Task 1:
# - Fit the day-1 style linear regression model BA_2021 ~ BA_2020
# - Fit a binomial GLM of the form
#     cbind(H_2021, AB_2021 - H_2021) ~ BA_2020
# - For the GLM, remember that each player has AB_2021 Bernoulli trials, not just one

model_1 = lm(BA_2021 ~ BA_2020, data = ba_data)
summary(model_1)

model_2 = glm(cbind(H_2021, AB_2021 - H_2021) ~ BA_2020,
              data = ba_data,
              family = binomial(link = "logit"))
summary(model_2)


# Task 2:
# - Compare the fitted mean curves from the linear model and the binomial GLM
# - Plot BA_2021 against BA_2020
# - Add both fitted curves to the same figure
# - Make it visually clear which players had more at-bats in 2021

plot_1 = ggplot(data = ba_data, aes(x = BA_2020, y = BA_2021)) + geom_point(aes(size = AB_2021)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_function(fun = function(x) {
    predict(model_2, newdata = data.frame(BA_2020 = x), type = "response")
  }, color = "red")
plot_1 

#Since players with a better batting average tend to get more hits,
#the binomial model is a better fit because it takes a weighted average



# Task 3:
# - Interpret the GLM coefficient on the log-odds scale
# - Translate the coefficient into the effect of a 0.010 increase in BA_2020
# - Explain why this GLM is a more natural model for hits/outs than ordinary linear regression

summary(model_2)
confint(model_2)

#BA_2020 has an estimate of 1.48 with SE 0.145 and CI [1.198, 1.765]
#A 0.010 increase in BA_2020 means a 0.148 increase in log-odds of getting a hit
#in 2021. This roughly translates to a 1.5% increase in the odds of getting a hit. 


# Task 4:
# - Pick one hypothetical player with BA_2020 = 0.260
# - Using your fitted GLM, estimate that player's 2021 hit probability p
# - Then compare two possible workloads:
#     (a) AB_2021 = 60
#     (b) AB_2021 = 600
# - For each workload, report:
#     * expected hits = AB_2021 * p
#     * an approximate 95% interval for batting average using p +/- 1.96 * sqrt(p(1-p)/AB_2021)
# - Briefly explain why the interval is much wider for the low-AB player


p1 = predict(model_2, newdata = data.frame(BA_2020 = 0.260), type = "response")
p1 #This predicts a hit probability of 0.254

ab = c(60, 600)

ex = p1 * ab

lower = p1 - 1.96 * sqrt(p1 * (1 - p1) / ab)
upper = p1 + 1.96 * sqrt(p1 * (1 - p1) / ab)

data.frame(
  AB      = ab,
  expected_hits = ex,
  lower   = lower,
  upper   = upper
)

#The lower at bat player has a wider interval because with less samples 
#there is more variation since a singular at bat will influence the average more










#######################################
### PART 2: FIELD-GOAL SUCCESS GAM ###
#######################################

fg_data = read_csv("../data/04_field-goals.csv.gz", show_col_types = FALSE)
print(fg_data)

# Task 1:
# - Fit at least 3 competing probability models for fg_made
# - Include:
#     * one logistic GLM with a simple functional form in ydl
#     * one richer logistic GLM (for example quadratic in ydl, possibly with kq)
#     * one logistic GAM using mgcv::gam(...)
# - A good starting point for the GAM is:
#     gam(fg_made ~ s(ydl, k = 12) + kq, family = "binomial", method = "REML")

fgm_1 = glm(fg_made ~ ydl + kq,
            data = fg_data, 
            family = binomial(link = "logit"))
summary(fgm_1)

fgm_2 = glm(fg_made ~ ydl + I(ydl^2) + kq,
            data = fg_data, 
            family = binomial(link = "logit"))
summary(fgm_2)


fgm_3 = gam(fg_made ~ s(ydl) + kq,
                data = fg_data,
                family = binomial(link = "logit"))

summary(fgm_3)




# Task 2:
# - Compare the models using out-of-sample predictive performance
# - Use test-set log loss or cross-validated log loss as the main metric
# - State clearly which model you prefer and why


# Log loss helper function
log_loss = function(actual, predicted) {
  predicted = pmax(pmin(predicted, 1 - 1e-15), 1e-15) # clip to avoid log(0)
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

# Train/test split (80/20)
set.seed(42)
n = nrow(fg_data)
train_idx = sample(1:n, size = 0.8 * n)
train_data = fg_data[train_idx, ]
test_data  = fg_data[-train_idx, ]

# Refit models on training data
fgm_1_train = glm(fg_made ~ ydl + kq,
                  data = train_data,
                  family = binomial(link = "logit"))

fgm_2_train = glm(fg_made ~ ydl + I(ydl^2) + kq,
                  data = train_data,
                  family = binomial(link = "logit"))

fgm_3_train = gam(fg_made ~ s(ydl, k = 20) + kq,
                  data = train_data,
                  family = binomial(link = "logit"))

# Predict on test data
pred_1 = predict(fgm_1_train, newdata = test_data, type = "response")
pred_2 = predict(fgm_2_train, newdata = test_data, type = "response")
pred_3 = predict(fgm_3_train, newdata = test_data, type = "response")

# Compute log loss
ll_1 = log_loss(test_data$fg_made, pred_1)
ll_2 = log_loss(test_data$fg_made, pred_2)
ll_3 = log_loss(test_data$fg_made, pred_3)

data.frame(
  model    = c("GLM linear", "GLM quadratic", "GAM"),
  log_loss = c(ll_1, ll_2, ll_3)
)

#The GAM has the lowest log loss value, but it is relatively close. This suggests
#that based on the data we have right now, a GAM is good but not particularily
#necessary. 



# Task 3:
# - For the selected GAM, report:
#     * the estimated parametric coefficient(s)
#     * the effective degrees of freedom (edf) of the smooth term
# - Explain what it means if the edf is noticeably larger than 1

s = summary(fgm_3)
kq_est = s$p.coef["kq"]
kq_se  = s$se["kq"]

# Compute 95% CI manually
kq_lower = kq_est - 1.96 * kq_se
kq_upper = kq_est + 1.96 * kq_se

data.frame(
  estimate = kq_est,
  se       = kq_se,
  lower    = kq_lower,
  upper    = kq_upper
)

s

#The estimated coefficient is 0.270 with SE 0.0534 and CI [0.165, 0.374]. 
#The effective degrees of freedom are 4.837. An edf noticeably larger than 1
#means that there is some level of complexity and we don't just have a line





# Task 4:
# - Plot predicted make probability against ydl for your preferred GLM and your preferred GAM
# - Also compute binned observed make rates and overlay them on the plot
# - Comment on where the GAM improves on the simpler GLM


# Task 4:

# Create prediction grid over range of ydl, using median kq
ydl_grid = data.frame(
  ydl = seq(min(fg_data$ydl), max(fg_data$ydl), length.out = 200),
  kq  = median(fg_data$kq)
)

# Predicted probabilities from fgm_2 and fgm_3
ydl_grid$pred_glm = predict(fgm_2, newdata = ydl_grid, type = "response")
ydl_grid$pred_gam = predict(fgm_3, newdata = ydl_grid, type = "response")

# Binned observed make rates
binned = fg_data %>%
  mutate(ydl_bin = cut(ydl, breaks = seq(min(ydl), max(ydl), length.out = 15))) %>%
  group_by(ydl_bin) %>%
  summarise(
    obs_rate = mean(fg_made),
    mid_ydl  = mean(ydl),
    n        = n()
  )

# Plot
ggplot() +
  geom_line(data = ydl_grid, aes(x = ydl, y = pred_glm, color = "GLM (quadratic)"), linewidth = 1) +
  geom_line(data = ydl_grid, aes(x = ydl, y = pred_gam, color = "GAM"),             linewidth = 1) +
  geom_point(data = binned,  aes(x = mid_ydl, y = obs_rate, size = n), color = "black") +
  scale_color_manual(values = c("GLM (quadratic)" = "blue", "GAM" = "red")) +
  labs(
    x     = "Yards from line of scrimmage (ydl)",
    y     = "Predicted make probability",
    color = "Model",
    size  = "Attempts in bin"
  )

#GAM improves on the right tail while GLM performs similarly on the left





# Task 5:
# - For a kicker with league-median kq, estimate make probability at:
#     * 20 yards from the opponent's end zone
#     * 35 yards from the opponent's end zone
#     * 50 yards from the opponent's end zone
# - For at least one of these yard lines, compute an approximate 95% confidence interval
#   using predict(..., type = "link", se.fit = TRUE) and then transform back with plogis()

med_kq = median(fg_data$kq)


new_data = data.frame(
  ydl = c(20, 35, 50),
  kq  = med_kq
)


new_data$pred_prob = predict(fgm_3, newdata = new_data, type = "response")


pred_link = predict(fgm_3, newdata = new_data, type = "link", se.fit = TRUE)

new_data$lower = plogis(pred_link$fit - 1.96 * pred_link$se.fit)
new_data$upper = plogis(pred_link$fit + 1.96 * pred_link$se.fit)

new_data

#20yd: p = 0.867 with CI [0.854, 0.879]
#35yd: p = 0.633 with CI [0.608, 0.657]
#50yd: p = 0.138 with CI [0.0612, 0.281]


# Task 6:
# - Briefly explain the difference between
#     * choosing polynomial terms by hand in a GLM, and
#     * letting a GAM learn a smooth curve with a wiggliness penalty
# - State one reason a GAM can help and one reason it can hurt


#When you choose by hand, you are effectively making a guess by your observations.
#A GAM can help by allowing for more flexibility and making it more likely 
#that you have the correct number of "turns" in the data. It can hurt by 
#increasing the chance that the model is overfit
