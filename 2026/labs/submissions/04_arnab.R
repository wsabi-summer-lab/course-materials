########################
### INSTALL PACKAGES ###
########################

install.packages(c("mgcv", "readr"))

library(dplyr)
library(ggplot2)
library(mgcv)
library(readr)

###############################
### PART 1: BATTING AVERAGE ###
###############################

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
ba_data = read_csv("../data/04_ba-2020-2021.csv.gz", show_col_types = FALSE)

# Task 1:
# - Fit the day-1 style linear regression model BA_2021 ~ BA_2020
reg_lm = lm(BA_2021 ~ BA_2020, data = ba_data)
# - Fit a binomial GLM of the form
#     cbind(H_2021, AB_2021 - H_2021) ~ BA_2020
# - For the GLM, remember that each player has AB_2021 Bernoulli trials, not just one
binomial_glm = glm(cbind(H_2021, AB_2021 - H_2021) ~ BA_2020, data = ba_data, family = binomial(link = "logit"))

# Task 2:
# - Compare the fitted mean curves from the linear model and the binomial GLM
# - Plot BA_2021 against BA_2020
# - Add both fitted curves to the same figure
# - Make it visually clear which players had more at-bats in 2021
ba_data$fit_lm = fitted(reg_lm)
ba_data$fit_glm = fitted(binomial_glm)
ggplot(ba_data, mapping = aes(x = BA_2020, y = BA_2021)) + 
  geom_point() + 
  geom_line(aes(y = fit_lm, color = "Linear")) + 
  geom_line(aes(y = fit_glm, color = "binomial")) + geom_point(aes(size = AB_2021))

# Task 3:
# - Interpret the GLM coefficient on the log-odds scale
# - Translate the coefficient into the effect of a 0.010 increase in BA_2020
# - Explain why this GLM is a more natural model for hits/outs than ordinary linear regression
summary(binomial_glm)
#A unit increase in 2020 batting average leads to a 1.4814 log odds increase in 2021 batting average
#.01*1.4814 = .014814, thus an .014814 increase in log odds of 2021 BA.
#This glm is more natural because it predicts individual outcomes, and the overall probability of hits given those amounts.
#At its core, that is batting average, amount of hits out of total ABs. 


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
single_row <- data.frame(BA_2020 = .260)
player1_pred <- predict(binomial_glm, newdata = single_row, type = "link", se.fit = TRUE)
player1_hp = plogis(player1_pred$fit)
player1_hp
expected_hits60 = 60*player1_hp
expected_hits600 = 600*player1_hp

print(expected_hits60)
print(expected_hits600)

# for 60 AB's its 15.2346, for 600, its 152.346.

CI60Upp = player1_hp + 1.96 * sqrt(player1_hp*(1-player1_hp)/60)
CI60Low = player1_hp - 1.96 * sqrt(player1_hp*(1-player1_hp)/60)
CI600Upp = player1_hp + 1.96 * sqrt(player1_hp*(1-player1_hp)/600)
CI600Low = player1_hp - 1.96 * sqrt(player1_hp*(1-player1_hp)/600)

print(CI60Upp)
print(CI60Low)

print(CI600Upp)
print(CI600Low)

# the CI for 60 was 0.1437775, 0.3640427
# the CI for 600 was 0.2190831, 0.2887371
# the reason for the wider interval for 60 is because there are less trials, which theoretically means more uncertainty. This is exemplified in that you divide by n trials in the calculation, which would be greater for a smaller number. 

#######################################
### PART 2: FIELD-GOAL SUCCESS GAM ###
#######################################

fg_data = read_csv("../data/04_field-goals.csv.gz", show_col_types = FALSE)

# Task 1:
# - Fit at least 3 competing probability models for fg_made
# - Include:
#     * one logistic GLM with a simple functional form in ydl
#     * one richer logistic GLM (for example quadratic in ydl, possibly with kq)
#     * one logistic GAM using mgcv::gam(...)
# - A good starting point for the GAM is:
#     gam(fg_made ~ s(ydl, k = 12) + kq, family = "binomial", method = "REML")

basic_log = glm(fg_made ~ ydl, data = fg_data, family = binomial(link = "logit"))
rich_log = glm(fg_made ~ ydl + I(ydl^2) + kq, data = fg_data, family = binomial(link = "logit"))
gam_log = gam(fg_made ~ s(ydl, k = 12) + kq, data = fg_data, family = "binomial", method = "REML")

# Task 2:
# - Compare the models using out-of-sample predictive performance
# - Use test-set log loss or cross-validated log loss as the main metric
# - State clearly which model you prefer and why
set.seed(123)


n <- nrow(fg_data)
train_idx <- sample(seq_len(n), size = 0.8 * n)

train <- fg_data[train_idx, ]
test  <- fg_data[-train_idx, ]


basic_log1 <- glm(
  fg_made ~ ydl,
  data = train,
  family = binomial(link = "logit")
)

rich_log1 <- glm(
  fg_made ~ ydl + I(ydl^2) + kq,
  data = train,
  family = binomial(link = "logit")
)

gam_log1 <- gam(
  fg_made ~ s(ydl, k = 12) + kq,
  data = train,
  family = binomial,
  method = "REML"
)


p_basic <- predict(basic_log1, newdata = test, type = "response")
p_rich  <- predict(rich_log1,  newdata = test, type = "response")
p_gam   <- predict(gam_log1,   newdata = test, type = "response")


log_loss <- function(y, p) {
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}


results <- data.frame(
  Model = c("Basic Logistic", "Rich Logistic", "GAM"),
  Test_LogLoss = c(
    log_loss(test$fg_made, p_basic),
    log_loss(test$fg_made, p_rich),
    log_loss(test$fg_made, p_gam)
  )
)

results
#The GAM has the lowest log loss so I'd choose that. 


# Task 3:
# - For the selected GAM, report:
#     * the estimated parametric coefficient(s)
#     * the effective degrees of freedom (edf) of the smooth term
# - Explain what it means if the edf is noticeably larger than 1
summary(gam_log)
#intercept is 2.04594, kq has .26965. edf is 4.453. It means its likely a nonlinear relationship since EDF measures the complexity of the curve.

# Task 4:
# - Plot predicted make probability against ydl for your preferred GLM and your preferred GAM
# - Also compute binned observed make rates and overlay them on the plot
# - Comment on where the GAM improves on the simpler GLM
plot_df <- fg_data %>%
  mutate(bin = cut(ydl, breaks = seq(0, 70, by = 2))) %>%
  group_by(bin) %>%
  summarize(
    ydl = mean(ydl, na.rm = TRUE),
    rich_prob = mean(rich_fit, na.rm = TRUE),
    gam_prob = mean(gam_fit, na.rm = TRUE),
    obs_rate = mean(fg_made, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plot_df, aes(x = ydl)) +
  geom_line(aes(y = rich_prob, color = "Rich logistic"), linewidth = 1.2) +
  geom_line(aes(y = gam_prob, color = "GAM"), linewidth = 1.2) +
  geom_point(aes(y = obs_rate), size = 2, alpha = 0.7) +
  labs(
    x = "Yard line distance",
    y = "Make probability",
    color = "Model",
    title = "Predicted and Observed Make Probability by Distance"
  ) +
  theme_minimal()

#The GAM does a better job of adapting to the further field goal attempts, where there is a major drop off
# Task 5:
# - For a kicker with league-median kq, estimate make probability at:
#     * 20 yards from the opponent's end zone
#     * 35 yards from the opponent's end zone
#     * 50 yards from the opponent's end zone
# - For at least one of these yard lines, compute an approximate 95% confidence interval
#   using predict(..., type = "link", se.fit = TRUE) and then transform back with plogis()
kicker1 <- data.frame(ydl = c(20,35,50), kq = median(fg_data$kq))

kicker1_pred <- predict(gam_log, newdata = kicker1, type = "link", se.fit = TRUE)
print(plogis(kicker1_pred$fit))
ciUpp = kicker1_pred$fit + 1.96*kicker1_pred$se.fit
cilow = kicker1_pred$fit - 1.96*kicker1_pred$se.fit
print(plogis(ciUpp))
print(plogis(cilow))
# Task 6:
# - Briefly explain the difference between
#     * choosing polynomial terms by hand in a GLM, and
#     * letting a GAM learn a smooth curve with a wiggliness penalty
# - State one reason a GAM can help and one reason it can hurt
#The GAM will better incorporate distinctions in data, while the GLM will have a tougher time doing that given it can only follow a general curve. 
#The wiggliness penalty will ensure that the distinctions arent incorporated too much, and prevent overfitting.

# GAMs are useful for when the relationship seems fairly smooth, but can't be modeled by a basic transformation.
#The downside is that its not particularly strong at extrapolation since its changing so much. 
