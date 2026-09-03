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

# Task 1:
# Linear regression
lm_model <- lm(BA_2021 ~ BA_2020, data = ba_data)
summary(lm_model)
# = 0.16213 + 0.32473 * BA2020

#Binomial GLM
glm_model <- glm(cbind(H_2021, AB_2021 - H_2021) ~ BA_2020,
                 family = binomial(link = "logit"),
                 data = ba_data)
summary(glm_model)

# logit(pi) = -1.4630 + 1.4814*BA2020

# Task 2:

ba_seq <- seq(min(ba_data$BA_2020), max(ba_data$BA_2020), length.out = 200)

# Predictions
lm_pred <- predict(lm_model, newdata = data.frame(BA_2020 = ba_seq))
glm_pred <- predict(glm_model, newdata = data.frame(BA_2020 = ba_seq),
                    type = "response")

# Plot
plot(ba_data$BA_2020, ba_data$BA_2021,
     cex = sqrt(ba_data$AB_2021) / 15,
     pch = 21, bg = adjustcolor("steelblue", alpha.f = 0.3),
     col = adjustcolor("steelblue", alpha.f = 0.4),
     xlab = "BA 2020", ylab = "BA 2021",
     main = "Batting Average: LM vs Binomial GLM",
     xlim = c(0.13, 0.40), ylim = c(0.08, 0.35))

lines(ba_seq, glm_pred, col = "darkgreen", lwd = 3)
lines(ba_seq, lm_pred, col = "red", lwd = 2, lty = 2)

legend("topleft", legend = c("LM", "Binomial GLM"),
       col = c("red", "darkgreen"), lwd = c(2, 3),
       lty = c(2, 1), bty = "n")

#The fits differ. The linear regression model is slightly steeper compared to the binomial GLM, but alos has a lower intercept. GLM is a more natural fit here because it is a binomial outcome so a binomial model will fit better. 

# Task 3:
# Coefficients, SEs, and CIs
coef(summary(glm_model))
confint(glm_model)

#the slope is 1.48 which means that an increase in say 0.01 in BA2020 will increase the log-odds by 0.01*1.4814 or increase the odds by e^1.014, and following our class discussion we know that near 0 the tagent line to e^x is 1+x so there is approximately a 1.4 percent increase in the odds

# Task 4:
new_player <- data.frame(BA_2020 = 0.260)
p_hat <- predict(glm_model, newdata = new_player, type = "response")
p_hat

# Expected hits at each workload
AB_low <- 60
AB_high <- 600

hits_low <- p_hat * AB_low
hits_high <- p_hat * AB_high

# 95% intervals for observed BA
ci_low <- c(p_hat - 1.96 * sqrt(p_hat * (1 - p_hat) / AB_low),
            p_hat + 1.96 * sqrt(p_hat * (1 - p_hat) / AB_low))

ci_high <- c(p_hat - 1.96 * sqrt(p_hat * (1 - p_hat) / AB_high),
             p_hat + 1.96 * sqrt(p_hat * (1 - p_hat) / AB_high))

# Print results
cat("60 AB - Expected hits:", hits_low, "\n")
cat("60 AB - 95% CI for BA:", ci_low, "\n")
cat("600 AB - Expected hits:", hits_high, "\n")
cat("600 AB - 95% CI for BA:", ci_high, "\n")

#The CI for the 600 AB is so much wider because it is the SE is calculated using division by the number of at bats so the SE is larger since the n is smaller even though the probability is the same.

#######################################
### PART 2: FIELD-GOAL SUCCESS GAM ###
#######################################

fg_data = read_csv("../data/04_field-goals.csv.gz", show_col_types = FALSE)

# Task 1:
glm_simple <- glm(fg_made ~ ydl, 
                  family = binomial(link = "logit"),
                  data = fg_data)


glm_rich <- glm(fg_made ~ ydl + kq + I(kq^2),
                family = binomial(link = "logit"),
                data = fg_data)# Task 2:

gam_model <- gam(fg_made ~ s(ydl, bs = "cr", k = 5) + kq,
                 family = binomial(link = "logit"),
                 data = fg_data)
summary(gam_model)
# Task 2:
# Train/test split (80/20)
set.seed(42)
train_idx <- sample(nrow(fg_data), 0.8 * nrow(fg_data))
train <- fg_data[train_idx, ]
test <- fg_data[-train_idx, ]

# Refit models on train
glm_simple_tr <- glm(fg_made ~ ydl,
                     family = binomial(link = "logit"), data = train)

glm_rich_tr <- glm(fg_made ~ ydl + kq + I(kq^2),
                   family = binomial(link = "logit"), data = train)

gam_tr <- gam(fg_made ~ s(ydl, bs = "cr", k = 5) + kq,
              family = binomial(link = "logit"), data = train)

# Log loss function
log_loss <- function(actual, predicted) {
  predicted <- pmax(pmin(predicted, 1 - 1e-15), 1e-15)
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

# Predict on test set
ll_simple <- log_loss(test$fg_made, predict(glm_simple_tr, test, type = "response"))
ll_rich   <- log_loss(test$fg_made, predict(glm_rich_tr, test, type = "response"))
ll_gam    <- log_loss(test$fg_made, predict(gam_tr, test, type = "response"))

cat("Simple GLM log loss:", ll_simple, "\n")
cat("Rich GLM log loss:",   ll_rich, "\n")
cat("GAM log loss:",        ll_gam, "\n")

# Task 3:
summary(gam_model)


#A one percent increase in kicker quality increases the log-odds of making a field goal by 0.26 if you hold the ydl constant. The edf being significantly bvove one shows that the yard line and make probability is nonlinear.

# Task 4:
# Sequence of yard lines
ydl_seq <- seq(min(fg_data$ydl), max(fg_data$ydl), length.out = 200)
median_kq <- median(fg_data$kq)

# Predictions from both models
glm_pred <- predict(glm_simple, newdata = data.frame(ydl = ydl_seq, kq = median_kq), 
                    type = "response")
gam_pred <- predict(gam_model, newdata = data.frame(ydl = ydl_seq, kq = median_kq), 
                    type = "response")

# Binned observed make rates
fg_data$ydl_bin <- cut(fg_data$ydl, breaks = 20)
bin_rates <- aggregate(fg_made ~ ydl_bin, data = fg_data, mean)
bin_mid <- tapply(fg_data$ydl, fg_data$ydl_bin, mean)

# Plot
plot(ydl_seq, gam_pred, type = "l", col = "darkgreen", lwd = 3,
     xlab = "Yard Line (from opponent's end zone)",
     ylab = "P(Field Goal Made)",
     main = "FG Make Probability vs Yard Line",
     ylim = c(0, 1))

lines(ydl_seq, glm_pred, col = "red", lwd = 2, lty = 2)

points(bin_mid, bin_rates$fg_made, pch = 16, cex = 1.2, col = "steelblue")

legend("topright", legend = c("GAM", "Simple GLM", "Observed rates"),
       col = c("darkgreen", "red", "steelblue"),
       lwd = c(3, 2, NA), lty = c(1, 2, NA), pch = c(NA, NA, 16), bty = "n")
#The GAM improves upon the simple GLM model when predicting deeper kicks since the GLM does not decrease rapidly enough at the end. THe two fits are very similar for close kicks.

# Task 5:
# Median kq
median_kq <- median(fg_data$kq)

# Three yard lines
newdat_pred <- data.frame(ydl = c(20, 35, 50), kq = median_kq)

# Predictions on response scale
preds <- predict(gam_model, newdata = newdat_pred, type = "response")
cat("20 ydl:", preds[1], "\n")
cat("35 ydl:", preds[2], "\n")
cat("50 ydl:", preds[3], "\n")

# CI on logit scale for ydl = 35, then transform back
pred_link <- predict(gam_model, newdata = newdat_pred, type = "link", se.fit = TRUE)

logit_est <- pred_link$fit[2]
logit_se  <- pred_link$se.fit[2]

ci_lower <- plogis(logit_est - 1.96 * logit_se)
ci_upper <- plogis(logit_est + 1.96 * logit_se)
cat("35 ydl 95% CI: (", ci_lower, ",", ci_upper, ")\n")


# Task 6:

#choosing the polynomial terms by hand in the glm is more difficult to know what function is important to include and there is less flexibility in curve shape whereas GAM can fit practically any shape. GAM can help fit complicated shapes and curve to a shape better, but a risk of GAM is more difficult to interpret and explain compared to a simple B coefficient.   
