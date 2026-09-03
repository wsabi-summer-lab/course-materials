########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "mgcv", "readr"))

library(dplyr)
library(ggplot2)
library(mgcv)
library(readr)
library(mgcv)

###############################
### PART 1: BATTING AVERAGE ###
###############################

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
ba_data = read_csv("C:/Users/sundw/Downloads/04_ba-2020-2021.csv", show_col_types = FALSE)

# Task 1:
# - Fit the day-1 style linear regression model BA_2021 ~ BA_2020
# - Fit a binomial GLM of the form
#     cbind(H_2021, AB_2021 - H_2021) ~ BA_2020
# - For the GLM, remember that each player has AB_2021 Bernoulli trials, not just one

model_lm <- lm(BA_2021 ~ BA_2020, data = ba_data)
summary(model)

# Linear model:
# y_i = 0.1621 + 0.3247x_i

model_glm <- glm(
  cbind(H_2021, AB_2021 - H_2021) ~ BA_2020,
  data   = ba_data,
  family = binomial(link = "logit")
)
summary(model_glm)

# Binomial GLM:
# logit(y_i) = -1.4630 + 1.4814x_i

# Task 2:
# - Compare the fitted mean curves from the linear model and the binomial GLM
# - Plot BA_2021 against BA_2020
# - Add both fitted curves to the same figure
# - Make it visually clear which players had more at-bats in 2021

plot(
  ba_data$BA_2020, ba_data$BA_2021,
  xlab = "2020 Batting Average",
  ylab = "2021 Batting Average",
  main = "2021 BA vs. 2020 BA",
  pch  = 16,
  col  = adjustcolor("steelblue", alpha.f = 0.5),
  cex  = sqrt(ba_data$AB_2021) / sqrt(max(ba_data$AB_2021)) * 3
)

# OLS line
abline(model_lm, col = "firebrick", lwd = 2)

# GLM curve
x_seq <- seq(min(ba_data$BA_2020), max(ba_data$BA_2020), length.out = 500)
y_glm <- predict(model_glm, newdata = data.frame(BA_2020 = x_seq), type = "response")
lines(x_seq, y_glm, col = "darkgreen", lwd = 2)

legend("topleft",
       legend = c("OLS", "Binomial GLM"),
       col    = c("firebrick", "darkgreen"),
       lwd    = 2, bty = "n")

# Task 3:
# - Interpret the GLM coefficient on the log-odds scale
# - Translate the coefficient into the effect of a 0.010 increase in BA_2020
# - Explain why this GLM is a more natural model for hits/outs than ordinary linear regression

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.4630     0.0372  -39.33   <2e-16 ***
#   BA_2020       1.4814     0.1445   10.25   <2e-16 ***

# For every x increase in BA_2020, the predicted value of BA_2021 is multiplied by e^1.4814x

# The GLM works better because it more directly models what's happening in BA. Batting average is a
# binomial experiment, with hits being a 1 and outs being a 0. The GLM also makes it so that the
# response variable (BA_2021) is restricted to the [0,1] interval, which the linear model doesn't do.

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

predict(model_glm, newdata = data.frame(BA_2020 = 0.260), type = "response")

p_hat <- predict(model_glm, newdata = data.frame(BA_2020 = 0.260), type = "response")

for (ab in c(60, 600)) {
  expected_hits <- ab * p_hat
  se <- sqrt(ab * p_hat * (1 - p_hat))
  ci_low  <- expected_hits - 1.96 * se
  ci_high <- expected_hits + 1.96 * se
  cat("AB =", ab, "| Expected hits:", round(expected_hits, 1),
      "| 95% CI: [", round(ci_low, 1), ",", round(ci_high, 1), "]\n")
}

# AB = 60 | Expected hits: 15.2 | 95% CI: [ 8.6 , 21.8 ]
# AB = 600 | Expected hits: 152.3 | 95% CI: [ 131.4 , 173.2 ]

# The interval for the 600-AB player is much narrower (relative to the sample sizes) because the larger
# sample size reduces the variance, therefore giving a narrower confidence interval

#######################################
### PART 2: FIELD-GOAL SUCCESS GAM ###
#######################################

fg_data = read_csv("C:/Users/sundw/Downloads/04_field-goals.csv", show_col_types = FALSE)

# Task 1:
# - Fit at least 3 competing probability models for fg_made
# - Include:
#     * one logistic GLM with a simple functional form in ydl
#     * one richer logistic GLM (for example quadratic in ydl, possibly with kq)
#     * one logistic GAM using mgcv::gam(...)
# - A good starting point for the GAM is:
#     gam(fg_made ~ s(ydl, k = 12) + kq, family = "binomial", method = "REML")

model_fg_GLM1 <- glm(fg_made ~ ydl, data = fg_data, family = binomial(link = "logit"))

summary(model_fg_GLM1)

model_fg_GLM2 <- glm(fg_made ~ ydl + kq, data = fg_data, family = binomial(link = "logit"))

summary(model_fg_GLM2)

model_fg_GAM <- gam(fg_made ~ s(ydl) + kq, data = fg_data, family = binomial(link = "logit"))

summary(model_fg_GAM)

kq_med  <- median(fg_data$kq)
newdata <- data.frame(ydl = 1:50, kq = kq_med)

newdata$GLM1 <- predict(model_fg_GLM1, newdata, type = "response")
newdata$GLM2 <- predict(model_fg_GLM2, newdata, type = "response")
newdata$GAM  <- predict(model_fg_GAM,  newdata, type = "response")

# Empirical make rate per ydl (for the points)
emp <- aggregate(fg_made ~ ydl, data = fg_data, FUN = mean)
emp$n <- aggregate(fg_made ~ ydl, data = fg_data, FUN = length)$fg_made

# Reshape predictions to long format for ggplot
library(tidyr)
pred_long <- pivot_longer(newdata, cols = c(GLM1, GLM2, GAM),
                          names_to = "model", values_to = "pred")

ggplot() +
  geom_point(data = emp, aes(x = ydl, y = fg_made, size = n),
             colour = "grey50", alpha = 0.6) +
  geom_line(data = pred_long, aes(x = ydl, y = pred, colour = model, linetype = model),
            linewidth = 0.9) +
  scale_size_continuous(name = "Attempts", range = c(1, 6)) +
  scale_colour_manual(name = "Model",
                      values = c(GLM1 = "#185FA5", GLM2 = "#1D9E75", GAM = "#D85A30")) +
  scale_linetype_manual(name = "Model",
                        values = c(GLM1 = "solid", GLM2 = "dashed", GAM = "dotdash")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(x = "Yards from goal line (ydl)",
       y = "P(field goal made)",
       title = "Field goal make probability by distance",
       subtitle = paste("Model curves evaluated at median kq =", round(kq_med, 3))) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# Task 2:
# - Compare the models using out-of-sample predictive performance
# - Use test-set log loss or cross-validated log loss as the main metric
# - State clearly which model you prefer and why

set.seed(123)
n         <- nrow(fg_data)
train_idx <- sample(n, size = 0.8 * n)
train     <- fg_data[ train_idx, ]
test      <- fg_data[-train_idx, ]

model_fg_GLM1 <- glm(fg_made ~ ydl,         data = train, family = binomial(link = "logit"))
model_fg_GLM2 <- glm(fg_made ~ ydl + kq,    data = train, family = binomial(link = "logit"))
model_fg_GAM  <- gam(fg_made ~ s(ydl) + kq, data = train, family = binomial(link = "logit"))

p1 <- predict(model_fg_GLM1, test, type = "response")
p2 <- predict(model_fg_GLM2, test, type = "response")
p3 <- predict(model_fg_GAM,  test, type = "response")

y  <- test$fg_made

rmse     <- function(y, p) sqrt(mean((y - p)^2))
rse <- function(y, p) sqrt(sum((y - p)^2) / (length(y) - 2))
log_loss <- function(y, p) {
  p <- pmax(pmin(p, 1 - 1e-15), 1e-15)   # clip to avoid log(0)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

metrics <- data.frame(
  Model    = c("GLM1 (ydl)", "GLM2 (ydl + kq)", "GAM (s(ydl) + kq)"),
  RMSE     = c(rmse(y, p1),     rmse(y, p2),     rmse(y, p3)),
  RSE      = c(rse(y, p1),      rse(y, p2),      rse(y, p3)),
  Log_Loss = c(log_loss(y, p1), log_loss(y, p2), log_loss(y, p3))
)

metrics[, 2:4] <- round(metrics[, 2:4], 4)
print(metrics)

# Comparing log loss, the preferred model is the GAM  with ydl and kq.

# Task 3:
# - For the selected GAM, report:
#     * the estimated parametric coefficient(s)
#     * the effective degrees of freedom (edf) of the smooth term
# - Explain what it means if the edf is noticeably larger than 1

summary(model_fg_GAM)

# Parametric coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  2.06780    0.04596  44.991  < 2e-16 ***
#   kq           0.22157    0.05966   3.714 0.000204 ***

# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq p-value    
# s(ydl) 4.407  5.425  781.7  <2e-16 ***

# The edf being greater than 1 means that the drop-off in make probability isn't constant over
# different distances.

# Task 4:
# - Plot predicted make probability against ydl for your preferred GLM and your preferred GAM
# - Also compute binned observed make rates and overlay them on the plot
# - Comment on where the GAM improves on the simpler GLM

kq_med  <- median(fg_data$kq)
newdata <- data.frame(ydl = 1:50, kq = kq_med)
newdata$GAM  <- predict(model_fg_GAM,  newdata, type = "response")
newdata$GLM2 <- predict(model_fg_GLM2, newdata, type = "response")

fg_data$ydl_bin <- cut(fg_data$ydl, breaks = seq(0, 50, by = 5), include.lowest = TRUE)
bins <- aggregate(fg_made ~ ydl_bin, data = fg_data, FUN = mean)
bins$n       <- aggregate(fg_made ~ ydl_bin, data = fg_data, FUN = length)$fg_made
bins$ydl_mid <- seq(2.5, 47.5, by = 5)

pred_long <- pivot_longer(newdata, cols = c(GAM, GLM2),
                          names_to = "model", values_to = "pred")

ggplot() +
  geom_point(data = bins, aes(x = ydl_mid, y = fg_made, size = n),
             colour = "grey40", alpha = 0.7) +
  geom_line(data = pred_long, aes(x = ydl, y = pred, colour = model, linetype = model),
            linewidth = 1) +
  scale_size_continuous(name = "Attempts", range = c(2, 8)) +
  scale_colour_manual(name = "Model",
                      values = c(GLM2 = "#185FA5", GAM = "#D85A30")) +
  scale_linetype_manual(name = "Model",
                        values = c(GLM2 = "dashed", GAM = "solid")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(x = "Yards from goal line (ydl)",
       y = "P(field goal made)",
       title = "Predicted make probability vs. binned observed rates",
       subtitle = paste("Curves evaluated at median kq =", round(kq_med, 3),
                        "| Points sized by attempts per bin")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# The GAM improves on the GLM in the tail. The GLM doesn't curve as well towards the actual extremity
# as the GAM

# Task 5:
# - For a kicker with league-median kq, estimate make probability at:
#     * 20 yards from the opponent's end zone
#     * 35 yards from the opponent's end zone
#     * 50 yards from the opponent's end zone
# - For at least one of these yard lines, compute an approximate 95% confidence interval
#   using predict(..., type = "link", se.fit = TRUE) and then transform back with plogis()

newdata <- data.frame(
  ydl = c(20, 35, 50),
  kq  = mean(fg_data$kq)
)

newdata$pred <- predict(model_fg_GAM, newdata, type = "response")
print(newdata)

newdata_50 <- data.frame(ydl = 50, kq = mean(fg_data$kq))
pred_50    <- predict(model_fg_GAM, newdata_50, type = "link", se.fit = TRUE)

lwr <- plogis(pred_50$fit - 1.96 * pred_50$se.fit)
upr <- plogis(pred_50$fit + 1.96 * pred_50$se.fit)

cat("50-yd predicted probability:", round(plogis(pred_50$fit), 4), "\n")
cat("95% CI: [", round(lwr, 4), ",", round(upr, 4), "]\n")

# ydl       kq      pred
# 1  20 0.205721 0.8684544
# 2  35 0.205721 0.6436462
# 3  50 0.205721 0.1478862

# 50-yd predicted probability: 0.1479

# 95% CI: [ 0.0659 , 0.2992 ]

# Task 6:
# - Briefly explain the difference between
#     * choosing polynomial terms by hand in a GLM, and
#     * letting a GAM learn a smooth curve with a wiggliness penalty
# - State one reason a GAM can help and one reason it can hurt

# Choosing by hand in a GLM just depends on yourself, while a GAM uses the data to make a better fit.
# The GAM fits the data better, but is harder to intuitively interpret due to its complexity.
