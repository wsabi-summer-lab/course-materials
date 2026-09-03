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

day_1_model = lm(BA_2021 ~ BA_2020, data = ba_data)
binomial_glm = glm(cbind(H_2021, AB_2021-H_2021) ~ BA_2020, data = ba_data, family = binomial(link = "logit"))

grid = tibble(BA_2020 = seq(min(ba_data$BA_2020), max(ba_data$BA_2020), length.out = 200))

grid <- grid %>%
  mutate(
    `Linear ` = predict(day_1_model, newdata = grid),
    `Binomial` = predict(binomial_glm,  newdata = grid, type = "response")
  )


grid_long <- grid %>%
  pivot_longer(-BA_2020, names_to = "model", values_to = "pred")

ggplot() + geom_point(data = ba_data, aes(x = BA_2020, y = BA_2021, size = AB_2021), alpha = 0.4) +
  geom_line(data = grid_long, aes(BA_2020, pred, color = model), linewidth = 1)

summary(binomial_glm)
confint(binomial_glm)
#binomial model is a better fit for hits and outs because the outcomes follow a binomial distribution with successes(hits) and failures(outs)

#GLM coefficients: a0 = -1.46, b0 = 1.48
#SE(a0) = 0.037, SE(a1) = 0.144
#95% confdence intervalL a0 = [-1.54, -1.39], a1 = [1.20, 1.76]
# a 0.01 increase in batting average leads to a 0.0148 increase in log-odds of hitting in 2021

new_player = data.frame(BA_2020 = 0.260)
predict(binomial_glm, newdata = new_player, type = "response")
#######################################
### PART 2: FIELD-GOAL SUCCESS GAM ###
#######################################

fg_data = read_csv("../data/04_field-goals.csv.gz", show_col_types = FALSE)

n          <- nrow(fg_data)
train_idx  <- sample(seq_len(n), size = 0.8 * n) 
train_data <- fg_data[train_idx, ]
test_data  <- fg_data[-train_idx, ]

linear_model = lm(fg_made ~ ydl, data = train_data)
log_model = glm(fg_made ~ ydl + I(ydl^2) + kq, data = train_data, family = "binomial")
logistic_gam = gam(fg_made ~ s(ydl, k = 12) + kq, data = train_data, family = "binomial", method = "REML")

test_data$p_lm   <- predict(linear_model,   newdata = test_data)                    
test_data$p_log <- predict(log_model, newdata = test_data, type = "response")
test_data$p_log_gam <- predict(logistic_gam, newdata = test_data, type = "response")

logloss <- function(p, y) {
  p <- pmin(pmax(p, 1e-15), 1 - 1e-15) #bounding linear values to [0,1], <0 -> 0, >1 -> 1
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

#log-loss: linear -> 0.439, Logistic -> 0.388, Logistic GAM -> 0.388
results <- tibble(
  model   = c("Linear", "Logistic", "Logistic GAM"),
  logloss = c(logloss(test_data$p_lm,   test_data$fg_made),
              logloss(test_data$p_log, test_data$fg_made),
              logloss(test_data$p_log_gam, test_data$fg_made))
)
results

summary(logistic_gam) #kq = 0.293, SE = 0.06, confint: [0.18, 0.41]
pt <- summary(logistic_gam)$p.table 
pt

est <- pt["kq", "Estimate"]
se  <- pt["kq", "Std. Error"]
c(lower = est - 1.96 * se, upper = est + 1.96 * se)

grid <- tibble(ydl = seq(min(fg_data$ydl), max(fg_data$ydl), length.out = 200),
               kq  = kq_med)
grid$`GLM (quadratic)` <- predict(log_model,    newdata = grid, type = "response")
grid$`GAM (smooth)`    <- predict(logistic_gam, newdata = grid, type = "response")
grid_long <- pivot_longer(grid, c(`GLM (quadratic)`, `GAM (smooth)`),
                          names_to = "model", values_to = "p_hat")

binned <- fg_data %>%
  mutate(ydl_bin = cut(ydl, breaks = seq(0, 51, by = 3))) %>%
  group_by(ydl_bin) %>%
  summarise(ydl_mid = mean(ydl), make_rate = mean(fg_made), n = n(), .groups = "drop")

ggplot() +
  geom_point(data = binned, aes(ydl_mid, make_rate, size = n),
             alpha = 0.5, color = "grey25") +
  geom_line(data = grid_long, aes(ydl, p_hat, color = model), linewidth = 1) +
  scale_size_continuous(range = c(1, 6), name = "kicks in bin") +
  labs(x = "Yard line (ydl)", y = "Make probability", color = NULL,
       title = "FG make probability vs. yard line (league-median kicker)") +
  theme_minimal()

kq_med <- median(fg_data$kq)
new_kicks <- data.frame(ydl = c(20, 35, 50), kq = kq_med)

predict(logistic_gam, newdata = new_kicks, type = "response")

pred <- predict(logistic_gam, newdata = new_kicks, type = "link", se.fit = TRUE)
eta  <- pred$fit        # fitted log-odds
se   <- pred$se.fit     # SE on the logit scale

ci <- data.frame(
  ydl   = new_kicks$ydl,
  p_hat = plogis(eta),
  lower = plogis(eta - 1.96 * se),
  upper = plogis(eta + 1.96 * se)
)
ci

#The GAM captures the right tail probabilities much better due to the added flexibility. One optential risk of using a GAM is overfitting.

