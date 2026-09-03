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
# - Fit the day-1 style linear regression model BA_2021 ~ BA_2020
# - Fit a binomial GLM of the form
#     cbind(H_2021, AB_2021 - H_2021) ~ BA_2020
# - For the GLM, remember that each player has AB_2021 Bernoulli trials, not just one
summary(ba_data)
linear=lm(data=ba_data, BA_2021~BA_2020)
binomial=glm(data=ba_data, cbind(H_2021, AB_2021 - H_2021) ~ BA_2020, family="binomial")

ba_data=ba_data|>
  mutate(ylinear=predict(linear),
         ybinomial=predict(binomial, type="response"))
ggplot(ba_data) +
  geom_point(aes(x = BA_2020, y = BA_2021, size=AB_2021),
             alpha = 0.6) +
  geom_line(
    aes(
      x = BA_2020,
      y = ylinear,
      color = "Linear Regression"
    ),
    linewidth = 1
  ) +
  scale_size_continuous(range = c(0.5, 3))+
  geom_line(
    aes(
      x = BA_2020,
      y = ybinomial,
      color = "Binomial Logistic Regression"
    ),
    linewidth = 1
  ) +
  scale_color_manual(
    name = "Model",
    values = c(
      "Linear Regression" = "hotpink",
      "Binomial Logistic Regression" = "steelblue"
    )
  ) +
  labs(
    title = "Predicting 2021 Batting Average from 2020 Batting Average",
    subtitle = "Comparison of Linear and Binomial Regression Fits",
    x = "2020 Batting Average",
    y = "2021 Batting Average"
  ) +
  geom_smooth(aes(x = BA_2020, y = BA_2021), color="limegreen" , span=.0431035
              , method="loess")+
  theme_minimal()
# Task 2:
# - Compare the fitted mean curves from the linear model and the binomial GLM
# - Plot BA_2021 against BA_2020
# - Add both fitted curves to the same figure
# - Make it visually clear which players had more at-bats in 2021

# Task 3:
# - Interpret the GLM coefficient on the log-odds scale
# - Translate the coefficient into the effect of a 0.010 increase in BA_2020
# - Explain why this GLM is a more natural model for hits/outs than ordinary linear regression
# Extract GLM coefficients
coef(binomial)

beta0 <- coef(binomial)[1]
beta1 <- coef(binomial)[2]

# Effect of a 0.010 increase in BA_2020 on the log-odds scale
log_odds_change <- beta1 * 0.010

# Corresponding multiplicative change in odds
odds_ratio <- exp(log_odds_change)

cat("Intercept:", beta0, "\n")
cat("Slope:", beta1, "\n")
cat("Log-odds change for a 0.010 increase in BA_2020:", log_odds_change, "\n")
cat("Odds ratio for a 0.010 increase in BA_2020:", odds_ratio, "\n")
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
# Hypothetical player
player <- data.frame(BA_2020 = 0.260)

# Estimated 2021 hit probability
p <- predict(binomial,
             newdata = player,
             type = "response")

# Workloads
AB <- c(60, 600)

results <- data.frame(
  AB_2021 = AB,
  p = p,
  Expected_Hits = AB * p,
  Lower_BA = p - 1.96 * sqrt(p * (1 - p) / AB),
  Upper_BA = p + 1.96 * sqrt(p * (1 - p) / AB)
)

results
#######################################
### PART 2: FIELD-GOAL SUCCESS GAM ###
#######################################
library(splines)
fg_data = read_csv("../data/04_field-goals.csv.gz", show_col_types = FALSE)

# Task 1:
# - Fit at least 3 competing probability models for fg_made
# - Include:
#     * one logistic GLM with a simple functional form in ydl
#     * one richer logistic GLM (for example quadratic in ydl, possibly with kq)
#     * one logistic GAM using mgcv::gam(...)
# - A good starting point for the GAM is:
#     gam(fg_made ~ s(ydl, k = 12) + kq, family = "binomial", method = "REML")
summary(fg_data)
# Task 2:
# - Compare the models using out-of-sample predictive performance
# - Use test-set log loss or cross-validated log loss as the main metric
# - State clearly which model you prefer and why
# 80-20 train/test split
train_idx <- sample(
  seq_len(nrow(fg_data)),
  size = 0.8 * nrow(fg_data)
)

train <- fg_data[train_idx, ]
test  <- fg_data[-train_idx, ]

# Model 1: Simple logistic GLM
mod_glm <- glm(
  fg_made ~ ydl + kq,
  data = train,
  family = "binomial"
)

# --------------------------------------------------
# Choose spline complexity by elbow method
# --------------------------------------------------
dfs <- 3:12

logloss_vals <- numeric(length(dfs))

for(i in seq_along(dfs)) {
  
  fit <- glm(
    fg_made ~ bs(ydl, df = dfs[i]) + kq,
    data = train,
    family = "binomial"
  )
  
  p <- predict(
    fit,
    newdata = test,
    type = "response"
  )
  
  p <- pmax(pmin(p, 1 - 1e-15), 1e-15)
  
  logloss_vals[i] <- -mean(
    test$fg_made * log(p) +
      (1 - test$fg_made) * log(1 - p)
  )
}

plot(
  dfs,
  logloss_vals,
  type = "b",
  xlab = "Spline Degrees of Freedom",
  ylab = "Test Log Loss",
  main = "Choosing Spline Complexity"
)

# Pick elbow value after inspecting plot
best_df <- 6

# Model 2: Logistic GLM with B-spline
mod_bs <- glm(
  fg_made ~ bs(ydl, df = best_df) + kq,
  data = train,
  family = "binomial"
)

# Model 3: Logistic GAM
mod_gam <- gam(
  fg_made ~ s(ydl, k = 12) + kq,
  data = train,
  family = "binomial",
  method = "REML"
)

# --------------------------------------------------
# Test-set predictions
# --------------------------------------------------

p_glm <- predict(
  mod_glm,
  newdata = test,
  type = "response"
)

p_bs <- predict(
  mod_bs,
  newdata = test,
  type = "response"
)

p_gam <- predict(
  mod_gam,
  newdata = test,
  type = "response"
)

# Avoid log(0)
eps <- 1e-15

log_loss <- function(y, p) {
  p <- pmax(pmin(p, 1 - eps), eps)
  
  -mean(
    y * log(p) +
      (1 - y) * log(1 - p)
  )
}

results <- data.frame(
  Model = c(
    "Logistic GLM",
    "Logistic B-Spline",
    "Logistic GAM"
  ),
  LogLoss = c(
    log_loss(test$fg_made, p_glm),
    log_loss(test$fg_made, p_bs),
    log_loss(test$fg_made, p_gam)
  )
)

results[order(results$LogLoss), ]

# Prediction grid
plot_df <- data.frame(
  ydl = seq(min(fg_data$ydl),
            max(fg_data$ydl),
            length.out = 500),
  kq = mean(fg_data$kq)
)

# Predictions
plot_df$GLM <- predict(
  mod_glm,
  newdata = plot_df,
  type = "response"
)

plot_df$BSpline <- predict(
  mod_bs,
  newdata = plot_df,
  type = "response"
)

plot_df$GAM <- predict(
  mod_gam,
  newdata = plot_df,
  type = "response"
)

# Plot
ggplot() +
  geom_point(
    data = fg_data,
    aes(x = ydl, y = fg_made),
    alpha = 0.1
  ) +
  geom_line(
    data = plot_df,
    aes(x = ydl, y = GLM,
        color = "Logistic GLM"),
    linewidth = 1.2
  ) +
  geom_line(
    data = plot_df,
    aes(x = ydl, y = BSpline,
        color = "Logistic B-Spline"),
    linewidth = 1.2
  ) +
  geom_line(
    data = plot_df,
    aes(x = ydl, y = GAM,
        color = "Logistic GAM"),
    linewidth = 1.2
  ) +
  scale_color_manual(
    name = "Model",
    values = c(
      "Logistic GLM" = "hotpink",
      "Logistic B-Spline" = "steelblue",
      "Logistic GAM" = "darkgreen"
    )
  ) +
  labs(
    title = "Field Goal Make Probability vs Yardline",
    subtitle = paste(
      "Predictions shown at average kicker quality (kq =",
      round(mean(fg_data$kq), 2), ")"
    ),
    x = "Yards to Goal Line",
    y = "Predicted Make Probability"
  ) +
  theme_minimal()
# Task 3:
# - For the selected GAM, report:
#     * the estimated parametric coefficient(s)
#     * the effective degrees of freedom (edf) of the smooth term
# - Explain what it means if the edf is noticeably larger than 1

# Task 4:
# - Plot predicted make probability against ydl for your preferred GLM and your preferred GAM
# - Also compute binned observed make rates and overlay them on the plot
# - Comment on where the GAM improves on the simpler GLM

# Task 5:
# - For a kicker with league-median kq, estimate make probability at:
#     * 20 yards from the opponent's end zone
#     * 35 yards from the opponent's end zone
#     * 50 yards from the opponent's end zone
# - For at least one of these yard lines, compute an approximate 95% confidence interval
#   using predict(..., type = "link", se.fit = TRUE) and then transform back with plogis()

# Task 6:
# - Briefly explain the difference between
#     * choosing polynomial terms by hand in a GLM, and
#     * letting a GAM learn a smooth curve with a wiggliness penalty
# - State one reason a GAM can help and one reason it can hurt

# Prediction grid
plot_df <- data.frame(
  ydl = seq(min(fg_data$ydl),
            max(fg_data$ydl),
            length.out = 500),
  kq = median(fg_data$kq)
)

plot_df$BSpline <- predict(
  mod_bs,
  newdata = plot_df,
  type = "response"
)

plot_df$GAM <- predict(
  mod_gam,
  newdata = plot_df,
  type = "response"
)

# Binned observed make rates
binned <- fg_data %>%
  mutate(bin = cut(ydl, breaks = seq(0, 50, by = 2.5))) %>%
  group_by(bin) %>%
  summarize(
    ydl = mean(ydl),
    MakeRate = mean(fg_made),
    .groups = "drop"
  )

ggplot(data = binned) +
  geom_point(aes(x = ydl, y = MakeRate),
    color = "black",
    size = 2
  ) +
  geom_line(
    data = plot_df,
    aes(x = ydl, y = BSpline,
        color = "B-Spline GLM"),
    linewidth = 1.2
  ) +
  geom_line(
    data = plot_df,
    aes(x = ydl, y = GAM,
        color = "GAM"),
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = c(
      "B-Spline GLM" = "steelblue",
      "GAM" = "darkgreen"
    )
  ) +
  labs(
    title = "Field Goal Make Probability vs Yard Line",
    subtitle = "Binned observed make rates overlaid",
    x = "Yards from Opponent End Zone",
    y = "Make Probability",
    color = "Model"
  ) +
  theme_minimal()
yardlines <- c(20, 35, 50)

pred_df <- data.frame(
  ydl = yardlines,
  kq = median(fg_data$kq)
)

pred_df$Prob <- predict(
  mod_gam,
  newdata = pred_df,
  type = "response"
)

pred_df
ci_df <- data.frame(
  ydl = 35,
  kq = median(fg_data$kq)
)

pred_link <- predict(
  mod_gam,
  newdata = ci_df,
  type = "link",
  se.fit = TRUE
)

ci_results <- data.frame(
  YardLine = 35,
  Estimate = plogis(pred_link$fit),
  Lower95 = plogis(pred_link$fit - 1.96 * pred_link$se.fit),
  Upper95 = plogis(pred_link$fit + 1.96 * pred_link$se.fit)
)

ci_results
