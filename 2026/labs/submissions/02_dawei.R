#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(readr)
library(dplyr)
library(gridExtra)

# set seed
set.seed(3)

##############
### PART 1 ###
##############

# load data
nba_four_factors = read_csv("C:/Users/sundw/Downloads/02_nba-four-factors.csv")

# Task 1:
# - Compute each variable's mean, standard deviation, minimum, and maximum
# - Plot the marginal distribution of each explanatory variable
# - Make scatterplots of wins against each of the four factors
# - Compute correlations between each pair of explanatory variables
# - Identify which variables look most strongly related to wins before fitting a model

df <- read_csv("C:/Users/sundw/Downloads/02_nba-four-factors.csv")

# x1: Effective FG% - Opponent Effective FG%
df$x1 <- df$`EFG%` - df$`OPP EFG%`

# x2: Offensive REB% + Defensive REB% - 100
df$x2 <- df$`OREB%` + df$`DREB%` - 100

# x3: Opponent TOV% - Own TOV%
df$x3 <- df$`OPP TOV %` - df$`TOV%`

# x4: FT Rate - Opponent FT Rate
df$x4 <- df$`FT Rate` - df$`OPP FT Rate`

write_csv(df, "02_nba-four-factors-updated.csv.gz")

summary_stats <- df %>%
  summarise(across(
    c(x1, x2, x3, x4),
    list(
      mean = ~mean(., na.rm = TRUE),
      sd   = ~sd(.,   na.rm = TRUE),
      max  = ~max(.,  na.rm = TRUE),
      min  = ~min(.,  na.rm = TRUE)
    )
  )) %>%
  pivot_longer(
    everything(),
    names_to  = c("factor", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

print(summary_stats)

#  factor       mean     sd    max     min
#   x1      0.000417  2.76   8.4    -7.30  
#   x2     -0.0265    2.72   7.3    -8.90  
#   x3     -0.00146   1.45   3.8    -4.1   
#   x4     -0.0000865 0.0297 0.0832 -0.0856

# --- Marginal distributions (density plots) ---
p1 <- ggplot(df, aes(x = x1)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", alpha = 0.6) +
  geom_density(color = "navy", linewidth = 1) +
  labs(title = "x1: Shooting (EFG% Diff)", x = "x1", y = "Density") +
  theme_minimal()

p2 <- ggplot(df, aes(x = x2)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "tomato", alpha = 0.6) +
  geom_density(color = "darkred", linewidth = 1) +
  labs(title = "x2: Rebounding (REB% Diff)", x = "x2", y = "Density") +
  theme_minimal()

p3 <- ggplot(df, aes(x = x3)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "seagreen", alpha = 0.6) +
  geom_density(color = "darkgreen", linewidth = 1) +
  labs(title = "x3: Turnovers (TOV% Diff)", x = "x3", y = "Density") +
  theme_minimal()

p4 <- ggplot(df, aes(x = x4)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "mediumpurple", alpha = 0.6) +
  geom_density(color = "purple4", linewidth = 1) +
  labs(title = "x4: Free Throws (FT Rate Diff)", x = "x4", y = "Density") +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol = 2)

# --- Scatterplots of wins vs each factor ---
s1 <- ggplot(df, aes(x = x1, y = W)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "navy", se = TRUE) +
  labs(title = "Wins vs x1 (Shooting)", x = "x1", y = "Wins") +
  theme_minimal()

s2 <- ggplot(df, aes(x = x2, y = W)) +
  geom_point(alpha = 0.4, color = "tomato") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(title = "Wins vs x2 (Rebounding)", x = "x2", y = "Wins") +
  theme_minimal()

s3 <- ggplot(df, aes(x = x3, y = W)) +
  geom_point(alpha = 0.4, color = "seagreen") +
  geom_smooth(method = "lm", color = "darkgreen", se = TRUE) +
  labs(title = "Wins vs x3 (Turnovers)", x = "x3", y = "Wins") +
  theme_minimal()

s4 <- ggplot(df, aes(x = x4, y = W)) +
  geom_point(alpha = 0.4, color = "mediumpurple") +
  geom_smooth(method = "lm", color = "purple4", se = TRUE) +
  labs(title = "Wins vs x4 (Free Throws)", x = "x4", y = "Wins") +
  theme_minimal()

grid.arrange(s1, s2, s3, s4, ncol = 2)

# Correlation between each pair of the four factors:

cor_matrix <- cor(df[, c("x1", "x2", "x3", "x4")], use = "complete.obs")

cor_df <- as.data.frame(round(cor_matrix, 4))
print(cor_df)

#         x1      x2      x3      x4
#  x1  1.0000 -0.0736  0.0602  0.2586
#  x2 -0.0736  1.0000 -0.2907  0.0724
#  x3  0.0602 -0.2907  1.0000 -0.1378
#  x4  0.2586  0.0724 -0.1378  1.0000

# The four factors, when plotted against wins, seems to indicate that shooting is the most strongly
# correlated with wins.

# Task 2:
# - Fit the multivariable model: wins ~ x1 + x2 + x3 + x4
# - Write down the fitted regression equation
# - Interpret each coefficient in context
# - Check whether the coefficient signs make sense given the variable definitions
# - Identify which factors look strongest and weakest after adjustment

model <- lm(W ~ x1 + x2 + x3 + x4, data = df)

summary(model)

# Fitted regression equation (W = wins):
# W = 40.1867 + 3.6749x1 + 1.3403x2 + 3.0588x3 + 77.0691x4

# Interpretation: Each unit of increase in x1 gains you 3.6749 wins, each unit of increase in x2 gains
# you 1.3403 wins, and so on. Note that the coefficient for x4 is large because FT rate was represented
# as a decimal instead of a percentage like most other variables in the table.

# The coefficient signs make sense, since we defined them in a way such that positive numbers are good
# Looking at the t-values, x1 > x3 > x2 > x4 in terms of association with wins

# Task 3:
# - Standardize the four predictors
# - Fit the standardized model
# - Rank the factors by absolute standardized coefficient size
# - Compare the original and standardized models for interpretability
# - Compare fitted values from both models and explain why they match or differ

df$x1_s <- scale(df$x1)
df$x2_s <- scale(df$x2)
df$x3_s <- scale(df$x3)
df$x4_s <- scale(df$x4)

model_scaled <- lm(W ~ x1_s + x2_s + x3_s + x4_s, data = df)

summary(model_scaled)

# Absolute standardized coefficient size: x1 > x3 > x2 > x4

# The standardized model is much easier to interpret, since the coefficients of the standardized
# model can now be directly compared to see which predictor is most heavily correlated with wins

# The fitted values from the two models would be the same, since by standardizing, we essentially divide
# each predictor by a constant (its standard deviation). The span (column space) doesn't really change,
# since the span of four linearly independent vectors does not change if they are each divided by a
# constant.

# Task 4:
# - Report the residual standard error and interpret it in wins
# - Report coefficient standard errors and 95% confidence intervals
# - Identify which effects are clearly different from zero
# - Choose one team and compute a point prediction, confidence interval, and prediction interval
# - State which interval is wider and why

# Residual standard error: 3.977
# This number is the average difference between predicted and actual number of wins

# Coefficient standard errors:
# x1: 0.1896
# x2: 0.1905
# x3: 0.1919
# x4: 0.1908

model <- lm(W ~ x1_s + x2_s + x3_s + x4_s, data = df)

confint(model, level = 0.95)

# Confidence Intervals:
# x1         9.762035 10.506983
# x2         3.266077  4.014824
# x3         4.063610  4.817913
# x4         1.911685  2.661586

# All of them have incredibly small p-values, so they are all distinguishable from 0.

rockets_2020 <- df[df$Team_Season == "Houston Rockets2020", c("x1_s", "x2_s", "x3_s", "x4_s")]

predict(model_scaled, newdata = rockets_2020)
predict(model_scaled, newdata = rockets_2020, interval = "confidence", level = 0.95)
predict(model_scaled, newdata = rockets_2020, interval = "prediction", level = 0.95)

# Confidence interval: 43.93228, 45.50312
# Prediction interval: 36.86271 52.57269
# The prediction interval is wider because it deals with the single season given the Rockets' stats,
# but the confidence interval finds the expected number of wins of a team with those stats

# Task 5:
# - Randomly split the data into training and test sets
# - Fit the original and standardized models on the training set
# - Compute test-set RMSE for both models
# - Compare predictive performance

set.seed(42)

n <- nrow(df)
train_idx <- sample(1:n, size = floor(0.8 * n))

train <- df[train_idx, ]
test  <- df[-train_idx, ]

# Fit both models on training set
model_train        <- lm(W ~ x1 + x2 + x3 + x4, data = train)
model_scaled_train <- lm(W ~ x1_s + x2_s + x3_s + x4_s, data = train)

# Predictions on test set
pred        <- predict(model_train,        newdata = test)
pred_scaled <- predict(model_scaled_train, newdata = test)

# RMSE
rmse        <- sqrt(mean((test$W - pred)^2))
rmse_scaled <- sqrt(mean((test$W - pred_scaled)^2))

cat("RMSE (original):     ", round(rmse, 4), "\n")
cat("RMSE (standardized): ", round(rmse_scaled, 4), "\n")

# The standardized model just makes the numbers easier to compare, so there should not be any 
# difference in predictive performance.

##############
### PART 2 ###
##############

# load data
punts = read_csv("C:/Users/sundw/Downloads/02_punts.csv")

# Task 1:
# - Plot post-punt yard line against starting yard line
# - Bin punts by starting field position and plot average post-punt yard line in each bin
# - Describe the shape of the relationship and where it bends
# - Plot or summarize the distribution of punter quality

ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(alpha = 0.4, color = "#2c7bb6", size = 1.5) +
  labs(
    title = "Punt: Starting vs. Resulting Field Position",
    x = "ydl (yards from own end zone — before punt)",
    y = "next_ydl (yards from own end zone — after punt)"
  ) +
  theme_minimal()

punts_binned <- punts %>%
  mutate(ydl_bin = cut(ydl, breaks = seq(0, 100, by = 5), right = FALSE)) %>%
  group_by(ydl_bin) %>%
  summarise(
    avg_next_ydl = mean(next_ydl, na.rm = TRUE),
    n = n()
  )

ggplot(punts_binned, aes(x = ydl_bin, y = avg_next_ydl)) +
  geom_col(fill = "#2c7bb6", alpha = 0.8) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5, size = 3) +
  labs(
    title = "Average Post-Punt Field Position by Starting Field Position",
    x = "Starting yard line (yards from own end zone)",
    y = "Avg next_ydl (yards from own end zone)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# The graph seems to curve downwards, bending around ydl = 55

ggplot(punts, aes(x = pq)) +
  geom_histogram(fill = "#2c7bb6", alpha = 0.8, bins = 30) +
  labs(
    title = "Distribution of Punter Quality (pq)",
    x = "Punter Quality",
    y = "Count"
  ) +
  theme_minimal()

# The distribution of punter quality is somewhat normal, though it is left-skewed.

# Task 2:
# - Fit competing punt models: linear, quadratic, quadratic plus punter quality, and spline
# - Visualize the fitted curves from each model
# - Use train/test RMSE or cross-validation to choose a preferred model
# - Compare the linear, quadratic, and spline tradeoffs
# - Assess whether punter quality improves out-of-sample prediction
# - Interpret the punter-quality coefficient if it is included in the selected model

m_linear <- lm(next_ydl ~ ydl,                 data = punts)
m_quad   <- lm(next_ydl ~ ydl + I(ydl^2),      data = punts)
m_pq     <- lm(next_ydl ~ ydl + I(ydl^2) + pq, data = punts)

m_linear
m_quad
m_pq

ydl_seq <- tibble(ydl = seq(min(punts$ydl), max(punts$ydl), length.out = 200),
                  pq  = mean(punts$pq))

curve_df <- ydl_seq %>%
  mutate(
    Linear           = predict(m_linear, newdata = ydl_seq),
    Quadratic        = predict(m_quad,   newdata = ydl_seq),
    `Quadratic + pq` = predict(m_pq,     newdata = ydl_seq)
  ) %>%
  pivot_longer(c(Linear, Quadratic, `Quadratic + pq`),
               names_to = "model", values_to = "next_ydl")

ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(alpha = 0.2, size = 1, color = "grey50") +
  geom_line(data = curve_df, aes(color = model), linewidth = 1) +
  labs(
    title = "Fitted Punt Models",
    x     = "Starting yard line (ydl)",
    y     = "Post-punt yard line (next_ydl)",
    color = "Model"
  ) +
  theme_minimal()

set.seed(42)

train_idx <- sample(nrow(punts), 0.8 * nrow(punts))
train     <- punts[train_idx, ]
test      <- punts[-train_idx, ]

m_linear <- lm(next_ydl ~ ydl,                 data = train)
m_quad   <- lm(next_ydl ~ ydl + I(ydl^2),      data = train)
m_pq     <- lm(next_ydl ~ ydl + I(ydl^2) + pq, data = train)

rmse <- function(model, data) {
  sqrt(mean((data$next_ydl - predict(model, newdata = data))^2))
}

results <- tibble(
  model      = c("Linear", "Quadratic", "Quadratic + pq"),
  train_rmse = c(rmse(m_linear, train), rmse(m_quad, train), rmse(m_pq, train)),
  test_rmse  = c(rmse(m_linear, test),  rmse(m_quad, test),  rmse(m_pq, test))
)

print(results)

results %>%
  pivot_longer(c(train_rmse, test_rmse), names_to = "split", values_to = "rmse") %>%
  mutate(model = factor(model, levels = c("Linear", "Quadratic", "Quadratic + pq")),
         split = recode(split, train_rmse = "Train", test_rmse = "Test")) %>%
  ggplot(aes(x = model, y = rmse, fill = split)) +
  geom_col(position = "dodge", alpha = 0.85) +
  labs(title = "Train vs. Test RMSE by Model", x = NULL, y = "RMSE", fill = NULL) +
  theme_minimal()

# The quadratic model is better (lower RMSE). The tradeoff is that the quadratic fit will always fit at
# least as well, but will be more difficult to interpret.

# Punter quality does not seem to improve the model in any meaningful way.

# Task 3:
# - Plot the fitted mean response for the selected punt model
# - Add a 95% confidence band for the expected response
# - Add a 95% prediction band for one individual punt
# - Explain why the prediction band is wider
# - Identify where the model is most uncertain

ydl_seq <- tibble(ydl = seq(min(punts$ydl), max(punts$ydl), length.out = 200))

conf_band <- predict(m_quad, newdata = ydl_seq, interval = "confidence") %>%
  as_tibble() %>% rename(fit_conf = fit, lwr_conf = lwr, upr_conf = upr)

pred_band <- predict(m_quad, newdata = ydl_seq, interval = "prediction") %>%
  as_tibble() %>% rename(fit_pred = fit, lwr_pred = lwr, upr_pred = upr)

plot_df <- bind_cols(ydl_seq, conf_band, pred_band)

ggplot(plot_df, aes(x = ydl)) +
  geom_point(data = punts, aes(y = next_ydl), alpha = 0.2, size = 1, color = "grey50") +
  geom_ribbon(aes(ymin = lwr_pred, ymax = upr_pred, fill = "95% Prediction"), alpha = 0.2) +
  geom_ribbon(aes(ymin = lwr_conf, ymax = upr_conf, fill = "95% Confidence"), alpha = 0.4) +
  geom_line(aes(y = fit_conf), color = "#2c7bb6", linewidth = 1) +
  scale_fill_manual(values = c("95% Confidence" = "#2c7bb6", "95% Prediction" = "#f4a442")) +
  labs(
    title = "Quadratic Model: Confidence and Prediction Bands",
    x     = "Starting yard line (ydl)",
    y     = "Post-punt yard line (next_ydl)",
    fill  = NULL
  ) +
  theme_minimal()

# The confidence band is the predicted value of the expected value, or mean, of all punts from that
# distance, while the prediction is for a single punt.

# Task 4:
# - Define punt yards over expected so that positive values are better punts
# - Compute PYOE for each punt
# - For each punter, compute average PYOE, number of punts, and standard error of average PYOE
# - Rank punters by average PYOE
# - Visualize punter rankings with uncertainty intervals
# - Identify which punters look clearly above average and which rankings are unstable

punts <- punts %>%
  mutate(
    expected_next_ydl = predict(m_quad, newdata = punts),
    pyoe              = expected_next_ydl - next_ydl
  )

# ── Punter summary ────────────────────────────────────────────────────────────
punter_summary <- punts %>%
  group_by(punter) %>%
  summarise(
    n       = n(),
    avg_pyoe = mean(pyoe),
    se      = sd(pyoe) / sqrt(n)
  ) %>%
  mutate(
    lwr = avg_pyoe - 1.96 * se,
    upr = avg_pyoe + 1.96 * se
  ) %>%
  arrange(desc(avg_pyoe)) %>%
  mutate(punter = fct_reorder(punter, avg_pyoe))

print(punter_summary)

# ── Visualise rankings ────────────────────────────────────────────────────────
ggplot(punter_summary, aes(x = avg_pyoe, y = punter)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.3, color = "grey60") +
  geom_point(aes(size = n, color = avg_pyoe > 0)) +
  scale_color_manual(values = c("TRUE" = "#2c7bb6", "FALSE" = "#d7191c"), guide = "none") +
  scale_size_continuous(range = c(2, 6), name = "# punts") +
  labs(
    title = "Punter Rankings by Punt Yards Over Expected (PYOE)",
    x     = "Average PYOE (positive = better than expected)",
    y     = NULL
  ) +
  theme_minimal()

# Butler, Mann, and Long are clearly above average
# Fields and Redfern are unstable due to punt-to-punt variability, while Catanzaro and Lanning are
# unstable due to sample size

# Final reflection:
# - Explain how adding columns changed what the model could fit

# Adding columns allowed us to make the model curve to fit the data better

# - Explain when flexibility helped and when it could hurt

# Flexibility helped fit the model better, but it risks building the model specifically to fit the data

# - Interpret the residual standard error in this setting

summary(m_quad)$sigma

# The RSE is the average difference between the value predicted by the model and the actual value

# - Explain why prediction intervals are wider than confidence intervals

# The prediction interval deals with one specific instance, while the confidence interval is the 95%
# confidence interval for the expected value

# - Note one coefficient, prediction, or ranking you would interpret cautiously

# The punter quality might be skewed due to the large number of punters who've only punted a handful of times

