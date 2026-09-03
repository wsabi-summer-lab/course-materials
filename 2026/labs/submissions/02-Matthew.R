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
nba_four_factors=nba_four_factors %>% 
  filter(GP==82) %>% 
  mutate(`Protecting Factor`=-`Protecting Factor`)
# Task 1:
# - Compute each variable's mean, standard deviation, minimum, and maximum
# - Plot the marginal distribution of each explanatory variable
# - Make scatterplots of wins against each of the four factors
# - Compute correlations between each pair of explanatory variables
# - Identify which variables look most strongly related to wins before fitting a model

summary(nba_four_factors)
nba_four_factors_long <- nba_four_factors %>%
  pivot_longer(
    cols = c(`Shooting Factor`,
             `Crashing Factor`,
             `Protecting Factor`,
             `Attacking Factor`),
    names_to = "Factor",
    values_to = "Value"
  )

ggplot(nba_four_factors_long,
       aes(x = Value, y = W, color = Factor)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Factor, scales = "free_x") +
  labs(
    x = "Factor Value",
    y = "Wins",
    color = "Four Factor"
  ) +
  theme_minimal()
cor(
  nba_four_factors[, c("Shooting Factor",
                       "Crashing Factor",
                       "Protecting Factor",
                       "Attacking Factor")],
  nba_four_factors$W
)
# Task 2:
# - Fit the multivariable model: wins ~ x1 + x2 + x3 + x4
# - Write down the fitted regression equation
# - Interpret each coefficient in context
# - Check whether the coefficient signs make sense given the variable definitions
# - Identify which factors look strongest and weakest after adjustment

four_factor_model=lm(data=nba_four_factors, W~`Shooting Factor`+`Crashing Factor`+`Protecting Factor`+`Attacking Factor`)
four_factor_model
# Task 3:
# - Standardize the four predictors
# - Fit the standardized model
# - Rank the factors by absolute standardized coefficient size
# - Compare the original and standardized models for interpretability
# - Compare fitted values from both models and explain why they match or differ
standardized_nba_four_factors <- nba_four_factors %>%
  mutate(
    standardizedSF = as.numeric(scale(`Shooting Factor`)),
    standardizedCF = as.numeric(scale(`Crashing Factor`)),
    standardizedPF = as.numeric(scale(`Protecting Factor`)),
    standardizedAF = as.numeric(scale(`Attacking Factor`))
  )  

lm(data=standardized_nba_four_factors, W~standardizedSF+standardizedCF+standardizedPF+standardizedAF)
# Task 4:
# - Report the residual standard error and interpret it in wins
# - Report coefficient standard errors and 95% confidence intervals
# - Identify which effects are clearly different from zero
# - Choose one team and compute a point prediction, confidence interval, and prediction interval
# - State which interval is wider and why
summary(four_factor_model)
nba_four_factors <- nba_four_factors %>%
  mutate(
    fitted = fitted(four_factor_model),
    residual = resid(four_factor_model)
  )

# 2007 Dallas Mavericks
mavericks_2007 <- nba_four_factors %>%
  filter(Team_Season == "Dallas Mavericks2007")

# Residual standard error
sigma(four_factor_model)

# Coefficient estimates, standard errors, t-statistics, and p-values
coef(summary(four_factor_model))

# 95% confidence intervals for coefficients
confint(four_factor_model)

# Point prediction, confidence interval, and prediction interval
predict(
  four_factor_model,
  newdata = mavericks_2007,
  interval = "confidence",
  level = 0.95
)

predict(
  four_factor_model,
  newdata = mavericks_2007,
  interval = "prediction",
  level = 0.95
)
# Task 5:
# - Randomly split the data into training and test sets
# - Fit the original and standardized models on the training set
# - Compute test-set RMSE for both models
# - Compare predictive performance
train_idx <- sample(
  seq_len(nrow(nba_four_factors)),
  size = 0.8 * nrow(nba_four_factors)
)

train <- nba_four_factors[train_idx, ]
test  <- nba_four_factors[-train_idx, ]

# Original model
original_model <- lm(
  W ~ `Shooting Factor` +
    `Crashing Factor` +
    `Protecting Factor` +
    `Attacking Factor`,
  data = train
)

# Standardize using TRAINING means and sds
sf_mean <- mean(train$`Shooting Factor`)
sf_sd   <- sd(train$`Shooting Factor`)

cf_mean <- mean(train$`Crashing Factor`)
cf_sd   <- sd(train$`Crashing Factor`)

pf_mean <- mean(train$`Protecting Factor`)
pf_sd   <- sd(train$`Protecting Factor`)

af_mean <- mean(train$`Attacking Factor`)
af_sd   <- sd(train$`Attacking Factor`)

train_std <- train %>%
  mutate(
    SF = (`Shooting Factor` - sf_mean) / sf_sd,
    CF = (`Crashing Factor` - cf_mean) / cf_sd,
    PF = (`Protecting Factor` - pf_mean) / pf_sd,
    AF = (`Attacking Factor` - af_mean) / af_sd
  )

test_std <- test %>%
  mutate(
    SF = (`Shooting Factor` - sf_mean) / sf_sd,
    CF = (`Crashing Factor` - cf_mean) / cf_sd,
    PF = (`Protecting Factor` - pf_mean) / pf_sd,
    AF = (`Attacking Factor` - af_mean) / af_sd
  )

# Standardized model
standardized_model <- lm(
  W ~ SF + CF + PF + AF,
  data = train_std
)

# Predictions
pred_original <- predict(original_model, newdata = test)

pred_standardized <- predict(
  standardized_model,
  newdata = test_std
)

# RMSE function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Test-set RMSEs
rmse_original <- rmse(test$W, pred_original)
rmse_standardized <- rmse(test$W, pred_standardized)

cat("Original Model RMSE:", rmse_original, "\n")
cat("Standardized Model RMSE:", rmse_standardized, "\n")
##############
### PART 2 ###
##############

# load data
punts = read_csv("../data/02_punts.csv.gz")
summary(punts)
colnames(punts)
# Task 1:
# - Plot post-punt yard line against starting yard line
# - Bin punts by starting field position and plot average post-punt yard line in each bin
# - Describe the shape of the relationship and where it bends
# - Plot or summarize the distribution of punter quality
# 1. Scatterplot: post-punt yard line vs starting yard line
ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(alpha = 0.2) +
  geom_smooth(color="hotpink", method="loess", span=.05) +
  labs(
    x = "Starting Yard Line",
    y = "Post-Punt Yard Line",
    title = "Post-Punt Yard Line vs Starting Yard Line"
  )

# 2. Bin starting field position and compute average post-punt yard line
punt_bins <- punts %>%
  mutate(
    ydl_bin = cut_width(ydl, width = 5)
  ) %>%
  group_by(ydl_bin) %>%
  summarize(
    avg_ydl = mean(ydl, na.rm = TRUE),
    avg_next_ydl = mean(next_ydl, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(punt_bins, aes(x = avg_ydl, y = avg_next_ydl)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Average Starting Yard Line",
    y = "Average Post-Punt Yard Line",
    title = "Average Post-Punt Position by Starting Field Position Bin"
  )+
  geom_smooth(color="hotpink")

# 3. Numerical summaries to help identify where the relationship bends
punt_bins

# 4. Distribution of punter quality
summary(punts$pq)

ggplot(punts, aes(x = pq)) +
  geom_histogram(bins = 300) +
  labs(
    x = "Punter Quality (pq)",
    y = "Count",
    title = "Distribution of Punter Quality"
  )

# Optional boxplot
ggplot(punts, aes(y = pq)) +
  geom_boxplot() +
  labs(
    y = "Punter Quality (pq)",
    title = "Boxplot of Punter Quality"
  )
# Task 2:
# - Fit competing punt models: linear, quadratic, quadratic plus punter quality, and spline
# - Visualize the fitted curves from each model
# - Use train/test RMSE or cross-validation to choose a preferred model
# - Compare the linear, quadratic, and spline tradeoffs
# - Assess whether punter quality improves out-of-sample prediction
# - Interpret the punter-quality coefficient if it is included in the selected model
# Train/test split
train_idx <- sample(seq_len(nrow(punts)), size = 0.8 * nrow(punts))

train <- punts[train_idx, ]
test  <- punts[-train_idx, ]

# Models
linear_mod <- lm(next_ydl ~ ydl, data = train)

quad_mod <- lm(next_ydl ~ ydl + I(ydl^2), data = train)

quad_pq_mod <- lm(next_ydl ~ ydl + I(ydl^2) + pq, data = train)

# Prediction grid for plotting
grid <- tibble(
  ydl = seq(min(punts$ydl), max(punts$ydl), length.out = 500)
)

grid <- grid %>%
  mutate(
    linear = predict(linear_mod, newdata = grid),
    quadratic = predict(quad_mod, newdata = grid)
  )

# Fitted curves
ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(alpha = 0.05) +
  geom_line(data = grid,
            aes(y = linear, color = "Linear"),
            linewidth = 1) +
  geom_line(data = grid,
            aes(y = quadratic, color = "Quadratic"),
            linewidth = 1) +
  labs(
    x = "Starting Yard Line",
    y = "Post-Punt Yard Line",
    color = "Model",
    title = "Competing Punt Models"
  ) +
  theme_minimal()

# RMSE function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Test RMSEs
rmse_results <- tibble(
  Model = c(
    "Linear",
    "Quadratic",
    "Quadratic + PQ"
  ),
  RMSE = c(
    rmse(test$next_ydl,
         predict(linear_mod, test)),
    
    rmse(test$next_ydl,
         predict(quad_mod, test)),
    
    rmse(test$next_ydl,
         predict(quad_pq_mod, test))
    
  )
)
rmse_results

# Task 3:
# - Plot the fitted mean response for the selected punt model
# - Add a 95% confidence band for the expected response
# - Add a 95% prediction band for one individual punt
# - Explain why the prediction band is wider
# - Identify where the model is most uncertain

# Prediction grid
grid <- tibble(
  ydl = seq(min(punts$ydl), max(punts$ydl), length.out = 500)
)

# Confidence and prediction intervals from quadratic model
conf_band <- predict(
  quad_mod,
  newdata = grid,
  interval = "confidence"
)

pred_band <- predict(
  quad_mod,
  newdata = grid,
  interval = "prediction"
)

grid <- grid %>%
  mutate(
    fit = conf_band[, "fit"],
    conf_lwr = conf_band[, "lwr"],
    conf_upr = conf_band[, "upr"],
    pred_lwr = pred_band[, "lwr"],
    pred_upr = pred_band[, "upr"]
  )

# Plot
ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(alpha = 0.05) +
  
  # Prediction band (wider)
  geom_ribbon(
    data = grid,
    aes(x = ydl, ymin = pred_lwr, ymax = pred_upr),
    inherit.aes = FALSE,
    alpha = 0.15
  ) +
  
  # Confidence band (narrower)
  geom_ribbon(
    data = grid,
    aes(x = ydl, ymin = conf_lwr, ymax = conf_upr),
    inherit.aes = FALSE,
    alpha = 0.3
  ) +
  
  # Quadratic fit
  geom_line(
    data = grid,
    aes(x = ydl, y = fit),
    linewidth = 1.2
  ) +
  
  labs(
    x = "Starting Yard Line",
    y = "Post-Punt Yard Line",
    title = "Quadratic Punt Model with 95% Confidence and Prediction Bands"
  ) +
  theme_minimal()

# Task 4:
# - Define punt yards over expected so that positive values are better punts
# - Compute PYOE for each punt
# - For each punter, compute average PYOE, number of punts, and standard error of average PYOE
# - Rank punters by average PYOE
# - Visualize punter rankings with uncertainty intervals
# - Identify which punters look clearly above average and which rankings are unstable
# Compute Punt Yards Over Expected (PYOE)
punts_pyoe <- punts %>%
  mutate(
    expected_next_ydl = predict(quad_mod, newdata = punts),
    pyoe = expected_next_ydl - next_ydl
  )

# Punter summaries
punter_rankings <- punts_pyoe %>%
  group_by(punter) %>%
  summarize(
    avg_pyoe = mean(pyoe),
    n_punts = n(),
    sd_pyoe = sd(pyoe),
    se_pyoe = sd_pyoe / sqrt(n_punts),
    lower95 = avg_pyoe - 1.96 * se_pyoe,
    upper95 = avg_pyoe + 1.96 * se_pyoe,
    .groups = "drop"
  ) %>%
  arrange(desc(avg_pyoe))

# Rankings table
punter_rankings

# Top punters
head(punter_rankings, 10)

# Bottom punters
tail(punter_rankings, 10)

# Ranking plot with uncertainty intervals
punter_rankings %>%
  mutate(
    punter = reorder(punter, avg_pyoe)
  ) %>%
  ggplot(aes(x = avg_pyoe, y = punter)) +
  geom_point() +
  geom_errorbarh(
    aes(xmin = lower95, xmax = upper95),
    height = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Average Punt Yards Over Expected",
    y = "Punter",
    title = "Punter Rankings with 95% Confidence Intervals"
  ) +
  theme_minimal()

# Most stable rankings (lots of punts, small SE)
punter_rankings %>%
  arrange(se_pyoe) %>%
  select(punter, avg_pyoe, n_punts, se_pyoe)

# Least stable rankings
punter_rankings %>%
  arrange(desc(se_pyoe)) %>%
  select(punter, avg_pyoe, n_punts, se_pyoe)
# Final reflection:
# - Explain how adding columns changed what the model could fit
# - Explain when flexibility helped and when it could hurt
# - Interpret the residual standard error in this setting
# - Explain why prediction intervals are wider than confidence intervals
# - Note one coefficient, prediction, or ranking you would interpret cautiously
