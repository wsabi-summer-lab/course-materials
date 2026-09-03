
library(splines)
library(ggplot2)
library(caret)
library(broom)

set.seed(123)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

mlb_team_seasons = read_csv("03_nba-four-factors.csv.gz")

head(mlb_team_seasons)

# 3.1 Four Factors Basketball

# Task 1

mlb_team_seasons$x_1 = mlb_team_seasons$`EFG%` - mlb_team_seasons$`OPP EFG%`

mlb_team_seasons$x_2 = mlb_team_seasons$`OREB%` - mlb_team_seasons$`DREB%`

mlb_team_seasons$x_3 = mlb_team_seasons$`TOV%` - mlb_team_seasons$`OPP TOV %`

mlb_team_seasons$x_4 = mlb_team_seasons$`FT Rate` - mlb_team_seasons$`OPP FT Rate`

summary_table <- data.frame(
  Variable = c("x_1", "x_2", "x_3", "x_4"),
  Mean = sapply(mlb_team_seasons[, c("x_1", "x_2", "x_3", "x_4")], mean, na.rm = TRUE),
  Std = sapply(mlb_team_seasons[, c("x_1", "x_2", "x_3", "x_4")], sd, na.rm = TRUE),
  Min = sapply(mlb_team_seasons[, c("x_1", "x_2", "x_3", "x_4")], min, na.rm = TRUE),
  Max = sapply(mlb_team_seasons[, c("x_1", "x_2", "x_3", "x_4")], max, na.rm = TRUE))

summary_table$Mean <- format(summary_table$Mean, scientific = FALSE, digits = 3)

summary_table

# Density plots

ggplot(mlb_team_seasons, aes(x = x_1)) +
  geom_density(fill = "tomato", alpha = 0.6) +
  labs(title = "Density of x_1", x = "x_1", y = "Density") +
  theme_minimal()

ggplot(mlb_team_seasons, aes(x = x_2)) +
  geom_density(fill = "tomato", alpha = 0.6) +
  labs(title = "Density of x_1", x = "x_1", y = "Density") +
  theme_minimal()

ggplot(mlb_team_seasons, aes(x = x_3)) +
  geom_density(fill = "tomato", alpha = 0.6) +
  labs(title = "Density of x_1", x = "x_1", y = "Density") +
  theme_minimal()

ggplot(mlb_team_seasons, aes(x = x_4)) +
  geom_density(fill = "tomato", alpha = 0.6) +
  labs(title = "Density of x_1", x = "x_1", y = "Density") +
  theme_minimal()

# Correlation Table

cor(mlb_team_seasons[, c("x_1", "x_2", "x_3", "x_4")], use = "complete.obs")

# Task 2

train_index <- createDataPartition(mlb_team_seasons$W, p = 0.8, list = FALSE)
train_data <- mlb_team_seasons[train_index, ]
test_data <- mlb_team_seasons[-train_index, ]

model <- lm(W ~ x_1 + x_2 + x_3 + x_4, data = train_data)
summary(model)

train_data$x_1_stand <- as.vector(scale(train_data$x_1))
train_data$x_2_stand <- as.vector(scale(train_data$x_2))
train_data$x_3_stand <- as.vector(scale(train_data$x_3))
train_data$x_4_stand <- as.vector(scale(train_data$x_4))

model_stand <- lm(W ~ x_1_stand + x_2_stand + x_3_stand + x_4_stand, data = train_data)
summary(model_stand)

# The stanardized version tells you the relative value of the four factors
# This is because they can now be compared since they've been standardized

coef_df <- tidy(model_stand) %>%
  filter(term != "(Intercept)") %>%
  mutate(AbsCoefficient = abs(estimate))

ggplot(coef_df, aes(x = reorder(term, AbsCoefficient), y = AbsCoefficient, fill = term)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Relative Size of Standardized Coefficients",
    x = "Variable",
    y = "Absolute Coefficient"
  ) +
  theme_minimal()

# Ranking of Importance (x_1, x_3, x_4, x_2)

# Comparing out of Sample Performance

test_data$x_1 <- test_data$`EFG%` - test_data$`OPP EFG%`
test_data$x_2 <- test_data$`OREB%` - test_data$`DREB%`
test_data$x_3 <- test_data$`TOV%` - test_data$`OPP TOV %`
test_data$x_4 <- test_data$`FT Rate` - test_data$`OPP FT Rate`

test_data$x_1_stand <- as.vector(scale(test_data$x_1))
test_data$x_2_stand <- as.vector(scale(test_data$x_2))
test_data$x_3_stand <- as.vector(scale(test_data$x_3))
test_data$x_4_stand <- as.vector(scale(test_data$x_4))

pred_model <- predict(model, newdata = test_data)
rmse <- sqrt(mean((test_data$W - pred_model)^2))

pred_model_stand <- predict(model_stand, newdata = test_data)
rmse_stand <- sqrt(mean((test_data$W - pred_model_stand)^2))

rmse
rmse_stand

# 3.2 Expected Outcome of a Punt

# Task 1

mlb_punts <- read_csv("03_punts.csv.gz")

train_index <- createDataPartition(mlb_punts$ydl, p = 0.8, list = FALSE)
train_data <- mlb_punts[train_index, ]
test_data <- mlb_punts[-train_index, ]

head(train_data)

ggplot(train_data, aes(x = ydl, y = next_ydl)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "next_ydl vs. YDL (Training Set)",
    x = "Next YDL",
    y = "YDL"
  ) +
  theme_minimal()

ggplot(train_data, aes(x = pq, y = next_ydl)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "next_ydl vs. Punter Quality (Training Set)",
    x = "Punter Quality",
    y = "YDL"
  ) +
  theme_minimal()

model_next_ydl <- lm(next_ydl ~ ydl + pq, data = train_data)
summary(model_next_ydl)

model_next_ydl_spline <- lm(next_ydl ~ ns(ydl, df = 3) + pq, data = train_data)
summary(model_next_ydl_spline)

ydl_grid <- seq(min(train_data$ydl, na.rm = TRUE),
                max(train_data$ydl, na.rm = TRUE),
                length.out = 100)

pq_mean <- mean(train_data$pq, na.rm = TRUE)

pred_data <- data.frame(
  ydl = ydl_grid,
  pq = pq_mean
)

pred_data$next_ydl_pred <- predict(model_next_ydl_spline, newdata = pred_data)

ggplot() +
  geom_point(data = train_data, aes(x = ydl, y = next_ydl), alpha = 0.3) +
  geom_line(data = pred_data, aes(x = ydl, y = next_ydl_pred), color = "blue", linewidth = 1.2) +
  labs(
    title = "Expected Next Yard Line vs. Current Yard Line",
    x = "Current Yard Line (YDL)",
    y = "Expected Next Yard Line"
  ) +
  theme_minimal()

# Visualizing expected next yard line vs current yard line for various punter quality


ydl_grid <- seq(min(train_data$ydl, na.rm = TRUE),
                max(train_data$ydl, na.rm = TRUE), length.out = 100)

pq_levels <- quantile(train_data$pq, probs = c(0.2, 0.5, 0.8), na.rm = TRUE)

pred_grid <- expand.grid(
  ydl = ydl_grid,
  pq = pq_levels
)

pred_grid$pred_next_ydl <- predict(model_next_ydl_spline, newdata = pred_grid)

ggplot(pred_grid, aes(x = ydl, y = pred_next_ydl, color = factor(pq))) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Expected Next Yard Line vs. Current Yard Line",
    x = "Current Yard Line (YDL)",
    y = "Expected Next Yard Line",
    color = "Punter Quality"
  ) +
  theme_minimal()

# Task 2

train_data$expected_next_ydl <- predict(model_next_ydl_spline, newdata = train_data)

train_data$PYOE <- train_data$next_ydl - train_data$expected_next_ydl


punter_pyoe <- train_data %>%
  group_by(punter) %>%
  summarise(
    mean_PYOE = mean(PYOE, na.rm = TRUE),
    total_punts = n()
  ) %>%
  arrange(desc(mean_PYOE))

punter_pyoe$punter <- fct_reorder(punter_pyoe$punter, punter_pyoe$mean_PYOE)

ggplot(punter_pyoe, aes(x = mean_PYOE, y = punter)) +
  geom_point(color = "darkred", size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Punter Rankings by Mean Punt Yards Over Expected (PYOE)",
    x = "Mean PYOE",
    y = "Punter"
  ) +
  theme_minimal(base_size = 13)


