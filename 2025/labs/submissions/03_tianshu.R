#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set working directory to wherever your data is
setwd("/Users/tianshufeng/Documents/GitHub/summer-lab/2025/labs/data")

##############
### PART 1 ###
##############

# load data
nba_factors = read_csv("03_nba-four-factors.csv.gz")

nba_factors = nba_factors %>% 
  mutate(x1 = `EFG%` / `OPP EFG%`) %>%
  mutate(x2 = `OREB%` / `DREB%`) %>%
  mutate(x3 = `TOV%` - `OPP TOV %`) %>%
  mutate(x4 = `FT Rate` - `OPP FT Rate`)

nba_factors %>%
  select(c(x1, x2, x3, x4)) %>%
  summary()

nba_factors_long = pivot_longer(nba_factors, cols = c(x1, x2, x3, x4),
                         names_to = "variable", values_to = "value")

nba_factors_long %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(bins = 30, alpha = 0.6, color = "white") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Marginal Distributions Histogram")

cor(nba_factors %>% select(c(x1, x2, x3, x4)))

model = lm(W ~ x1 + x2 + x3 + x4, data = nba_factors)
summary(model)

nba_factors$x1_std = as.numeric(scale(nba_factors$x1))
nba_factors$x2_std = as.numeric(scale(nba_factors$x2))
nba_factors$x3_std = as.numeric(scale(nba_factors$x3))
nba_factors$x4_std = as.numeric(scale(nba_factors$x4))

model_std = lm(W ~ x1_std + x2_std + x3_std + x4_std, data = nba_factors)
summary(model_std)

# The standardized model tells about the relative importance about winning
sort(abs(model_std$coefficients[2:5]), decreasing = TRUE)

set.seed(42)
n <- nrow(nba_factors)
train_indices <- sample(seq_len(n), size = 0.8 * n)
nba_factors_train = nba_factors[train_indices, ]
nba_factors_test = nba_factors[-train_indices, ]

model_sample = lm(W ~ x1 + x2 + x3 + x4, data = nba_factors_train)
model_sample_std = lm(W ~ x1_std + x2_std + x3_std + x4_std, data = nba_factors_train)

nba_factors_test$W_pred = predict(model_sample, newdata=nba_factors_test)
nba_factors_test$W_pred_std = predict(model_sample_std, newdata=nba_factors_test)

# Two models generate the same results as the models are linear
nba_factors_test %>% ggplot(aes(x = W)) +
  geom_point(aes(y = W_pred, color = "Original"), alpha = 0.6) +
  geom_point(aes(y = W_pred_std, color = "Standardized"), alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Predicted vs Actual W",
       x = "Actual W",
       y = "Predicted W",
       color = "Model")

##############
### PART 2 ###
##############

# load data
punts = read_csv("03_punts.csv.gz")

punts %>% ggplot(aes(x = ydl, y = next_ydl)) +
  geom_point() + labs(title = "ydl vs next_ydl")

n <- nrow(punts)
train_indices <- sample(seq_len(n), size = 0.8 * n)
punts_train = punts[train_indices, ]
punts_test = punts[-train_indices, ]

model_punts1 = lm(next_ydl ~ ydl + I(ydl^2) + pq, data = punts_train)
punts_test$pred_ydl1 = predict(model_punts1, newdata = punts_test)

model_punts2 = lm(next_ydl ~ splines::bs(ydl, degree = 3, df = 5) + pq, data = punts_train)
punts_test$pred_ydl2 = predict(model_punts2, newdata = punts_test)

mse1 = mean((punts_test$next_ydl - punts_test$pred_ydl1)^2)
mse2 = mean((punts_test$next_ydl - punts_test$pred_ydl2)^2)
print(c(mse1, mse2))

model_punts = lm(next_ydl ~ splines::bs(ydl, degree = 3, df = 5) + pq, data = punts)
summary(model_punts)

punts$pred_ydl = predict(model_punts, newdata = punts)
punts %>% filter(pq > 0.5) %>% 
  ggplot(aes(x = ydl)) +
  geom_point(aes(y = pred_ydl)) +
  labs(title = "Predicted vs Actual ydl (pq > 0.5)",
       x = "ydl",
       y = "Predicted ydl")

punts %>% filter(pq <= 0.5 & pq >= 0) %>% 
  ggplot(aes(x = ydl)) +
  geom_point(aes(y = pred_ydl)) +
  labs(title = "Predicted vs Actual ydl (0 <= pq <= 0.5)",
       x = "ydl",
       y = "Predicted ydl")

punts %>% filter(pq < 0) %>% 
  ggplot(aes(x = ydl)) +
  geom_point(aes(y = pred_ydl)) +
  labs(title = "Predicted vs Actual ydl (pq < 0)",
       x = "ydl",
       y = "Predicted ydl")

punts %>% mutate(pyoe = next_ydl - pred_ydl) %>% 
  group_by(punter) %>% 
  summarise(pyoe = mean(pyoe, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(punter, pyoe), y = pyoe)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = "Punt yards over expected (PYOE) ranking",
       x = "Punter",
       y = "PYOE") +
  theme_minimal()