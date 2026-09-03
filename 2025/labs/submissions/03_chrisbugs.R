#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set working directory to wherever your data is
# setwd("filepath")

##############
### PART 1 ###
##############

# load data
mlb_team_seasons = read_csv("../data/03_nba-four-factors.csv.gz")

nba_four_factors = mlb_team_seasons

nba_four_factors

colnames(nba_four_factors)

# Create 4 variables
variables = nba_four_factors %>%
  mutate(
    efg_diff = `EFG%` - `OPP EFG%`,
    reb_diff = `OREB%` - `DREB%`,
    tov_diff = `TOV%` - `OPP TOV %`,
    ft_diff = `FT Rate` - `OPP FT Rate`
  )

# Summary of mean, standard deviation, max and min for each variable
summarized = variables %>% 
  summarise(
    efg_diff_mean = mean(efg_diff, na.rm = TRUE),
    efg_diff_sd = sd(efg_diff, na.rm = TRUE),
    efg_diff_max = max(efg_diff, na.rm = TRUE),
    efg_diff_min = min(efg_diff, na.rm = TRUE),
    
    reb_diff_mean = mean(reb_diff, na.rm = TRUE),
    reb_diff_sd = sd(reb_diff, na.rm = TRUE),
    reb_diff_max = max(reb_diff, na.rm = TRUE),
    reb_diff_min = min(reb_diff, na.rm = TRUE),
    
    tov_diff_mean = mean(tov_diff, na.rm = TRUE),
    tov_diff_sd = sd(tov_diff, na.rm = TRUE),
    tov_diff_max = max(tov_diff, na.rm = TRUE),
    tov_diff_min = min(tov_diff, na.rm = TRUE),
    
    ft_diff_mean = mean(ft_diff, na.rm = TRUE),
    ft_diff_sd = sd(ft_diff, na.rm = TRUE),
    ft_diff_max = max(ft_diff, na.rm = TRUE),
    ft_diff_min = min(ft_diff, na.rm = TRUE)
  )

# Graphs of each variable
ggplot(variables, aes(x = efg_diff)) +
  geom_histogram(bins = 30, colour = "white", fill = "steelblue") +
  labs(title = "EFG% Differential", x = "efg_diff", y = "Count") +
  theme_minimal()

ggplot(variables, aes(x = reb_diff)) +
  geom_histogram(bins = 30, colour = "white", fill = "steelblue") +
  labs(title = "Rebound% Differential", x = "reb_diff", y = "Count") +
  theme_minimal()

ggplot(variables, aes(x = tov_diff)) +
  geom_histogram(bins = 30, colour = "white", fill = "steelblue") +
  labs(title = "Turnover% Differential", x = "tov_diff", y = "Count") +
  theme_minimal()

ggplot(variables, aes(x = ft_diff)) +
  geom_histogram(bins = 30, colour = "white", fill = "steelblue") +
  labs(title = "FT-Rate Differential", x = "ft_diff", y = "Count") +
  theme_minimal()

# Correlation between each variable
correlation_matrix = variables %>%
  select(efg_diff, reb_diff, tov_diff, ft_diff) %>%
  cor(use = "complete.obs")

print(correlation_matrix) 


install.packages("corrplot")  
library(corrplot)
corrplot::corrplot(correlation_matrix, tl.cex = 0.8)

# Fit the model to predict wins (W) by our variables
model = lm(W ~ efg_diff + reb_diff + tov_diff + ft_diff, data = variables)

# Standardize each variable
standardized_variables <- variables %>%
  mutate(
    efg_diff_std = (efg_diff - mean(efg_diff, na.rm = TRUE)) / sd(efg_diff, na.rm = TRUE),
    reb_diff_std = (reb_diff - mean(reb_diff, na.rm = TRUE)) / sd(reb_diff, na.rm = TRUE),
    tov_diff_std = (tov_diff - mean(tov_diff, na.rm = TRUE)) / sd(tov_diff, na.rm = TRUE),
    ft_diff_std = (ft_diff - mean(ft_diff, na.rm = TRUE)) / sd(ft_diff, na.rm = TRUE)
  )

# Fit the model to predict wins (W) by standardized variables
model_std = lm(W ~ efg_diff_std + reb_diff_std + tov_diff_std + ft_diff_std, data = standardized_variables)

### The standardized model tells more about the relative importance of each 
### variable in predicting wins, as it accounts for the scale of each variable.

# Extract coefficients of standardized model ordered by importance
coefficients_std_v2 = abs(model_std$coefficients)
coefficients_std_v2 = coefficients_std_v2[-1]  # Removes intercept
coefficients_std_v2_ordered = coefficients_std_v2[order(coefficients_std_v2, decreasing = TRUE)]
coefficients_std_v2_ordered

# Bar plot of importance in ggplot
importance_df = data.frame(
  Variable = names(coefficients_std_v2_ordered),
  Coefficient = coefficients_std_v2_ordered
)
ggplot(importance_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Importance of Variables in Predicting Wins", x = "Variable", y = "Coefficient") +
  theme_minimal()

# Perform out of sample test to compare predictive performance of the two models
set.seed(20250604)  

n          <- nrow(variables)              
train_idx  <- sample(seq_len(n),            
                     size = floor(0.80*n))

train_data <- variables[train_idx, ] 
test_data  <- variables[-train_idx, ] 

# Checking if worked
prop.table(table(c("train","test")[1 + !(seq_len(n) %in% train_idx)]))

train_std_data <- standardized_variables[train_idx, ]
test_std_data  <- standardized_variables[-train_idx, ]


# Fit the models on training data
model_normal <- lm(W ~ efg_diff + reb_diff + tov_diff + ft_diff, data = train_data)
model_std <- lm(W ~ efg_diff_std + reb_diff_std + tov_diff_std + ft_diff_std, data = train_std_data)

# Predict on test data
predictions_normal <- predict(model_normal, newdata = test_data)
predictions_std <- predict(model_std, newdata = test_std_data)

# Compare the models' performance
mse_normal <- mean((test_data$W - predictions_normal)^2)
mse_std <- mean((test_std_data$W - predictions_std)^2)

mse_normal
mse_std

# Plot to show the difference
comparison_df <- data.frame(
  Model = c("Normal", "Standardized"),
  MSE = c(mse_normal, mse_std)
)
ggplot(comparison_df, aes(x = Model, y = MSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Squared Error of Models", x = "Model", y = "MSE") +
  theme_minimal()


##############
### PART 2 ###
##############

# load data
mlb_payrolls = read_csv("../data/03_punts.csv.gz")

punts = mlb_payrolls

punts

# Plot the data as a scatterplot of next_ydl vs. ydl
ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point(aes(color = pq)) +
  labs(title = "Next Yards Gained vs. Yards to Go",
       x = "Yards to Go (ydl)",
       y = "Next Yards Gained (next_ydl)",
       color = "Play Quality (pq)") +
  theme_minimal()

# Model next_ydl as a function of ydl and pq
model_punts_linear = lm(next_ydl ~ ydl + pq, data = punts)

# MSE for linear model
mse_linear = mean((punts$next_ydl - predict(model_punts_linear))^2)
mse_linear

# Graph the model
ggplot(punts, aes(x = ydl, y = next_ydl, color = pq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Next Yards Gained vs. Yards to Go and Play Quality (Linear)",
       x = "Yards to Go (ydl)",
       y = "Next Yards Gained (next_ydl)",
       color = "Play Quality (pq)") +
  theme_minimal()

# Model next_ydl as a function of ydl and pq as a quadratic
model_punts_quadratic = lm(next_ydl ~ ydl + I(ydl^2) + pq, data = punts)

# MSE for quadratic
mse_quadratic = mean((punts$next_ydl - predict(model_punts_quadratic))^2)
mse_quadratic

# Graph the quadratic model
ggplot(punts, aes(x = ydl, y = next_ydl, color = pq)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(title = "Next Yards Gained vs. Yards to Go and Play Quality (Quadratic)",
       x = "Yards to Go (ydl)",
       y = "Next Yards Gained (next_ydl)",
       color = "Play Quality (pq)") +
  theme_minimal()

# Model next_ydl as a function of ydl and pq as a cubic spline
library(splines)
model_punts_cubic_spline = lm(next_ydl ~ splines::bs(ydl, df = 3) + pq, data = punts)

# Graph the cubic spline model
ggplot(punts, aes(x = ydl, y = next_ydl, color = pq)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3), se = FALSE) +
  labs(title = "Next Yards Gained vs. Yards to Go and Play Quality (Cubic Spline)",
       x = "Yards to Go (ydl)",
       y = "Next Yards Gained (next_ydl)",
       color = "Play Quality (pq)") +
  theme_minimal()

# MSE for cubic spline
mse_cubic_spline = mean((punts$next_ydl - predict(model_punts_cubic_spline))^2)
mse_cubic_spline

# Out of sample test for cubic spline
set.seed(20250604)

n          <- nrow(punts)
train_idx  <- sample(seq_len(n),            
                     size = floor(0.80*n))
train_data <- punts[train_idx, ]
test_data  <- punts[-train_idx, ]

# Fit the cubic spline model on training data
model_punts_cubic_spline_train = lm(next_ydl ~ splines::bs(ydl, df = 3) + pq, data = train_data)

# Predict on test data
predictions_cubic_spline <- predict(model_punts_cubic_spline_train, newdata = test_data)

# MSE for cubic spline on test data
mse_cubic_spline_test = mean((test_data$next_ydl - predictions_cubic_spline)^2)
mse_cubic_spline_test

# Plot the predicted yardline vs current yard line
ggplot(test_data, aes(x = ydl, y = next_ydl)) +
  geom_line(aes(y = predictions_cubic_spline), color = "red", size = 1) +
  labs(title = "Predicted Next Yards Gained vs. Yards to Go (Cubic Spline)",
       x = "Yards to Go (ydl)",
       y = "Next Yards Gained (next_ydl)",
       color = "Play Quality (pq)") +
  theme_minimal()

# Rank punters by PYOE (punter yards over expected) - actual next_ydl over mean of predicted next_ydl
punts_ranked <- test_data %>%
  mutate(
    next_ydl_pred = predictions_cubic_spline,
    PYOE = next_ydl - next_ydl_pred
  ) %>%
  arrange(desc(PYOE))

####################################
### Apologies for the terrible graph
####################################

punter_ranks <- test_data %>% 
  mutate(PYOE = next_ydl - predictions_cubic_spline) %>% 
  group_by(punter) %>% 
  summarise(mean_PYOE = mean(PYOE), .groups = "drop") %>% 
  arrange(desc(mean_PYOE)) %>% 
  mutate(rank = row_number())          # 1 = best, 2 = next, …

# --- 2.  Bar chart showing the rank itself (1, 2, 3, …) ---
max_rank <- nrow(punter_ranks)

ggplot(punter_ranks,
       aes(x = reorder(punter, rank),
           y = max_rank - rank + 1)) +  # invert so Rank 1 has tallest bar
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(breaks = 1:max_rank,
                     labels = max_rank:1,
                     name   = "Rank (1 = Best)") +
  labs(title = "Punter Rankings by Mean PYOE",
       x     = "Punter") +
  theme_minimal(base_size = 12)

