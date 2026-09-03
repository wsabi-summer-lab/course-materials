#############
### SETUP ###
#############
getwd()
# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(dplyr)
library(stringr)
####################
### PARK EFFECTS ###
####################

# load data
park_data = read_csv("data/05_park-effects.csv.gz")
glimpse(park_data)
# find means of INN_RUNS grouping by PARK
park_means = park_data %>%
  group_by(PARK) %>%
  summarise(mean_runs = mean(INN_RUNS, na.rm = TRUE)) %>%
  arrange(desc(mean_runs))
park_means
# plot the means
ggplot(park_means, aes(x = reorder(PARK, -mean_runs), y = mean_runs)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Runs by Park", x = "Park", y = "Average Runs") +
  theme_minimal()
#find means of INN_RUNS grouping by OT_YEAR
year_means = park_data %>%
  group_by(OT_YR) %>%
  summarise(mean_runs = mean(INN_RUNS, na.rm = TRUE)) %>%
  arrange(desc(mean_runs))
year_means
# plot the means  
ggplot(year_means, aes(x = OT_YR, y = mean_runs)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Runs by Team", x = "Year", y = "Average Runs") +
  theme_minimal()
unique(park_data$PARK)
#do a .8 training test split
set.seed(123)  # For reproducibility
train_indices = sample(1:nrow(park_data), size = 0.8 * nrow(park_data))
park_train = park_data[train_indices, ]
park_test = park_data[-train_indices, ]

#Create Linear Model to model INN_RUNS by PARK, OT_YR, and DT_YR using one hot encoding
park_train$PARK = as.factor(park_train$PARK)
park_train$OT_YR = as.factor(park_train$OT_YR)
park_train$DT_YR = as.factor(park_train$DT_YR)
park_model = lm(INN_RUNS ~ PARK + OT_YR + DT_YR, data = park_train)
summary(park_model)
#Extract coefficients
coefficients = as.data.frame(coef(park_model))
coefficients 
str(coefficients)

# Step 1: Extract numeric coefficients and names
coeff_vector <- coefficients[[1]]              # Pull out the numeric column
names(coeff_vector) <- rownames(coefficients)  # Add the row names as names

# Step 2: Turn into tidy data frame
coeff_df <- data.frame(
  term = names(coeff_vector),
  estimate = coeff_vector,
  stringsAsFactors = FALSE
)

# Step 3: Split into separate data frames


coeff_park <- coeff_df %>% filter(str_starts(term, "PARK"))
coeff_ot_yr <- coeff_df %>% filter(str_starts(term, "OT_YR"))
coeff_dt_yr <- coeff_df %>% filter(str_starts(term, "DT_YR"))
coeff_park
coeff_ot_yr
coeff_dt_yr
#plot each coefficient in a desceding histogram
ggplot(coeff_park, aes(x = reorder(term, -estimate), y = estimate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Park Coefficients", x = "Park", y = "Coefficient Estimate") +
  theme_minimal()
ggplot(coeff_ot_yr, aes(x = reorder(term, -estimate), y = estimate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "OT Year Coefficients", x = "OT Year", y = "Coefficient Estimate") +
  theme_minimal()  
ggplot(coeff_dt_yr, aes(x = reorder(term, -estimate), y = estimate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "DT Year Coefficients", x = "DT Year", y = "Coefficient Estimate") +
  theme_minimal()
# Predict on test set
park_test$PARK = as.factor(park_test$PARK)
park_test$OT_YR = as.factor(park_test$OT_YR)
park_test$DT_YR = as.factor(park_test$DT_YR)
park_predictions = predict(park_model, newdata = park_test)

#plot coefficients for park
ggplot(coeff_park, aes(x = reorder(term, -estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Park Coefficients", x = "Park", y = "Coefficient Estimate") +
  theme_minimal()

# Combine predictions with actual values
park_results = data.frame(
  Actual = park_test$INN_RUNS,
  Predicted = park_predictions
)
# Calculate RMSE
rmse = sqrt(mean((park_results$Actual - park_results$Predicted)^2))
rmse
#plot INN_RUNS histogram
ggplot(park_data, aes(x = INN_RUNS)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of INN_RUNS", x = "INN_RUNS", y = "Frequency") +
  theme_minimal()


# Poisson model for home runs
poisson_model = glm(INN_RUNS ~ PARK + OT_YR + DT_YR, data = park_train, family = poisson())
summary(poisson_model)

# Step 1: Extract named coefficient vector
poisson_coeff_vector <- coef(poisson_model)

# Step 2: Create tidy data frame with rate ratios
poisson_coeff_df <- data.frame(
  term = names(poisson_coeff_vector),
  estimate = poisson_coeff_vector,
  exp_estimate = exp(poisson_coeff_vector),  # rate ratio (multiplicative effect)
  stringsAsFactors = FALSE
)

# Step 3: Create separate coefficient groups
poisson_coeff_park <- poisson_coeff_df %>% filter(str_starts(term, "PARK"))
poisson_coeff_ot_yr <- poisson_coeff_df %>% filter(str_starts(term, "OT_YR"))
poisson_coeff_dt_yr <- poisson_coeff_df %>% filter(str_starts(term, "DT_YR"))

# Park
ggplot(poisson_coeff_park, aes(x = reorder(term, -exp_estimate), y = exp_estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Poisson Park Effects (Rate Ratios)", x = "Park", y = "Rate Ratio") +
  theme_minimal()

# OT Year
ggplot(poisson_coeff_ot_yr, aes(x = reorder(term, -exp_estimate), y = exp_estimate)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Poisson OT Year Effects", x = "OT Year", y = "Rate Ratio") +
  theme_minimal()

# DT Year
ggplot(poisson_coeff_dt_yr, aes(x = reorder(term, -exp_estimate), y = exp_estimate)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  coord_flip() +
  labs(title = "Poisson DT Year Effects", x = "DT Year", y = "Rate Ratio") +
  theme_minimal()

# Match factor levels
park_test$PARK <- factor(park_test$PARK, levels = levels(park_train$PARK))
park_test$OT_YR <- factor(park_test$OT_YR, levels = levels(park_train$OT_YR))
park_test$DT_YR <- factor(park_test$DT_YR, levels = levels(park_train$DT_YR))

# Predict expected INN_RUNS
poisson_predictions <- predict(poisson_model, newdata = park_test, type = "response")

# Store prediction results
poisson_results <- data.frame(
  Actual = park_test$INN_RUNS,
  Predicted = poisson_predictions
)

# RMSE
poisson_rmse <- sqrt(mean((poisson_results$Actual - poisson_results$Predicted)^2))
poisson_rmse
# Plot actual vs predicted
ggplot(poisson_results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Actual vs Predicted INN_RUNS (Poisson Model)",
       x = "Actual INN_RUNS",
       y = "Predicted INN_RUNS") +
  theme_minimal()

library(MASS)

# Negative Binomial model
nbd_model <- glm.nb(INN_RUNS ~ PARK + OT_YR + DT_YR, data = park_train)
summary(nbd_model)

# Coefficients
nbd_coeff_vector <- coef(nbd_model)
nbd_coeff_df <- data.frame(
  term = names(nbd_coeff_vector),
  estimate = nbd_coeff_vector,
  exp_estimate = exp(nbd_coeff_vector),
  stringsAsFactors = FALSE
)

# Split
nbd_coeff_park   <- nbd_coeff_df %>% filter(str_starts(term, "PARK"))
nbd_coeff_ot_yr  <- nbd_coeff_df %>% filter(str_starts(term, "OT_YR"))
nbd_coeff_dt_yr  <- nbd_coeff_df %>% filter(str_starts(term, "DT_YR"))

#plot Negative Binomial coefficients
ggplot(nbd_coeff_park, aes(x = reorder(term, -exp_estimate), y = exp_estimate)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Negative Binomial Park Effects (Rate Ratios)", x = "Park", y = "Rate Ratio") +
  theme_minimal()
ggplot(nbd_coeff_ot_yr, aes(x = reorder(term, -exp_estimate), y = exp_estimate)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Negative Binomial OT Year Effects", x = "OT Year", y = "Rate Ratio") +
  theme_minimal()
ggplot(nbd_coeff_dt_yr, aes(x = reorder(term, -exp_estimate), y = exp_estimate)) +
  geom_bar(stat = "identity", fill = "cyan") +
  coord_flip() +
  labs(title = "Negative Binomial DT Year Effects", x = "DT Year", y = "Rate Ratio") +
  theme_minimal()
# Predict
nbd_predictions <- predict(nbd_model, newdata = park_test, type = "response")
nbd_results <- data.frame(
  Actual = park_test$INN_RUNS,
  Predicted = nbd_predictions
)
nbd_rmse <- sqrt(mean((nbd_results$Actual - nbd_results$Predicted)^2))
nbd_rmse
mean(park_train$INN_RUNS)
var(park_train$INN_RUNS)

library(dplyr)
library(ggplot2)
library(tidyr)

# Add predictions to park_data
park_data_preds <- park_data %>%
  mutate(
    pred_linear = predict(park_model, newdata = park_data, type = "response"),
    pred_poisson = predict(poisson_model, newdata = park_data, type = "response"),
    pred_nbd = predict(nbd_model, newdata = park_data, type = "response")
  )

# Group by PARK and calculate mean predicted runs per inning for each model
park_preds_summary <- park_data_preds %>%
  group_by(PARK) %>%
  summarise(
    mean_linear = mean(pred_linear, na.rm = TRUE),
    mean_poisson = mean(pred_poisson, na.rm = TRUE),
    mean_nbd = mean(pred_nbd, na.rm = TRUE),
    mean_actual = mean(INN_RUNS, na.rm = TRUE)
  ) %>%
  ungroup()

# Reshape to long format for plotting
park_preds_long <- park_preds_summary %>%
  pivot_longer(cols = starts_with("mean_"),
               names_to = "model",
               values_to = "mean_runs") %>%
  mutate(model = case_when(
    model == "mean_linear" ~ "Linear",
    model == "mean_poisson" ~ "Poisson",
    model == "mean_nbd" ~ "Negative Binomial",
    model == "mean_actual" ~ "Actual"
  ))

# Plot
ggplot(park_preds_long, aes(x = reorder(PARK, mean_runs), y = mean_runs, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Mean Predicted Runs per Inning by Park and Model",
    x = "Park",
    y = "Mean Runs per Inning",
    fill = "Model"
  ) +
  theme_minimal()

# calculate rmse of using the mean of the park
mean_park = mean(park_data$INN_RUNS, na.rm = TRUE)
mean_park_rmse = sqrt(mean((park_data$INN_RUNS - mean_park)^2))
mean_park_rmse
#see which park in the linear model differs most from the actual
park_data_preds <- park_data_preds %>%
  mutate(
    diff_linear = abs(INN_RUNS - pred_linear),
    diff_poisson = abs(INN_RUNS - pred_poisson),
    diff_nbd = abs(INN_RUNS - pred_nbd)
  )
# Find the park with the maximum difference for each model
max_diff_linear <- park_data_preds %>%
  filter(diff_linear == max(diff_linear, na.rm = TRUE)) %>%
  dplyr::select(PARK, diff_linear)
max_diff_poisson <- park_data_preds %>%
  filter(diff_poisson == max(diff_poisson, na.rm = TRUE)) %>%
  dplyr::select(PARK, diff_poisson)
max_diff_nbd <- park_data_preds %>%
  filter(diff_nbd == max(diff_nbd, na.rm = TRUE)) %>%
  dplyr::select(PARK, diff_nbd)
max_diff_linear
max_diff_poisson
max_diff_nbd

# Extract unique parks and teams (years used as team IDs)
parks <- unique(park_data$PARK)
teams <- unique(park_data$OT_YR)  # assuming OT_YR = offensive team ID (e.g. year)

# Create full combination: each offensive team vs each defensive team at each park
theoretical_games <- expand.grid(
  PARK = parks,
  OT_YR = teams,
  DT_YR = teams,
  stringsAsFactors = FALSE
)

# Make sure factors match training data levels for predictions
theoretical_games <- theoretical_games %>%
  mutate(
    PARK = factor(PARK, levels = levels(park_train$PARK)),
    OT_YR = factor(OT_YR, levels = levels(park_train$OT_YR)),
    DT_YR = factor(DT_YR, levels = levels(park_train$DT_YR))
  )

# Predict runs per inning for each model
theoretical_games <- theoretical_games %>%
  mutate(
    pred_linear = predict(park_model, newdata = ., type = "response"),
    pred_poisson = predict(poisson_model, newdata = ., type = "response"),
    pred_nbd = predict(nbd_model, newdata = ., type = "response")
  )

# Average predicted runs by park
theoretical_summary <- theoretical_games %>%
  group_by(PARK) %>%
  summarise(
    mean_linear = mean(pred_linear, na.rm = TRUE),
    mean_poisson = mean(pred_poisson, na.rm = TRUE),
    mean_nbd = mean(pred_nbd, na.rm = TRUE)
  ) %>%
  ungroup()

# (Optional) Add actual average runs for reference
actual_means <- park_data %>%
  group_by(PARK) %>%
  summarise(mean_actual = mean(INN_RUNS, na.rm = TRUE)) %>%
  ungroup()

theoretical_summary <- left_join(theoretical_summary, actual_means, by = "PARK")

# Reshape for plotting
plot_data <- theoretical_summary %>%
  pivot_longer(cols = c(mean_linear, mean_poisson, mean_nbd, mean_actual),
               names_to = "model",
               values_to = "mean_runs") %>%
  mutate(model = case_when(
    model == "mean_linear" ~ "Linear",
    model == "mean_poisson" ~ "Poisson",
    model == "mean_nbd" ~ "Negative Binomial",
    model == "mean_actual" ~ "Actual"
  ))

# Plot
ggplot(plot_data, aes(x = reorder(PARK, mean_runs), y = mean_runs, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Theoretical Mean Runs per Inning by Park (All Teams vs All Teams)",
    x = "Park",
    y = "Mean Runs per Inning",
    fill = "Model"
  ) +
  theme_minimal()

# Calculate difference from actual for each model
diffs <- theoretical_summary %>%
  mutate(
    diff_linear = mean_linear - mean_actual,
    diff_poisson = mean_poisson - mean_actual,
    diff_nbd = mean_nbd - mean_actual,
    abs_diff_linear = abs(diff_linear),
    abs_diff_poisson = abs(diff_poisson),
    abs_diff_nbd = abs(diff_nbd)
  )

# Find park with largest absolute difference for each model
largest_diff_linear <- diffs %>% filter(abs_diff_linear == max(abs_diff_linear)) %>% 
  dplyr::select(PARK, diff_linear)
largest_diff_poisson <- diffs %>% filter(abs_diff_poisson == max(abs_diff_poisson)) %>% 
  dplyr::select(PARK, diff_poisson)
largest_diff_nbd <- diffs %>% filter(abs_diff_nbd == max(abs_diff_nbd)) %>% 
  dplyr::select(PARK, diff_nbd)

# Combine into one table for easy viewing
largest_diffs <- tibble(
  Model = c("Linear", "Poisson", "Negative Binomial"),
  Park = c(largest_diff_linear$PARK, largest_diff_poisson$PARK, largest_diff_nbd$PARK),
  Difference = c(largest_diff_linear$diff_linear, largest_diff_poisson$diff_poisson, largest_diff_nbd$diff_nbd)
)

print(largest_diffs)
