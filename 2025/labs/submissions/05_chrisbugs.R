#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

####################
### PARK EFFECTS ###
####################

# load data
park_data = read_csv("../data/05_park-effects.csv.gz")
park_data

# Compute mean runs scored in each half inning at each ballpark
park_effects = park_data %>%
  group_by(PARK) %>%
  summarise(mean_runs = mean(INN_RUNS, na.rm = TRUE)) %>%
  arrange(desc(mean_runs))

park_effects

# Plot the mean runs scored in each half inning at each ballpark with the mean highlighted on the graph
ggplot(park_effects, aes(x = reorder(PARK, -mean_runs), y = mean_runs)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = mean(park_effects$mean_runs), color = "red") +
  labs(
    title = "Mean Runs Scored in Each Half Inning at Each Ballpark",
    x = "Ballpark",
    y = "Mean Runs Scored"
  ) +
  theme_minimal() +
  coord_flip()

### Potential confounders:
# Teams playing & their quality (players too - especially pitchers & hitters)
# Weather conditions (temperature, humidity, wind)
# Time of day (day/night games)
# Game context (score, inning, etc.)
# Park natures (fence distance, height, etc)


# How many runs a team scores at a park compared to that teams average runs scored across all parks
# Offensive quality = mean runs scored
# Defensive quality = mean runs against

# Offensive quality
offensive_quality = park_data %>%
  group_by(OT_YR) %>%
  summarise(of_quality = mean(INN_RUNS, na.rm = TRUE)) %>%
  ungroup()

# Defensive quality
defensive_quality = park_data %>%
  group_by(DT_YR) %>%
  summarise(df_quality = mean(INN_RUNS, na.rm = TRUE)) %>%
  ungroup()

offensive_quality
defensive_quality

# Merge offensive and defensive quality with park data
park_data = park_data %>%
  left_join(offensive_quality, by = "OT_YR") %>%
  left_join(defensive_quality, by = "DT_YR")

park_data

# Drop mean_runs_scored and mean_runs_against columns
park_data = park_data %>%
  select(-mean_runs_scored, -mean_runs_against)

park_data

# Make of_quality and df_quality centered around the mean
park_data = park_data %>%
  mutate(
    of_quality = of_quality - mean(of_quality, na.rm = TRUE),
    df_quality = df_quality - mean(df_quality, na.rm = TRUE)
  )

park_data

# Train test split
set.seed(123)

n = nrow(park_data)
train_idx = sample(seq_len(n), size = floor(0.8 * n))
train_data = park_data[train_idx, ]
test_data = park_data[-train_idx, ]

# Fit a linear model on the training data
linear_model = lm(INN_RUNS ~ PARK + of_quality + df_quality, data = train_data)

# Get coefficient for PARK
coefficients <- summary(linear_model)$coefficients

coefficients = as.data.frame(coefficients)
coef_table <- coefficients[grep("^PARK", rownames(coefficients)), , drop = FALSE]

coef_table = coef_table %>%
  select(Estimate)

coef_table

# Get predicted runs scored by park
predicted_runs = predict(linear_model, newdata = test_data)
predicted_runs = data.frame(PARK = test_data$PARK, Predicted_Runs = predicted_runs)
predicted_runs

# Average predicted runs by park
average_predicted_runs = predicted_runs %>%
  group_by(PARK) %>%
  summarise(Average_Predicted_Runs = mean(Predicted_Runs, na.rm = TRUE)) %>%
  arrange(desc(Average_Predicted_Runs))

average_predicted_runs

# Plot the average predicted runs by park
ggplot(average_predicted_runs, aes(x = reorder(PARK, -Average_Predicted_Runs), y = Average_Predicted_Runs)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = mean(average_predicted_runs$Average_Predicted_Runs), color = "red") +
  labs(
    title = "Average Predicted Runs by Park",
    x = "Park",
    y = "Average Predicted Runs"
  ) +
  theme_minimal() +
  coord_flip()


# Get the difference of estimated average runs per inning and the mean runs scored in each half inning at each ballpark
park_effects_merged = park_effects %>%
  left_join(average_predicted_runs, by = "PARK") %>%
  mutate(Difference = mean_runs - Average_Predicted_Runs)

# Plot the difference
ggplot(park_effects_merged, aes(x = reorder(PARK, -Difference), y = Difference)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Difference Between Estimated And Naive Mean Runs Scored",
    x = "Park",
    y = "Difference"
  ) +
  theme_minimal() +
  coord_flip()

# Get an abs value of the difference
park_effects_merged_abs = park_effects_merged %>%
  mutate(Abs_Difference = abs(Difference))

# Plot the absolute difference
ggplot(park_effects_merged_abs, aes(x = reorder(PARK, -Abs_Difference), y = Abs_Difference)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Absolute Difference Between Estimated And Naive Mean Runs Scored",
    x = "Park",
    y = "Absolute Difference"
  ) +
  theme_minimal() +
  coord_flip()

# Get average for naive_table in training data by park
naive_table <- train_data %>%                       
  group_by(PARK) %>%
  summarise(mean_runs_naive = mean(INN_RUNS, na.rm = TRUE)) %>%
  ungroup()

naive_table

pred_naive <- test_data %>%
  left_join(naive_table, by = "PARK") %>%
  mutate(mean_runs_naive = ifelse(is.na(mean_runs_naive),
                                  mean(train_data$INN_RUNS, na.rm = TRUE),
                                  mean_runs_naive)) %>%
  pull(mean_runs_naive)

pred_naive

# Linear MSE
linear_preds = predict(linear_model, newdata = test_data)
linear_mse = mean((test_data$INN_RUNS - linear_preds)^2)
linear_mse

# Naive MSE
naive_mse = mean((test_data$INN_RUNS - pred_naive)^2)
naive_mse

# Compare MSEs on a simple bar chart
mse_data = data.frame(
  Model = c("Linear Model", "Naive Model"),
  MSE = c(linear_mse, naive_mse)
)
ggplot(mse_data, aes(x = Model, y = MSE, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Mean Squared Error of Linear and Naive Models",
    x = "Model",
    y = "Mean Squared Error"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "darkorange")) +
  theme(legend.position = "none")
