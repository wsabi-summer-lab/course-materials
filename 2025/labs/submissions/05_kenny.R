library(ggplot2)
library(tidyverse)
library(dplyr)
library(rsample)
library(Metrics)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

park_data = read_csv("05_park-effects.csv.gz")

set.seed(123)

split <- initial_split(park_data, prop = 0.8)
train_data <- training(split)
test_data <- testing(split)

avg_runs_per_half_inning <- park_data %>%
  group_by(PARK) %>%
  summarise(
    avg_runs = mean(INN_RUNS, na.rm = TRUE),
    total_half_innings = n()
  ) %>%
  arrange(desc(avg_runs))

train_data <- train_data %>%
  mutate(
    OT_YR = as.factor(OT_YR),
    DT_YR = as.factor(DT_YR),
    PARK = as.factor(PARK),
    BAT_HOME_IND = as.factor(BAT_HOME_IND)
  )

model <- lm(INN_RUNS ~ OT_YR + DT_YR + BAT_HOME_IND + PARK, data = train_data)

coefs <- coef(model)
park_coefs <- coefs[grep("^PARK", names(coefs))]

park_effects <- tibble(
  PARK = gsub("PARK", "", names(park_coefs)),
  Effect = park_coefs
)

ref_park <- levels(train_data$PARK)[1]
park_effects <- bind_rows(
  tibble(PARK = ref_park, Effect = 0),
  park_effects
)

park_effects <- park_effects %>% arrange(desc(Effect))

ggplot(park_effects, aes(x = reorder(PARK, Effect), y = Effect)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Park Effects on Runs per Half-Inning (Model-Based)",
       x = "Park",
       y = "Estimated Effect (vs. baseline park)") +
  theme_minimal()

overall_avg <- mean(park_data$INN_RUNS, na.rm = TRUE)

avg_runs_per_half_inning <- avg_runs_per_half_inning %>%
  mutate(
    diff_from_league_avg = avg_runs - overall_avg
  ) %>%
  arrange(desc(diff_from_league_avg))

comparison_df <- left_join(park_effects, avg_runs_per_half_inning, by = "PARK")

ggplot(comparison_df, aes(x = diff_from_league_avg, y = Effect, label = PARK)) +
  geom_point(color = "dodgerblue", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  geom_text(nudge_y = 0.01, size = 3) +
  labs(
    title = "Park Effects: Raw Average vs Model-Based (Train Only)",
    x = "Raw Park Effect (avg runs - league avg)",
    y = "Model-Based Park Effect"
  ) +
  theme_minimal()

# Comparing in and out of sample

raw_avg_by_park <- train_data %>%
  group_by(PARK) %>%
  summarise(raw_avg = mean(INN_RUNS, na.rm = TRUE))

model_a_test <- test_data %>%
  left_join(raw_avg_by_park, by = "PARK")

overall_train_avg <- mean(train_data$INN_RUNS, na.rm = TRUE)
model_a_test <- model_a_test %>%
  mutate(
    pred_raw_avg = ifelse(is.na(raw_avg), overall_train_avg, raw_avg)
  )

test_data <- test_data %>%
  mutate(
    OT_YR = as.factor(OT_YR),
    DT_YR = as.factor(DT_YR),
    PARK = factor(PARK, levels = levels(train_data$PARK)),
    BAT_HOME_IND = as.factor(BAT_HOME_IND)
)

model_b_test <- test_data %>%
  mutate(pred_lm = predict(model, newdata = test_data))

rmse_a <- rmse(model_a_test$INN_RUNS, model_a_test$pred_raw_avg)
rmse_b <- rmse(model_b_test$INN_RUNS, model_b_test$pred_lm)

rmse_a
rmse_b

# Comparing in RE

mean_run <- mean(train_data$INN_RUNS, na.rm = TRUE)

rmse_mean <- rmse(test_data$INN_RUNS, rep(mean_run, nrow(test_data)))

re_model_a <- (rmse_mean - rmse_a) / rmse_mean
re_model_b <- (rmse_mean - rmse_b) / rmse_mean

re_model_a
re_model_b

# RMSE is slightly better for regression model but not much

comparison_df <- comparison_df %>%
  mutate(
    model_vs_avg_diff = Effect - diff_from_league_avg,
    abs_diff = abs(model_vs_avg_diff)
)

biggest_differences <- comparison_df %>%
  arrange(desc(abs_diff))

ggplot(biggest_differences, aes(x = reorder(PARK, abs_diff), y = abs_diff)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Top Parks with Biggest Disagreement Between Model and Raw Average",
    x = "Park",
    y = "|Model Effect - Raw Average| in Runs Per Half-Inning"
  ) +
  theme_minimal()

