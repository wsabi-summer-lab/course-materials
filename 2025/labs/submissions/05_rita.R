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

head(park_data)

mean_runs <- park_data %>%
  group_by(PARK) %>%
  summarize(
    mean_runs = mean(yi, na.rm = TRUE)
  )

print(head(mean_runs))

park_data = park_data %>%
  mutate(
    park1 = factor(PARK),
    otyr1 = factor(OT_YR),
    dtyr1 = factor(DT_YR)
  )

park_data <- park_data %>%
  mutate(
    yi = INN_RUNS,
    pi = PARK,
    oi = OT_YR,
    di = DT_YR
  )

full_matrix = model.matrix(~ park1 + otyr1 + dtyr1, data = park_data)
y = park_data$yi

#chatgpt helped set up the matrix (46-54)

set.seed(2025)
n_total = nrow(park_data)
train_idx = sample(seq_len(n_total), size = floor(0.7 * n_total))
test_idx = setdiff(seq_len(n_total), train_idx)

X_train = full_matrix[train_idx, , drop = FALSE]
y_train = y[train_idx]
X_test  = full_matrix[test_idx,  , drop = FALSE]
y_test  = y[test_idx]

train_df = park_data[train_idx, ]
test_df  = park_data[test_idx,  ]

refined_model_train = lm(yi ~ park1 + otyr1 + dtyr1, data = train_df)

pred_refined_train = predict(refined_model_train, newdata = train_df)
pred_refined_test = predict(refined_model_train, newdata = test_df)

rmse = function(pred, obs) {
  sqrt(mean((pred - obs) ^ 2, na.rm = TRUE))
}

rmse_refined_train = rmse(pred_refined_train, train_df$yi)
rmse_refined_test  = rmse(pred_refined_test,  test_df$yi)

mean_runs_train = train_df %>%
  group_by(PARK) %>%
  summarize(mean_runs = mean(yi, na.rm = TRUE), .groups = "drop")

overall_train_mean = mean(train_df$yi, na.rm = TRUE)

rmse_naive_test = rmse(overall_train_mean, test_df$yi)

coef_model_train = coef(refined_model_train)
coef_intercept_train = coef_model_train["(Intercept)"]
park_coef_indices_train = grep("^park1", names(coef_model_train), value = TRUE)

park_effects_df = tibble(PARK = levels(train_df$park1)) %>%
  rowwise() %>%
  mutate(
    coef_name      = paste0("park1", PARK),
    refined_effect = if (coef_name %in% park_coef_indices_train) {
      coef_intercept_train + coef_model_train[coef_name]
    } else {
      coef_intercept_train
    }
  ) %>%
  ungroup() %>%
  select(PARK, refined_effect)

ggplot(park_effects_df, aes(x = reorder(PARK, refined_effect), y = refined_effect)) +
  geom_col(fill = "lightpink") +
  coord_flip() +
  labs(
    x     = "Park",
    y     = "Adjusted Runs/Half Inning",
    title = "Train Plot"
  ) +
  theme_minimal()

naive_effects_train = mean_runs_train 

compare_df = naive_effects_train %>%
  inner_join(park_effects_df, by = "PARK") %>%
  arrange(PARK)

compare_long = compare_df %>%
  pivot_longer(cols = c("mean_runs", "refined_effect"), names_to = "method", values_to = "runs")

ggplot(compare_long, aes(x = reorder(PARK, runs), y = runs, fill = method)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    x     = "Park",
    y     = "Runs/Half‐Inning",
    fill  = "Estimate",
    title = "Naive vs Refined"
  ) +
  theme_minimal()

top_diff_df = compare_df %>%
  mutate(difference = abs(mean_runs - refined_effect)) %>%
  arrange(desc(difference))

print(top_diff_df)

ggplot(top_diff_df, aes(x = reorder(PARK, difference), y = difference)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(
    x     = "Park",
    y     = "Abs Diff",
    title = "Biggest Difference"
  ) +
  theme_minimal()


avg_park_effect = mean(refined_parks$refined_effect)
print(avg_park_effect)



