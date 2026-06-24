############################################################
# Lab 17 Task 1: Baselines and Formation Signal
# Goal: Predict play action pass vs designed run
############################################################

# -----------------------------
# 0. Load packages
# -----------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# -----------------------------
# 1. Load data
# -----------------------------

# Make sure this CSV is in your working directory.
# In RStudio, you can check with:
# getwd()

pa <- read_csv("17_play-action-vs-run.csv")

# Quick check
glimpse(pa)

# -----------------------------
# 2. Basic data checks
# -----------------------------

# Target:
# playActionPass = 1 means play action dropback pass
# playActionPass = 0 means designed run

table(pa$split)
table(pa$playActionPass)

# Check missingness in the most important variables
pa %>%
  summarize(
    missing_split = sum(is.na(split)),
    missing_target = sum(is.na(playActionPass)),
    missing_formation = sum(is.na(offenseFormation)),
    missing_alignment = sum(is.na(receiverAlignment))
  )

# -----------------------------
# 3. Class balance by split
# -----------------------------

class_balance <- pa %>%
  group_by(split) %>%
  summarize(
    n_plays = n(),
    n_runs = sum(playActionPass == 0, na.rm = TRUE),
    n_play_action = sum(playActionPass == 1, na.rm = TRUE),
    play_action_rate = mean(playActionPass, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(match(split, c("train", "validation", "test")))

print(class_balance)

# -----------------------------
# 4. Create train / validation / test sets
# -----------------------------

train <- pa %>% filter(split == "train")
valid <- pa %>% filter(split == "validation")
test  <- pa %>% filter(split == "test")

# -----------------------------
# 5. Define log loss function
# -----------------------------

log_loss <- function(actual, predicted) {
  # Avoid log(0) by clipping predictions slightly away from 0 and 1
  eps <- 1e-15
  predicted <- pmin(pmax(predicted, eps), 1 - eps)
  
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

# -----------------------------
# 6. Baseline 1: Training mean
# -----------------------------

train_mean_rate <- mean(train$playActionPass)

cat("\nTraining play action rate:", round(train_mean_rate, 4), "\n")

# Predict same probability for every validation and test play
valid_pred_mean <- rep(train_mean_rate, nrow(valid))
test_pred_mean  <- rep(train_mean_rate, nrow(test))

# Evaluate
valid_logloss_mean <- log_loss(valid$playActionPass, valid_pred_mean)
test_logloss_mean  <- log_loss(test$playActionPass, test_pred_mean)

cat("\nTraining Mean Baseline\n")
cat("Validation log loss:", round(valid_logloss_mean, 4), "\n")
cat("Test log loss:", round(test_logloss_mean, 4), "\n")

# -----------------------------
# 7. Baseline 2: Formation + alignment rate
# -----------------------------

formation_rates <- train %>%
  group_by(offenseFormation, receiverAlignment) %>%
  summarize(
    n_train = n(),
    n_play_action = sum(playActionPass == 1),
    formation_alignment_rate = mean(playActionPass),
    .groups = "drop"
  ) %>%
  arrange(desc(formation_alignment_rate))

print(formation_rates)

# Join training-estimated rates onto validation set
valid_form <- valid %>%
  left_join(
    formation_rates,
    by = c("offenseFormation", "receiverAlignment")
  ) %>%
  mutate(
    pred_formation_rate = if_else(
      is.na(formation_alignment_rate),
      train_mean_rate,
      formation_alignment_rate
    )
  )

# Join training-estimated rates onto test set
test_form <- test %>%
  left_join(
    formation_rates,
    by = c("offenseFormation", "receiverAlignment")
  ) %>%
  mutate(
    pred_formation_rate = if_else(
      is.na(formation_alignment_rate),
      train_mean_rate,
      formation_alignment_rate
    )
  )

# Evaluate formation-rate baseline
valid_logloss_form <- log_loss(
  valid_form$playActionPass,
  valid_form$pred_formation_rate
)

test_logloss_form <- log_loss(
  test_form$playActionPass,
  test_form$pred_formation_rate
)

cat("\nFormation + Alignment Rate Baseline\n")
cat("Validation log loss:", round(valid_logloss_form, 4), "\n")
cat("Test log loss:", round(test_logloss_form, 4), "\n")

# -----------------------------
# 8. Summary table for baselines
# -----------------------------

baseline_results <- tibble(
  Model = c("Training mean", "Formation + alignment rate"),
  Validation_Log_Loss = c(valid_logloss_mean, valid_logloss_form),
  Test_Log_Loss = c(test_logloss_mean, test_logloss_form)
)

print(baseline_results)

# -----------------------------
# 9. Formation/alignment table using training data only
# -----------------------------

# Full table, including small sample cells
formation_signal_table <- formation_rates %>%
  mutate(
    play_action_rate_pct = percent(formation_alignment_rate, accuracy = 0.1)
  ) %>%
  select(
    offenseFormation,
    receiverAlignment,
    n_train,
    n_play_action,
    formation_alignment_rate,
    play_action_rate_pct
  )

print(formation_signal_table)

# More reliable version: require at least 25 training plays
formation_signal_table_25 <- formation_signal_table %>%
  filter(n_train >= 25)

cat("\nFormation/alignment cells with at least 25 training plays:\n")
print(formation_signal_table_25)

# Highest play action rate cells
highest_cells <- formation_signal_table_25 %>%
  arrange(desc(formation_alignment_rate)) %>%
  slice_head(n = 10)

cat("\nHighest play action rate cells, minimum n = 25:\n")
print(highest_cells)

# Lowest play action rate cells
lowest_cells <- formation_signal_table_25 %>%
  arrange(formation_alignment_rate) %>%
  slice_head(n = 10)

cat("\nLowest play action rate cells, minimum n = 25:\n")
print(lowest_cells)

# -----------------------------
# 10. Heatmap of play action rate by formation and alignment
# -----------------------------

plot_data <- formation_signal_table_25 %>%
  mutate(
    pct_label = percent(formation_alignment_rate, accuracy = 1)
  )

p_heatmap <- ggplot(
  plot_data,
  aes(
    x = receiverAlignment,
    y = offenseFormation,
    fill = formation_alignment_rate
  )
) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(
    aes(label = pct_label),
    size = 3,
    color = "black"
  ) +
  scale_fill_gradient(
    low = "#e8f1ee",
    high = "#e76f51",
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Play Action Rate by Formation and Alignment",
    subtitle = "Training data only; cells shown only if n >= 25",
    x = "Receiver Alignment",
    y = "Offensive Formation",
    fill = "Play Action\nRate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.title.x = element_text(size = 12, margin = margin(t = 8)),
    axis.title.y = element_text(size = 12, margin = margin(r = 8)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

print(p_heatmap)

ggsave(
  "task1_play_action_heatmap.png",
  plot = p_heatmap,
  width = 10,
  height = 4.8,
  dpi = 300
)