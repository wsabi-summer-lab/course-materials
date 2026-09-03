
library(dplyr)
library(ggplot2)
library(readr)

# set seed
set.seed(7)

diving_data <- read_csv("06_diving.csv.gz", show_col_types = FALSE)

#task 1
diving_data <- diving_data %>%
  group_by(Event, Round, Diver, DiveNo) %>%
  mutate(
    sum_all     = sum(JScore),
    n_all       = n(),
    mean_other  = (sum_all - JScore) / (n_all - 1),  # leave-one-out mean
    discrepancy = JScore - mean_other,
    match       = (Country == JCountry)               # TRUE = same-country dive
  ) %>%
  ungroup()

# Observed test statistic per judge:
#   obs_stat = mean discrepancy (matched) - mean discrepancy (unmatched)
obs_stats <- diving_data %>%
  group_by(Judge, JCountry) %>%
  summarise(
    n_match   = sum(match),
    n_unmatch = sum(!match),
    obs_stat  = mean(discrepancy[match == TRUE]) -
                mean(discrepancy[match == FALSE]),
    .groups = "drop"
  )

cat("=== Task 1: Observed Test Statistics ===\n")
print(obs_stats %>% arrange(desc(obs_stat)) %>% mutate(obs_stat = round(obs_stat, 4)))


#task 2 

N_PERMS <- 10000

run_perm_test <- function(judge_df, n_perms = N_PERMS) {
  disc   <- judge_df$discrepancy
  labels <- judge_df$match

  obs <- mean(disc[labels]) - mean(disc[!labels])

  perm_stats <- replicate(n_perms, {
    perm <- sample(labels)
    mean(disc[perm]) - mean(disc[!perm])
  })

  p_val <- mean(perm_stats >= obs)
  list(obs_stat = obs, p_value = p_val)
}

# Only test judges who have at least one matched AND one unmatched dive
testable_judges <- obs_stats %>%
  filter(n_match > 0, n_unmatch > 0) %>%
  pull(Judge)

perm_list <- lapply(testable_judges, function(j) {
  res <- run_perm_test(diving_data %>% filter(Judge == j))
  data.frame(Judge = j, obs_stat = res$obs_stat, p_value = res$p_value)
})

perm_results <- bind_rows(perm_list)

cat("\n=== Task 2: Unadjusted Permutation P-Values ===\n")
print(perm_results %>% arrange(p_value) %>% mutate(across(where(is.numeric), ~round(., 4))))


#task 3 

perm_results$p_adjusted <- p.adjust(perm_results$p_value, method = "BH")

final_results <- obs_stats %>%
  left_join(perm_results %>% select(Judge, p_value, p_adjusted), by = "Judge") %>%
  arrange(p_value)

cat("\n=== Task 3: Unadjusted & BH-Adjusted P-Values ===\n")
print(final_results %>%
  select(Judge, JCountry, obs_stat, n_match, n_unmatch, p_value, p_adjusted) %>%
  mutate(across(c(obs_stat, p_value, p_adjusted), ~round(., 4))))


#task 4
cat("\n=== Task 4: Judges with Nationality Bias ===\n")

cat("\nBefore BH adjustment (p < 0.05):\n")
bias_before <- final_results %>% filter(p_value < 0.05)
if (nrow(bias_before) == 0) {
  cat("  None at alpha = 0.05\n")
} else {
  print(bias_before %>% select(Judge, JCountry, obs_stat, p_value) %>%
    mutate(across(where(is.numeric), ~round(., 4))))
}

cat("\nAfter BH adjustment (p_adj < 0.05):\n")
bias_after <- final_results %>% filter(p_adjusted < 0.05)
if (nrow(bias_after) == 0) {
  cat("  None at alpha = 0.05\n")
} else {
  print(bias_after %>% select(Judge, JCountry, obs_stat, p_value, p_adjusted) %>%
    mutate(across(where(is.numeric), ~round(., 4))))
}

# Plot: discrepancy distributions for the judge with the smallest p-value
top_judge   <- final_results$Judge[1]
top_country <- final_results$JCountry[1]

plot_data <- diving_data %>%
  filter(Judge == top_judge) %>%
  mutate(match_label = ifelse(match, "Same Country", "Other Country"))

p1 <- ggplot(plot_data, aes(x = match_label, y = discrepancy, fill = match_label)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("Same Country" = "#E05C5C", "Other Country" = "#5C7FE0")) +
  labs(
    title    = paste0("Scoring Discrepancy: ", top_judge, " (", top_country, ")"),
    subtitle = paste0(
      "Unadjusted p = ",  round(final_results$p_value[1],    4),
      "  |  BH-adjusted p = ", round(final_results$p_adjusted[1], 4)
    ),
    x = "Diver-Judge Country Match",
    y = "Discrepancy (Judge Score − Mean Other Judges)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(p1)

#part 2 

# load data
tto_data <- read_csv("06_tto.csv.gz", show_col_types = FALSE)

# Variable map:  y_i = EVENT_WOBA_19,  t_i = ORDER_CT
#                BQ_i = WOBA_FINAL_BAT_19,  PQ_i = WOBA_FINAL_PIT_19
#                hand_i = HAND_MATCH,  home_i = BAT_HOME_IND

# Create TTO indicator variables
tto_data <- tto_data %>%
  mutate(
    tto2 = as.integer(ORDER_CT >= 2),   # 1{t_i >= 2TTO}
    tto3 = as.integer(ORDER_CT >= 3)    # 1{t_i >= 3TTO}
  )


# task 1 

model1 <- lm(
  EVENT_WOBA_19 ~ tto2 + tto3 +
    WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 +
    HAND_MATCH + BAT_HOME_IND,
  data = tto_data
)


# task 2

model2 <- lm(
  EVENT_WOBA_19 ~ ORDER_CT + tto2 + tto3 +
    WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 +
    HAND_MATCH + BAT_HOME_IND,
  data = tto_data
)


# task 3 

cat("\n=== PART 2: Model 1 Summary ===\n")
print(summary(model1))

cat("\n=== PART 2: Model 2 Summary ===\n")
print(summary(model2))

# Extract coefficients for easy comparison
coef_m1 <- as.data.frame(summary(model1)$coefficients)
coef_m2 <- as.data.frame(summary(model2)$coefficients)

cat("\n--- Model 1 Coefficients (rounded) ---\n")
print(round(coef_m1, 6))

cat("\n--- Model 2 Coefficients (rounded) ---\n")
print(round(coef_m2, 6))


# task 4 

cat("\n=== Task 4: Pitcher Decline Significance ===\n")

cat("\nModel 1 — step-change estimates:\n")
cat(sprintf("  β2 (2TTO indicator):  est = %+.5f  SE = %.5f  t = %6.3f  p = %.4f\n",
    coef_m1["tto2", "Estimate"],  coef_m1["tto2", "Std. Error"],
    coef_m1["tto2", "t value"],   coef_m1["tto2", "Pr(>|t|)"]))
cat(sprintf("  β3 (3TTO indicator):  est = %+.5f  SE = %.5f  t = %6.3f  p = %.4f\n",
    coef_m1["tto3", "Estimate"],  coef_m1["tto3", "Std. Error"],
    coef_m1["tto3", "t value"],   coef_m1["tto3", "Pr(>|t|)"]))

cat("\nModel 2 — linear + step-change estimates:\n")
cat(sprintf("  β1 (ORDER_CT linear): est = %+.5f  SE = %.5f  t = %6.3f  p = %.4f\n",
    coef_m2["ORDER_CT", "Estimate"], coef_m2["ORDER_CT", "Std. Error"],
    coef_m2["ORDER_CT", "t value"],  coef_m2["ORDER_CT", "Pr(>|t|)"]))
cat(sprintf("  β2 (2TTO indicator):  est = %+.5f  SE = %.5f  t = %6.3f  p = %.4f\n",
    coef_m2["tto2", "Estimate"],  coef_m2["tto2", "Std. Error"],
    coef_m2["tto2", "t value"],   coef_m2["tto2", "Pr(>|t|)"]))
cat(sprintf("  β3 (3TTO indicator):  est = %+.5f  SE = %.5f  t = %6.3f  p = %.4f\n",
    coef_m2["tto3", "Estimate"],  coef_m2["tto3", "Std. Error"],
    coef_m2["tto3", "t value"],   coef_m2["tto3", "Pr(>|t|)"]))

cat("\n--- Interpretation ---\n")
cat(
  "Model 1: β2 and β3 are the mean wOBA jumps at the 2nd and 3rd time through the\n",
  "order (relative to 1st TTO), after controlling for batter/pitcher quality,\n",
  "handedness, and home field. Significant positive estimates mean batters do better\n",
  "(pitchers tire/decline) as they face the lineup more.\n\n",
  "Model 2: β1 captures a within-TTO linear trend per batter faced; β2 and β3 are\n",
  "additional step-changes at TTO boundaries on top of that trend. Comparing\n",
  "significance across models reveals whether the decline is better described as\n",
  "a smooth linear ramp (Model 2's ORDER_CT term) or purely discrete jumps (Model 1).\n"
)
