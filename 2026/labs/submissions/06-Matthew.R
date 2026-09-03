#############
### SETUP ###
#############

# install.packages(c("dplyr", "ggplot2", "readr"))
library(dplyr)
library(ggplot2)
library(readr)

# set seed
set.seed(7)

##############
### PART 1 ###
##############

# load data
diving_data = read_csv("../data/06_diving.csv.gz", show_col_types = FALSE)

# Task 1:
# - Recreate the permutation-test setup from Lecture 6 for each judge
# - Build a judge-specific test statistic that compares the judge's scoring discrepancy
#   for same-country divers versus other divers
# - A natural starting point is:
#     observed_stat = mean(discrepancy for matched dives) - mean(discrepancy for unmatched dives)

diving_data <- diving_data |>
  group_by(DiveNo, Round, Diver, Event) |>
  mutate(
    trimmed_mean = (sum(JScore) - max(JScore) - min(JScore)) / (n() - 2)
  ) |>
  ungroup() |>
  mutate(
    discrepancy = JScore - trimmed_mean,
    match = ifelse(JCountry == Country, 1, 0)
  ) |>
  group_by(Judge) |>
  mutate(
    JDiscrepancy = mean(discrepancy[match == 1], na.rm = TRUE)-mean(discrepancy[match == 0], na.rm = TRUE)
  ) |>
  ungroup()
judge_summary <- diving_data |>
  group_by(Judge) |>
  summarize(
    JDiscrepancy = mean(JDiscrepancy, na.rm = TRUE),
    .groups = "drop"
  )
length(judge_summary)
ggplot(data =judge_summary)+
  geom_histogram(aes(x=JDiscrepancy), bins=12)

# Task 2:
# - For each judge, generate a permutation null distribution by shuffling the match labels
#   while holding fixed the judge's discrepancy values
# - Use enough permutations that your p-values are reasonably stable
# - Compute the unadjusted permutation p-value for each judge
set.seed(123)

B <- 5000

perm_results <- diving_data |>
  group_by(Judge) |>
  group_modify(~{
    
    obs_stat <- mean(.x$discrepancy[.x$match == 1], na.rm = TRUE) -
      mean(.x$discrepancy[.x$match == 0], na.rm = TRUE)
    
    perm_stats <- replicate(B, {
      
      perm_match <- sample(.x$match)
      
      mean(.x$discrepancy[perm_match == 1], na.rm = TRUE) -
        mean(.x$discrepancy[perm_match == 0], na.rm = TRUE)
    })
    
    tibble(
      obs_stat = obs_stat,
      p_value = mean(abs(perm_stats) >= abs(obs_stat))
    )
  }) |>
  ungroup()

ggplot(perm_results) +
  geom_histogram(aes(x = p_value), bins = 20)
# Task 3:
# - Adjust the judge-level p-values for multiple testing
# - A good default is p.adjust(..., method = "BH")
# - Report both:
#   * the unadjusted p-values
#   * the adjusted p-values

# Task 4:
# - Identify which judges show evidence of nationality bias before adjustment
# - Identify which judges still show evidence after adjustment
# - Make at least one plot that helps explain the strongest case(s)
# Task 3: Multiple-testing adjustment

perm_results <- perm_results |>
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  ) |>
  arrange(p_value)

# Report observed statistics, unadjusted p-values, and adjusted p-values
perm_results |>
  select(Judge, obs_stat, p_value, p_adj) |>
  arrange(p_value)

# Judges significant before adjustment
sig_unadj <- perm_results |>
  filter(p_value < 0.05)

# Judges significant after adjustment
sig_adj <- perm_results |>
  filter(p_adj < 0.05)

cat("Significant before adjustment:", nrow(sig_unadj), "\n")
cat("Significant after adjustment:", nrow(sig_adj), "\n")

print(sig_unadj)
print(sig_adj)

# Task 4: Plot strongest case

best_judge <- perm_results |>
  slice_min(p_adj, n = 1) |>
  pull(Judge)

best_data <- diving_data |>
  filter(Judge == best_judge)

ggplot(
  best_data,
  aes(
    x = factor(match,
               levels = c(0, 1),
               labels = c("Different Country", "Same Country")),
    y = discrepancy
  )
) +
  geom_boxplot() +
  labs(
    title = paste("Nationality Bias Check for Judge", best_judge),
    x = "",
    y = "Discrepancy from Trimmed Mean"
  )

# Ranked plot of all judges' observed bias statistics

bias_plot <- perm_results |>
  arrange(obs_stat) |>
  mutate(Judge = factor(Judge, levels = Judge))

ggplot(bias_plot,
       aes(x = Judge, y = obs_stat)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Observed Nationality Bias by Judge",
    x = "Judge",
    y = "Mean Discrepancy (Same Country) - Mean Discrepancy (Different Country)"
  )
##############
### PART 2 ###
##############

# load data
tto_data = read_csv("../data/06_tto.csv.gz", show_col_types = FALSE)
colnames(tto_data)
# Variable map for the lecture notation:
# - y_i      = EVENT_WOBA_19
# - t_i      = ORDER_CT
# - BQ_i     = WOBA_FINAL_BAT_19
# - PQ_i     = WOBA_FINAL_PIT_19
# - hand_i   = HAND_MATCH
# - home_i   = BAT_HOME_IND

# Task 1:
# - Fit Model 1 from the lab handout using lm()
# - Model 1 uses indicators for 2TTO and 3TTO, plus the batter/pitcher/home/hand controls

# Task 2:
# - Fit Model 2 from the lab handout using lm()
# - Model 2 adds a linear term in ORDER_CT on top of the Model 1 controls

# Task 3:
# - Run summary(...) on both models
# - Extract the coefficient estimates, standard errors, test statistics, and p-values
# - Interpret the coefficients tied to pitcher decline across times through the order

# Task 4:
# - State whether the estimated decline from one time through the order to the next
#   is statistically significant
# - Explain how the answer changes, if at all, between Model 1 and Model 2
# Model 1: TTO group means
model1 <- lm(
  EVENT_WOBA_19 ~ factor(ORDER_CT) +
    WOBA_FINAL_BAT_19 +
    WOBA_FINAL_PIT_19 +
    HAND_MATCH +
    BAT_HOME_IND,
  data = tto_data
)

model2 <- lm(
  EVENT_WOBA_19 ~ BATTER_SEQ_NUM +
    factor(ORDER_CT) +
    WOBA_FINAL_BAT_19 +
    WOBA_FINAL_PIT_19 +
    HAND_MATCH +
    BAT_HOME_IND,
  data = tto_data
)

summary(model1)
summary(model2)

plot_data <- tibble(
  BATTER_SEQ_NUM = seq(
    min(tto_data$BATTER_SEQ_NUM),
    max(tto_data$BATTER_SEQ_NUM),
    by = 1
  )
) |>
  mutate(
    ORDER_CT = case_when(
      BATTER_SEQ_NUM <= 9 ~ 1,
      BATTER_SEQ_NUM <= 18 ~ 2,
      TRUE ~ 3
    ),
    WOBA_FINAL_BAT_19 = mean(tto_data$WOBA_FINAL_BAT_19, na.rm = TRUE),
    WOBA_FINAL_PIT_19 = mean(tto_data$WOBA_FINAL_PIT_19, na.rm = TRUE),
    HAND_MATCH = mean(tto_data$HAND_MATCH, na.rm = TRUE),
    BAT_HOME_IND = mean(tto_data$BAT_HOME_IND, na.rm = TRUE)
  )

plot_data$pred_model1 <- predict(model1, newdata = plot_data)
plot_data$pred_model2 <- predict(model2, newdata = plot_data)

ggplot(plot_data, aes(x = BATTER_SEQ_NUM)) +
  geom_line(
    aes(y = pred_model1, color = "TTO Indicators Only"),
    linewidth = 1.2
  ) +
  geom_line(
    aes(y = pred_model2, color = "TTO Indicators + Batters Faced"),
    linewidth = 1.2
  ) +
  geom_vline(
    xintercept = c(9.5, 18.5),
    linetype = "dashed",
    alpha = 0.5
  ) +
  labs(
    x = "Batters Faced",
    y = "Predicted Event wOBA",
    color = NULL,
    title = "Times Through the Order Models"
  ) +
  theme_minimal()
