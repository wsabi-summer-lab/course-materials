library(ggplot2)
library(splines)
library(tidyverse)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

diving_data = read_csv("07_diving.csv.gz")

# Replicating Permutation test to detect judge nationality

diving_data <- diving_data %>%
  group_by(Diver, DiveNo, Round, Event) %>%
  mutate(Average_dive_score = mean(JScore, na.rm = TRUE)) %>%
  mutate(Discrepancy = JScore - Average_dive_score) %>%
  ungroup() %>%
  mutate(Nationality_match = JCountry == Country) %>%
  group_by(Nationality_match, Judge) %>%
  mutate(Judge_effect = mean(Discrepancy, na.rm = TRUE)) %>%
  ungroup()

n_perms <- 1000
perm_results <- list()

for(j in unique(diving_data$Judge)) {
  
  judge_data <- diving_data %>% filter(Judge == j)
  
  perm_diffs <- numeric(n_perms)
  
  for (i in 1:n_perms){
  permuted_match <- sample(diving_data$Nationality_match)
  avg_match <- mean(judge_data$Discrepancy[permuted_match == TRUE], na.rm = TRUE)
  avg_nomatch <- mean(judge_data$Discrepancy[permuted_match == FALSE], na.rm = TRUE)
  perm_diffs[i] <- avg_match - avg_nomatch
  }
  perm_results[[j]] <- perm_diffs
}

McFarland_perm_diffs <- perm_results$`McFARLAND Steve`

McFarland_actual_diff <- unique(diving_data$Judge_effect[diving_data$Judge == "McFARLAND Steve" & diving_data$Nationality_match == TRUE])

prop_greater <- mean(McFarland_perm_diffs > McFarland_actual_diff)

prop_greater

plot(density(McFarland_perm_diffs), 
     main = "Permutation Distribution for McFARLAND Steve", 
     xlab = "Difference in Discrepancy (Match - No Match)",
     ylab = "Density", 
     col = "blue", 
     lwd = 2)
abline(v = McFarland_actual_diff, col = "red", lty = 2)

# For all judges

result_df <- data.frame(
  Judge = character(),
  ActualDiff = numeric(),
  PropGreater = numeric(),
  stringsAsFactors = FALSE
)

for (j in unique(diving_data$Judge)) {
  perm_diffs <- perm_results[[j]]
  
  actual_diff <- unique(diving_data$Judge_effect[
    diving_data$Judge == j & diving_data$Nationality_match == TRUE
  ])
  
  if (length(actual_diff) == 0) next
  
  prop_greater <- mean(perm_diffs > actual_diff)
  
  result_df <- rbind(result_df, data.frame(
    Judge = j,
    ActualDiff = actual_diff,
    PropGreater = prop_greater
  ))
}

result_df

# Parametric Inference

mlb_data = read_csv("07_tto.csv.gz")

head(mlb_data)

model_1 <- lm(EVENT_WOBA_19 ~ HAND_MATCH + BAT_HOME_IND + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + as.factor(ORDER_CT), data = mlb_data)

summary(model_1)

# P-Value interpretation

# The probability that the you would observe the result
# randomly under the null hypothesis (a t-distibution with 214,379 df)
# basically a normal, all coefficients are significant

#  Pr(>|t|)    
#  (Intercept)           < 2e-16 ***
#  HAND_MATCH           1.45e-13 ***
#  BAT_HOME_IND         4.77e-06 ***
#  WOBA_FINAL_BAT_19     < 2e-16 ***
#  WOBA_FINAL_PIT_19     < 2e-16 ***
#  as.factor(ORDER_CT)2 7.89e-08 ***
#  as.factor(ORDER_CT)3 1.09e-10 ***

model_2 <- lm(EVENT_WOBA_19 ~ BATTER_SEQ_NUM + HAND_MATCH + as.factor(BAT_HOME_IND) + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + as.factor(ORDER_CT) + 0, data = mlb_data)

summary(model_2)

# P-Value interpretation

# The probability that the you would observe the result
# randomly under the null hypothesis (a t-distibution with 214,379 df)
# basically a normal, ORDER_CTs are not significant when model includes
# batter sequence number

#. Pr(>|t|)  
#  BATTER_SEQ_NUM           0.000781 ***
#  HAND_MATCH               2.68e-14 ***
#  as.factor(BAT_HOME_IND)0  < 2e-16 ***
#  as.factor(BAT_HOME_IND)1  < 2e-16 ***
#  WOBA_FINAL_BAT_19         < 2e-16 ***
#  WOBA_FINAL_PIT_19         < 2e-16 ***
#  as.factor(ORDER_CT)2     0.763064    
#  as.factor(ORDER_CT)3     0.274244  

# Is pitcher decline from one time through the order to the next significant
# No
