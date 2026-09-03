#############
### SETUP ###
#############

rm(list=ls())

# install.packages(c("ggplot2", "splines", "tidyverse"))
library(ggplot2)
library(splines)
library(tidyverse)


#########################
### PERMUTATION TESTS ###
#########################

# load data
diving_data = read_csv("../data/07_diving.csv.gz")
names(diving_data)

# find number of unique judges
num_judges = length(unique(diving_data$Judge)) # 25

dives_grouped <- diving_data %>%
  group_by(DiveNo, Round, Diver) %>%
  summarise(mean_score = mean(JScore, na.rm = TRUE), .groups = 'drop') %>% 
  # add the rest of the original dataframe back in
  left_join(diving_data, by = c("DiveNo", "Round", "Diver")) %>% 
  mutate(discrepancy = mean_score - JScore)

output <- data.frame(
  Judge = character(),
  matched_count = integer(),
  matched_avg_dev = numeric(),
  unmatched_count = integer(),
  unmatched_avg_dev = numeric(),
  diff_of_discrepancies = numeric(),
  p_value = numeric()
)

for (judge in unique(diving_data$Judge)) {
  judge_data <- dives_grouped %>% filter(Judge == judge)

  # add match or no match
  judge_data <- judge_data %>%
    mutate(match = ifelse(Country == JCountry, "matched", "unmatched"))
  
  # compute judge's mean discrepancy from mean_score for matched and unmatched dives (new columns for both)
  judge_data <- judge_data %>%
    group_by(DiveNo, match) %>%
    summarise(mean_discrepancy = mean(discrepancy, na.rm = TRUE), .groups = 'drop')
  
  # record judge's average of all the "mean_discrepancy" for matched and unmatched dives
  matched_avg_discrepancy <- mean(judge_data$mean_discrepancy[judge_data$match == "matched"], na.rm = TRUE)
  unmatched_avg_discrepancy <- mean(judge_data$mean_discrepancy[judge_data$match == "unmatched"], na.rm = TRUE)
  actual_diff <- matched_avg_discrepancy - unmatched_avg_discrepancy
  
  if (is.nan(actual_diff) | is.infinite(actual_diff)) {
    next  # skip this judge if the actual difference is NA, NaN, or infinite
  }
  
  num_permutations <- 10000
  # make new dataframe to store the permuted discrepancies
  permuted_discrepancies <- data.frame(
    matched_avg = numeric(num_permutations),
    unmatched_avg = numeric(num_permutations),
    diff = numeric(num_permutations)
  )
  
  for (i in 1:num_permutations) {
    permuted_match <- sample(judge_data$match)
    judge_data$permuted_match <- permuted_match
    
    permuted_discrepancies[i,1] <- mean(judge_data$mean_discrepancy[judge_data$permuted_match == "matched"], na.rm = TRUE)
    permuted_discrepancies[i,2] <- mean(judge_data$mean_discrepancy[judge_data$permuted_match == "unmatched"], na.rm = TRUE)
    permuted_discrepancies[i,3] <- permuted_discrepancies[i,1] - permuted_discrepancies[i,2]
    
    temp_matched_avg_discrepancy <- mean(judge_data$mean_discrepancy[judge_data$permuted_match == "matched"], na.rm = TRUE)
    temp_unmatched_avg_discrepancy <- mean(judge_data$mean_discrepancy[judge_data$permuted_match == "unmatched"], na.rm = TRUE)
  }
  
  # count number of rows where actual_diff >= permuted_discrepancies$diff
  sum = 0
  for (i in 1:num_permutations){
    if (actual_diff >= permuted_discrepancies$diff[i]) {
      sum = sum + 1
    }
  }
  p_val <- sum / num_permutations
  
  output <- rbind(output, data.frame(
    Judge = judge,
    matched_count = sum(judge_data$match == "matched"),
    matched_avg_dev = matched_avg_discrepancy,
    unmatched_count = sum(judge_data$match == "unmatched"),
    unmatched_avg_dev = unmatched_avg_discrepancy,
    diff_of_discrepancies = actual_diff,
    p_value = p_val
  ))
}

output


############################
### PARAMETRIC INFERENCE ###
############################

# load data
mlb_data = read_csv("../data/07_tto.csv.gz")
names(mlb_data)

mlb_data <- mlb_data %>% 
  mutate(tto2 = ifelse(BATTER_SEQ_NUM >= 9, 1, 0),
         tto3 = ifelse(BATTER_SEQ_NUM >= 18, 1, 0))

model1 <- lm(EVENT_WOBA_19 ~ factor(tto2) + factor(tto3) + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19
             + factor(HAND_MATCH) + factor(BAT_HOME_IND), data = mlb_data)
summary(model1)

model2 <- lm(EVENT_WOBA_19 ~ BATTER_SEQ_NUM + factor(tto2) + factor(tto3) + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19
             + factor(HAND_MATCH) + factor(BAT_HOME_IND), data = mlb_data)
summary(model2)

## p-value interpretation for a given \hat{beta}: 
# if p = 0.04, then under the null distribution of the test statistic, 
# we would expect to see values at least as extreme as our actual data 4% of the time.

## the new model includes batter sequence number which is a proxy for time elapsed, i.e., some sort of fatigue
# in the first model (without batter_seq_num), changes in TTO are statistically significant (***)
# but in the second model, batter_seq_num is significant (***) and TTO's are no longer as significant. 
# this implies that fatigue (time elapsed) is more significant than changes in TTO