#############
### SETUP ###
#############
setwd("/Users/tianshufeng/Documents/GitHub/summer-lab/2025/labs/data")

# install.packages(c("ggplot2", "splines", "tidyverse"))
library(ggplot2)
library(splines)
library(tidyverse)


#########################
### PERMUTATION TESTS ###
#########################

# load data
diving_data = read_csv("../data/07_diving.csv.gz")

# Permutation test
dive_scores = diving_data %>%
  group_by(Event, Diver, Round, DiveNo) %>%
  summarise(AvgScore = mean(JScore)) %>%
  ungroup()

# Add AvgScore to the original data
diving_data = diving_data %>%
  left_join(dive_scores, by = c("Event", "Diver", "Round", "DiveNo"))

# Calculate discrepancy for all dives
diving_data <- diving_data %>%
  mutate(Discrepancy = JScore - AvgScore)

# Matched dives: where Country == JCountry
matched <- diving_data %>%
  filter(Country == JCountry) %>%
  group_by(Judge, JCountry) %>%
  summarise(
    MatchedDivesNo = n(),
    AvgMatchedDiscrepancy = mean(Discrepancy),
    .groups = "drop"
  )

# Non-matched dives: where Country != JCountry
non_matched <- diving_data %>%
  filter(Country != JCountry) %>%
  group_by(Judge, JCountry) %>%
  summarise(
    NonMatchedDivesNo = n(),
    AvgNonMatchedDiscrepancy = mean(Discrepancy),
    .groups = "drop"
  )

# Merge both tables, ensuring no judge is dropped
result <- full_join(matched, non_matched, by = c("Judge", "JCountry")) %>%
  mutate(
    DoD = AvgMatchedDiscrepancy - AvgNonMatchedDiscrepancy
    # Optional: Replace NAs with 0 if that's how you want to treat missing sides
    # AvgMatchedDiscrepancy = replace_na(AvgMatchedDiscrepancy, 0),
    # AvgNonMatchedDiscrepancy = replace_na(AvgNonMatchedDiscrepancy, 0),
    # DoD = AvgMatchedDiscrepancy - AvgNonMatchedDiscrepancy
  )

# Permutation test for Matched-NonMatched: shuffled on JCountry
set.seed(42)
n_permutations = 1000
all_judges <- diving_data %>% distinct(Judge, JCountry)

permuted_results = replicate(n_permutations, {
  # Shuffle judge nationalities
  shuffled_judges = all_judges %>%
    mutate(JCountry_shuffled = sample(JCountry))
  
  # Replace JCountry with the shuffled version
  shuffled_data = diving_data %>%
    left_join(shuffled_judges, by = "Judge") %>%
    mutate(JCountry = JCountry_shuffled) %>%
    select(-JCountry_shuffled)
  
  # Calculate discrepancy
  shuffled_data <- shuffled_data %>%
    mutate(Discrepancy = JScore - AvgScore)
  
  # Matched and non-matched summaries
  matched <- shuffled_data %>%
    filter(Country == JCountry) %>%
    group_by(Judge, JCountry) %>%
    summarise(
      MatchedDivesNo = n(),
      AvgMatchedDiscrepancy = mean(Discrepancy),
      .groups = "drop"
    )
  
  non_matched <- shuffled_data %>%
    filter(Country != JCountry) %>%
    group_by(Judge, JCountry) %>%
    summarise(
      NonMatchedDivesNo = n(),
      AvgNonMatchedDiscrepancy = mean(Discrepancy),
      .groups = "drop"
    )
  
  # Combine both using full_join to include all judges
  result <- full_join(matched, non_matched, by = c("Judge", "JCountry")) %>%
    mutate(DoD = AvgMatchedDiscrepancy - AvgNonMatchedDiscrepancy)
  
  # Replace missing DoD with 0 or NA — up to you
  result$DoD
})

# Calculate p-value for each judge
p_values <- sapply(unique(result$Judge), function(judge) {
  observed_DoD <- result$DoD[result$Judge == judge]
  permuted_DoDs <- permuted_results[result$Judge == judge, ]
  
  # Calculate p-value as the proportion of permuted DoDs that are greater than or equal to the observed DoD
  mean(permuted_DoDs >= observed_DoD, na.rm = TRUE)
})
# Join to result
result <- result %>%
  mutate(p_value = p_values[match(Judge, names(p_values))])

############################
### PARAMETRIC INFERENCE ###
############################

# load data
mlb_data = read_csv("../data/07_tto.csv.gz")

mlb_data = mlb_data %>% 
  mutate(SECOND_TTO = ORDER_CT >= 2) %>%
  mutate(THIRD_TTO = ORDER_CT >= 3)

model1 = lm(EVENT_WOBA_19 ~ SECOND_TTO + THIRD_TTO + WOBA_FINAL_BAT_19
            + WOBA_FINAL_PIT_19 + HAND_MATCH + BAT_HOME_IND, data = mlb_data)
summary(model1)

model2 = lm(EVENT_WOBA_19 ~ ORDER_CT + SECOND_TTO + THIRD_TTO + WOBA_FINAL_BAT_19
            + WOBA_FINAL_PIT_19 + HAND_MATCH + BAT_HOME_IND, data = mlb_data)
summary(model2)

# For Model 1, the p-value for SECOND_TTO is 7.898e-08, indicating a significant effect.
# However, in Model 2, the p-value for SECOND_TTO is greater than 0.05 after adding
# ORDER_CT, suggesting that the effect is no longer significant.

# Not significant, as all efficients about ordering have p-values greater than 0.05.