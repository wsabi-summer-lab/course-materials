#############
### SETUP ###
#############

# install.packages(c("ggplot2", "splines", "tidyverse"))
library(ggplot2)
library(splines)
library(tidyverse)


#########################
### PERMUTATION TESTS ###
#########################

# load data
diving_data = read_csv("../data/07_diving.csv.gz")

diving_data

# Get mean score for each individual dive 
diving_data = diving_data %>%
  group_by(Event, Round, DiveNo, Diver) %>%
  mutate(mean_score = mean(JScore, na.rm = TRUE)) %>% 
  ungroup()

diving_data

# Create a variable match_country which is 1 if the Country and JCountry match
diving_data = diving_data %>%
  mutate(match_country = ifelse(Country == JCountry, 1, 0))

# Count how many matches we have
n_matches = sum(diving_data$match_country)
n_matches

# Calculate discrepency for each judge on each dive (JScore - mean_score for every dive)
diving_data = diving_data %>%
  group_by(Event, Round, DiveNo) %>%
  mutate(discrepancy = JScore - mean_score) %>% 
  ungroup()

print(diving_data, n=50)

# Get the average discrepency per judge
avg_discrepancy = diving_data %>%
  group_by(Judge) %>%
  summarise(avg_discrepancy = mean(discrepancy, na.rm = TRUE)) 

# Get the average discrepancy for each judge when the country matches
avg_discrepancy_match = diving_data %>%
  filter(match_country == 1) %>%
  group_by(Judge) %>%
  summarise(avg_discrepancy_match = mean(discrepancy, na.rm = TRUE))

# Get the average discrepancy for each judge when the country does not match
avg_discrepancy_no_match = diving_data %>%
  filter(match_country == 0) %>%
  group_by(Judge) %>%
  summarise(avg_discrepancy_no_match = mean(discrepancy, na.rm = TRUE))

# Merge them all back together
avg_discrepancy = avg_discrepancy %>%
  left_join(avg_discrepancy_match, by = "Judge") %>%
  left_join(avg_discrepancy_no_match, by = "Judge")

# Set NA = 0
avg_discrepancy[is.na(avg_discrepancy)] <- 0


print(avg_discrepancy, n=50)

# Calculate the difference in average discrepancy between matched and unmatched
avg_discrepancy = avg_discrepancy %>%
  mutate(DoD = avg_discrepancy_match - avg_discrepancy_no_match)

# Get the number of matched and unmatched dives from diving data for each judge
avg_discrepancy = avg_discrepancy %>%
  mutate(n_matched = sum(diving_data$match_country == 1 & diving_data$Judge == Judge),
         n_unmatched = sum(diving_data$match_country == 0 & diving_data$Judge == Judge))

# Drop those 2 rows if wrong
avg_discrepancy = avg_discrepancy %>%
  select(-n_matched)

# Get the country of each judge
avg_discrepancy = avg_discrepancy %>%
  left_join(diving_data %>% select(Judge, JCountry) %>% distinct(), by = "Judge")


judge_counts <- diving_data %>%                    # one line per judged dive
  group_by(Judge) %>%                              # per judge
  summarise(
    n_matched   = sum(match_country == 1),         # count TRUEs
    n_unmatched = sum(match_country == 0),         # count FALSEs
    .groups = "drop"
  )

judge_counts

avg_discrepancy <- avg_discrepancy %>% 
  left_join(judge_counts, by = "Judge")



print(avg_discrepancy, n=50)


#######################
# Permutation test time
#######################

print(diving_data, n=10)
print(avg_discrepancy, n=25)

# Function to calculate the discrepancy difference
calc_discrepancy_diff <- function(data) {
  data %>%
    group_by(Judge) %>%
    summarise(
      avg_discrepancy_match = mean(discrepancy[match_country == 1], na.rm = TRUE),
      avg_discrepancy_no_match = mean(discrepancy[match_country == 0], na.rm = TRUE),
      new_DoD = avg_discrepancy_match - avg_discrepancy_no_match,
      .groups = "drop"
    )
}

permutations = 1000

# For loop to shuffle match_country and calculate the discrepancy difference
for(judge in diving_data$Judge %>% unique()) {
  # Create vector to hold discrepancy differences
  new_DoDs <- c()
  
  # Get true for the judge
  true_DoD <- avg_discrepancy$DoD[avg_discrepancy$Judge == judge]
  
  for(i in 1:permutations) {
    shuffled_data <- diving_data %>%
      filter(Judge == judge) %>%
      mutate(match_country = sample(match_country))  # Shuffle match_country
    
    discrepancy_diff <- calc_discrepancy_diff(shuffled_data)
    
    # Add new_DoD to the vector
    new_DoDs <- c(new_DoDs, discrepancy_diff$new_DoD[discrepancy_diff$Judge == judge])
  }
  new_DoDs <- as.numeric(new_DoDs)
  #new_DoDs
  
  #true_DoDs
  
  
  # Calculate the p-value for each judge using vector of discrepancies
  p_value_count = sum(abs(new_DoDs) >= abs(true_DoDs))
  
  #p_value_count
  
  p_value <- p_value_count / permutations
  
  avg_discrepancy <- avg_discrepancy %>% 
    mutate(p_value = if_else(Judge == judge, !!p_value, .data$p_value))
  
  #avg_discrepancy
}

print(avg_discrepancy, n=25)


############################
### PARAMETRIC INFERENCE ###
############################

# load data
mlb_data = read_csv("../data/07_tto.csv.gz")

mlb_data

# Make a binary column for each TTO per ab (TTO1 is 1-9, TTO2 is 10-18, TTO3 is 19-27), BATTER_SEQ_NUM is the sequence number of the batter in the TTO
mlb_data = mlb_data %>%
  mutate(
    TTO1 = ifelse(BATTER_SEQ_NUM <= 9, 1, 0),
    TTO2 = ifelse(BATTER_SEQ_NUM > 9 & BATTER_SEQ_NUM <= 18, 1, 0),
    TTO3 = ifelse(BATTER_SEQ_NUM > 18 & BATTER_SEQ_NUM <= 27, 1, 0)
  )

model1 = lm(EVENT_WOBA_19 ~ factor(TTO1) + factor(TTO2) + factor(TTO3) + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + factor(BAT_HOME_IND) + factor(HAND_MATCH), data = mlb_data)

model2 = lm(EVENT_WOBA_19 ~ BATTER_SEQ_NUM + factor(TTO1) + factor(TTO2) + factor(TTO3) + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + factor(BAT_HOME_IND) + factor(HAND_MATCH), data = mlb_data)

summary(model1)

summary(model2)

# P-value interpretation: The TTO's have p-values of well over 0.05, indicating these are not statistically significant predictors of the event WOBA.
# We can reject the idea that TTO has a significant effect on the event WOBA.