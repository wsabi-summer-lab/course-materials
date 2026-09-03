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




#TASK 1

diving_data = read_csv("../data/07_diving.csv.gz")

diving_disc = diving_data %>%
  group_by(Event, Diver, DiveNo, Round) %>%
  mutate(Avg_score = mean(JScore, na.rm = TRUE),
         Disc = JScore - Avg_score)

judge1 = "CRUZ Julia"

disc_match = diving_disc %>%
  filter(Judge == judge1) %>%
  mutate(country_match = Country == JCountry) %>% 
  group_by(country_match) %>% 
  summarise(mean_disc = mean(Disc, na.rm = TRUE))

match_disc = disc_match %>%  filter(country_match == TRUE) %>% select(mean_disc) %>% pull()
unmatch_disc = disc_match %>% filter(country_match == FALSE) %>% select(mean_disc) %>% pull()

DOD_true = match_disc - unmatch_disc

DOD_list = c()

for(i in 1:1000) {
  
  set.seed(i)
  # randomly assign diver country codes
  diving_disc$Country = sample(diving_data$Country)
  
  disc_match = diving_disc %>% 
    filter(Judge == judge1) %>% 
    mutate(country_match = Country == JCountry) %>% 
    group_by(country_match) %>% 
    summarise(mean_disc = mean(Disc, na.rm = TRUE))
  
  match_disc = disc_match %>% filter(country_match == TRUE) %>% select(mean_disc) %>% pull()
  unmatch_disc = disc_match %>% filter(country_match == FALSE) %>% select(mean_disc) %>% pull()
  
  DOD = match_disc - unmatch_disc
  
  DOD_list = c(DOD_list, DOD)
}


plot = ggplot(data = data.frame(DOD_list), aes(x = DOD_list)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = DOD_true, color = "red", linetype = "dashed") +
  labs(title = "Distribution of Diver's Discrepancy (DOD)",
       x = "Diver's Discrepancy (DOD)",
       y = "Frequency") +
  theme_minimal()
plot

# calculate p-value
p_value = mean(DOD_list >= DOD_true)
p_value

DOD_list

true_DODs = c()
p_values = c()
# get unique judges
judges_1 = unique(diving_disc$Judge)

# loop over judges
for(j in judges_1) {
  # assign judge we're looking at
  judge_1 = j
  
  # true discrepancy
  disc_match = diving_disc %>% 
    filter(Judge == judge1) %>% 
    mutate(country_match = Country == JCountry) %>% 
    group_by(country_match) %>% 
    summarise(mean_disc = mean(Disc, na.rm = TRUE))
  
  match_disc =disc_match %>% filter(country_match == TRUE) %>% select(mean_disc) %>% pull()
  unmatch_disc = disc_match %>% filter(country_match == FALSE) %>% select(mean_disc) %>% pull()
  
  DOD_true = match_disc - unmatch_disc
  
  true_DODs = c(true_DODs, DOD_true)
  # define empty dod vector to store permutations
  DOD_looped = c()
  
  # permutation test
  for(iter in 1:1000) {
    # randomly assign diver country codes
    diving_disc$Country = sample(diving_data$Country)
    
    # calculate difference of discrepancy
    disc_match = diving_disc %>% 
      filter(Judge == judge1) %>% 
      mutate(country_match = Country == JCountry) %>% 
      group_by(country_match) %>% 
      summarise(mean_disc = mean(Disc, na.rm = TRUE))
    
    match_disc =disc_match %>% filter(country_match == TRUE) %>% select(mean_disc) %>% pull()
    unmatch_disc = disc_match %>% filter(country_match == FALSE) %>% select(mean_disc) %>% pull()
    
    # calculate perm DOD
    DOD = match_disc - unmatch_disc
    # assign to vector
    DOD_looped = c(DOD_looped, DOD)
  }
  # calculate p value
  p_value = mean(DOD_looped >= DOD_true)
  # save p value
  p_values = c(p_values, p_value)
}

data.frame(judge = judges_1, true_DOD = true_DODs, p_value = p_values)

#TASK 2
############################
### PARAMETRIC INFERENCE ###
############################

# load data
mlb_data = read_csv("../data/07_tto.csv.gz")

mlb_data = mlb_data %>% 
  mutate(tto2 = ifelse(BATTER_SEQ_NUM >= 9, 1, 0),
         tto3 = ifelse(BATTER_SEQ_NUM >= 18, 1, 0))

model1 = lm(EVENT_WOBA_19 ~ factor(tto2) + factor(tto3) + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 +
              factor(HAND_MATCH) + factor(BAT_HOME_IND), data = mlb_data)
summary(model1)

model2 = lm(EVENT_WOBA_19 ~ BATTER_SEQ_NUM + factor(tto2) + factor(tto3) + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 +
              factor(HAND_MATCH) + factor(BAT_HOME_IND), data = mlb_data)
summary(model2)

#the p-values are the probability of seeing a value this extreme given the distribution that we have. 
#all the p-values as very small except for handedness, which would mean there isn't significant evidence
#that it causes a significant change in the wOBA.
#pitcher decline IS significant.


