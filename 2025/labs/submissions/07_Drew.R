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

diving_disc=diving_data %>% 
group_by(Event, Diver, DiveNo, Round) %>%
  mutate(AvgScore = mean(JScore, na.rm = TRUE),
         Disc= JScore - AvgScore)

######################
### FOR ALL JUDGES ###
######################
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
prem_judge = data.frame(judge = judges_1, true_DOD = true_DODs, p_value = p_values)





############################
### PARAMETRIC INFERENCE ###
############################

# load data
mlb_data = read_csv("../data/07_tto.csv.gz")
mlb_data = mlb_data %>% 
  mutate(y = EVENT_WOBA_19,
         oc= ORDER_CT,
         bsn =BATTER_SEQ_NUM,
         hm = HAND_MATCH,
         bh =BAT_HOME_IND,
         tt02= oc >= 2,
         tt03= oc >= 3)
# fit model
model_1= lm( y ~ hm + bh + tt02 + tt03 + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19, data = mlb_data)
summary(model_1)

model_2= lm( y ~ hm + bh + tt02 + tt03 + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19+ bsn, data = mlb_data)
summary(model_2)