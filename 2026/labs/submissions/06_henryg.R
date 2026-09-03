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

baseline_scores = diving_data %>%
  group_by(Diver, Round, DiveNo, Event) %>%
  summarise(
    baseline = (sum(JScore) - max(JScore) - min(JScore)) / (n() - 2)
  )
baseline_scores

full_table = diving_data %>%
  left_join(baseline_scores, by = c("Diver", "Round", "DiveNo", "Event")) %>%
  mutate(
    Disc = JScore - baseline,
    Match = ifelse(Country == JCountry, 1, 0)
  )
full_table


judge_bias <- full_table %>%
  group_by(Judge, JCountry) %>%
  summarise(
    DoD = mean(Disc[Match == 1]) - mean(Disc[Match == 0])
  )
judge_bias



# Task 2:
# - For each judge, generate a permutation null distribution by shuffling the match labels
#   while holding fixed the judge's discrepancy values
# - Use enough permutations that your p-values are reasonably stable
# - Compute the unadjusted permutation p-value for each judge


B = 1000

perm_test = function(judge_data) {
  #Finding the judge bias again
  obs_DoD = mean(judge_data$Disc[judge_data$Match == 1]) - 
    mean(judge_data$Disc[judge_data$Match == 0])
  
  perm_DoDs = replicate(B, {
    perm_match <- sample(judge_data$Match)
    mean(judge_data$Disc[perm_match == 1]) - 
      mean(judge_data$Disc[perm_match == 0])
  })
  
  p_val = (1 + sum(perm_DoDs >= obs_DoD)) / (B + 1)
  
  return(c(DoD = obs_DoD, p_value = p_val))
}

perm_results = full_table %>%
  group_by(Judge, JCountry) %>%
  group_modify(~ as.data.frame(t(perm_test(.x)))) %>%
  ungroup()




#Adjustment: 

perm_results <- perm_results %>%
  mutate(p_adj = p.adjust(p_value, method = "bonferroni"))

perm_results



#Questions:
#Judges Alt, Barnett, Boothroyd, Boys, Cruz, Huber, Mena, 
#Mcfarland, Ruiz, Xu, and Zaitsev appear biased before adjustment. After
#adjustment, Judges Boothroyd, Boys, Cruz, Geissbuhler, Huber, and Ruiz no
#longer appear biased. 








##############
### PART 2 ###
##############

# load data
tto_data = read_csv("../data/06_tto.csv.gz", show_col_types = FALSE)
tto_data

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

model_1 = lm(EVENT_WOBA_19 ~ I(ORDER_CT >= 2) + I(ORDER_CT >= 3) + 
               WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + HAND_MATCH + BAT_HOME_IND, 
             data = tto_data)
summary(model_1)



model_2 = lm(EVENT_WOBA_19 ~ ORDER_CT + I(ORDER_CT >= 2) + I(ORDER_CT >= 3) + 
               WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + HAND_MATCH + BAT_HOME_IND, 
             data = tto_data)
summary(model_2)


#For the first model, the coefficient for after the first time through the order
#is significant while it isn't the second time. For the second model, neither
#penalty coefficient is significant. This change makes it appear that pitcher 
#decline from one time through the order to the next is not significant and
#is an incorrect explanation for why pitchers get worse throughout the game. As 
#more variables are added, we can see that the "time through the order penalty"
#is not very apparent. 


