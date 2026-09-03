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

diving_data <- diving_data %>%
  mutate(
    matched = Country == JCountry
  )


diving_data <- diving_data %>%
  group_by(Event, Round, Diver, DiveNo) %>%
  mutate(
    discrepancy = abs(
      JScore -
        (sum(JScore) - JScore) / (n() - 1)
    )
  ) %>%
  ungroup()


# Task 2:
# - For each judge, generate a permutation null distribution by shuffling the match labels
#   while holding fixed the judge's discrepancy values
# - Use enough permutations that your p-values are reasonably stable
# - Compute the unadjusted permutation p-value for each judge
judges <- unique(diving_data$Judge)

B <- 10000

results <- data.frame()

for(j in judges){
  
  judge_data <- diving_data %>%
    filter(Judge == j)
  
  n_total <- nrow(judge_data)
  n_matched <- sum(judge_data$matched)
  
  if(n_matched == 0 | n_matched == n_total) next
  
  obs_DoD <- judge_data %>%
    summarise(
      DoD =
        mean(discrepancy[matched]) -
        mean(discrepancy[!matched])
    ) %>%
    pull(DoD)
  
  perm_DoD <- numeric(B)
  
  for(b in 1:B){
    
    random_match <- rep(FALSE, n_total)
    
    random_match[
      sample(n_total, n_matched)
    ] <- TRUE
    
    perm_DoD[b] <-
      mean(judge_data$discrepancy[random_match]) -
      mean(judge_data$discrepancy[!random_match])
  }
  
  pval <- mean(abs(perm_DoD) >= abs(obs_DoD))
  
  results <- rbind(
    results,
    data.frame(
      Judge = j,
      n_total = n_total,
      n_matched = n_matched,
      observed_DoD = obs_DoD,
      p_value = pval
    )
  )
}

results %>%
  arrange(p_value)
# Task 3:
# - Adjust the judge-level p-values for multiple testing
# - A good default is p.adjust(..., method = "BH")
# - Report both:
#   * the unadjusted p-values
#   * the adjusted p-values

results %>% 
  mutate(adjusted_pvalues = p.adjust(results$p_value, method = "BH")) %>%
  arrange(adjusted_pvalues)
# Task 4:
# - Identify which judges show evidence of nationality bias before adjustment
# - Identify which judges still show evidence after adjustment
# - Make at least one plot that helps explain the strongest case(s)
#using a p value threshold of .05, judges Geissbuhler, Xu and Boussard seem to show evidence of nationality bias. 
#After the adjustment, none of them show statistically significant evidence of nationality bias. 

obs_DoD <- results$observed_DoD[
  results$Judge == "GEISSBUHLER Michael"
]
hist(
  perm_DoD,
  breaks = 30,
  xlim = c(min(perm_DoD), 0.5),
  col = "lightgray",
  border = "white"
)

abline(v = obs_DoD,
       col = "red",
       lwd = 3)

##############
### PART 2 ###
##############

# load data
tto_data = read_csv("../data/06_tto.csv.gz", show_col_types = FALSE)

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

model_1 = lm(EVENT_WOBA_19 ~ I(ORDER_CT >= 2) + I(ORDER_CT >= 3) + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + HAND_MATCH + BAT_HOME_IND , data = tto_data)
# Task 2:
# - Fit Model 2 from the lab handout using lm()
# - Model 2 adds a linear term in ORDER_CT on top of the Model 1 controls
model_2 = lm(EVENT_WOBA_19 ~ ORDER_CT + I(ORDER_CT >= 2) + I(ORDER_CT >= 3) + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + HAND_MATCH + BAT_HOME_IND, data = tto_data)

# Task 3:
# - Run summary(...) on both models
# - Extract the coefficient estimates, standard errors, test statistics, and p-values
# - Interpret the coefficients tied to pitcher decline across times through the order
summary(model_1)
summary(model_2)

summary(model_1)$coefficients
summary(model_2)$coefficients

#There is an increase in an at bat WOBA probability 2 times through the order and the 3rd time too, likely showing pitcher decline. 


# Task 4:
# - State whether the estimated decline from one time through the order to the next
#   is statistically significant
# - Explain how the answer changes, if at all, between Model 1 and Model 2
#The second time through is statistically significant, but the 3rd isn't for model 1. For model 3, both time through order and 2nd time are not significant. 