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

# Task 2:
# - For each judge, generate a permutation null distribution by shuffling the match labels
#   while holding fixed the judge's discrepancy values
# - Use enough permutations that your p-values are reasonably stable
# - Compute the unadjusted permutation p-value for each judge

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
