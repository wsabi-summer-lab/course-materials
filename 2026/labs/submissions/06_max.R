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

diving_data <- diving_data %>%
  group_by(Event, Round, Diver, Country, Rank, DiveNo, Difficulty) |> 
  mutate(mean = mean(JScore), discrepancy = JScore - mean) |>
  ungroup() |>
  mutate(match = Country == JCountry)

dod <- function(discrepancy, match)
  mean(discrepancy[match]) - mean(discrepancy[!match])

perm_test_judge <- function(judge, B = 10000) {
  d <- judge$discrepancy; m <- judge$match
  obs <- dod(d, m); n <- length(m); k <- sum(m)
  perm <- replicate(B, { idx <- sample.int(n, k); mean(d[idx]) - mean(d[-idx]) })
  tibble(n_dives = n, n_match = k, observed = obs,
         p_unadj = (1 + sum(perm >= obs)) / (B + 1))
}

mcfarland <- diving_data |> filter(Judge == "McFARLAND Steve")
perm_test_judge(mcfarland)

results <- diving_data |>
  group_by(Judge) |>
  group_modify(~ perm_test_judge(.x)) |>
  ungroup() |>
  filter(n_match > 0, n_match < n_dives)

results <- results |>
  mutate(p_bh = p.adjust(p_unadj, method = "BH"))

results

results |> filter(p_unadj < 0.05) |> arrange(p_unadj)
#Lowest unadjusted p: Walter Alt(0.0001), Madeleine Barnett(0.0001), Oleg Zaitsev(0.0001)

results |> filter(p_bh   < 0.05) |> arrange(p_bh)
#Lowest adjusted p: Walter Alt(0.00057), Madeleine Barnett(0.00057), Oleg Zaitsev(0.00057)

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

m1 <- lm(EVENT_WOBA_19 ~ I(ORDER_CT >= 2) + I(ORDER_CT >= 3) +
           WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + HAND_MATCH + BAT_HOME_IND,
         data = tto_data)
m2 <- lm(EVENT_WOBA_19 ~ BATTER_SEQ_NUM + I(ORDER_CT >= 2) + I(ORDER_CT >= 3) +
           WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + HAND_MATCH + BAT_HOME_IND,
         data = tto_data)
summary(m1)
summary(m2)

#In model 1, all p vlues are very small except for the one for the 3rd time through the order, suggesting that that coefficient is most likely to be 0 at 6.7%, not significant at 0.05
#In model 2, the 2nd time through the order coefficient has a high p of 0.76, meaning that is has a high likelihood of being 0 and the third time through the order is the next highest at 9.8%, both coefficients are not significant at 0.05.

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
