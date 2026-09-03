#############
### SETUP ###
#############

# install.packages(c("dplyr", "ggplot2", "readr"))
library(dplyr)
library(ggplot2)
library(readr)

##############
### PART 1 ###
##############

# load data
diving_data = read_csv("../data/06_diving.csv.gz", show_col_types = FALSE)

diving_data = diving_data %>%
  group_by(Event, Round, Diver, DiveNo) %>%
  mutate(dive_mean = mean(JScore, trim = 0.15), num = n()) %>%
  ungroup() %>%
  mutate(diff = JScore - dive_mean, match = (Country == JCountry))


judge_list = diving_data %>%
  group_by(Judge) %>%
  summarize(
    country = first(JCountry),
    total = n(), 
    matches = sum(match==TRUE),
    non_matches = sum(match==FALSE),
    DoD_obs = mean(diff[match==TRUE],trim=0) - mean(diff[match==FALSE],trim=0)
    )

#Permutation test stuff, single example for McFarland

# judge_name <- "McFARLAND Steve"
# 
# df_judge <- diving_data %>%
#   filter(Judge == judge_name)
# 
# DoD_obs <- mean(df_judge$diff[df_judge$match], trim = 0) -
#   mean(df_judge$diff[!df_judge$match], trim = 0)
# 
# n_match     <- sum(df_judge$match)
# n_nonmatch  <- sum(!df_judge$match)
# N           <- nrow(df_judge)
# 
# B <- 5000       # number of permutations
# 
# perm_DoD <- numeric(B)
# 
# for (b in 1:B) {
# 
#   # randomly assign which dives are "match"
#   perm_labels <- sample(c(rep(TRUE, n_match),
#                           rep(FALSE, n_nonmatch)))
# 
#   # compute permuted DoD
#   perm_DoD[b] <- mean(df_judge$diff[perm_labels], trim = 0) -
#     mean(df_judge$diff[!perm_labels], trim = 0)
# }
# 
# p_value <- mean(perm_DoD >= DoD_obs)


#P-values for all of them

perm_test_DoD <- function(df, judge, B = 5000) {
  
  # set seed
  set.seed(7)
  
  df_judge <- df %>% filter(Judge == judge)
  
  DoD_obs <- mean(df_judge$diff[df_judge$match], trim = 0) -
    mean(df_judge$diff[!df_judge$match], trim = 0)
  
  n_match    <- sum(df_judge$match)
  n_nonmatch <- sum(!df_judge$match)
  
  perm_DoD <- replicate(B, {
    perm_labels <- sample(c(rep(TRUE, n_match),
                            rep(FALSE, n_nonmatch)))
    mean(df_judge$diff[perm_labels], trim = 0) -
      mean(df_judge$diff[!perm_labels], trim = 0)
  })
  
  p_value <- mean(perm_DoD >= DoD_obs)
  
  tibble(
    Judge = judge,
    p_value = p_value
  )
  
}

perm_test_DoD(diving_data, "McFARLAND Steve")


p_values <- diving_data %>%
  distinct(Judge) %>%
  rowwise() %>%
  mutate(p_value = perm_test_DoD(diving_data, Judge)$p_value) %>%
  ungroup()

judge_list <- judge_list %>%
  left_join(p_values, by = "Judge")


#Adjust p-values

judge_list <- judge_list %>%
  mutate(p_value_BH = p.adjust(p_value, method = "BH"), #Benjamini Hochberg
         p_value_bonf = p.adjust(p_value, method = "bonferroni")) #Bonferroni 

#If we use alpha = 0.05

# Alt, Barnett, Burk, Mena, Zaitsev, Geissbuhler, McFarland, Xu, Cruz, Boothroyd, Ruiz-Pedreguera, Huber, Boys
# are biased under the unadjusted

# the same list for the BH adjustment

# Alt, Barnett, Burk, Mena, Zaitsev, Geissbuhler, McFarland, Xu
# are biased under bonferroni



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


model1 = lm(EVENT_WOBA_19 ~ (ORDER_CT >= 2) + (ORDER_CT >= 3) + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + HAND_MATCH + BAT_HOME_IND,
  data = tto_data)

model2 = lm(EVENT_WOBA_19 ~ BATTER_SEQ_NUM + (ORDER_CT >= 2) + (ORDER_CT >= 3) + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + HAND_MATCH + BAT_HOME_IND
  ,data = tto_data)


summary(model1)
summary(model2)

#The p-values are significant for everything (excepted TTO >= 3 which is slightly over 0.05) in model1
#While both TTO counts are insignificant in model2

#This means that pitcher decline is more linear relative to batter sequence number than it is with TTO


