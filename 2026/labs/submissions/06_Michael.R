#############
### SETUP ###
#############

# install.packages(c("dplyr", "ggplot2", "readr"))
library(dplyr)
library(ggplot2)
library(readr)

# set seed
set.seed(7)

setwd("C:/Users/micha/OneDrive/Documents/GitHub/lab-materials/2026/labs/data")
diving <- read.csv("06_diving.csv.gz")

##############
### PART 1 ###
##############

# load data
diving_data = read_csv("../data/06_diving.csv.gz", show_col_types = FALSE)
library(dplyr)

diving <- read.csv("06_diving.csv.gz")

judges <- unique(diving$Judge)
results <- data.frame(Judge = character(), ObsStat = numeric(), PValue = numeric())

set.seed(42)
n_perm <- 1000

for (judge in judges) {
  
  # filter to this judge's rows
  judge_data <- diving %>% filter(Judge == judge)
  
  # observed stat: mean score same country - mean score different country
  same <- judge_data %>% filter(JCountry == Country) %>% pull(JScore)
  diff <- judge_data %>% filter(JCountry != Country) %>% pull(JScore)
  obs_stat <- mean(same) - mean(diff)
  
  # permutation loop
  perm_stats <- numeric(n_perm)
  for (i in 1:n_perm) {
    shuffled <- judge_data %>% mutate(Country = sample(Country))
    same_perm <- shuffled %>% filter(JCountry == Country) %>% pull(JScore)
    diff_perm <- shuffled %>% filter(JCountry != Country) %>% pull(JScore)
    perm_stats[i] <- mean(same_perm) - mean(diff_perm)
  }
  s
  # p-value
  p_val <- mean(abs(perm_stats) >= abs(obs_stat))
  
  results <- rbind(results, data.frame(Judge = judge, ObsStat = obs_stat, PValue = p_val))
}

# BH correction
results$AdjPValue <- p.adjust(results$PValue, method = "BH")

print(results)

# Judges that exhibit evidnece of nationality bias before multiple-testing adjustment are Wang Facheng, Alt Walter, McFarland Steve, Zaitsev Oleg, Barnett Madeleine, Xu Yiming, Seaman Kathy. 
# The judges that still exhibit nationality bias are Wang Facheng, Alt Walter, McFarland Stave, Zaitsev Oleg, Barnett Madeleine, Xu Yiming, Seaman Kathy (all of them survived).   

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


# create indicator variables
tto_data <- tto_data %>%
  mutate(
    tto2 = as.integer(ORDER_CT >= 2),
    tto3 = as.integer(ORDER_CT >= 3)
  )

# Model 1
model1 <- lm(EVENT_WOBA_19 ~ tto2 + tto3 + 
               WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + 
               HAND_MATCH + BAT_HOME_IND, 
             data = tto_data)

summary(model1)

# Model 2
model2 <- lm(EVENT_WOBA_19 ~ BATTER_SEQ_NUM + tto2 + tto3 + 
               WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 + 
               HAND_MATCH + BAT_HOME_IND, 
             data = tto_data)

summary(model1)
#For model 1, tto2 is highly significant (p<0.001), but the tto of p = 0.067 is not significant at 0.05.


summary(model2)
#For model 2, the batter_seq_number is the most significant with a p-value of 0.0007, and tt02 and tt03 become insignificant at alpha 0.05.

#The pitcher decline from one time through the order to the next is not significant. Once batter sequence is factored in, tto2 and tto3 loose signficance showing that number of batters faced is the significant factor. 
