#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(broom)

# set seed
set.seed(8)

#######################
### NBA FREE THROWS ###
#######################

# load data
nba_players = read.csv("../data/08_nba-free-throws.csv.gz",sep = ";")
#group players together
nba_players = nba_players %>%
  mutate(
    TFTM_Team = G * FT, # total games played
    TFTA_Team = G * FTA, # total free throw attempts
  )
nba_players = nba_players %>%
  group_by(Player) %>%
  summarise(sum(TFTM_Team, na.rm = TRUE), 
            sum(TFTA_Team, na.rm = TRUE))
#filter to only include players with TFTA_Team >= 25
nba_players = nba_players %>%
  filter(`sum(TFTA_Team, na.rm = TRUE)` >= 25) %>%
  rename(
    TFTM = `sum(TFTM_Team, na.rm = TRUE)`,
    TFTA = `sum(TFTA_Team, na.rm = TRUE)`
  )


#Confidence Interval

nba_players=nba_players %>%
  mutate(
    FT_Percentage = TFTM / TFTA,
    SE = sqrt((FT_Percentage * (1 - FT_Percentage)) / TFTA),
    CI_Lower = FT_Percentage - 1.96 * SE,
    CI_Upper = FT_Percentage + 1.96 * SE
  )

nba_players = nba_players %>%
  mutate(NT= TFTA + 4,
         PT = (TFTM + 2)/NT,
         SE_T = (sqrt((PT * (1 - PT))) / NT),
         CI_Lower_T = (PT - 1.96) * SE_T,
         CI_Upper_T = (PT + 1.96) * SE_T
  )

nba_players = nba_players %>%
  slice_sample(n=30)

#plot 
ggplot(nba_players, aes(x = reorder(Player, -FT_Percentage), y = FT_Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color="orange", alpha = 0.5) +
  geom_errorbar(aes(ymin = CI_Lower_T, ymax = CI_Upper_T), width = 0.2, alpha = 0.5) +
  labs(title = "NBA Players Free Throw Percentage with Confidence Intervals",
       x = "Player",
       y = "Free Throw Percentage") +
  theme_minimal() +
  coord_flip()


####################
##### TASK 2 #######
####################


# discretize the interval the open interval(0,1) into 1000 intervals

p_vec = seq(0, 1, length.out = 1000)
# remove 0 and 1
p_vec = p_vec[-c(1, 1000)]
n_vec = c(10, 50, 100, 250, 500, 1000)
M=100

# Prepare an empty data frame to collect results
results <- data.frame(
  n = integer(),
  p = numeric(),
  Wald = numeric(),
  Agresti_Coull = numeric(),
  stringsAsFactors = FALSE
)
# Loop through each sample size
for (n in n_vec) {
  # Loop through each probability
  for (p in p_vec) {
    # Simulate M binomial samples
    x =  rbinom(M, size = n, prob = p)
    phat = x / n
    
    # Calculate the Wald CI

    wald_lower = phat - 1.96 * sqrt((phat * (1 - phat)) / n)
    wald_upper = phat + 1.96 * sqrt((phat * (1 - phat)) / n)
    cov_w <- mean((wald_lower <= p) & (p <= wald_upper))
    
    
    
    # Calculate the Agresti-Coull CI
    n_adj = n + 4
    p_adj = ( n + 2) / n_adj
    se_adj = sqrt((p_adj * (1 - p_adj)) / n_adj)
    agresti_coull_lower = p_adj - 1.96 * se_adj
    agresti_coull_upper = p_adj + 1.96 * se_adj
    cov_ac <- mean((agresti_coull_lower <= p) & (p <= agresti_coull_upper))
                                   
    
    # Append results to the data frame
    results <- rbind(results, data.frame(
      n = n,
      p = p,
      Wald = paste0("(", round(wald_lower, 3), ", ", round(wald_upper, 3), ")"),
      Agresti_Coull = paste0("(", round(agresti_coull_lower, 3), ", ", round(agresti_coull_upper, 3), ")")
    ))
  }
}
# Convert to long format for plotting
results_long <- pivot_longer(
  results,
  cols = -c(n, p),
  names_to = "method",
  values_to = "coverage"
)

# Plot faceted by n
ggplot(results_long, aes(x = p, y = coverage, color = method)) +
  geom_line() +
  facet_wrap(~ n, scales = "free_y") +
  labs(title = "Coverage Probability of Confidence Intervals",
       x = "True Probability (p)",
       y = "Coverage Probability") +
  theme_minimal() +
  scale_color_manual(values = c("Wald" = "blue", "Agresti_Coull" = "red")) +
  theme(legend.title = element_blank())

