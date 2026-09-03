#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
# install.packages(c("binom","boot"))
library(binom)
# set seed
set.seed(9)
#######################
######  M & M's #######
#######################

#––– 1. Specify your observed counts
n = 116   # total M&Ms
b =  22   # number of Red M&Ms
# sample proportion
p_hat = b / n
#––– 2. Create a vector of 1's and 0's
mm_vector = c(rep(1, 22), rep(0, 116 - 22))#––– 
# 3. Resample 1000 times with replacement
mm_sample = replicate(1000, sample(mm_vector, n, replace = TRUE))
#––– 4. Estimate the proportion of Red M&Ms
mm_prop = colMeans(mm_sample)
#––– 5. plot mm_prop as histogram
ggplot(data = data.frame(mm_prop), aes(x = mm_prop)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Proportion of Red M&Ms", x = "Proportion", y = "Frequency") +
  theme_minimal()
# Find the 2.5th quartile and 97.5th quartile of the distribution mm_prop and add a red line at each quartile
mm_prop_quantiles = quantile(mm_prop, probs = c(0.025, 0.975))
ggplot(data = data.frame(mm_prop), aes(x = mm_prop)) +
  geom_histogram(bins = 30, fill = "red", color = "black") +
  geom_vline(xintercept = mm_prop_quantiles[1], color = "blue", linetype = "dashed") +
  geom_vline(xintercept = mm_prop_quantiles[2], color = "blue", linetype = "dashed") +
  labs(title = "Proportion of Red M&Ms with Quantiles", x = "Proportion", y = "Frequency") +
  theme_minimal()
#––– 6. Construct a 95% Wald and Agresti-Coull confidence interval for the true probability Mars, Inc. makes of producing a red M&M
n = 116   # total M&Ms
b =  22   # number of Red M&Ms
# sample proportion
p_hat = b / n
wald_lower = p_hat - 1.96 * sqrt((p_hat * (1 - p_hat)) / 116)
wald_upper = p_hat + 1.96 * sqrt((p_hat * (1 - p_hat)) / 116)
wald_ci = c(wald_lower, wald_upper)
# Agresti-Coull adjustment
n_adj = 116+ 4
p_adj = ( 116 + 2) / n_adj
se_adj = sqrt((p_adj * (1 - p_adj)) / n_adj)
agresti_coull_lower = p_adj - 1.96 * se_adj
agresti_coull_upper = p_adj + 1.96 * se_adj
agresti_coull_ci = c(agresti_coull_lower, agresti_coull_upper)
#––– 7.Find the width of the wald_ci, agresti_coull_ci confidence, and mm_prop_quantiles intervals
wald_ci_width = diff(wald_ci)
agresti_coull_ci_width = diff(agresti_coull_ci)
mm_prop_quantiles_width = diff(mm_prop_quantiles)

#######################
### NBA FREE THROWS ###
#######################

# load data
nba_players = read_delim("../data/09_nba-free-throws.csv.gz", delim = ";")
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
# for each player make a vector 1's for TFTM and 0's for TFTA
nba_players2 = nba_players %>%
  mutate(
    FT_vec = map2(TFTM, TFTA, ~ c(rep(1, .x), rep(0, .y - .x))),
    FT_vec = map(FT_vec, ~ sample(.x, length(.x), replace = FALSE))
    
  )

### JP
player_bootstrap = tibble()

for (p in nba_players$Player) {
  player = p
  
  player_data = nba_players %>% filter(Player == player)
  
  player_fts = c(rep(1, player_data$TFTM), rep(0, player_data$TFTA - player_data$TFTM))
  player_sample = replicate(1000, sample(player_fts, length(player_fts), replace = TRUE))
  player_prop = colMeans(player_sample)
  
  player_mean = mean(player_prop)
  player_lower = quantile(player_prop, probs = 0.025)
  player_upper = quantile(player_prop, probs = 0.975)
  
  player_info = tibble(player, lower = player_lower, mean = player_mean, upper = player_upper)
  
  player_bootstrap = bind_rows(player_bootstrap, player_info)
}

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
# join nba_players and player_bootstrap
nba_players4 = nba_players %>%
  left_join(player_bootstrap, by = c("Player" = "player"))
# now just show players, lower, mean, upper, CI_lower, CI_upper, CI_Lower_T, CI_Upper_T in nba_players4 
nba_players4 = nba_players4 %>%
  select(Player, lower, mean, upper, CI_Lower, CI_Upper, CI_Lower_T, CI_Upper_T)
#Player Widths
nba_players4 = nba_players4 %>%
  mutate(
    Wald_Width = CI_Upper - CI_Lower,
    Agresti_Width = CI_Upper_T - CI_Lower_T,
    Bootstrap_Width = upper - lower
  )
# Create a tibble with the intervals and their widths
nba_player5 = nba_players4 %>%
  mutate(
    Wald_Width = CI_Upper - CI_Lower,
    Agresti_Width = CI_Upper_T - CI_Lower_T,
    Bootstrap_Width = upper - lower
  )
# plot Wald_Width, Agresti_Width, Bootstrap_Width against each other in nba_players5
ggplot(nba_player5, aes(x = Player)) +
  geom_bar(aes(y = Wald_Width), stat = "identity", fill = "blue", alpha = 0.5) +
  geom_bar(aes(y = Agresti_Width), stat = "identity", fill = "red", alpha = 0.5) +
  geom_bar(aes(y = Bootstrap_Width), stat = "identity", fill = "green", alpha = 0.5) +
  labs(title = "Widths of Confidence Intervals for NBA Players",
       x = "Player",
       y = "Width") +
  theme_minimal() +
  coord_flip()
# change the plot to only show 30 players
nba_player5 = nba_player5 %>%
  slice_sample(n = 30)
# plot Wald_Width, Agresti_Width, Bootstrap_Width against each other in nba_players5
ggplot(nba_player5, aes(x = Player)) +
  geom_bar(aes(y = Wald_Width), stat = "identity", fill = "blue", alpha = 0.5) +
  geom_bar(aes(y = Agresti_Width), stat = "identity", fill = "red", alpha = 0.5) +
  geom_bar(aes(y = Bootstrap_Width), stat = "identity", fill = "green", alpha = 0.5) +
  labs(title = "Widths of Confidence Intervals for NBA Players (Sampled)",
       x = "Player",
       y = "Width") +
  theme_minimal() +
  coord_flip()