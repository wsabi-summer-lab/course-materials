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
nba_players = read_delim("../data/08_nba-free-throws.csv.gz", delim = ";")

#TASK 1
summary(nba_players)
# view(nba_players)

nba_players25 = nba_players %>%
  group_by(Player) %>% 
  mutate(total_FTM_team = G * FT,
         total_FTA_team = G * FTA,
         total_FTM = sum(total_FTM_team, na.rm = TRUE), total_FTA = sum(total_FTA_team, na.rm = TRUE),
         FT_pct = (total_FTM / total_FTA)) %>% 
  filter(total_FTM >= 25) %>% 
  ungroup()

nba_summary = nba_players25 %>%
  mutate(wald_se = sqrt(FT_pct * (1 - FT_pct) / total_FTA),
         wald_lower = FT_pct - 1.96 * wald_se,
         wald_upper = FT_pct + 1.96 * wald_se)

nba_summary = nba_summary %>% 
  mutate(agresti_n = total_FTA + 4,
         agresti_p = (total_FTM / agresti_n),
         agresti_se = sqrt((agresti_p * (1 - agresti_p)) / agresti_n),
         ac_lower = agresti_p - 1.96 * agresti_se,
         ac_upper = agresti_p + 1.96 * agresti_se)

nba_sliced = nba_summary %>% 
  slice_sample(n = 25)

ggplot(nba_sliced, aes(x = reorder(Player, FT_pct), y = FT_pct)) +
  geom_bar(stat = "identity", fill = "violet", color = "black") +
  geom_errorbar(aes(ymin = wald_lower, ymax = wald_upper), width = 0.2, color = "blue", alpha = 0.7) +
  geom_errorbar(aes(ymin = ac_lower, ymax = ac_upper), width = 0.2, color = "maroon", linetype = "dashed", alpha = 0.7) +
  coord_flip() +
  labs(title = "NBA Free Throw Percentages with Confidence Intervals",
       x = "Player",
       y = "Free Throw Percentage") +
  theme_minimal()

#TASK 2
#discretise interval

interval = seq(0, 1, length.out = 1000)

n_values = c(10, 50, 100, 250, 500, 1000)
reps = 1:100
sim_grid = expand.grid(p = interval, n = n_values, rep = reps)
sim_grid$total_FTM = mapply(function(p, n) rbinom(1, n, p), sim_grid$p, sim_grid$n)
sim_grid$FT_pct = sim_grid$total_FTM / sim_grid$n

simulation = sim_grid %>% 
  mutate(wald_se = sqrt(FT_pct * (1 - FT_pct) / n),
         wald_lower = FT_pct - 1.96 * wald_se,
         wald_upper = FT_pct + 1.96 * wald_se,
         agresti_n = n + 4,
         agresti_p = (total_FTM / agresti_n),
         agresti_se = sqrt((agresti_p * (1 - agresti_p)) / agresti_n),
         ac_lower = agresti_p - 1.96 * agresti_se,
         ac_upper = agresti_p + 1.96 * agresti_se)

coverage = simulation %>% 
  mutate(wald_covered= (p >= wald_lower) & (p <= wald_upper),
         ac_covered = (p >= ac_lower) & (p <= ac_upper)) %>% 
  group_by(p, n) %>% 
  summarise(wald_coverage = mean(wald_covered), ac_coverage = mean(ac_covered),
  .groups = "drop")

coverage_long = coverage %>% 
  pivot_longer(cols = c(wald_coverage, ac_coverage), names_to = "method", values_to = "coverage")

ggplot(coverage_long, aes(x = p, y = coverage, color = as.factor(n)))+
  geom_line(size = 1) + 
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "violet") +
  facet_wrap(~ method, ncol = 1) +
  labs(title = "Coverage of Confidence Intervals for Free Throw Percentages",
       x = "True Proportion (p)",
       y = "Coverage Probability",
       color = "Sample Size (n)") +
  theme_minimal() 
  
