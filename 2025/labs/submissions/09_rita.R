#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(9)

#TASK 1

red = 25
n = 111
# yellow - 25, green - 22, orange - 23, brown - 18

#WALD
phat = red / n
se = sqrt((phat * (1 - phat) / n))
lower = phat - 1.96 * se
upper = phat + 1.96 * se
c(lower,upper)

#AC
phat_ac = (red + 2)/(n + 4)
se_ac = sqrt((phat_ac * (1 - phat_ac) / (n + 4)))
lower_ac = phat_ac - 1.96 * se_ac
upper_ac = phat_ac + 1.96 * se_ac
c(lower_ac, upper_ac)

#bootstrap

skittles = c(rep(1, red), rep(0, n - red))
n_boot = 10000
boot_props = numeric(n_boot)

for (i in 1:n_boot) {
  sample = sample(skittles, size = n, replace = TRUE)
  boot_props[i] = mean(sample)}
boot_lower = quantile(boot_props, 0.025)
boot_upper = quantile(boot_props, 0.975)
c(boot_lower, boot_upper)

upper - lower
upper_ac - lower_ac
boot_upper - boot_lower

ggplot(data.frame(boot_props), aes(x = boot_props)) +
  geom_histogram(bins = 30, fill = "red", color = "white", alpha = 0.8) +
  geom_vline(xintercept = boot_lower, linetype = "dashed", color = "purple", size = 1) +
  geom_vline(xintercept = boot_upper, linetype = "dashed", color = "purple", size = 1) +
  labs(title = "Bootstrap Distribution of Sample Prop Red Skittles",
       x = "Bootstrapped Proportion",
       y = "Frequency") +
  theme_minimal()


#######################
### NBA FREE THROWS ###
#######################
#TASK 2

# load data
set.seed(8)
nba_players = read_delim("../data/08_nba-free-throws.csv.gz", delim = ";")

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

total_FTM_all = sum(nba_players25$total_FTM_team, na.rm = TRUE)
total_FTA_all = sum(nba_players25$total_FTA_team, na.rm = TRUE)


n_boot_nba = 1000

bootstrap_ci = function(made, att, n_boot = 1000) {
  sample_vec = c(rep(1, made), rep(0, att - made))
  boot_props = replicate(n_boot, mean(sample(sample_vec, replace = TRUE)))
  quantile(boot_props, c(0.025, 0.975))}

nba_sliced <- nba_sliced %>%
  rowwise() %>%
  mutate(
    boot_ci = list(bootstrap_ci(total_FTM_team, total_FTA_team, n_boot)),
    boot_lower = boot_ci[[1]],
    boot_upper = boot_ci[[2]]
  ) %>%
  ungroup() %>%
  select(-boot_ci)

#compare widths
nba_sliced <- nba_sliced %>%
  mutate(
    wald_width = wald_upper - wald_lower,
    ac_width = ac_upper - ac_lower,
    boot_width = boot_upper - boot_lower
  )
nba_sliced %>%
  select(Player, wald_width, ac_width, boot_width) %>%
  arrange(desc(wald_width))
#the bootstrap width is generally in the middle of the ac and wald CIs

#overlay the bootstrap confidence intervals on the plot
ggplot(nba_sliced, aes(x = reorder(Player, FT_pct), y = FT_pct)) +
  geom_bar(stat = "identity", fill = "violet", color = "black") +
  geom_errorbar(aes(ymin = wald_lower, ymax = wald_upper), width = 0.2, color = "blue", alpha = 0.7) +
  geom_errorbar(aes(ymin = ac_lower, ymax = ac_upper), width = 0.2, color = "maroon", linetype = "dashed", alpha = 0.7) +
  geom_errorbar(aes(ymin = boot_lower, ymax = boot_upper), width = 0.2, color = "green", linetype = "dotdash", alpha = 0.7) +
  coord_flip() +
  labs(title = "NBA Free Throw Percentages with Confidence Intervals",
       x = "Player",
       y = "Free Throw Percentage") +
  theme_minimal()

#coverage
bootstrap_ci <- function(x, n, reps = 20) {
  values <- c(rep(1, x), rep(0, n - x))
  boot_props <- replicate(reps, mean(sample(values, size = n, replace = TRUE)))
  quantile(boot_props, c(0.025, 0.975))
}

simulation <- simulation %>%
  rowwise() %>%
  mutate(
    boot_bounds = list(bootstrap_ci(total_FTM, n)),
    boot_lower = boot_bounds[[1]],
    boot_upper = boot_bounds[[2]]
  ) %>%
  ungroup() %>%
  select(-boot_bounds)

coverage <- simulation %>% 
  mutate(
    wald_covered  = (p >= wald_lower)  & (p <= wald_upper),
    ac_covered    = (p >= ac_lower)    & (p <= ac_upper),
    boot_covered  = (p >= boot_lower)  & (p <= boot_upper)
  ) %>%
  group_by(p, n) %>%
  summarise(
    wald = mean(wald_covered),
    ac   = mean(ac_covered),
    boot = mean(boot_covered),
    .groups = "drop"
  )

coverage_long <- coverage %>%
  pivot_longer(cols = c(wald, ac, boot), names_to = "method", values_to = "coverage")


ggplot(coverage_long, aes(x = p, y = coverage, color = as.factor(n))) +
  geom_line(size = 1) + 
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "violet") +
  facet_wrap(~ method, ncol = 1) +
  labs(title = "Coverage of Confidence Intervals for Free Throw Percentages",
       x = "True Proportion (p)",
       y = "Coverage Probability",
       color = "Sample Size (n)") +
  theme_minimal()