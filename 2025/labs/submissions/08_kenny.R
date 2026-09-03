library(ggplot2)
library(tidyverse)
library(broom)

set.seed(8)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

# 8.1 NBA Free Throws

nba_players = read.csv.gz2("08_nba-free-throws.csv.gz")

head(nba_players)

nba_players <- nba_players %>%
  mutate(
    FT = as.numeric(FT),
    G = as.numeric(G),
    Total_Free_Throws = FT * G
  ) %>%
  mutate(
  FTA = as.numeric(FTA),
  Total_Free_Throw_Attempts = FTA * G
)

selected_data <- nba_players %>%
  select(Player, Total_Free_Throws, Total_Free_Throw_Attempts, G) %>%
  group_by(Player) %>%
  summarise(
    Total_FT = sum(Total_Free_Throws, na.rm = TRUE),
    Total_FTA = sum(Total_Free_Throw_Attempts, na.rm = TRUE),
    Total_G = sum(G, na.rm = TRUE),
    .groups = "drop"
) %>%
  mutate(Free_Throw_Percentage = Total_FT / Total_FTA * 100) %>%
  filter(Total_FT > 25)

selected_data$Total_FT = round(selected_data$Total_FT, 0)
selected_data$Total_FTA = round(selected_data$Total_FTA, 0)

ci_data <- selected_data %>%
  drop_na() %>%
  mutate(
    p_hat = Free_Throw_Percentage/100,
    n = Total_FTA,

    wald_se = sqrt(p_hat * (1 - p_hat) / n),
    wald_lower = p_hat - 1.96 * wald_se,
    wald_upper = p_hat + 1.96 * wald_se,

    z = 1.96,
    n_ac = n + z^2,
    p_ac = (Total_FT + z^2 / 2) / n_ac,
    ac_se = sqrt(p_ac * (1 - p_ac) / n_ac),
    agresti_lower = p_ac - z * ac_se,
    agresti_upper = p_ac + z * ac_se
  ) %>%
  slice_sample(n = 20)


ggplot(ci_data, aes(x = p_hat, y = reorder(Player, p_hat))) +
  geom_point(alpha = 0.4) +
  geom_errorbarh(aes(xmin = wald_lower, xmax = wald_upper), color = "blue", height = 0.2, alpha = 0.5) +
  geom_errorbarh(aes(xmin = agresti_lower, xmax = agresti_upper), color = "red", height = 0.2, alpha = 0.5) +
  labs(
    x = "Free Throw Percentage",
    y = "Player",
    title = "Sampled Wald (Blue) and Agresti-Coull (Red) 95% CIs for FT%"
  ) +
  theme_minimal()

# 8.2 Simulation Study

interval <- seq(0,1, by = 0.001)

n <- c(10, 50, 100, 250, 500, 1000)

sim_data <- expand.grid(p = interval, n = n) %>%
  mutate(
    wald_lower = p - 1.96 * sqrt(p * (1 - p) / n),
    wald_upper = p + 1.96 * sqrt(p * (1 - p) / n),
    agresti_lower = (p * n + 1.96^2 / 2) / (n + 1.96^2) - 1.96 * sqrt((p * (1 - p) + 1.96^2 / (4 * n)) / (n + 1.96^2)),
    agresti_upper = (p * n + 1.96^2 / 2) / (n + 1.96^2) + 1.96 * sqrt((p * (1 - p) + 1.96^2 / (4 * n)) / (n + 1.96^2))
)

ggplot(sim_data, aes(x = p)) +
  geom_ribbon(aes(ymin = wald_lower, ymax = wald_upper, fill = "Wald"), alpha = 0.3) +
  geom_ribbon(aes(ymin = agresti_lower, ymax = agresti_upper, fill = "Agresti-Coull"), alpha = 0.3) +
  scale_fill_manual(values = c("Wald" = "blue", "Agresti-Coull" = "red")) +
  labs(
    x = "True Proportion (p)",
    y = "Confidence Interval",
    title = "Wald and Agresti-Coull Confidence Intervals for Varying Sample Sizes"
  ) +
  theme_minimal() +
  facet_wrap(~n, scales = "free_y")

coverage_summary <- sim_data %>%
  group_by(n, p) %>%
  summarise(
    wald_coverage = mean(wald_cover),
    agresti_coverage = mean(agresti_cover),
    .groups = "drop"
  )

coverage_long <- coverage_summary %>%
  pivot_longer(
    cols = c(wald_coverage, agresti_coverage),
    names_to = "method",
    values_to = "coverage"
  ) %>%
  mutate(method = recode(method,
                         "wald_coverage" = "Wald",
                         "agresti_coverage" = "Agresti-Coull"))

ggplot(coverage_long, aes(x = p, y = coverage, color = method)) +
  geom_line(alpha = 0.8) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray40") +
  facet_wrap(~ n, ncol = 2, scales = "free_y") +
  labs(
    title = "Empirical 95% Confidence Interval Coverage",
    x = "True Proportion (p)",
    y = "Coverage",
    color = "Interval Method"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12)) +
  scale_color_manual(values = c("Wald" = "red", "Agresti-Coull" = "blue"))

# 8.3 

# Assume that you place n bets on -110 odds (meaning you bet $110 to win $100 each time). 
# Answer the following questions

# 1. What is the break-even success percentage for each bet?
# (i.e. the probability of winning that you need to achieve to make $0 profit)

p = 110/210

# 2. Discretize the interval [pBE , 1] into 100 equally spaced points. 
# At each point, find how many bets you’d need to make to be confident that 
# you’re actually profitable (ex: that the confidence interval for your true success rate exceeds pBE). 
# Plot this as a function of p

p_bets <- seq(p, 0.999, length.out = 100)

alpha <- 0.05

n_bets <- numeric()

for (i in 1:length(p_bets)) {
  
  p_hat <- p_bets[i]
  
  n_needed <- log(alpha)/log(1-p_hat)
  
  n_bets[i] <- n_needed
}

n_bets

df <- data.frame(
  p_bets = p_bets,
  n_bets = n_bets
)

ggplot(df, aes(x = p_bets, y = n_bets)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Number of Bets Needed vs. True Probability",
    x = "True Win Probability (p)",
    y = "Number of Bets Needed (log scale)") +
  theme_minimal()

