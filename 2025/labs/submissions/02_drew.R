install.packages("tidyverse")
library("tidyverse")
setwd("~/Downloads/Labs/Data")
mlb_team_seasons = read_csv("02_mlb-team-seasons.csv.gz")
model_data = mlb_team_seasons %>% 
  mutate(
    X=log(RS/RA),
    Y=log(WP/(1-WP))
  )
model = lm( Y ~ X, data = model_data)
model
alpha_hat <- coef(model)["X"]
cat(sprintf("Estimated alpha = %.6f\n", alpha_hat))
model_data <- model_data %>% 
  mutate(
    WP_hat_best   = (RS^alpha_hat) / (RS^alpha_hat + RA^alpha_hat),  # best‑fit α
    WP_hat_classic = (RS^2)        / (RS^2        + RA^2)            # Bill James α = 2
  )
library(ggplot2)

ggplot(model_data, aes(WP_hat_classic, WP)) +
  geom_point(color = "steelblue", alpha = .6) +                      # classic α=2
  geom_point(aes(WP_hat_best, WP), color = "darkorange", alpha = .6) + # best‑fit α̂
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Actual vs Predicted Win Percentage (2017–2021)",
       subtitle = sprintf("Orange = best‑fit α ≈ %.2f   |   Blue = classic α = 2", alpha_hat),
       x = "Predicted Win Pct",
       y = "Actual Win Pct") +
  theme_bw()
 

#Assignment 2
setwd("~/Downloads/Labs/Data")
payroll <- read_csv("02_mlb-payrolls.csv.gz") %>%
  rename(
    payroll_ratio = `Payroll/Median`,
    log_ratio     = `Log (Payroll/Median)`
  ) %>%
  filter(yearID != 2020)   


ggplot(payroll, aes(x = payroll_ratio, y = WP)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "red", linetype = "dashed") +
  geom_point(
    data = filter(payroll, name %in% c("Oakland Athletics", "New York Yankees")),
    aes(color = name)
)
#task 2
mod_A <- lm(WP ~ payroll_ratio, data = payroll)      # ordinary scale
mod_B <- lm(WP ~ log_ratio,     data = payroll) # Log scale
payroll <- payroll %>% 
  mutate(
    resid_A = WP - predict(mod_A, .),   # actual – fitted
    resid_B = WP - predict(mod_B, .)
  )
team_diff <- payroll %>% 
  group_by(name) %>%                     
  summarise(
    A_wins = mean(resid_A) * 162,
    B_wins = mean(resid_B) * 162,
    .groups = "drop"
  )
library(ggplot2)

plot_bar <- function(df, col, ttl, fill_col) {
  ggplot(df, aes(reorder(name, !!sym(col)), !!sym(col),
                 fill = !!sym(col) > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = fill_col, "FALSE" = "grey70")) +
    labs(x = "", y = "Avg. wins vs model", title = ttl) +
    geom_hline(yintercept = 0, colour = "black")
}

plot_bar(team_perf, "A_wins",
         "GM Performance (Model A: Payroll Ratio)", "dodgerblue")

plot_bar(team_perf, "B_wins",
         "GM Performance (Model B: log Payroll Ratio)", "firebrick")











