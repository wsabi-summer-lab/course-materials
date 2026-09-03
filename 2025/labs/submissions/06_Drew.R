#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "splines", "tidyverse"))
library(ggplot2)
library(nnet)
library(splines)
library(tidyverse)

#######################
### EXPECTED POINTS ###
#######################

# load data
nfl_data = read_csv("../data/06_expected-points.csv.gz")
nfl_data = nfl_data %>%
  mutate(EP = pts_next_score,
         YL = yardline_100,
         YTG = ydstogo
         )

#TASK 1

# 1.1
fit_1=multinom(EP ~ YL, data = nfl_data)
# create new data frame for just YL 
# Generate frame of yard lines
# Create grid of yard lines
YL_data <- data.frame(YL = seq(0, 100, 1))

# Get predicted probabilities for each possible next score
EP_1 <- predict(fit_1, newdata = YL_data, type = "probs")

# Compute expected points at each yard line
# Make sure the column names of 'probs' are the possible next scores
score_values <- as.numeric(colnames(EP_1))
YL_data$EP_YDmod <- as.numeric(EP_1 %*% score_values)

# Plot
ggplot(YL_data, aes(x = YL, y = EP_YDmod)) +
  geom_line() +
  labs(
    title = "EP vs Yard Line (Linear)",
    x = "Yard line (0 = own end zone; 100 = opp end zone)",
    y = "Expected points"
  ) +
  theme_minimal()

#1.2 splines on YL
fit_2 <- multinom(EP ~ bs(YL, df = 5), data = nfl_data)

EP_2 <- predict(fit_2, newdata = YL_data, type = "probs")
YL_data$EP_YLsplines <- as.numeric(EP_2 %*% score_values)
# Plot
ggplot(YL_data, aes(x = YL, y = EP_YLsplines)) +
  geom_line() +
  labs(
    title = "EP vs Yard Line (Splines)",
    x = "Yard line (0 = own end zone; 100 = opp end zone)",
    y = "Expected points"
  ) +
  theme_minimal()
# 1.3
fit_3 <- multinom(EP ~ YL + factor(down), data = nfl_data)
YL_downs_data <- expand_grid(
  YL = seq(0, 100, by = 1),
  down      = 1:4)
EP_3 <- predict(fit_3, newdata = YL_downs_data, type = "probs")
score_values <- as.numeric(colnames(EP_3))
YL_downs_data$EP_YLdown <- as.numeric(EP_3 %*% score_values)
# Plot
ggplot(YL_downs_data, aes(x = YL, y = EP_YLdown, color = factor(down))) +
  geom_line() +
  labs(
    title = "EP vs Yard Line (Downs)",
    x = "Yard line (0 = own end zone; 100 = opp end zone)",
    y = "Expected points",
    color = "Down"
  ) +
  theme_minimal()
#1.4  yard line, down, and yards to go
fit_4 <- multinom(
  EP ~ bs(YL, df = 5) + factor(down) + log1p(YTG),
  data = nfl_data
)
YL_downs_YTG_data <- expand_grid(
  YL = seq(0, 100, by = 2),
  down = 1:4,
  YTG = seq(1, 20, by = 1)
)
EP_4 <- predict(fit_4, newdata = YL_downs_YTG_data, type = "probs")
score_values <- as.numeric(colnames(EP_4))
YL_downs_YTG_data$EP_YLdownYTG <- as.numeric(EP_4 %*% score_values)
ggplot(YL_downs_YTG_data, aes(
  x = YL,
  y = EP_YLdownYTG,
  color     = YTG,              # continuous color gradient for YTG
  linetype  = factor(YTG),      # discrete linetypes
  group     = interaction(down, YTG)
)) +
  geom_line(size = 1.2) +
  facet_wrap(~ down, ncol = 2, labeller = label_both) +
  scale_color_viridis_c(
    option = "C",
    begin  = 0.2,
    end    = 0.8,
    name   = "Yards to Go"
  ) +
  scale_linetype_discrete(
    name = "Yards to Go"
  ) +
  labs(
    title    = "Expected Points by Field Position, Down & Distance",
    subtitle = "Continuous color gradient shows yards-to-go; panels show downs",
    x        = "Yard Line (0 = own end zone; 100 = opponent end zone)",
    y        = "Expected Points"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(size = 20, face = "bold", margin = margin(b = 10)),
    plot.subtitle      = element_text(size = 12, color = "gray30", margin = margin(b = 15)),
    strip.background   = element_rect(fill = "#F0F0F0", color = NA),
    strip.text         = element_text(size = 14, face = "bold"),
    axis.title         = element_text(face = "bold", size = 12),
    axis.text          = element_text(color = "gray20"),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(color = "gray85", size = 0.4),
    legend.position    = "bottom",
    legend.key.width   = unit(1.5, "cm"),
    legend.title       = element_text(face = "bold", size = 12),
    legend.text        = element_text(size = 10)
  )

#1.5 time included
fit_5 <- multinom(
  EP ~ bs(YL, df = 5) + factor(down) + log1p(YTG) + bs(half_seconds_remaining, df = 4),
  data = nfl_data
)

YL_downs_YTG_time_data <- expand_grid(
  YL = seq(0, 100, by = 2),
  down = 1,
  YTG = 10,
  half_seconds_remaining = c(0, 115, 120, 900, 1795, 1800)
)

EP_5 <- predict(fit_5, newdata = YL_downs_YTG_time_data, type = "probs")
score_values <- as.numeric(colnames(EP_5))
YL_downs_YTG_time_data$EP_YLdownYTGtime <- as.numeric(EP_5 %*% score_values)

ggplot(YL_downs_YTG_time_data, aes(
  x = YL,
  y = EP_YLdownYTGtime,
  color = factor(half_seconds_remaining)
)) +
  geom_line() +
  labs(
    title = "EP vs Yard Line (Downs, YTG, and Time)",
    x = "Yard line (0 = own end zone; 100 = opp end zone)",
    y = "Expected points",
    color = "Half Seconds Remaining"
  ) +
  theme_minimal()
###########################################################################################################################################################

#TASK 2
# 2.1
fit_Mprime <- multinom(
  EP ~ bs(YL, df = 5) 
  + factor(down) 
  + log1p(YTG) 
  + bs(half_seconds_remaining, df = 4) 
  + posteam_spread,
  data = nfl_data
)

YL_downs_YTG_time_spread_data <- expand_grid(
  YL = seq(0, 100, by = 2),
  down = 1,
  YTG = 10,
  half_seconds_remaining = c(0, 115, 120, 900, 1795, 1800),
  posteam_spread = 0
)
EP_M = predict(fit_5, newdata = YL_downs_YTG_time_spread_data, type = "probs")
EP_Mprime <- predict(fit_Mprime, newdata = YL_downs_YTG_time_spread_data, type = "probs")
YL_downs_YTG_time_spread_data$EP_Mprime <- as.numeric(EP_Mprime %*% score_values)
YL_downs_YTG_time_spread_data$EP_M <- as.numeric(EP_Mprime %*% score_values)

#plot
ggplot(YL_downs_YTG_time_spread_data, aes(x = YL)) +
  geom_line(aes(y = EP_M,      color = "M (original)")) +
  geom_line(aes(y = EP_Mprime, color = "M' (spread = 0)"), linetype = "dashed") +
  labs(
    title = "EP vs Yard Line: M vs. M' at Pre-game Spread = 0",
    x     = "Yard line (0 = own end zone; 100 = opp end zone)",
    y     = "Expected points",
    color = "Model"
  ) +
  theme_minimal()
###########################################################################################################################################################
