library(ggplot2)
library(nnet)
library(splines)
library(tidyverse)

# set seed
set.seed(6)

nfl_data <- read_csv("05_expected-points.csv.gz") %>%
  mutate(
    pts_next_score = factor(pts_next_score, levels = c(0, -7, -3, -2, 2, 3, 7)),
    down = factor(down, levels = 1:4)
  )

# Helper: compute EP from predicted probability matrix
compute_ep <- function(probs) {
  classes <- as.numeric(colnames(probs))
  as.vector(probs %*% classes)
}

yardline_grid <- 1:99

# ===========================================================
# TASK 1.1: Linear yardline only
model1 <- multinom(pts_next_score ~ yardline_100,
                   data = nfl_data, trace = FALSE)

pred1 <- predict(model1,
                 newdata = data.frame(yardline_100 = yardline_grid),
                 type = "probs")
df1 <- data.frame(yardline_100 = yardline_grid, EP = compute_ep(pred1))

p1 <- ggplot(df1, aes(x = yardline_100, y = EP)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(
    title = "Task 1.1: EP vs Yard Line (Linear)",
    subtitle = "The model forces a monotone linear relationship, but true EP is non-linear\n(e.g., it should curve sharply near both end zones).",
    x = "Yard Line (yards to opponent end zone)",
    y = "Expected Points"
  ) +
  theme_minimal()

ggsave("task1_1_linear.png", p1, width = 8, height = 5)
cat("Saved task1_1_linear.png\n")

# ===========================================================
# TASK 1.2: Spline on yardline
# ===========================================================
model2 <- multinom(pts_next_score ~ bs(yardline_100, df = 5),
                   data = nfl_data, trace = FALSE)

pred2 <- predict(model2,
                 newdata = data.frame(yardline_100 = yardline_grid),
                 type = "probs")
df2 <- data.frame(yardline_100 = yardline_grid, EP = compute_ep(pred2))

p2 <- ggplot(df2, aes(x = yardline_100, y = EP)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(
    title = "Task 1.2: EP vs Yard Line (Spline)",
    subtitle = "The spline captures non-linearity: EP rises steeply near the opponent's end zone and\nfalls near one's own end zone, which a linear term cannot represent.",
    x = "Yard Line (yards to opponent end zone)",
    y = "Expected Points"
  ) +
  theme_minimal()

ggsave("task1_2_spline.png", p2, width = 8, height = 5)
cat("Saved task1_2_spline.png\n")

# TASK 1.3: Spline yardline + down (as factor)

model3 <- multinom(pts_next_score ~ bs(yardline_100, df = 5) + down,
                   data = nfl_data, trace = FALSE)

grid3 <- expand.grid(
  yardline_100 = yardline_grid,
  down = factor(1:4, levels = 1:4)
)
pred3 <- predict(model3, newdata = grid3, type = "probs")
grid3$EP <- compute_ep(pred3)

p3 <- ggplot(grid3, aes(x = yardline_100, y = EP, color = down)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1", name = "Down") +
  labs(
    title = "Task 1.3: EP vs Yard Line by Down",
    subtitle = "Down should be encoded as a factor because the EP gaps between downs are not\nequally spaced — 4th down is categorically different from 1st through 3rd.",
    x = "Yard Line (yards to opponent end zone)",
    y = "Expected Points"
  ) +
  theme_minimal()

ggsave("task1_3_down.png", p3, width = 8, height = 5)
cat("Saved task1_3_down.png\n")


# TASK 1.4: Spline yardline + down + log(ydstogo)

model4 <- multinom(pts_next_score ~ bs(yardline_100, df = 5) + down + log(ydstogo),
                   data = nfl_data, trace = FALSE)

ydstogo_vals <- c(1, 5, 10, 15, 20)

grid4 <- expand.grid(
  yardline_100 = yardline_grid,
  down = factor(1:4, levels = 1:4),
  ydstogo = ydstogo_vals
)
pred4 <- predict(model4, newdata = grid4, type = "probs")
grid4$EP <- compute_ep(pred4)
grid4$ydstogo_label <- factor(grid4$ydstogo,
                               levels = ydstogo_vals,
                               labels = paste0(ydstogo_vals, " yds"))

p4 <- ggplot(grid4, aes(x = yardline_100, y = EP, color = ydstogo_label)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~down,
             labeller = labeller(down = c("1" = "1st Down", "2" = "2nd Down",
                                          "3" = "3rd Down", "4" = "4th Down"))) +
  scale_color_viridis_d(name = "Yards to Go") +
  labs(
    title = "Task 1.4: EP vs Yard Line by Down and Yards to Go",
    subtitle = "More yards to go suppresses EP most sharply on 3rd and 4th down, where a failure\nto convert ends the possession; the effect is more muted on early downs.",
    x = "Yard Line (yards to opponent end zone)",
    y = "Expected Points"
  ) +
  theme_minimal()

ggsave("task1_4_ydstogo.png", p4, width = 10, height = 7)
cat("Saved task1_4_ydstogo.png\n")

# TASK 1.5: + Time remaining (linear vs spline)
time_vals   <- c(60, 300, 900, 1800)
time_labels <- c("1 min", "5 min", "15 min", "30 min")

grid5 <- expand.grid(
  yardline_100           = yardline_grid,
  down                   = factor(1, levels = 1:4),
  ydstogo                = 10,
  half_seconds_remaining = time_vals
)
grid5$time_label <- factor(grid5$half_seconds_remaining,
                            levels = time_vals, labels = time_labels)

# 1.5a: Linear time
model5a <- multinom(
  pts_next_score ~ bs(yardline_100, df = 5) + down + log(ydstogo) + half_seconds_remaining,
  data = nfl_data, trace = FALSE
)
pred5a <- predict(model5a, newdata = grid5, type = "probs")
grid5$EP_linear <- compute_ep(pred5a)

p5a <- ggplot(grid5, aes(x = yardline_100, y = EP_linear, color = time_label)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(name = "Time Remaining") +
  labs(
    title = "Task 1.5a: EP vs Yard Line — Linear Time Remaining",
    subtitle = "1st down, 10 yards to go",
    x = "Yard Line (yards to opponent end zone)",
    y = "Expected Points"
  ) +
  theme_minimal()

ggsave("task1_5a_linear_time.png", p5a, width = 8, height = 5)
cat("Saved task1_5a_linear_time.png\n")

# 1.5b: Spline time
model5b <- multinom(
  pts_next_score ~ bs(yardline_100, df = 5) + down + log(ydstogo) + bs(half_seconds_remaining, df = 4),
  data = nfl_data, trace = FALSE
)
pred5b <- predict(model5b, newdata = grid5, type = "probs")
grid5$EP_spline <- compute_ep(pred5b)

p5b <- ggplot(grid5, aes(x = yardline_100, y = EP_spline, color = time_label)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(name = "Time Remaining") +
  labs(
    title = "Task 1.5b: EP vs Yard Line — Spline Time Remaining",
    subtitle = "1st down, 10 yards to go",
    x = "Yard Line (yards to opponent end zone)",
    y = "Expected Points"
  ) +
  theme_minimal()

ggsave("task1_5b_spline_time.png", p5b, width = 8, height = 5)
cat("Saved task1_5b_spline_time.png\n")

cat("
Comparison (1.5): The linear time model shifts EP curves up/down uniformly as time
changes. The spline time model captures non-linear end-of-half effects (e.g., near-zero
time collapses EP regardless of field position), producing more realistic separation
between very low and mid-half time values.
\n")

# TASK 2: Adjust for team quality (posteam_spread)
# M  = model5b (best Task 1 model: spline yardline + down + log ydstogo + spline time)
# M' = M + linear posteam_spread

model_M <- model5b

model_Mprime <- multinom(
  pts_next_score ~ bs(yardline_100, df = 5) + down + log(ydstogo) +
    bs(half_seconds_remaining, df = 4) + posteam_spread,
  data = nfl_data, trace = FALSE
)

# Prediction grid: 1st down, 10 ydstogo, 15 min remaining, spread = 0
grid_compare <- data.frame(
  yardline_100           = yardline_grid,
  down                   = factor(1, levels = 1:4),
  ydstogo                = 10,
  half_seconds_remaining = 900,
  posteam_spread         = 0
)

pred_M      <- predict(model_M,      newdata = grid_compare, type = "probs")
pred_Mprime <- predict(model_Mprime, newdata = grid_compare, type = "probs")

grid_compare$EP_M      <- compute_ep(pred_M)
grid_compare$EP_Mprime <- compute_ep(pred_Mprime)
grid_compare$EP_diff   <- grid_compare$EP_Mprime - grid_compare$EP_M

# 2a: Overlay of M vs M' at spread = 0
df_overlay <- grid_compare %>%
  pivot_longer(cols = c(EP_M, EP_Mprime), names_to = "Model", values_to = "EP") %>%
  mutate(Model = recode(Model,
                        "EP_M"      = "M (no spread)",
                        "EP_Mprime" = "M' (spread = 0)"))

p_overlay <- ggplot(df_overlay, aes(x = yardline_100, y = EP, color = Model, linetype = Model)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  labs(
    title = "Task 2: Overlay of M vs M' at Spread = 0",
    subtitle = "1st down, 10 yards to go, 15 min remaining",
    x = "Yard Line (yards to opponent end zone)",
    y = "Expected Points"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("task2_overlay.png", p_overlay, width = 8, height = 5)
cat("Saved task2_overlay.png\n")

# 2b: Difference EP(M') - EP(M) as function of yard line
p_diff <- ggplot(grid_compare, aes(x = yardline_100, y = EP_diff)) +
  geom_line(color = "purple", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Task 2: EP(M') - EP(M) as a Function of Yard Line",
    subtitle = "M estimates EP averaged over the observed play distribution (over-sampling good teams).\nM' at spread=0 targets a neutral-quality matchup. They differ because conditioning on\nspread removes the selection bias from good teams dominating the observed data.",
    x = "Yard Line (yards to opponent end zone)",
    y = "EP Difference (M' minus M)"
  ) +
  theme_minimal()

ggsave("task2_difference.png", p_diff, width = 8, height = 5)
cat("Saved task2_difference.png\n")

cat("\nAll tasks complete. 7 plots saved to working directory.\n")
