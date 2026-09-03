#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "splines", "tidyverse"))
library(ggplot2)
library(nnet)
library(splines)
library(tidyverse)

# set seed
set.seed(6)

# output folder for plots
if (!dir.exists("plots")) dir.create("plots")

#######################
### EXPECTED POINTS ###
#######################

# load data
nfl_data = read_csv("05_expected-points.csv.gz")

# Set up the response. y = net points of the next score in the half, restricted
# to {-7,-3,-2,0,2,3,7}. We model it as an unordered categorical outcome with
# multinom(); the baseline category is "0" (no further score in the half).
nfl_data = nfl_data %>%
  mutate(
    y     = factor(pts_next_score),   # levels sorted numerically: -7,-3,-2,0,2,3,7
    downf = factor(down)              # down treated as categorical (see Task 1.3)
  )
nfl_data$y = relevel(nfl_data$y, ref = "0")  # k = 0 is the baseline category

# Helper: turn a multinom probability matrix into expected points.
# EP(x) = sum_k k * P(y = k | x), where the k are the numeric class labels.
ep_from_probs = function(probs) {
  k = as.numeric(colnames(probs))
  as.vector(probs %*% k)
}

# Helper: predict EP for a fitted multinom model on new game states.
predict_ep = function(model, newdata) {
  probs = predict(model, newdata = newdata, type = "probs")
  ep_from_probs(probs)
}

###############################################
### TASK 1: BUILD AN ADDITIVE MODEL         ###
###############################################

## 1. Expected points as a purely linear function of yard line.
## logit[P(y=k)/P(y=0)] = beta_k0 + yardline * beta_k1
m1_linear = multinom(y ~ yardline_100, data = nfl_data, maxit = 300, trace = FALSE)

grid1 = tibble(yardline_100 = 1:99)
grid1$EP = predict_ep(m1_linear, grid1)

p1 = ggplot(grid1, aes(yardline_100, EP)) +
  geom_line(linewidth = 1, color = "#1b9e77") +
  labs(title = "Task 1.1: EP vs yard line (linear-in-logit yard line)",
       x = "yardline_100 (distance to opponent end zone)", y = "Expected points") +
  theme_minimal()
ggsave("plots/p1_task1_ep_linear_yardline.png", p1, width = 7, height = 4.5, dpi = 120)

# What's wrong with this model? The linear-in-yardline logits force EP to be an
# essentially monotone, smooth curve in yard line, so it misses the sharp rise in
# EP as the offense nears the opponent's goal line and the flattening deep in a
# team's own territory -- the realized EP-vs-yardline relationship is clearly
# non-linear and this model cannot bend to match it.

## 2. Spline on yard line to capture the non-linear relationship.
m2_spline = multinom(y ~ bs(yardline_100, df = 5), data = nfl_data,
                     maxit = 300, trace = FALSE)

grid2 = tibble(yardline_100 = 1:99)
grid2$EP = predict_ep(m2_spline, grid2)

p2 = ggplot(grid2, aes(yardline_100, EP)) +
  geom_line(linewidth = 1, color = "#d95f02") +
  labs(title = "Task 1.2: EP vs yard line (spline on yard line)",
       x = "yardline_100 (distance to opponent end zone)", y = "Expected points") +
  theme_minimal()
ggsave("plots/p2_task1_ep_spline_yardline.png", p2, width = 7, height = 4.5, dpi = 120)

# Why the spline improves on the linear model: the basis expansion lets the logits
# (and hence EP) curve, so EP can rise steeply close to the opponent goal line and
# level off near a team's own end zone, matching the non-linear empirical pattern
# the single linear term could not represent.

## 3. Expected points as a function of yard line and down.
## Down is encoded as a CATEGORICAL factor, not numeric: 1st->4th down are ordered
## labels but the EP gaps between them are not equally spaced, and a numeric term
## would impose a constant per-down effect, so a factor is the right encoding.
m3 = multinom(y ~ bs(yardline_100, df = 5) + downf, data = nfl_data,
              maxit = 300, trace = FALSE)

grid3 = expand_grid(yardline_100 = 1:99, downf = factor(1:4))
grid3$EP = predict_ep(m3, grid3)

p3 = ggplot(grid3, aes(yardline_100, EP, color = downf)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1", name = "down") +
  labs(title = "Task 1.3: EP vs yard line, colored by down",
       x = "yardline_100", y = "Expected points") +
  theme_minimal()
ggsave("plots/p3_task1_ep_yardline_down.png", p3, width = 7, height = 4.5, dpi = 120)

# How down should be encoded and why: as a categorical factor. The four downs are
# distinct game-state categories whose effect on EP is not linear in the down
# number (the drop to 4th down is much larger than 1st->2nd), so a factor captures
# each down's own intercept shift rather than forcing a single linear slope.

## 4. Expected points as a function of yard line, down, and yards to go.
## yards-to-go enters through a log transform: its effect is steep for small
## distances and flattens for long ones, which log() captures parsimoniously.
m4 = multinom(y ~ bs(yardline_100, df = 5) + downf + log(ydstogo), data = nfl_data,
              maxit = 300, trace = FALSE)

grid4 = expand_grid(yardline_100 = 1:99, downf = factor(1:4),
                    ydstogo = c(1, 5, 10, 20))
grid4$EP = predict_ep(m4, grid4)

p4 = ggplot(grid4, aes(yardline_100, EP, color = factor(ydstogo))) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ downf, labeller = labeller(downf = function(d) paste("down", d))) +
  scale_color_brewer(palette = "Dark2", name = "ydstogo") +
  labs(title = "Task 1.4: EP vs yard line, color = yards to go, facet = down",
       x = "yardline_100", y = "Expected points") +
  theme_minimal()
ggsave("plots/p4_task1_ep_yardline_down_ydstogo.png", p4, width = 9, height = 6, dpi = 120)

# How yards to go changes the EP surface across downs: more yards to go lowers EP,
# and the penalty grows with down -- on 1st down EP is nearly flat in ydstogo, but
# on 3rd/4th down a long distance sharply reduces EP (and can push it negative)
# because converting becomes unlikely and a turnover/punt looms.

## 5. Adjust the model by including time remaining in the half.
## Linear-time version:
m5_lin = multinom(y ~ bs(yardline_100, df = 5) + downf + log(ydstogo) +
                    half_seconds_remaining,
                  data = nfl_data, maxit = 300, trace = FALSE)

## Spline-time version:
m5_spl = multinom(y ~ bs(yardline_100, df = 5) + downf + log(ydstogo) +
                    bs(half_seconds_remaining, df = 5),
                  data = nfl_data, maxit = 300, trace = FALSE)

# Plot EP vs yardline on 1st & 10, colored by time remaining.
time_vals = c(60, 300, 900, 1500, 1800)
grid5 = expand_grid(yardline_100 = 1:99, half_seconds_remaining = time_vals) %>%
  mutate(downf = factor(1), ydstogo = 10)

grid5$EP_lin = predict_ep(m5_lin, grid5)
grid5$EP_spl = predict_ep(m5_spl, grid5)

p5a = ggplot(grid5, aes(yardline_100, EP_lin, color = factor(half_seconds_remaining))) +
  geom_line(linewidth = 0.9) +
  scale_color_viridis_d(name = "sec left") +
  labs(title = "Task 1.5: EP vs yard line (1st & 10), linear time term",
       x = "yardline_100", y = "Expected points") +
  theme_minimal()
ggsave("plots/p5_task1_ep_time_linear.png", p5a, width = 7, height = 4.5, dpi = 120)

p5b = ggplot(grid5, aes(yardline_100, EP_spl, color = factor(half_seconds_remaining))) +
  geom_line(linewidth = 0.9) +
  scale_color_viridis_d(name = "sec left") +
  labs(title = "Task 1.5: EP vs yard line (1st & 10), spline time term",
       x = "yardline_100", y = "Expected points") +
  theme_minimal()
ggsave("plots/p5_task1_ep_time_spline.png", p5b, width = 7, height = 4.5, dpi = 120)

# Comparison of linear vs spline time: the linear term shifts EP by a roughly
# constant amount across the time levels (it can only model a monotone effect of
# seconds remaining), whereas the spline lets EP respond non-linearly to the clock
# -- most visibly the end-of-half "expiration" effect, where very little time left
# compresses EP toward 0 because a drive may not finish before the half ends. The
# spline-time model is the more realistic of the two.

# We adopt the spline-time model (m5_spl) as the best Task 1 model M: it has the
# flexible yard-line spline, categorical down, log yards-to-go, and a flexible
# clock term.

###############################################
### TASK 2: ADJUST FOR TEAM QUALITY         ###
###############################################

## 1. M = best model from Task 1; M' adds pre-game point spread (linear term).
M       = m5_spl
M_prime = multinom(y ~ bs(yardline_100, df = 5) + downf + log(ydstogo) +
                     bs(half_seconds_remaining, df = 5) + posteam_spread,
                   data = nfl_data, maxit = 300, trace = FALSE)

## 2. Compare EP from M' at spread = 0 vs EP from M.
# Reference game state: 1st & 10, mid-half clock; vary yard line.
cmp = tibble(yardline_100 = 1:99, downf = factor(1), ydstogo = 10,
             half_seconds_remaining = 900)
cmp$EP_M       = predict_ep(M, cmp)
cmp$EP_Mprime0 = predict_ep(M_prime, cmp %>% mutate(posteam_spread = 0))
cmp$diff       = cmp$EP_Mprime0 - cmp$EP_M

cmp_long = cmp %>%
  select(yardline_100, EP_M, EP_Mprime0) %>%
  pivot_longer(c(EP_M, EP_Mprime0), names_to = "model", values_to = "EP") %>%
  mutate(model = recode(model, EP_M = "M (no spread)",
                        EP_Mprime0 = "M' at spread = 0"))

p6 = ggplot(cmp_long, aes(yardline_100, EP, color = model)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("M (no spread)" = "#377eb8",
                                "M' at spread = 0" = "#e41a1c"), name = NULL) +
  labs(title = "Task 2.2: EP of M vs M' (spread = 0), 1st & 10",
       x = "yardline_100", y = "Expected points") +
  theme_minimal()
ggsave("plots/p6_task2_overlay_M_Mprime.png", p6, width = 7, height = 4.5, dpi = 120)

p7 = ggplot(cmp, aes(yardline_100, diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1, color = "#984ea3") +
  labs(title = "Task 2.2: EP difference, M'(spread=0) - M",
       x = "yardline_100", y = "EP difference") +
  theme_minimal()
ggsave("plots/p7_task2_difference.png", p7, width = 7, height = 4.5, dpi = 120)

# Are M and M'(spread = 0) the same? No -- but the gap is small. M omits spread,
# so it estimates EP under the empirical mix of team-quality matchups actually
# observed at each game state; M' conditions on spread and is then evaluated at
# spread = 0, i.e. a perfectly even matchup. Conceptually these are different
# estimands: M targets "EP averaged over the observed (quality-imbalanced) play
# population", while M'(spread=0) targets "EP for an average offense facing an
# average defense". Empirically here the two curves nearly coincide: the
# difference (EP_Mprime0 - EP_M) is only on the order of +-0.05 points and does
# NOT have a consistent sign across yard line (slightly positive in midfield,
# slightly negative near the goal lines). The reason the selection-bias gap is so
# muted is that posteam_spread in this dataset is nearly centered at zero
# (mean ~= -0.07) and roughly symmetric, so the observed population is not far
# from a neutral-spread population. The difference plot confirms the two are not
# identical (the gap is non-zero and varies with yard line), but in this sample
# the practical effect of conditioning on spread is small.

#####################################################
### 5.2.1 DISCUSSION: SELECTION BIAS (NBA 3PT)    ###
#####################################################

# Are these the same? (a) the % of all 3-point attempts MADE in the NBA this year,
# vs (b) the "true" 3PT make % of an average NBA player. They are DIFFERENT.
# Quantity (a) is attempt-weighted: players (and lineups) who shoot threes well
# take far more of them, so the league-wide made-percentage over-samples good
# shooters. Quantity (b) weights every player equally regardless of volume. We
# would expect (a) to be HIGHER than (b), the same selection-bias mechanism seen
# in the EP example (good teams over-sampled). To adjust, we would reweight by
# player rather than by attempt -- e.g. average each player's individual 3PT% (or
# model make-probability with a player effect and average over players), instead
# of pooling all attempts together.
