#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(splines)

# set seed
set.seed(3)

# directory for saved plots
if (!dir.exists("plots")) dir.create("plots")

##############
### PART 1 ###
##############

# load data
nba_four_factors = read_csv("02_nba-four-factors.csv.gz")

# Build the four factors so that larger values are always better for the team.
# x1 = eFG% - opp eFG%            (shooting advantage)
# x2 = OREB% + DREB% - 100        (rebounding advantage)
# x3 = opp TOV% - TOV%            (turnover advantage)
# x4 = FT rate - opp FT rate      (free-throw-rate advantage)
nba = nba_four_factors %>%
  transmute(
    wins = W,
    x1 = `EFG%` - `OPP EFG%`,
    x2 = `OREB%` + `DREB%` - 100,
    x3 = `OPP TOV %` - `TOV%`,
    x4 = `FT Rate` - `OPP FT Rate`
  )

# Task 1:
# - Compute each variable's mean, standard deviation, minimum, and maximum
# - Plot the marginal distribution of each explanatory variable
# - Make scatterplots of wins against each of the four factors
# - Compute correlations between each pair of explanatory variables
# - Identify which variables look most strongly related to wins before fitting a model

# summary statistics (mean, sd, min, max) for wins and each factor
summary_stats = nba %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(mean = mean(value), sd = sd(value),
            min = min(value), max = max(value), .groups = "drop")
print(summary_stats)

# marginal distribution of each explanatory variable
nba_long = nba %>%
  select(x1, x2, x3, x4) %>%
  pivot_longer(everything(), names_to = "factor", values_to = "value")
p_marg = ggplot(nba_long, aes(value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ factor, scales = "free") +
  labs(title = "Marginal distributions of the four factors", x = NULL, y = "count")
ggsave("plots/p1_marginal_distributions.png", p_marg, width = 8, height = 6)

# scatterplots of wins against each of the four factors
nba_scatter = nba %>%
  pivot_longer(c(x1, x2, x3, x4), names_to = "factor", values_to = "value")
p_scatter = ggplot(nba_scatter, aes(value, wins)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ factor, scales = "free_x") +
  labs(title = "Wins against each of the four factors", x = "factor value", y = "wins")
ggsave("plots/p1_wins_vs_factors.png", p_scatter, width = 8, height = 6)

# correlations between each pair of explanatory variables
factor_cor = cor(nba %>% select(x1, x2, x3, x4))
cat("\nCorrelation matrix of the four factors:\n")
print(round(factor_cor, 3))

# correlation of each factor with wins (used to judge marginal strength)
cor_with_wins = sapply(nba %>% select(x1, x2, x3, x4),
                       function(col) cor(col, nba$wins))
cat("\nCorrelation of each factor with wins:\n")
print(round(cor_with_wins, 3))

# Which variables look most strongly related to wins before modeling?
# - The four factors are only weakly correlated with one another (the largest
#   pairwise correlation is about -0.29), so they carry mostly independent
#   information.
# - Shooting (x1) has by far the largest marginal correlation with wins (~0.85).
#   The others are much smaller: free-throw-rate advantage x4 (~0.36) and
#   turnover advantage x3 (~0.29) come next, and rebounding x2 (~0.14) is the
#   weakest marginally. So x1 looks most strongly related to wins.

# Task 2:
# - Fit the multivariable model: wins ~ x1 + x2 + x3 + x4
# - Write down the fitted regression equation
# - Interpret each coefficient in context
# - Check whether the coefficient signs make sense given the variable definitions
# - Identify which factors look strongest and weakest after adjustment

fit = lm(wins ~ x1 + x2 + x3 + x4, data = nba)
print(summary(fit))

# Fitted regression equation (rounded coefficients, filled from summary above):
#   wins_hat = b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4
#   wins_hat = 40.19 + 3.67*x1 + 1.34*x2 + 3.06*x3 + 77.07*x4
#
# Interpretation of each coefficient (holding the other three factors fixed):
# - b0 (intercept): the expected wins for a hypothetical team whose four-factor
#   advantages are all exactly zero (an average team); about 40, i.e. ~.500.
# - b1: each one-percentage-point increase in shooting advantage (eFG% edge) is
#   associated with ~3.7 more wins, on average.
# - b2: each one-point increase in rebounding advantage adds ~1.3 wins.
# - b3: each one-point increase in turnover advantage adds ~3.1 wins.
# - b4: the coefficient is ~77, but x4 (a difference of FT *rates*) lives on a
#   tiny scale (sd ~0.03), so a "one-unit" change is enormous relative to its
#   range; a more realistic one-sd change in x4 is worth only a couple of wins.
#   This large raw number is exactly why standardization (Task 3) is needed.
#
# Do the signs make sense? Yes. All four x's were defined so that larger values
# are better for the team, so we expect all four slopes to be positive, and they
# all are.
#
# After adjustment, shooting (x1) is the strongest factor; the raw slopes are on
# very different scales (x4's is huge only because its scale is tiny), so they are
# not directly comparable, which is why Task 3 standardizes them.

# Task 3:
# - Standardize the four predictors
# - Fit the standardized model
# - Rank the factors by absolute standardized coefficient size
# - Compare the original and standardized models for interpretability
# - Compare fitted values from both models and explain why they match or differ

nba_std = nba %>%
  mutate(z1 = scale(x1)[, 1], z2 = scale(x2)[, 1],
         z3 = scale(x3)[, 1], z4 = scale(x4)[, 1])

fit_std = lm(wins ~ z1 + z2 + z3 + z4, data = nba_std)
print(summary(fit_std))

# rank factors by absolute standardized coefficient
std_coefs = coef(fit_std)[c("z1", "z2", "z3", "z4")]
std_ranking = sort(abs(std_coefs), decreasing = TRUE)
cat("\nFactors ranked by |standardized coefficient|:\n")
print(round(std_ranking, 3))

# Which model is easier for comparing relative importance?
# - The STANDARDIZED model. Each standardized coefficient is the expected change
#   in wins for a one-standard-deviation increase in that factor, so all four are
#   on the same footing and can be compared directly. The original coefficients
#   are on the factors' raw scales and are not comparable.
#
# Ranking by |standardized coefficient| is x1 (shooting) > x3 (turnovers) >
# x2 (rebounding) > x4 (free-throw rate).

# Compare fitted values from the two models
fitted_diff = max(abs(fitted(fit) - fitted(fit_std)))
cat("\nMax abs difference in fitted values (original vs standardized):",
    fitted_diff, "\n")

# Are the fitted values the same? YES (difference is ~0, machine precision).
# Column-space view: standardizing a predictor is an invertible linear rescale
# (and recentering, absorbed by the intercept). With an intercept in the model,
# the columns {1, z1, z2, z3, z4} span exactly the same subspace of R^n as
# {1, x1, x2, x3, x4}. The fitted values are the orthogonal projection of y onto
# that subspace, and the subspace is unchanged, so the projection -- the fitted
# values -- is identical. Only the coordinates (coefficients) change.

# Task 4:
# - Report the residual standard error and interpret it in wins
# - Report coefficient standard errors and 95% confidence intervals
# - Identify which effects are clearly different from zero
# - Choose one team and compute a point prediction, confidence interval, and prediction interval
# - State which interval is wider and why

# residual standard error: sigma_hat = sqrt(RSS / (n - p))
sigma_hat = summary(fit)$sigma
cat("\nResidual standard error (wins):", round(sigma_hat, 3), "\n")
# Interpretation: a typical team's actual wins fall about this many wins away
# from the value the four-factor model predicts (roughly +/- 4 wins).

# coefficient standard errors and 95% confidence intervals
coef_table = cbind(estimate = coef(fit),
                   std_error = summary(fit)$coefficients[, "Std. Error"],
                   confint(fit))
cat("\nCoefficient estimates, standard errors, and 95% CIs:\n")
print(round(coef_table, 3))

# Which effects are clearly different from zero?
# - All four factors have 95% confidence intervals that exclude 0 (and tiny
#   p-values), so every effect is clearly distinguishable from zero in this large
#   sample (n = 480).
# - In relative terms x4 is the least important effect: its standardized
#   coefficient (Task 3) is the smallest, so although it is statistically
#   nonzero, it contributes the least to wins of the four factors.

# choose one team and predict
team_row = nba_four_factors$TEAM[1]
one_team = nba[1, ]
cat("\nChosen team:", team_row, "\n")
pred_point = predict(fit, newdata = one_team)
pred_conf  = predict(fit, newdata = one_team, interval = "confidence", level = 0.95)
pred_pred  = predict(fit, newdata = one_team, interval = "prediction", level = 0.95)
cat("Point prediction (wins):", round(pred_point, 2), "\n")
cat("95% confidence interval for expected wins:",
    round(pred_conf[2], 2), "to", round(pred_conf[3], 2), "\n")
cat("95% prediction interval for actual wins:",
    round(pred_pred[2], 2), "to", round(pred_pred[3], 2), "\n")

# Which interval is wider? The PREDICTION interval. The confidence interval only
# accounts for uncertainty in the estimated mean response (the fitted line). The
# prediction interval adds the irreducible game-to-game/season-to-season noise of
# a single team's outcome (the sigma_hat above), so it is always wider.

# Task 5:
# - Randomly split the data into training and test sets
# - Fit the original and standardized models on the training set
# - Compute test-set RMSE for both models
# - Compare predictive performance

rmse = function(actual, predicted) sqrt(mean((actual - predicted)^2))

n = nrow(nba_std)
train_idx = sample(seq_len(n), size = floor(0.7 * n))
train = nba_std[train_idx, ]
test  = nba_std[-train_idx, ]

fit_train     = lm(wins ~ x1 + x2 + x3 + x4, data = train)
fit_train_std = lm(wins ~ z1 + z2 + z3 + z4, data = train)

rmse_orig = rmse(test$wins, predict(fit_train,     newdata = test))
rmse_std  = rmse(test$wins, predict(fit_train_std, newdata = test))
cat("\nTest-set RMSE (original model): ", round(rmse_orig, 4), "\n")
cat("Test-set RMSE (standardized model):", round(rmse_std, 4), "\n")

# Which model predicts better? Neither -- they are identical. Standardizing
# predictors is just an invertible rescaling, so both models fit the same plane
# and produce the same predictions; their test-set RMSE is the same up to
# floating-point error. Standardization changes interpretability, not prediction.

##############
### PART 2 ###
##############

# load data
punts = read_csv("02_punts.csv.gz")
# columns: ydl (starting yard line from opponent goal line), next_ydl (outcome y),
# season, punter (name), pq (punter quality).

# Task 1:
# - Plot post-punt yard line against starting yard line
# - Bin punts by starting field position and plot average post-punt yard line in each bin
# - Describe the shape of the relationship and where it bends
# - Plot or summarize the distribution of punter quality

# raw scatter of outcome against starting yard line
p_punt_scatter = ggplot(punts, aes(ydl, next_ydl)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Post-punt yard line vs. starting yard line",
       x = "starting yard line (yards from opponent goal)",
       y = "post-punt yard line (opponent perspective)")
ggsave("plots/p2_scatter.png", p_punt_scatter, width = 7, height = 5)

# bin by starting field position and plot the average outcome in each bin
punts_binned = punts %>%
  mutate(ydl_bin = cut(ydl, breaks = seq(0, 100, by = 5))) %>%
  group_by(ydl_bin) %>%
  summarise(ydl_mid = mean(ydl), mean_next = mean(next_ydl),
            n = n(), .groups = "drop")
p_punt_bins = ggplot(punts_binned, aes(ydl_mid, mean_next)) +
  geom_point(aes(size = n)) +
  geom_line() +
  labs(title = "Average post-punt yard line by starting-position bin",
       x = "starting yard line (bin midpoint)", y = "average post-punt yard line")
ggsave("plots/p2_binned.png", p_punt_bins, width = 7, height = 5)

# Shape of the relationship:
# - For most of the field (long-to-mid range, large ydl) the average outcome
#   rises roughly linearly with the starting yard line: punting from farther out
#   leaves the opponent farther from their own goal.
# - The relationship bends (flattens / curves) as the punting team gets close to
#   the opponent's goal (small ydl), where touchbacks and limited room cap how
#   much field position can be gained. This curvature motivates the quadratic and
#   spline models below.

# distribution of punter quality
p_pq = ggplot(punts, aes(pq)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "white") +
  labs(title = "Distribution of punter quality (pq)", x = "pq", y = "count")
ggsave("plots/p2_pq_distribution.png", p_pq, width = 7, height = 5)
cat("\nPunter quality (pq) summary:\n")
print(summary(punts$pq))

# Task 2:
# - Fit competing punt models: linear, quadratic, quadratic plus punter quality, and spline
# - Visualize the fitted curves from each model
# - Use train/test RMSE or cross-validation to choose a preferred model
# - Compare the linear, quadratic, and spline tradeoffs
# - Assess whether punter quality improves out-of-sample prediction
# - Interpret the punter-quality coefficient if it is included in the selected model

# train/test split for model selection
np = nrow(punts)
ptrain_idx = sample(seq_len(np), size = floor(0.7 * np))
ptrain = punts[ptrain_idx, ]
ptest  = punts[-ptrain_idx, ]

m_lin   = lm(next_ydl ~ ydl, data = ptrain)
m_quad  = lm(next_ydl ~ poly(ydl, 2, raw = TRUE), data = ptrain)
m_quadq = lm(next_ydl ~ poly(ydl, 2, raw = TRUE) + pq, data = ptrain)
m_spl   = lm(next_ydl ~ ns(ydl, df = 5) + pq, data = ptrain)

punt_rmse = c(
  linear        = rmse(ptest$next_ydl, predict(m_lin,   newdata = ptest)),
  quadratic     = rmse(ptest$next_ydl, predict(m_quad,  newdata = ptest)),
  quadratic_pq  = rmse(ptest$next_ydl, predict(m_quadq, newdata = ptest)),
  spline_pq     = rmse(ptest$next_ydl, predict(m_spl,   newdata = ptest))
)
cat("\nTest-set RMSE for competing punt models:\n")
print(round(punt_rmse, 4))

# visualize the fitted curves (pq held at its mean so the curves are comparable)
grid = tibble(ydl = seq(min(punts$ydl), max(punts$ydl), length.out = 200),
              pq = mean(punts$pq))
curves = grid %>%
  mutate(linear       = predict(m_lin,   newdata = grid),
         quadratic    = predict(m_quad,  newdata = grid),
         quadratic_pq = predict(m_quadq, newdata = grid),
         spline_pq    = predict(m_spl,   newdata = grid)) %>%
  pivot_longer(c(linear, quadratic, quadratic_pq, spline_pq),
               names_to = "model", values_to = "fit")
p_curves = ggplot() +
  geom_point(data = punts, aes(ydl, next_ydl), alpha = 0.08) +
  geom_line(data = curves, aes(ydl, fit, color = model), linewidth = 1) +
  labs(title = "Fitted punt curves (pq at its mean)",
       x = "starting yard line", y = "post-punt yard line")
ggsave("plots/p2_fitted_curves.png", p_curves, width = 8, height = 5)

# Tradeoffs and model choice (filled from the RMSE table above):
# - The LINEAR model underfits: it cannot capture the flattening of the curve, so
#   it has the clearly largest test RMSE (~10.83).
# - The QUADRATIC model captures the curvature and gives the big improvement, down
#   to ~10.66; this is where almost all of the predictive gain comes from.
# - The SPLINE is the most flexible but only ties the quadratic here (~10.656),
#   so its extra parameters buy essentially nothing out-of-sample -- a case where
#   added flexibility does not pay off and mainly risks overfitting sparse regions.
# - Adding punter quality (pq) lowers test RMSE only marginally (~10.6636 ->
#   ~10.6566), so it helps out-of-sample prediction, but barely. Because the punt
#   outcome is dominated by huge play-to-play variance, no model gets RMSE much
#   below ~10.6 yards. The preferred model is the spline-plus-pq model (lowest
#   test RMSE), though the quadratic-plus-pq is essentially equivalent and simpler.

# selected model
m_select = m_spl
cat("\nPunter-quality coefficient in the selected (spline + pq) model:\n")
print(round(coef(m_select)["pq"], 3))
# Interpretation: holding starting yard line fixed, a one-unit increase in pq is
# associated with a ~1.4-yard change in the expected post-punt opponent yard line
# (the estimate is positive, ~ +1.4). Its practical effect is small relative to
# the ~10.6-yard residual spread of individual punts, so pq adds only a little
# predictive signal once starting field position is accounted for -- a coefficient
# worth interpreting cautiously.

# Task 3:
# - Plot the fitted mean response for the selected punt model
# - Add a 95% confidence band for the expected response
# - Add a 95% prediction band for one individual punt
# - Explain why the prediction band is wider
# - Identify where the model is most uncertain

# refit selected model on the full data for the uncertainty bands
m_final = lm(next_ydl ~ ns(ydl, df = 5) + pq, data = punts)
band_grid = tibble(ydl = seq(min(punts$ydl), max(punts$ydl), length.out = 200),
                   pq = mean(punts$pq))
conf = predict(m_final, newdata = band_grid, interval = "confidence", level = 0.95)
pred = predict(m_final, newdata = band_grid, interval = "prediction", level = 0.95)
band_df = band_grid %>%
  mutate(fit = conf[, "fit"],
         conf_lwr = conf[, "lwr"], conf_upr = conf[, "upr"],
         pred_lwr = pred[, "lwr"], pred_upr = pred[, "upr"])
p_bands = ggplot(band_df, aes(ydl)) +
  geom_point(data = punts, aes(ydl, next_ydl), alpha = 0.06) +
  geom_ribbon(aes(ymin = pred_lwr, ymax = pred_upr), fill = "orange", alpha = 0.25) +
  geom_ribbon(aes(ymin = conf_lwr, ymax = conf_upr), fill = "blue", alpha = 0.35) +
  geom_line(aes(y = fit), color = "black", linewidth = 1) +
  labs(title = "Selected punt model: mean fit with 95% confidence (blue) and prediction (orange) bands",
       x = "starting yard line", y = "post-punt yard line")
ggsave("plots/p2_uncertainty_bands.png", p_bands, width = 8, height = 5)

# Why is the prediction band wider?
# - The confidence band reflects uncertainty only in the estimated mean curve.
#   The prediction band adds the residual variance of a single punt (sigma^2), so
#   it is always wider -- it must cover where one individual punt could land.
#
# Where is the model most uncertain?
# - Both bands widen at the edges of the field (very small and very large ydl),
#   where punts are sparse. Uncertainty increases in parts of the field with
#   fewer punts, exactly as expected.

# Task 4:
# - Define punt yards over expected so that positive values are better punts
# - Compute PYOE for each punt
# - For each punter, compute average PYOE, number of punts, and standard error of average PYOE
# - Rank punters by average PYOE
# - Visualize punter rankings with uncertainty intervals
# - Identify which punters look clearly above average and which rankings are unstable

# PYOE = E_hat[y | x] - y. Smaller opponent field position is better for the
# punting team, so a punt that lands the opponent CLOSER than expected
# (y < fitted) gives a POSITIVE PYOE = better-than-expected punt.
punts = punts %>%
  mutate(expected = predict(m_final, newdata = punts),
         pyoe = expected - next_ydl)

punter_pyoe = punts %>%
  group_by(punter) %>%
  summarise(avg_pyoe = mean(pyoe),
            n_punts = n(),
            se_pyoe = sd(pyoe) / sqrt(n()),
            .groups = "drop") %>%
  arrange(desc(avg_pyoe))
cat("\nPunter PYOE rankings (top and bottom):\n")
print(head(punter_pyoe, 10))
print(tail(punter_pyoe, 10))

# visualize rankings with +/- 1.96 SE uncertainty intervals (punters with a
# reasonable sample size so the plot is readable)
plot_punters = punter_pyoe %>% filter(n_punts >= 50)
p_rank = ggplot(plot_punters,
                aes(x = avg_pyoe, y = reorder(punter, avg_pyoe))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point() +
  geom_errorbarh(aes(xmin = avg_pyoe - 1.96 * se_pyoe,
                     xmax = avg_pyoe + 1.96 * se_pyoe), height = 0.3) +
  labs(title = "Punter rankings by average PYOE (>= 50 punts, 95% intervals)",
       x = "average PYOE (positive = better than expected)", y = "punter")
ggsave("plots/p2_punter_rankings.png", p_rank, width = 8, height = 9)

# Reading the rankings:
# - Punters whose entire 95% interval sits above 0 are clearly above average.
# - Rankings are unstable when the interval is wide -- caused by a small number
#   of punts (large SE) or high punt-to-punt variability. Those punters can sit
#   high or low in the order mostly by chance, so their rank should not be trusted.

# Final reflection:
# - Explain how adding columns changed what the model could fit
# - Explain when flexibility helped and when it could hurt
# - Interpret the residual standard error in this setting
# - Explain why prediction intervals are wider than confidence intervals
# - Note one coefficient, prediction, or ranking you would interpret cautiously
#
# 1. Adding columns to the design matrix enlarged the column space, so the model
#    could fit richer shapes: one column (ydl) only allows a straight line; adding
#    ydl^2 (or spline basis columns) lets the fit bend to match the flattening near
#    the goal line; adding pq lets the height of the curve shift by punter skill.
# 2. Flexibility helped where the true relationship was genuinely curved (the bend
#    in the punt data) and where pq carried real signal -- test RMSE dropped.
# 3. Flexibility can hurt by overfitting: very flexible fits chase noise, especially
#    in sparse regions (the edges of the field with few punts), giving wide,
#    unreliable predictions and worse out-of-sample error.
# 4. The residual standard error sigma_hat is the typical size of a model error: in
#    the NBA model, actual wins land within about +/- 3.5 wins of the prediction;
#    in the punt model it is the typical yards by which one punt deviates from its
#    expected outcome.
# 5. A prediction interval is wider because it covers a single new outcome, which
#    carries the irreducible residual noise (sigma^2) ON TOP OF the uncertainty in
#    the estimated mean that the confidence interval already captures.
# 6. I would be cautious about punter PYOE rankings for punters with few punts:
#    their wide uncertainty intervals mean their rank is largely chance, and a high
#    average PYOE on a small sample should not be over-interpreted as true skill.
