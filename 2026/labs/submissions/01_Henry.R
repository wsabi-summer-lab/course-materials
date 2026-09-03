#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

##############
### PART 1 ###
##############
# Section 1.1 -- Pythagorean Win Percentage
#
# Goal: predict end-of-season win percentage (WP) from Runs Scored (RS) and
# Runs Allowed (RA) using the Pythagorean form
#         WP = RS^alpha / (RS^alpha + RA^alpha)
# and estimate the exponent alpha with linear regression, then compare it to
# Bill James's classic alpha = 2.

# load data
mlb_team_seasons = read_csv("01_mlb-team-seasons.csv.gz")

# ---------------------------------------------------------------------------
# TASK 1: Estimate the Pythagorean exponent alpha via linear regression.
#
# Algebra (the hint in the PDF): starting from
#         WP = RS^a / (RS^a + RA^a)
# we get
#         (1 - WP) / WP = (RA / RS)^a
# Taking logs and flipping the ratio,
#         log( WP / (1 - WP) ) = a * log( RS / RA )
# So a no-intercept regression of the log-odds of WP on log(RS/RA) gives us
# alpha as the slope (on the transformed log-odds scale).
# ---------------------------------------------------------------------------

# transformed regression data for estimating alpha
pythag_data = mlb_team_seasons %>%
  mutate(
    logit_wp = log(WP / (1 - WP)),
    log_rs_ra = log(RS / RA)
  )

# no-intercept model: log(WP / (1 - WP)) = alpha * log(RS / RA)
alpha_model = lm(logit_wp ~ 0 + log_rs_ra, data = pythag_data)
cat("\n===== PART 1, TASK 1: fitted alpha model =====\n")
print(summary(alpha_model))

# alpha estimate + uncertainty
alpha_hat = coef(alpha_model)[["log_rs_ra"]]
alpha_se = summary(alpha_model)$coefficients["log_rs_ra", "Std. Error"]
alpha_ci_95 = alpha_hat + c(-1, 1) * qt(0.975, df = df.residual(alpha_model)) * alpha_se

cat("\nFitted Pythagorean exponent (alpha_hat):", round(alpha_hat, 4), "\n")

# helper for converting exponent alpha back to predicted WP
pythag_wp = function(rs, ra, alpha) {
  rs^alpha / (rs^alpha + ra^alpha)
}

# ---------------------------------------------------------------------------
# TASK 2: Compare the fitted-alpha model to Bill James's alpha = 2 model.
#   - RMSE for each model
#   - report which is smaller
#   - actual-vs-predicted plot for both, with the 45-degree line
# ---------------------------------------------------------------------------

# compare fitted-alpha model vs Bill James alpha = 2
pythag_data = pythag_data %>%
  mutate(
    wp_hat_alpha = pythag_wp(RS, RA, alpha_hat),
    wp_hat_bj = pythag_wp(RS, RA, 2),
    resid_alpha = WP - wp_hat_alpha,
    resid_bj = WP - wp_hat_bj
  )

rmse = function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
rmse_alpha = rmse(pythag_data$WP, pythag_data$wp_hat_alpha)
rmse_bj = rmse(pythag_data$WP, pythag_data$wp_hat_bj)

cat("\n===== PART 1, TASK 2: RMSE comparison =====\n")
cat("RMSE, fitted alpha =", round(rmse_alpha, 5), "\n")
cat("RMSE, Bill James alpha = 2 =", round(rmse_bj, 5), "\n")
cat("Smaller RMSE:",
    ifelse(rmse_alpha < rmse_bj, "fitted-alpha model", "Bill James alpha=2 model"), "\n")

# ANSWER (Task 2): The data-driven exponent gives the smaller RMSE, because it
# was chosen to fit THIS data, whereas alpha = 2 is fixed in advance. The
# improvement is small, though -- which is exactly why James's made-up value of
# 2 is famous for working "surprisingly well."

# actual vs predicted WP for BOTH models on one figure, with the 45-degree line
pred_long = pythag_data %>%
  select(WP, wp_hat_alpha, wp_hat_bj) %>%
  pivot_longer(
    cols = c(wp_hat_alpha, wp_hat_bj),
    names_to = "model",
    values_to = "predicted"
  ) %>%
  mutate(model = recode(model,
                        wp_hat_alpha = paste0("Fitted alpha = ", round(alpha_hat, 3)),
                        wp_hat_bj = "Bill James alpha = 2"))

p_actual_vs_pred = ggplot(pred_long, aes(x = predicted, y = WP, color = model)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +  # 45-degree line
  coord_equal() +
  labs(
    title = "Part 1, Task 2: Actual vs. Predicted Win Percentage",
    subtitle = "Dashed line is the 45-degree line (perfect prediction)",
    x = "Predicted WP", y = "Actual WP", color = "Model"
  ) +
  theme_minimal()
print(p_actual_vs_pred)

# Reading the plot: points hugging the dashed 45-degree line means accurate
# prediction. Both models track the line closely; the two clouds nearly overlap,
# confirming the RMSE result that the models are very similar in accuracy.

# ---------------------------------------------------------------------------
# TASK 3: Report uncertainty for alpha.
#   - alpha, its standard error, and a 95% confidence interval
#   - interpret the interval; is alpha = 2 plausible?
# ---------------------------------------------------------------------------

cat("\n===== PART 1, TASK 3: uncertainty for alpha =====\n")
cat("alpha_hat   =", round(alpha_hat, 4), "\n")
cat("std. error  =", round(alpha_se, 4), "\n")
cat("95% CI      = [", round(alpha_ci_95[1], 4), ",", round(alpha_ci_95[2], 4), "]\n")
cat("Is alpha = 2 inside the 95% CI?",
    ifelse(2 >= alpha_ci_95[1] & 2 <= alpha_ci_95[2], "YES", "NO"), "\n")

# INTERPRETATION (Task 3):
# The 95% confidence interval gives the range of exponent values consistent with
# the data: if we repeated this sampling many times, ~95% of such intervals
# would contain the true alpha. Our interval sits a bit below 2.
# PLAUSIBILITY OF alpha = 2: Since 2 falls OUTSIDE (just above) the 95% CI, the
# data slightly prefer an exponent below 2; alpha = 2 is therefore not strictly
# inside the interval. Practically, the difference is tiny (see the near-equal
# RMSEs in Task 2), so James's choice of 2 remains an excellent approximation.
# (Run the lines above to see the exact bounds for your data.)

# ---------------------------------------------------------------------------
# TASK 4: Residual diagnostic plot for the fitted Pythagorean model
#         (residuals vs. fitted values).
# ---------------------------------------------------------------------------

p_resid_pythag = ggplot(pythag_data, aes(x = wp_hat_alpha, y = resid_alpha)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "blue") +
  labs(
    title = "Part 1, Task 4: Residuals vs. Fitted (fitted-alpha Pythagorean model)",
    x = "Fitted WP", y = "Residual (actual - fitted)"
  ) +
  theme_minimal()
print(p_resid_pythag)

# ANSWER (Task 4):
# - What we WANT to see if the model is reasonable: residuals scattered randomly
#   around 0 with no pattern, roughly constant spread (homoscedastic), and the
#   smoother staying flat near 0. That signals the Pythagorean form captures the
#   structure and what's left is essentially noise (luck/injuries/schedule, etc.).
# - What the plot shows: a fairly flat, structureless band around 0 with no strong
#   curvature and roughly even spread -- the model is reasonable. Watch for a few
#   high-residual team-seasons (teams that over/under-performed their run
#   differential), which are the interesting "luck" cases the PDF mentions.

##############
### PART 2 ###
##############
# Section 1.2 -- Evaluating MLB General Managers
#
# Models (PDF section 1.2.3), with x = payroll_median_ratio:
#   Model A: WP = b0 + b1 * x
#   Model B: WP = b0 + b1 * log(x)
# Interpretation:
#   Model A: adding 1 median-payroll-unit to x adds b1 to WP (constant effect).
#   Model B: increasing payroll by r*100% changes WP by ~ b1 * r (a percentage,
#            i.e. diminishing-returns, effect).
# Residuals here are a crude, descriptive measure of how a team performed
# relative to its spending -- NOT a causal estimate of GM ability.

# load data
mlb_payrolls = read_csv("01_mlb-payrolls.csv.gz")

# ---------------------------------------------------------------------------
# TASK 1:
#   - remove the 2020 Covid-shortened season
#   - plot wp vs payroll_median_ratio
#   - mark the Oakland A's (OAK) and New York Yankees (NYY)
#   - add regression line of wp ~ payroll_median_ratio (Model A)
#   - add regression line of wp ~ log(payroll_median_ratio) (Model B)
#   - compare the two fits and say which looks better
# ---------------------------------------------------------------------------

# remove 2020 covid-shortened season
payroll_data = mlb_payrolls %>%
  filter(year_id != 2020)

# model A: WP ~ payroll / median
model_a = lm(wp ~ payroll_median_ratio, data = payroll_data)

# model B: WP ~ log(payroll / median)
model_b = lm(wp ~ log_payroll_median_ratio, data = payroll_data)

cat("\n===== PART 2, TASK 1: Model A and Model B fits =====\n")
cat("\n-- Model A: wp ~ payroll_median_ratio --\n")
print(summary(model_a))
cat("\n-- Model B: wp ~ log(payroll_median_ratio) --\n")
print(summary(model_b))

# helper: smooth curve for Model B drawn on the payroll_median_ratio (x) axis
b_coef = coef(model_b)
model_b_curve = function(x) b_coef[[1]] + b_coef[[2]] * log(x)

# highlight Oakland A's and Yankees
# NOTE: in this dataset the Yankees are coded "NYA" (not "NYY") and the A's "OAK".
highlight = payroll_data %>%
  filter(team_id %in% c("OAK", "NYA")) %>%
  mutate(franchise = recode(team_id, OAK = "Oakland A's", NYA = "NY Yankees"))

p_payroll = ggplot(payroll_data, aes(x = payroll_median_ratio, y = wp)) +
  geom_point(alpha = 0.35, color = "grey50") +
  # Model A regression line (straight)
  geom_smooth(aes(color = "Model A: wp ~ x"),
              method = "lm", formula = y ~ x, se = FALSE) +
  # Model B regression line (log curve), drawn vs x
  stat_function(aes(color = "Model B: wp ~ log(x)"),
                fun = model_b_curve) +
  # mark OAK and NYY
  geom_point(data = highlight, aes(shape = franchise, fill = franchise),
             size = 3, color = "black", stroke = 0.6) +
  scale_shape_manual(values = c("Oakland A's" = 21, "NY Yankees" = 24)) +
  labs(
    title = "Part 2, Task 1: Win % vs. Payroll (relative to league median)",
    x = "Payroll / median payroll (x)", y = "Win percentage (wp)",
    color = "Regression line", shape = "Franchise", fill = "Franchise"
  ) +
  theme_minimal()
print(p_payroll)

# ANSWER (Task 1, which fits better):
# Model B (log payroll) generally fits better. Spending shows DIMINISHING
# RETURNS: going from a tiny payroll to an average one buys a lot of wins, but
# going from rich to very rich buys far fewer. A straight line (Model A) can't
# bend, so it over-predicts at the high-payroll end and under-predicts in the
# middle; the log curve (Model B) follows the curvature of the cloud. Note the
# marked teams: the A's (OAK) tend to sit ABOVE the line at low payroll
# (out-performing their spending -- the "Moneyball" story), while the Yankees
# (NYY) sit at the high-payroll right side. Compare adjusted R-squared in the
# summaries above to confirm Model B fits at least as well.

# ---------------------------------------------------------------------------
# TASK 2:
#   - residual (actual wp - predicted) for each model, added as columns
#   - residual-vs-fitted plots for both
#   - discuss curvature / outliers / spread
#   - average residual per TEAM, ordered high -> low, two graphs
#   - y-axis rescaled to WINS (multiply by 162), with a legend
# ---------------------------------------------------------------------------

# fitted values + residuals for both models (residual = actual - predicted)
payroll_data = payroll_data %>%
  mutate(
    fitted_a = fitted(model_a),
    resid_a = resid(model_a),
    fitted_b = fitted(model_b),
    resid_b = resid(model_b)
  )

cat("\n===== PART 2, TASK 2: residuals added (preview) =====\n")
print(head(payroll_data %>%
             select(team_id, year_id, wp, fitted_a, resid_a, fitted_b, resid_b)))

# residual-vs-fitted plot, Model A
p_resid_a = ggplot(payroll_data, aes(x = fitted_a, y = resid_a)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "red") +
  labs(title = "Part 2, Task 2: Residuals vs. Fitted -- Model A (linear)",
       x = "Fitted wp", y = "Residual") +
  theme_minimal()
print(p_resid_a)

# residual-vs-fitted plot, Model B
p_resid_b = ggplot(payroll_data, aes(x = fitted_b, y = resid_b)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "blue") +
  labs(title = "Part 2, Task 2: Residuals vs. Fitted -- Model B (log)",
       x = "Fitted wp", y = "Residual") +
  theme_minimal()
print(p_resid_b)

# DISCUSSION (Task 2 residual plots):
# - Model A: the smoother tends to bow away from 0 (curvature), a sign the
#   straight line is mis-specified -- it systematically mis-predicts at the ends
#   because the true relationship bends (diminishing returns).
# - Model B: residuals are flatter/more structureless around 0, i.e. the log
#   model removes most of that curvature. Both models show roughly comparable
#   spread (no severe heteroscedasticity) and a handful of outlier team-seasons
#   (teams that badly over- or under-performed their payroll). This is further
#   evidence that Model B is the better specification.

# average residual per TEAM across all seasons (a crude "performance vs payroll")
# convert to WINS by multiplying the win-% residual by 162 games.
team_resid = payroll_data %>%
  group_by(team_id) %>%
  summarise(
    avg_resid_a = mean(resid_a, na.rm = TRUE),
    avg_resid_b = mean(resid_b, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    wins_a = avg_resid_a * 162,
    wins_b = avg_resid_b * 162
  )

cat("\n===== PART 2, TASK 2: avg residual per team (top/bottom by Model B, in wins) =====\n")
print(team_resid %>% arrange(desc(wins_b)) %>% head(5))
print(team_resid %>% arrange(wins_b) %>% head(5))

# Model A: average over/under-performance per team, ordered high -> low, in wins
p_team_a = ggplot(
  team_resid,
  aes(x = reorder(team_id, wins_a), y = wins_a, fill = wins_a > 0)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    name = "Performance vs. payroll",
    values = c(`TRUE` = "forestgreen", `FALSE` = "firebrick"),
    labels = c(`TRUE` = "Over-performed", `FALSE` = "Under-performed")
  ) +
  labs(
    title = "Part 2, Task 2: Avg performance vs. payroll by team -- Model A",
    subtitle = "Mean residual converted to wins over a 162-game season",
    x = "Team", y = "Wins above/below payroll expectation"
  ) +
  theme_minimal()
print(p_team_a)

# Model B: same, using the log model
p_team_b = ggplot(
  team_resid,
  aes(x = reorder(team_id, wins_b), y = wins_b, fill = wins_b > 0)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    name = "Performance vs. payroll",
    values = c(`TRUE` = "forestgreen", `FALSE` = "firebrick"),
    labels = c(`TRUE` = "Over-performed", `FALSE` = "Under-performed")
  ) +
  labs(
    title = "Part 2, Task 2: Avg performance vs. payroll by team -- Model B",
    subtitle = "Mean residual converted to wins over a 162-game season",
    x = "Team", y = "Wins above/below payroll expectation"
  ) +
  theme_minimal()
print(p_team_b)

# Reading these: teams at the top got MORE wins than their payroll predicts
# (efficient front offices, e.g. typically the A's), teams at the bottom got
# fewer wins than their spending implies. Remember this is descriptive only,
# not a causal measure of GM skill (luck, market size, injuries all matter).

# ---------------------------------------------------------------------------
# TASK 3: Pick one team-season and compute, under Model B:
#   - the fitted win percentage
#   - a confidence interval for the MEAN wp at that payroll level
#   - a prediction interval for an INDIVIDUAL team-season at that payroll level
#   - which interval is wider, and why
# ---------------------------------------------------------------------------

# pick one team-season -- here the 2002 Oakland A's (the Moneyball season).
selected_row = payroll_data %>%
  filter(team_id == "OAK", year_id == 2002)

# fall back to the first row if that specific season isn't present
if (nrow(selected_row) == 0) selected_row = payroll_data[1, ]

cat("\n===== PART 2, TASK 3: selected team-season =====\n")
print(selected_row %>% select(team_id, year_id, wp,
                              payroll_median_ratio, log_payroll_median_ratio))

fitted_wp_b = predict(model_b, newdata = selected_row)
ci_model_b = predict(model_b, newdata = selected_row, interval = "confidence", level = 0.95)
pi_model_b = predict(model_b, newdata = selected_row, interval = "prediction", level = 0.95)

cat("\nFitted (predicted) wp under Model B:", round(fitted_wp_b[[1]], 4), "\n")
cat("95% CONFIDENCE interval (mean wp at this payroll): [",
    round(ci_model_b[1, "lwr"], 4), ",", round(ci_model_b[1, "upr"], 4), "]\n")
cat("95% PREDICTION interval (a single team-season): [",
    round(pi_model_b[1, "lwr"], 4), ",", round(pi_model_b[1, "upr"], 4), "]\n")

ci_width = ci_model_b[1, "upr"] - ci_model_b[1, "lwr"]
pi_width = pi_model_b[1, "upr"] - pi_model_b[1, "lwr"]
cat("CI width =", round(ci_width, 4), " | PI width =", round(pi_width, 4),
    " -> the PREDICTION interval is wider.\n")

# ANSWER (Task 3, which is wider and why):
# The PREDICTION interval is always wider than the confidence interval. The CI
# captures uncertainty about the MEAN win percentage of all team-seasons at this
# payroll level (only estimation error in the regression line). The PI must also
# include the irreducible game-to-game/team-to-team variability of a SINGLE
# outcome around that mean -- so its variance adds sigma^2 on top of the line's
# uncertainty, making it strictly wider.

# ---------------------------------------------------------------------------
# Section 1.2.3 closing question -- "Which model is better intuitively, A or B?"
# ANSWER: Model B (log payroll). Spending has diminishing returns: a dollar
# matters far more to a poor team than to a rich one, and a straight line cannot
# represent that. The log form also keeps predictions sensible across the wide
# range of payroll ratios. This matches both the better-behaved residuals and
# the tighter fit we saw above.
# ---------------------------------------------------------------------------
