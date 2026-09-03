########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "mgcv", "readr"))

library(dplyr)
library(ggplot2)
library(mgcv)
library(readr)

set.seed(2026)

# Small helpers
rmse = function(actual, pred) sqrt(mean((actual - pred)^2))
log_loss = function(y, p) {
  eps = 1e-15
  p = pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

if (!dir.exists("plots")) dir.create("plots")

###############################
### PART 1: BATTING AVERAGE ###
###############################

# Use the CSV file in this folder
ba_data = read_csv("04_ba-2020-2021.csv.gz", show_col_types = FALSE)

# Task 1:
# - Fit the day-1 style linear regression model BA_2021 ~ BA_2020
# - Fit a binomial GLM of the form
#     cbind(H_2021, AB_2021 - H_2021) ~ BA_2020
# - For the GLM, remember that each player has AB_2021 Bernoulli trials, not just one

lm_ba = lm(BA_2021 ~ BA_2020, data = ba_data)
glm_ba = glm(cbind(H_2021, AB_2021 - H_2021) ~ BA_2020,
             family = binomial, data = ba_data)

# Fitted models (from the script output):
# Linear:  E[BA_2021 | BA_2020] = 0.1621 + 0.3247 * BA_2020
# Binomial GLM (log-odds of a hit):
#          logit(p) = -1.4630 + 1.4814 * BA_2020

###############################################
### Task 2: Compare the fitted mean curves. ###
###############################################
# - Plot BA_2021 against BA_2020
# - Add both fitted curves to the same figure
# - Make it visually clear which players had more at-bats in 2021

grid_ba = data.frame(BA_2020 = seq(min(ba_data$BA_2020), max(ba_data$BA_2020),
                                   length.out = 200))
grid_ba$lm_fit = predict(lm_ba, newdata = grid_ba)
grid_ba$glm_fit = predict(glm_ba, newdata = grid_ba, type = "response")

p_ba = ggplot(ba_data, aes(BA_2020, BA_2021)) +
  geom_point(aes(size = AB_2021), alpha = 0.5, color = "grey30") +
  geom_line(data = grid_ba, aes(BA_2020, lm_fit, color = "Linear regression"),
            linewidth = 1.1) +
  geom_line(data = grid_ba, aes(BA_2020, glm_fit, color = "Binomial GLM"),
            linewidth = 1.1) +
  scale_color_manual(values = c("Linear regression" = "#D55E00",
                                "Binomial GLM" = "#0072B2")) +
  scale_size_continuous(name = "AB 2021", range = c(0.5, 6)) +
  labs(title = "BA 2021 vs BA 2020: linear vs binomial GLM fits",
       x = "BA 2020", y = "BA 2021", color = "Model") +
  theme_minimal()
ggsave("plots/p1_task2_ba_fits.png", p_ba, width = 8, height = 5, dpi = 150)

# How the two fits differ and why the GLM is more natural:
# The two mean curves are very close over the observed range of BA_2020,
# because batting averages live in a narrow band (~0.20-0.32) where the logit
# curve is nearly linear. The key difference is structural, not visual: the
# linear model can predict averages below 0 or above 1 and treats every player
# as one equally-weighted observation. The binomial GLM models hits out of
# at-bats, so it (a) keeps the fitted mean in [0,1] by construction, (b)
# automatically gives players with more at-bats more influence (more Bernoulli
# trials = more information), and (c) builds in the correct mean-variance link
# for count data (Var = AB * p(1-p)) instead of assuming constant-variance
# Gaussian noise. That makes it the natural model for hits and outs.

##############################################
### Task 3: Interpret the binomial GLM.    ###
##############################################
# - Interpret the GLM coefficient on the log-odds scale
# - Translate the coefficient into the effect of a 0.010 increase in BA_2020
# - Explain why this GLM is more natural than ordinary linear regression

glm_summary = summary(glm_ba)
print(glm_summary$coefficients)
glm_ci = confint.default(glm_ba)        # Wald 95% CI
print(glm_ci)

slope = coef(glm_ba)["BA_2020"]         # ~1.4814

# Interpretation (numbers from the output above):
# - Slope on the log-odds scale: a 1.000 increase in BA_2020 multiplies the
#   odds of a hit by exp(1.4814) ~ 4.40. A full unit of batting average is not
#   physically meaningful, so the per-0.010 translation below is the useful one.
# - A 0.010 increase in BA_2020 multiplies the odds of a 2021 hit by
#   exp(0.010 * 1.4814) = exp(0.01481) ~ 1.0149, i.e. about a 1.5% increase in
#   the odds of a hit per 10 points of 2020 batting average.
# - 95% Wald CI for the slope is roughly (1.198, 1.765), so the positive
#   association is clearly distinguishable from zero.
# - Why the GLM is more natural than OLS for hits/outs: the response is a count
#   of successes out of a known number of trials. The GLM keeps the predicted
#   probability in [0,1], uses the binomial mean-variance relationship instead
#   of assuming constant Gaussian variance, and weights each player by AB so
#   that a 600-AB season counts for more than a 60-AB season.

####################################################
### Task 4: Show why the denominator matters.    ###
####################################################
# - Hypothetical player with BA_2020 = 0.260
# - Estimate 2021 hit probability p from the GLM
# - Compare AB = 60 vs AB = 600: expected hits and approx 95% BA interval

p_hat = predict(glm_ba, newdata = data.frame(BA_2020 = 0.260),
                type = "response")
p_hat = as.numeric(p_hat)               # ~0.254

ba_interval = function(p, AB) {
  se = sqrt(p * (1 - p) / AB)
  c(expected_hits = AB * p,
    lower = p - 1.96 * se,
    upper = p + 1.96 * se,
    width = 2 * 1.96 * se)
}

cat("p_hat for BA_2020 = 0.260:", round(p_hat, 4), "\n")
print(round(ba_interval(p_hat, 60), 4))
print(round(ba_interval(p_hat, 600), 4))

# Results (from output):
# - Estimated hit probability p ~ 0.254 for both workloads (p depends only on
#   BA_2020, not on AB).
# - AB = 60:  expected hits ~ 15.2, approx 95% BA interval ~ (0.144, 0.364),
#   width ~ 0.220.
# - AB = 600: expected hits ~ 152.3, approx 95% BA interval ~ (0.219, 0.289),
#   width ~ 0.070.
# - Why the low-AB player's interval is much wider: the standard error of an
#   observed batting average scales like sqrt(p(1-p)/AB), i.e. with 1/sqrt(AB).
#   Going from 60 to 600 at-bats multiplies AB by 10, so the interval shrinks by
#   sqrt(10) ~ 3.16. Same true hit probability, far less sampling noise when you
#   observe more trials.

#######################################
### PART 2: FIELD-GOAL SUCCESS GAM ###
#######################################

fg_data = read_csv("04_field-goals.csv.gz", show_col_types = FALSE)

####################################################
### Task 1: Fit competing probability models.    ###
####################################################
# - one logistic GLM with a simple functional form in ydl
# - one richer logistic GLM (quadratic in ydl, plus kq)
# - one logistic GAM via mgcv::gam(...)

# Train/test split for out-of-sample comparison (Task 2)
n = nrow(fg_data)
train_idx = sample(seq_len(n), size = floor(0.7 * n))
fg_train = fg_data[train_idx, ]
fg_test = fg_data[-train_idx, ]

# Model A: simple logistic GLM, linear in ydl
glm_simple = glm(fg_made ~ ydl, family = binomial, data = fg_train)

# Model B: richer logistic GLM, quadratic in ydl + kicker quality
#   logit(p) = b0 + b1*ydl + b2*ydl^2 + b3*kq
glm_rich = glm(fg_made ~ ydl + I(ydl^2) + kq, family = binomial, data = fg_train)

# Model C: logistic GAM with a smooth in ydl + linear kq
gam_fg = gam(fg_made ~ s(ydl, k = 12) + kq, family = "binomial",
             method = "REML", data = fg_train)

####################################################
### Task 2: Compare the models out of sample.    ###
####################################################
# - Compare with test-set log loss (main metric)

pred_simple = predict(glm_simple, newdata = fg_test, type = "response")
pred_rich = predict(glm_rich, newdata = fg_test, type = "response")
pred_gam = as.numeric(predict(gam_fg, newdata = fg_test, type = "response"))

ll_table = data.frame(
  model = c("GLM simple (ydl)", "GLM rich (ydl + ydl^2 + kq)", "GAM s(ydl)+kq"),
  test_log_loss = c(log_loss(fg_test$fg_made, pred_simple),
                    log_loss(fg_test$fg_made, pred_rich),
                    log_loss(fg_test$fg_made, pred_gam))
)
print(ll_table)

# Preferred model (numbers from output):
# - Test-set log loss: simple GLM ~ 0.3751, rich GLM ~ 0.3743, GAM ~ 0.3742.
# - The richer GLM and the GAM both beat the straight-line GLM, because make
#   probability is not linear in ydl on the logit scale (it is flat and near 1
#   for short kicks, then drops off). The GAM has the best (lowest) test log
#   loss, with the quadratic GLM essentially tied (the three are close because
#   over most of the data the curve is fairly smooth). I prefer the GAM: it
#   matches the best predictive performance while letting the data choose the
#   shape of the ydl effect rather than forcing a specific polynomial, and it
#   also models kicker quality. The quadratic GLM is a close, simpler runner-up.

####################################################
### Task 3: Interpret the GAM fit.               ###
####################################################
# - estimated parametric coefficient(s) (kq) + SE + 95% CI
# - effective degrees of freedom (edf) of the smooth term
# - what edf noticeably larger than 1 means

gam_sum = summary(gam_fg)
print(gam_sum$p.table)        # parametric coefficients (intercept, kq)
print(gam_sum$s.table)        # smooth term: edf etc.

kq_coef = gam_sum$p.table["kq", "Estimate"]
kq_se = gam_sum$p.table["kq", "Std. Error"]
kq_ci = kq_coef + c(-1, 1) * 1.96 * kq_se
cat("kq coef:", round(kq_coef, 4), " SE:", round(kq_se, 4),
    " 95% CI: (", round(kq_ci[1], 4), ",", round(kq_ci[2], 4), ")\n")
edf_ydl = gam_sum$s.table["s(ydl)", "edf"]
cat("edf of s(ydl):", round(edf_ydl, 3), "\n")

# Interpretation (from output):
# - kq coefficient ~ 0.324 (SE ~ 0.065), 95% CI ~ (0.197, 0.451). On the
#   log-odds scale, a one-unit increase in kicker quality multiplies the odds of
#   a make by exp(0.324) ~ 1.38, holding distance fixed; the interval excludes 0
#   so better kickers make significantly more field goals.
# - edf of s(ydl) ~ 4.06, well above 1.
# - An edf noticeably larger than 1 means the smooth is meaningfully nonlinear:
#   edf = 1 would be a straight line on the logit scale, so edf ~ 4.06 says the
#   penalized fit uses roughly 4 effective parameters worth of curvature to
#   capture how make probability bends with distance (flat for short kicks, then
#   falling off steeply for long ones). The wiggliness penalty kept it from
#   chasing noise, so this is genuine signal, not overfitting.

####################################################
### Task 4: Visualize the fitted curves.         ###
####################################################
# - Predicted make prob vs ydl for preferred GLM and preferred GAM
# - Overlay binned observed make rates
# - Comment on where the GAM improves on the simpler GLM

# Hold kq at its median so the curves are comparable across models
kq_med = median(fg_data$kq)
ydl_grid = data.frame(ydl = seq(min(fg_data$ydl), max(fg_data$ydl),
                                length.out = 200),
                      kq = kq_med)
ydl_grid$glm_rich = predict(glm_rich, newdata = ydl_grid, type = "response")
ydl_grid$gam = as.numeric(predict(gam_fg, newdata = ydl_grid, type = "response"))

# Binned observed make rates
binned = fg_data %>%
  mutate(ydl_bin = cut(ydl, breaks = seq(0, max(ydl) + 5, by = 5))) %>%
  group_by(ydl_bin) %>%
  summarise(ydl_mid = mean(ydl), make_rate = mean(fg_made), n = n(),
            .groups = "drop")

p_fg = ggplot() +
  geom_point(data = binned, aes(ydl_mid, make_rate, size = n),
             alpha = 0.5, color = "grey30") +
  geom_line(data = ydl_grid, aes(ydl, glm_rich, color = "GLM (quadratic + kq)"),
            linewidth = 1.1) +
  geom_line(data = ydl_grid, aes(ydl, gam, color = "GAM s(ydl) + kq"),
            linewidth = 1.1) +
  scale_color_manual(values = c("GLM (quadratic + kq)" = "#D55E00",
                                "GAM s(ydl) + kq" = "#0072B2")) +
  scale_size_continuous(name = "kicks in bin") +
  labs(title = "Field-goal make probability vs distance (kq at median)",
       x = "Yard line (from opponent end zone)", y = "P(make)",
       color = "Model") +
  theme_minimal()
ggsave("plots/p2_task4_fg_curves.png", p_fg, width = 8, height = 5, dpi = 150)

# Where the GAM improves on the simpler GLM:
# - Both curves agree closely in the well-populated mid range (~15-45 yards),
#   where there is plenty of data and the relationship is smooth.
# - The GAM tracks the binned observed rates better at the extremes: it stays
#   near 1 for very short kicks and bends more flexibly for long attempts,
#   whereas the fixed quadratic is forced into a symmetric parabola shape and
#   can curve the wrong way (even turning back up) at the longest distances
#   where data are sparse. The GAM lets the data set the shape instead.

####################################################
### Task 5: Make concrete predictions.           ###
####################################################
# - For median-kq kicker, P(make) at ydl = 20, 35, 50
# - Build a 95% CI on the logit scale, transform back with plogis()

pred_pts = data.frame(ydl = c(20, 35, 50), kq = kq_med)
link_pred = predict(gam_fg, newdata = pred_pts, type = "link", se.fit = TRUE)
p_pred = plogis(link_pred$fit)
ci_lo = plogis(link_pred$fit - 1.96 * link_pred$se.fit)
ci_hi = plogis(link_pred$fit + 1.96 * link_pred$se.fit)

pred_out = data.frame(ydl = pred_pts$ydl,
                      p_make = round(p_pred, 4),
                      ci_lower = round(ci_lo, 4),
                      ci_upper = round(ci_hi, 4))
print(pred_out)

# Results (from output, median kq ~ 0.118; note ydl is measured from the
# opponent end zone, so ydl = 50 is a ~67-yard attempt, hence the low prob):
# - ydl = 20: P(make) ~ 0.87, 95% CI ~ (0.85, 0.88).
# - ydl = 35: P(make) ~ 0.62, 95% CI ~ (0.60, 0.65).
# - ydl = 50: P(make) ~ 0.14, 95% CI ~ (0.06, 0.28).
# Make probability falls steeply with distance, and the CI is built on the logit
# scale (where the normal approximation is better behaved) and mapped back with
# plogis(), which guarantees the interval stays inside [0, 1].

####################################################
### Task 6: Reflect.                             ###
####################################################
# Difference between hand-chosen polynomial terms in a GLM and a GAM smooth:
# - In a GLM you must decide the functional form yourself (linear, quadratic,
#   cubic, knot locations, interactions). The shape is fixed before you see the
#   fit, and a polynomial is a single global formula whose tails can behave
#   badly. In a GAM you specify a flexible spline basis and a wiggliness penalty
#   (chosen by REML); the data decide how curved the relationship is, and the
#   penalty shrinks toward a straight line unless the data justify more wiggle.
# - One reason a GAM can help: it captures nonlinear structure you did not
#   anticipate (e.g. the flat-then-falling make-rate curve) without manual
#   trial-and-error, usually improving out-of-sample fit.
# - One reason a GAM can hurt: the extra flexibility can overfit in sparse
#   regions (long kicks here) or just be harder to interpret and communicate
#   than a single coefficient, and with little data the penalty may not fully
#   protect against chasing noise.

cat("\nScript completed successfully.\n")
