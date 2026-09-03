########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "mgcv", "readr"))

library(dplyr)
library(ggplot2)
library(mgcv)
library(readr)

###############################
### PART 1: BATTING AVERAGE ###
###############################

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
ba_data <- read_csv("~/Desktop/04_ba-2020-2021.csv.gz", show_col_types = FALSE)

# Task 1:
# - Fit the day-1 style linear regression model BA_2021 ~ BA_2020
# - Fit a binomial GLM of the form
#     cbind(H_2021, AB_2021 - H_2021) ~ BA_2020
# - For the GLM, remember that each player has AB_2021 Bernoulli trials, not just one

ba_lm = lm(BA_2021 ~ BA_2020, data = ba_data)

ba_glm = glm(
  cbind(H_2021, AB_2021 - H_2021) ~ BA_2020,
  data = ba_data,
  family = binomial
)

summary(ba_lm)
summary(ba_glm)

# Task 2:
# - Compare the fitted mean curves from the linear model and the binomial GLM
# - Plot BA_2021 against BA_2020
# - Add both fitted curves to the same figure
# - Make it visually clear which players had more at-bats in 2021

ba_grid = data.frame(
  BA_2020 = seq(min(ba_data$BA_2020), max(ba_data$BA_2020), length.out = 200)
)

ba_grid = ba_grid %>%
  mutate(
    lm_pred = predict(ba_lm, newdata = ba_grid),
    glm_pred = predict(ba_glm, newdata = ba_grid, type = "response")
  )

ba_plot = ggplot(ba_data, aes(x = BA_2020, y = BA_2021)) +
  geom_point(aes(size = AB_2021), alpha = 0.45) +
  geom_line(data = ba_grid, aes(y = lm_pred, linetype = "Linear model"), linewidth = 1) +
  geom_line(data = ba_grid, aes(y = glm_pred, linetype = "Binomial GLM"), linewidth = 1) +
  labs(
    x = "2020 batting average",
    y = "2021 batting average",
    size = "2021 AB",
    linetype = "Model"
  )

print(ba_plot)



# Task 3:
# - Interpret the GLM coefficient on the log-odds scale
# - Translate the coefficient into the effect of a 0.010 increase in BA_2020
# - Explain why this GLM is a more natural model for hits/outs than ordinary linear regression

glm_coef = coef(ba_glm)["BA_2020"]
odds_ratio_0010 = exp(glm_coef * 0.010)

#The binomial GLM fits more here because each player has H_2021 hits out of AB_2021 hit-or-out trials, so the response is a count of successes out of a known number of attempts which is best described with the binomial. 


# Task 4:
# - Pick one hypothetical player with BA_2020 = 0.260
# - Using your fitted GLM, estimate that player's 2021 hit probability p
# - Then compare two possible workloads:
#     (a) AB_2021 = 60
#     (b) AB_2021 = 600
# - For each workload, report:
#     * expected hits = AB_2021 * p
#     * an approximate 95% interval for batting average using p +/- 1.96 * sqrt(p(1-p)/AB_2021)
# - Briefly explain why the interval is much wider for the low-AB player

hyp_player = data.frame(BA_2020 = 0.260)
hyp_p = predict(ba_glm, newdata = hyp_player, type = "response")

ba_workloads = data.frame(AB_2021 = c(60, 600)) %>%
  mutate(
    p = as.numeric(hyp_p),
    expected_hits = AB_2021 * p,
    se_ba = sqrt(p * (1 - p) / AB_2021),
    lower_95 = p - 1.96 * se_ba,
    upper_95 = p + 1.96 * se_ba
  )

print(ba_workloads)

# the standard error gets smaller as the number of at-bats increases.


#######################################
### PART 2: FIELD-GOAL SUCCESS GAM ###
#######################################

fg_data <- read_csv("~/Desktop/04_field-goals.csv.gz") 

set.seed(4)
train_index = sample(1:nrow(fg_data), size = round(0.7 * nrow(fg_data)))
fg_train = fg_data[train_index, ]
fg_test = fg_data[-train_index, ] 

# Task 1:
# - Fit at least 3 competing probability models for fg_made
# - Include:
#     * one logistic GLM with a simple functional form in ydl
#     * one richer logistic GLM (for example quadratic in ydl, possibly with kq)
#     * one logistic GAM using mgcv::gam(...)
# - A good starting point for the GAM is:
#     gam(fg_made ~ s(ydl, k = 12) + kq, family = "binomial", method = "REML")

fg_glm_simple = glm(fg_made ~ ydl, data = fg_train, family = binomial)

fg_glm_rich = glm(
  fg_made ~ ydl + I(ydl^2) + kq,
  data = fg_train,
  family = binomial
)

fg_gam = gam(
  fg_made ~ s(ydl, k = 12) + kq,
  data = fg_train,
  family = binomial,
  method = "REML"
)

summary(fg_glm_simple)
summary(fg_glm_rich)
summary(fg_gam)

# Task 2:
# - Compare the models using out-of-sample predictive performance
# - Use test-set log loss or cross-validated log loss as the main metric
# - State clearly which model you prefer and why

log_loss = function(y, p) {
  p = pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

fg_model_results = data.frame(
  model = c("Simple logistic GLM", "Richer logistic GLM", "Logistic GAM"),
  test_log_loss = c(
    log_loss(fg_test$fg_made, predict(fg_glm_simple, newdata = fg_test, type = "response")),
    log_loss(fg_test$fg_made, predict(fg_glm_rich, newdata = fg_test, type = "response")),
    log_loss(fg_test$fg_made, predict(fg_gam, newdata = fg_test, type = "response"))
  )
)

print(fg_model_results)

preferred_model_name = fg_model_results$model[which.min(fg_model_results$test_log_loss)]

# GAM because it has the lowest test-set log loss

# Task 3:
# - For the selected GAM, report:
#     * the estimated parametric coefficient(s)
#     * the effective degrees of freedom (edf) of the smooth term
# - Explain what it means if the edf is noticeably larger than 1

gam_summary = summary(fg_gam)
gam_parametric_coefficients = gam_summary$p.table
gam_smooth_table = gam_summary$s.table

print(gam_parametric_coefficients)
print(gam_smooth_table)



# Task 4:
# - Plot predicted make probability against ydl for your preferred GLM and your preferred GAM
# - Also compute binned observed make rates and overlay them on the plot
# - Comment on where the GAM improves on the simpler GLM

preferred_glm = fg_glm_rich
median_kq = median(fg_data$kq, na.rm = TRUE)

fg_grid = data.frame(
  ydl = seq(min(fg_data$ydl), max(fg_data$ydl), length.out = 200),
  kq = median_kq
) %>%
  mutate(
    glm_pred = predict(preferred_glm, newdata = ., type = "response"),
    gam_pred = predict(fg_gam, newdata = ., type = "response")
  )

fg_binned = fg_data %>%
  mutate(ydl_bin = cut(ydl, breaks = seq(min(ydl), max(ydl) + 5, by = 5), include.lowest = TRUE)) %>%
  group_by(ydl_bin) %>%
  summarize(
    ydl_mid = mean(ydl),
    observed_make_rate = mean(fg_made),
    n = n(),
    .groups = "drop"
  )

fg_plot = ggplot() +
  geom_line(data = fg_grid, aes(x = ydl, y = glm_pred, linetype = "Preferred GLM"), linewidth = 1) +
  geom_line(data = fg_grid, aes(x = ydl, y = gam_pred, linetype = "GAM"), linewidth = 1) +
  geom_point(data = fg_binned, aes(x = ydl_mid, y = observed_make_rate, size = n), alpha = 0.65) +
  labs(
    x = "Yard line",
    y = "Predicted / observed make probability",
    linetype = "Model",
    size = "Bin count"
  )

print(fg_plot)

cat("Part 2 Task 4 answer: the GAM improves where the outliers go out of a GLM curve like the end points. 
")

# Task 5:
# - For a kicker with league-median kq, estimate make probability at:
#     * 20 yards from the opponent's end zone
#     * 35 yards from the opponent's end zone
#     * 50 yards from the opponent's end zone
# - For at least one of these yard lines, compute an approximate 95% confidence interval
#   using predict(..., type = "link", se.fit = TRUE) and then transform back with plogis()

fg_hyp = data.frame(
  ydl = c(20, 35, 50),
  kq = median_kq
)

fg_link_pred = predict(fg_gam, newdata = fg_hyp, type = "link", se.fit = TRUE)

fg_prob_table = fg_hyp %>%
  mutate(
    fit_link = as.numeric(fg_link_pred$fit),
    se_link = as.numeric(fg_link_pred$se.fit),
    make_probability = plogis(fit_link),
    lower_95 = plogis(fit_link - 1.96 * se_link),
    upper_95 = plogis(fit_link + 1.96 * se_link)
  )

print(fg_prob_table)



# Task 6:
# - Briefly explain the difference between
#     * choosing polynomial terms by hand in a GLM, and
#     * letting a GAM learn a smooth curve with a wiggliness penalty
# - State one reason a GAM can help and one reason it can hurt

 # We choose the polynomial terms like (ydl ...) manually in a GLM. However, I think the GAM might be at risk of overfitting even from the graph earlier. 