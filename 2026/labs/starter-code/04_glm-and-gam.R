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
ba_data = read_csv("../data/04_ba-2020-2021.csv.gz", show_col_types = FALSE)

# Task 1:
# - Fit the day-1 style linear regression model BA_2021 ~ BA_2020
# - Fit a binomial GLM of the form
#     cbind(H_2021, AB_2021 - H_2021) ~ BA_2020
# - For the GLM, remember that each player has AB_2021 Bernoulli trials, not just one

# Task 2:
# - Compare the fitted mean curves from the linear model and the binomial GLM
# - Plot BA_2021 against BA_2020
# - Add both fitted curves to the same figure
# - Make it visually clear which players had more at-bats in 2021

# Task 3:
# - Interpret the GLM coefficient on the log-odds scale
# - Translate the coefficient into the effect of a 0.010 increase in BA_2020
# - Explain why this GLM is a more natural model for hits/outs than ordinary linear regression

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

#######################################
### PART 2: FIELD-GOAL SUCCESS GAM ###
#######################################

fg_data = read_csv("../data/04_field-goals.csv.gz", show_col_types = FALSE)

# Task 1:
# - Fit at least 3 competing probability models for fg_made
# - Include:
#     * one logistic GLM with a simple functional form in ydl
#     * one richer logistic GLM (for example quadratic in ydl, possibly with kq)
#     * one logistic GAM using mgcv::gam(...)
# - A good starting point for the GAM is:
#     gam(fg_made ~ s(ydl, k = 12) + kq, family = "binomial", method = "REML")

# Task 2:
# - Compare the models using out-of-sample predictive performance
# - Use test-set log loss or cross-validated log loss as the main metric
# - State clearly which model you prefer and why

# Task 3:
# - For the selected GAM, report:
#     * the estimated parametric coefficient(s)
#     * the effective degrees of freedom (edf) of the smooth term
# - Explain what it means if the edf is noticeably larger than 1

# Task 4:
# - Plot predicted make probability against ydl for your preferred GLM and your preferred GAM
# - Also compute binned observed make rates and overlay them on the plot
# - Comment on where the GAM improves on the simpler GLM

# Task 5:
# - For a kicker with league-median kq, estimate make probability at:
#     * 20 yards from the opponent's end zone
#     * 35 yards from the opponent's end zone
#     * 50 yards from the opponent's end zone
# - For at least one of these yard lines, compute an approximate 95% confidence interval
#   using predict(..., type = "link", se.fit = TRUE) and then transform back with plogis()

# Task 6:
# - Briefly explain the difference between
#     * choosing polynomial terms by hand in a GLM, and
#     * letting a GAM learn a smooth curve with a wiggliness penalty
# - State one reason a GAM can help and one reason it can hurt
