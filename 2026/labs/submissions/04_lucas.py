
########################
### SETUP / IMPORTS  ###
########################

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
import statsmodels.formula.api as smf
from pygam import LogisticGAM, s, l
from sklearn.model_selection import train_test_split
from sklearn.metrics import log_loss
from sklearn.model_selection import KFold


###############################
### PART 1: BATTING AVERAGE ###
###############################

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
ba_data = pd.read_csv("../data/04_ba-2020-2021.csv.gz")

# Task 1:
# - Fit the day-1 style linear regression model BA_2021 ~ BA_2020
# - Fit a binomial GLM of the form
#     cbind(H_2021, AB_2021 - H_2021) ~ BA_2020
# - For the GLM, remember that each player has AB_2021 Bernoulli trials, not just one

# Linear Regression
lm = smf.ols("BA_2021 ~ BA_2020", data=ba_data).fit()
print("Linear Regression:")
print(lm.summary())
print(f"\nFitted model: E[BA_2021 | BA_2020] = {lm.params['Intercept']:.4f} + {lm.params['BA_2020']:.4f} * BA_2020")

# Binomial GLM
y = np.column_stack([ba_data["H_2021"], ba_data["AB_2021"] - ba_data["H_2021"]])
X = sm.add_constant(ba_data["BA_2020"])

glm = sm.GLM(y, X, family=sm.families.Binomial()).fit()
print("\nBinomial GLM:")
print(glm.summary())

ba_data["p_hat"] = glm.predict()


# Task 2:
# - Compare the fitted mean curves from the linear model and the binomial GLM
# - Plot BA_2021 against BA_2020
# - Add both fitted curves to the same figure
# - Make it visually clear which players had more at-bats in 2021

x_range = np.linspace(ba_data["BA_2020"].min(), ba_data["BA_2020"].max(), 300)

lm_preds = lm.params["Intercept"] + lm.params["BA_2020"] * x_range

log_odds = glm.params["const"] + glm.params["BA_2020"] * x_range
glm_preds = 1 / (1 + np.exp(-log_odds))

# Plot
fig, ax = plt.subplots(figsize=(8, 6))

# Scatter: dot size reflects AB_2021
scatter = ax.scatter(
    ba_data["BA_2020"], ba_data["BA_2021"],
    s=ba_data["AB_2021"] / 5,   # scale down so dots aren't huge
    alpha=0.4, color="gray", label="Players (size = AB_2021)"
)

# Fitted curves
ax.plot(x_range, lm_preds, color="blue", linewidth=2, label="Linear regression")
ax.plot(x_range, glm_preds, color="red", linewidth=2, label="Binomial GLM")

ax.set_xlabel("BA 2020")
ax.set_ylabel("BA 2021")
ax.set_title("BA 2021 vs BA 2020 with fitted mean curves")
ax.legend()
plt.tight_layout()
plt.savefig("../data/plots/task2_fitted_curves.png", dpi=150)
plt.show()

# Task 3:
# - Interpret the GLM coefficient on the log-odds scale
# - Translate the coefficient into the effect of a 0.010 increase in BA_2020
# - Explain why this GLM is a more natural model for hits/outs than ordinary linear regression

print("GLM coefficient = 1.48 --> a 1 unit batting average increase in 2020 increases log odds by 1.48")
print("Converting to normal odds, we get an odds factor of e^1.48x, so a 0.01 batting increase in 2020 results in a e^0.0148=1.015 increase in odds of getting a hit in 2021")
print("""Constrains batting averages from 0 to 1 and scales proportionally using log loss 
      --> meaning we increasingly punish larger magnitude errors compared to treating them equal in LSS""")

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

def predict_p(ba_2020):
    log_odds = glm.params["const"] + glm.params["BA_2020"] * ba_2020
    return 1 / (1 + np.exp(-log_odds))

player = ba_data[ba_data["playerID"] == "adamewi01"].iloc[0]
ba_2020 = player["BA_2020"]
p = predict_p(ba_2020)

print(f"Player: adamewi01")
print(f"BA_2020 = {ba_2020:.4f}")
print(f"Estimated hit probability p = {p:.4f}")

for ab in [60, 600]:
    expected_hits = ab * p
    margin = 1.96 * np.sqrt(p * (1 - p) / ab)
    lower = p - margin
    upper = p + margin
    print(f"\nAB_2021 = {ab}:")
    print(f"  Expected hits:        {expected_hits:.1f}")
    print(f"  95% interval for BA:  ({lower:.4f}, {upper:.4f})")
    print(f"  Interval width:       {2 * margin:.4f}")
    
print("The interval as seen in the prior equation is proportional to 1/sqrt(AB) so fewer ABs decreases our confidence in the interval.")

#######################################
### PART 2: FIELD-GOAL SUCCESS GAM ###
#######################################

fg_data = pd.read_csv("../data/04_field-goals.csv.gz")

# Task 1:
# - Fit at least 3 competing probability models for fg_made
# - Include:
#     * one logistic GLM with a simple functional form in ydl
#     * one richer logistic GLM (for example quadratic in ydl, possibly with kq)
#     * one logistic GAM using mgcv::gam(...)
# - A good starting point for the GAM is:
#     gam(fg_made ~ s(ydl, k = 12) + kq, family = "binomial", method = "REML")

X1 = sm.add_constant(fg_data["ydl"])
glm1 = sm.GLM(fg_data["fg_made"], X1, family=sm.families.Binomial()).fit()
print("=== Model 1: Simple logistic GLM ===")
print(glm1.summary())
print(f"\nlogit(p) = {glm1.params['const']:.4f} + {glm1.params['ydl']:.4f} * ydl")

fg_data["ydl2"] = fg_data["ydl"] ** 2
X2 = sm.add_constant(fg_data[["ydl", "ydl2", "kq"]])
glm2 = sm.GLM(fg_data["fg_made"], X2, family=sm.families.Binomial()).fit()
print("\n=== Model 2: Quadratic GLM + kq ===")
print(glm2.summary())
print(f"\nlogit(p) = {glm2.params['const']:.4f} + {glm2.params['ydl']:.4f}*ydl + {glm2.params['ydl2']:.4f}*ydl^2 + {glm2.params['kq']:.4f}*kq")

X_gam = fg_data[["ydl", "kq"]].values
y_gam = fg_data["fg_made"].values

gam = LogisticGAM(s(0, n_splines=12) + l(1)).fit(X_gam, y_gam)
print("\n=== Model 3: Logistic GAM ===")
print(gam.summary())

# Task 2:
# - Compare the models using out-of-sample predictive performance
# - Use test-set log loss or cross-validated log loss as the main metric
# - State clearly which model you prefer and why

kf = KFold(n_splits=5, shuffle=True, random_state=42)

ll1, ll2, ll3 = [], [], []

for train_idx, test_idx in kf.split(fg_data):
    train = fg_data.iloc[train_idx].copy()
    test  = fg_data.iloc[test_idx].copy()
    y_test = test["fg_made"].values

    # Model 1
    X1_train = sm.add_constant(train["ydl"])
    X1_test  = sm.add_constant(test["ydl"])
    glm1_cv = sm.GLM(train["fg_made"], X1_train, family=sm.families.Binomial()).fit()
    ll1.append(log_loss(y_test, glm1_cv.predict(X1_test)))

    # Model 2
    train["ydl2"] = train["ydl"] ** 2
    test["ydl2"]  = test["ydl"] ** 2
    X2_train = sm.add_constant(train[["ydl", "ydl2", "kq"]])
    X2_test  = sm.add_constant(test[["ydl", "ydl2", "kq"]])
    glm2_cv = sm.GLM(train["fg_made"], X2_train, family=sm.families.Binomial()).fit()
    ll2.append(log_loss(y_test, glm2_cv.predict(X2_test)))

    # Model 3
    gam_cv = LogisticGAM(s(0, n_splines=12) + l(1)).fit(
        train[["ydl", "kq"]].values, train["fg_made"].values
    )
    ll3.append(log_loss(y_test, gam_cv.predict_proba(test[["ydl", "kq"]].values)))

print(f"Model 1 (simple GLM):     CV log loss = {np.mean(ll1):.4f} ")
print(f"Model 2 (quadratic + kq): CV log loss = {np.mean(ll2):.4f} ")
print(f"Model 3 (GAM):            CV log loss = {np.mean(ll3):.4f} ")

print("\n We will choose Model 3 because it has the lowest cross-validated log loss ")

# Task 3:
# - For the selected GAM, report:
#     * the estimated parametric coefficient(s)
#     * the effective degrees of freedom (edf) of the smooth term
# - Explain what it means if the edf is noticeably larger than 1
print("edf: 8.1")
print("The edf being noticeably larger than 1 indicates that patterns change throughout the data --> multiple piecewise cubic functions needed to capture the changes")

# Task 4:
# - Plot predicted make probability against ydl for your preferred GLM and your preferred GAM
# - Also compute binned observed make rates and overlay them on the plot
# - Comment on where the GAM improves on the simpler GLM


ydl_range = np.linspace(fg_data["ydl"].min(), fg_data["ydl"].max(), 300)
kq_mean = fg_data["kq"].mean()

# Model 2 predictions - need const, ydl, ydl2, kq
X2_plot = pd.DataFrame({
    "const": 1,
    "ydl":   ydl_range,
    "ydl2":  ydl_range ** 2,
    "kq":    kq_mean
})
preds_glm = glm2.predict(X2_plot)

# Model 3 predictions (GAM)
X_gam_plot = np.column_stack([ydl_range, np.full(300, kq_mean)])
preds_gam = gam.predict_proba(X_gam_plot)

# Binned observed make rates (small bins)
fg_data["ydl_bin"] = pd.cut(fg_data["ydl"], bins=30)
binned = fg_data.groupby("ydl_bin", observed=True)["fg_made"].mean().reset_index()
bin_midpoints = binned["ydl_bin"].apply(lambda x: x.mid)

# Plot
fig, ax = plt.subplots(figsize=(9, 6))

# Binned observed rates as dots
ax.scatter(bin_midpoints, binned["fg_made"], color="black", zorder=5, s=40, label="Binned observed rate")

# Fitted curves as lines
ax.plot(ydl_range, preds_glm, color="blue", linewidth=2, label="Model 2 (quadratic GLM + kq)")
ax.plot(ydl_range, preds_gam, color="red",  linewidth=2, label="Model 3 (GAM)")

ax.set_xlabel("Yard line (ydl)")
ax.set_ylabel("P(make)")
ax.set_title("Predicted make probability vs yard line")
ax.legend()
plt.tight_layout()
plt.savefig("../data/plots/task4_fg_curves.png", dpi=150)
plt.show()

print("The GAM improves on the GLM because the different points allow us to capture changing trends throughout the data")

# Task 5:
# - For a kicker with league-median kq, estimate make probability at:
#     * 20 yards from the opponent's end zone
#     * 35 yards from the opponent's end zone
#     * 50 yards from the opponent's end zone
# - For at least one of these yard lines, compute an approximate 95% confidence interval
#   using predict(..., type = "link", se.fit = TRUE) and then transform back with plogis()

print("\n")
kq_median = fg_data["kq"].median()
print(f"Avg kicker quality: {kq_median:.4f}")

for ydl in [20, 35, 50]:
    X_pred = np.array([[ydl, kq_median]])
    p = gam.predict_proba(X_pred)[0]
    print(f"Make prediction at {ydl}: {p:.4f}")

X_ci = np.array([[50, kq_median]])
ci = gam.confidence_intervals(X_ci, width=0.95)
print(f"95% interval at 50: ({ci[0, 0]:.4f}, {ci[0, 1]:.4f})")


# Task 6:
# - Briefly explain the difference between
#     * choosing polynomial terms by hand in a GLM, and
#     * letting a GAM learn a smooth curve with a wiggliness penalty
# - State one reason a GAM can help and one reason it can hurt

print("""Choosing polynomial terms means we hard set the flexibility, 
      while a GAM can calculate the optimal flexibility by punishing overfitting and error""")

print("A GAM can capture a data's complexity by allowing for optimal flexibility; however, it is complicated and less easily interpretable")