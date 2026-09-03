#############
### SETUP ###
#############

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats

# set seed
RANDOM_STATE = 7
np.random.seed(RANDOM_STATE)

##############
### PART 1 ###
##############

# load data
diving = pd.read_csv("../data/06_diving.csv.gz")

# Task 1:
# - Recreate the permutation-test setup from Lecture 6 for each judge
# - Build a judge-specific test statistic that compares the judge's scoring discrepancy
#   for same-country divers versus other divers
# - A natural starting point is:
#     observed_stat = mean(discrepancy for matched dives) - mean(discrepancy for unmatched dives

dive_avg = diving.groupby(["Diver", "DiveNo"])["JScore"].transform("mean")
diving["Discrepancy"] = diving["JScore"] - dive_avg
diving["SameCountry"] = (diving["Country"] == diving["JCountry"]).astype(int)

obs_stats = {}
for judge, grp in diving.groupby("Judge"):
    same = grp.loc[grp["SameCountry"] == 1, "Discrepancy"].values
    diff = grp.loc[grp["SameCountry"] == 0, "Discrepancy"].values
    if len(same) == 0 or len(diff) == 0:
        continue
    obs_stats[judge] = same.mean() - diff.mean()

# Task 2:
# - For each judge, generate a permutation null distribution by shuffling the match labels
#   while holding fixed the judge's discrepancy values
# - Use enough permutations that your p-values are reasonably stable
# - Compute the unadjusted permutation p-value for each judge

N_PERM = 10_000
results = []

for judge, grp in diving.groupby("Judge"):
    disc   = grp["Discrepancy"].values
    labels = grp["SameCountry"].values
    if judge not in obs_stats:
        continue

    perm_stats = np.empty(N_PERM)
    for i in range(N_PERM):
        shuffled = np.random.permutation(labels)        # one shuffle → exact group sizes preserved
        perm_stats[i] = disc[shuffled == 1].mean() - disc[shuffled == 0].mean()

    p_unadj = np.mean(perm_stats >= obs_stats[judge])

    results.append({
        "Judge":    judge,
        "JCountry": grp["JCountry"].iloc[0],
        "N_Same":   (labels == 1).sum(),
        "N_Diff":   (labels == 0).sum(),
        "ObsStat":  obs_stats[judge],
        "P_unadj":  p_unadj,
    })

results_df = pd.DataFrame(results).sort_values("P_unadj").reset_index(drop=True)

# Task 3:
# - Adjust the judge-level p-values for multiple testing
# - A good default is statsmodels.stats.multitest.multipletests(..., method="fdr_bh")
# - Report both:
#   * the unadjusted p-values
#   * the adjusted p-values

n = len(results_df)
p = results_df["P_unadj"].values
order = np.argsort(p)
p_adj_bh = np.empty(n)
p_adj_bh[order] = np.minimum.accumulate(
    (p[order] * n / np.arange(1, n + 1))[::-1]
)[::-1]
results_df["P_adj_BH"]   = np.minimum(p_adj_bh, 1.0)
results_df["Sig_BH"]     = results_df["P_adj_BH"] < 0.05

# Bonferroni
results_df["P_adj_Bonf"] = np.minimum(p * n, 1.0)
results_df["Sig_Bonf"]   = results_df["P_adj_Bonf"] < 0.05

# Task 4:
# - Identify which judges show evidence of nationality bias before adjustment
# - Identify which judges still show evidence after adjustment
# - Make at least one plot that helps explain the strongest case(s)

pre_adj  = results_df[results_df["P_unadj"]  < 0.05]
post_adj = results_df[results_df["Sig_BH"]]

# Plot discrepancy distributions for the top 4 judges by observed statistic
top_judge = results_df.iloc[0]["Judge"]
grp   = diving[diving["Judge"] == top_judge]
disc  = grp["Discrepancy"].values
labels = grp["SameCountry"].values
row   = results_df.iloc[0]

perm_stats = np.empty(N_PERM)
for i in range(N_PERM):
    shuffled = np.random.permutation(labels)
    perm_stats[i] = disc[shuffled == 1].mean() - disc[shuffled == 0].mean()

fig, ax = plt.subplots(figsize=(9, 5))
ax.hist(perm_stats, bins=50, color="steelblue", edgecolor="white", linewidth=0.4)
ax.axvline(row["ObsStat"], color="red", linewidth=2, label=f"Observed DoD = {row['ObsStat']:.3f}")
ax.set_xlabel("Difference of discrepancies (DoD)", fontsize=12)
ax.set_ylabel("Count across random permutations", fontsize=12)
ax.set_title(
    f"Permutation null distribution for Judge {top_judge.split()[-1]}\n"
    f"{len(disc)} judged dives, {labels.sum()} nationality matches, "
    f"Monte Carlo p-value ≈ {row['P_unadj']:.4f}",
    fontsize=12
)
ax.legend(fontsize=10)
plt.tight_layout()
plt.savefig("part1_permutation_null.png", dpi=150)
plt.show()

print("Judges with evidence of bias (unadjusted p < 0.05):")
for _, row in results_df[results_df["P_unadj"] < 0.05].iterrows():
    print(f"  {row['Judge']} ({row['JCountry']}): p = {row['P_unadj']:.4f}")

print("\nJudges with evidence of bias after Bonferroni correction:")
for _, row in results_df[results_df["Sig_Bonf"]].iterrows():
    print(f"  {row['Judge']} ({row['JCountry']}): p_adj = {row['P_adj_Bonf']:.4f}")



##############
### PART 2 ###
##############

# load data
tto = pd.read_csv("../data/06_tto.csv.gz")

# Variable map for the lecture notation:
# - y_i      = EVENT_WOBA_19
# - t_i      = ORDER_CT
# - BQ_i     = WOBA_FINAL_BAT_19
# - PQ_i     = WOBA_FINAL_PIT_19
# - hand_i   = HAND_MATCH
# - home_i   = BAT_HOME_IND

# Task 1:
# - Fit Model 1 from the lab handout using statsmodels.formula.api.ols or sm.OLS
# - Model 1 uses indicators for 2TTO and 3TTO, plus the batter/pitcher/home/hand control

controls = ["WOBA_FINAL_BAT_19", "WOBA_FINAL_PIT_19", "BAT_HOME_IND", "HAND_MATCH"]
y = tto["EVENT_WOBA_19"].values

def fit_ols(X_df, y):
    X = np.column_stack([np.ones(len(y)), X_df.values])
    coef, _, _, _ = np.linalg.lstsq(X, y, rcond=None)
    n, k = X.shape
    sigma2 = np.sum((y - X @ coef)**2) / (n - k)
    se = np.sqrt(np.diag(sigma2 * np.linalg.inv(X.T @ X)))
    t_vals = coef / se
    p_vals = 2 * (1 - stats.t.cdf(np.abs(t_vals), df=n - k))
    return pd.DataFrame({"coef": coef, "SE": se, "t": t_vals, "p": p_vals},
                        index=["Intercept"] + list(X_df.columns))
    
tto["TTO2"] = (tto["ORDER_CT"] >= 2).astype(float)
tto["TTO3"] = (tto["ORDER_CT"] >= 3).astype(float)

m1 = fit_ols(tto[["TTO2", "TTO3"] + controls], y)

# Task 2:
# - Fit Model 2 from the lab handout using lm() equivalent in Python
# - Model 2 adds a linear term in ORDER_CT on top of the Model 1 controls

m2 = fit_ols(tto[["BATTER_SEQ_NUM", "TTO2", "TTO3"] + controls], y)

# Task 3:
# - Run summary() on both models (model.summary())
# - Extract the coefficient estimates, standard errors, test statistics, and p-values
# - Interpret the coefficients tied to pitcher decline across times through the order

print("Model 1 summary:")
print(m1.round(4))

print("\nModel 2 summary:")
print(m2.round(4))


# Task 4:
# - State whether the estimated decline from one time through the order to the next
#   is statistically significant
# - Explain how the answer changes, if at all, between Model 1 and Model 2
# ...existing code...

print("\n--- Task 4: significance of pitcher decline ---")

print("\nModel 1:")
for c in ["TTO2", "TTO3"]:
    row = m1.loc[c]
    sig = "significant" if row["p"] < 0.05 else "not significant"
    print(f"  {c}: coef={row['coef']:+.4f}, p={row['p']:.4f} → {sig}")

print("\nModel 2:")
for c in ["BATTER_SEQ_NUM", "TTO2", "TTO3"]:
    row = m2.loc[c]
    sig = "significant" if row["p"] < 0.05 else "not significant"
    print(f"  {c}: coef={row['coef']:+.4f}, p={row['p']:.4f} → {sig}")