#######################
### Authors: RB, JP ###
#######################

#############
### SETUP ###
#############

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import matplotlib.colors as mcolors
from matplotlib.patches import Patch
from matplotlib.lines import Line2D

rng = np.random.default_rng(12)

########################
### HELPER FUNCTIONS ###
########################

def rmse(truth, prediction):
    return np.sqrt(np.mean((truth - prediction) ** 2))

def beta_prior_from_rates(made, attempts):
    raw_rate = made / attempts
    mu = made.sum() / attempts.sum()
    sampling_noise = np.mean(mu * (1 - mu) / attempts)
    tau2 = max(np.var(raw_rate, ddof=1) - sampling_noise, np.finfo(float).eps)
    strength = mu * (1 - mu) / tau2 - 1

    return pd.DataFrame({
        "alpha": [mu * strength],
        "beta": [(1 - mu) * strength],
        "center": [mu],
        "tau2": [tau2],
        "strength": [strength]
    })

########################
### BATTING AVERAGES ###
########################

batting = pd.read_csv("../data/12_ba-2020-2021.csv.gz")
batting = batting[batting["AB_2020"] > 0]

# Method-of-moments estimated prior from the 2020 player population.
mu_hat = batting["H_2020"].sum() / batting["AB_2020"].sum()
C_hat = mu_hat * (1 - mu_hat)
tau2_hat = max(np.var(batting["BA_2020"], ddof=1) - np.mean(C_hat / batting["AB_2020"]), 0)

# Beta prior with the same mean and between-player variance.
beta_strength = mu_hat * (1 - mu_hat) / tau2_hat - 1
alpha_hat = mu_hat * beta_strength
beta_hat = (1 - mu_hat) * beta_strength

batting = batting.assign(
    mle=batting["BA_2020"],
    complete_pooling=mu_hat,
    sigma2=C_hat / batting["AB_2020"],
).assign(
    posterior_variance=lambda df: 1 / (1 / df["sigma2"] + 1 / tau2_hat),
    lambda_=lambda df: tau2_hat / (tau2_hat + df["sigma2"]),
).assign(
    empirical_bayes=lambda df: mu_hat + df["lambda_"] * (df["mle"] - mu_hat),
    beta_binomial_eb=(batting["H_2020"] + alpha_hat) / (batting["AB_2020"] + alpha_hat + beta_hat)
)

batting_eval = batting[batting["AB_2021"] >= 100]

# TODO: make a shrinkage-arrow plot from mle to empirical_bayes.

batting_plot = batting.reset_index(drop=True)

fig, axes = plt.subplots(1, 3, figsize=(18, 6))

plot_configs = [
    (np.abs(batting_plot["mle"] - mu_hat), "Distance from League Mean", "Purples"),
    (batting_plot["AB_2020"],               "Number of 2020 At-Bats",   "Oranges"),
    (batting_plot["lambda_"],               "Shrinkage Weight λ̂",       "Greens"),
]

x_min = batting_plot[["mle", "empirical_bayes"]].min().min() - 0.02
x_max = batting_plot[["mle", "empirical_bayes"]].max().max() + 0.02
y_min = batting_plot["mle"].min() - 0.02
y_max = batting_plot["mle"].max() + 0.02

for ax, (color_vals, title, cmap_name) in zip(axes, plot_configs):
    norm = mcolors.Normalize(vmin=color_vals.min(), vmax=color_vals.max())
    cmap = cm.get_cmap(cmap_name)

    for idx in range(len(batting_plot)):
        row = batting_plot.iloc[idx]
        color = cmap(norm(color_vals.iloc[idx]))
        ax.annotate(
            "",
            xy=(row["empirical_bayes"], row["mle"]),
            xytext=(row["mle"], row["mle"]),
            arrowprops=dict(arrowstyle="->", color=color, lw=0.8)
        )

    sm = cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])
    fig.colorbar(sm, ax=ax, shrink=0.6, label=title)


    ax.axvline(mu_hat, color="black", linestyle="--", linewidth=1, label=f"League mean ({mu_hat:.3f})")
    ax.set_xlim(x_min, x_max)
    ax.set_ylim(y_min, y_max)
    ax.set_xlabel("Batting Average")
    ax.set_ylabel("MLE (2020 BA)")
    ax.set_title(f"Shrinkage Colored by\n{title}")
    ax.legend(fontsize=8)

plt.suptitle("MLE → Empirical-Bayes Shrinkage Arrow Plot", fontsize=14, fontweight="bold")
plt.tight_layout()
plt.show()


# TODO: compare estimators against BA_2021 using batting_eval and rmse().

rmse_results = {
    "MLE":              rmse(batting_eval["BA_2021"], batting_eval["mle"]),
    "Complete Pooling": rmse(batting_eval["BA_2021"], batting_eval["complete_pooling"]),
    "Empirical Bayes":  rmse(batting_eval["BA_2021"], batting_eval["empirical_bayes"]),
}
print("Overall RMSE:")
for name, val in rmse_results.items():
    print(f"  {name}: {val:.4f}")

median_ab = batting_eval["AB_2020"].median()
low_ab  = batting_eval[batting_eval["AB_2020"] <= median_ab]
high_ab = batting_eval[batting_eval["AB_2020"] >  median_ab]

for group_name, group in [("Low AB", low_ab), ("High AB", high_ab)]:
    print(f"\nRMSE ({group_name}, AB_2020 median split = {median_ab:.0f}):")
    for name, col in [("MLE", "mle"), ("Complete Pooling", "complete_pooling"), ("Empirical Bayes", "empirical_bayes")]:
        print(f"  {name}: {rmse(group['BA_2021'], group[col]):.4f}")

fig, axes = plt.subplots(1, 3, figsize=(15, 5), sharey=True)

estimators = [
    ("mle",              "MLE"),
    ("complete_pooling", "Complete Pooling"),
    ("empirical_bayes",  "Empirical Bayes"),
]

for ax, (col, label) in zip(axes, estimators):
    ax.scatter(batting_eval["BA_2021"], batting_eval[col],
               alpha=0.5, s=20, color="steelblue")
    lims = [
        min(batting_eval["BA_2021"].min(), batting_eval[col].min()) - 0.01,
        max(batting_eval["BA_2021"].max(), batting_eval[col].max()) + 0.01,
    ]
    ax.plot(lims, lims, "k--", linewidth=1, label="y = x")
    ax.set_xlabel("2021 BA (truth)")
    ax.set_ylabel("Predicted BA") if ax == axes[0] else None
    ax.set_title(f"{label}\nRMSE = {rmse_results.get(label, rmse(batting_eval['BA_2021'], batting_eval[col])):.4f}")
    ax.set_xlim(lims); ax.set_ylim(lims)
    ax.legend(fontsize=8)

plt.suptitle("Prediction vs 2021 Batting Average (AB_2021 ≥ 100)", fontsize=13, fontweight="bold")
plt.tight_layout()
plt.show()

# TODO: compare the Normal-Normal and exact Beta-Binomial EB estimates.

print("RMSE comparison (batting_eval, AB_2021 >= 100):")
print(f"  Normal-Normal EB: {rmse(batting_eval['BA_2021'], batting_eval['empirical_bayes']):.4f}")
print(f"  Beta-Binomial EB: {rmse(batting_eval['BA_2021'], batting_eval['beta_binomial_eb']):.4f}")

batting = batting.assign(eb_diff=lambda df: np.abs(df["empirical_bayes"] - df["beta_binomial_eb"]))

print("Top 10 players by NN vs BB difference:")
print(batting[["AB_2020", "mle", "empirical_bayes", "beta_binomial_eb", "eb_diff"]]
      .sort_values("eb_diff", ascending=False)
      .head(10)
      .to_string(index=False))

print("Differences are very low across all batters; It seems they differ the most when the MLE is extreme (far from avg --> around .35 or .15)")
# TODO: repeat with 0.5 * tau2_hat and 2 * tau2_hat.

for scale in [0.5, 1.0, 2.0]:
    tau2_scaled = scale * tau2_hat
    lambda_scaled = tau2_scaled / (tau2_scaled + C_hat / batting["AB_2020"])
    eb_scaled = mu_hat + lambda_scaled * (batting["mle"] - mu_hat)

    idx = batting_eval.index
    rmse_val = rmse(batting_eval["BA_2021"], eb_scaled.loc[idx])
    lambda_mean = lambda_scaled.mean()

    print(f"tau2 x{scale}: mean lambda = {lambda_mean:.3f}, RMSE = {rmse_val:.4f}")

print("RMSE with scale by 1x (no multiple); as you increase lambda, the shrinkage decreases")

###################
### FIELD GOALS ###
###################

field_goals = pd.read_csv("../data/12_field-goals.csv.gz")
field_goals["distance_group"] = pd.cut(
    field_goals["ydl"],
    bins=[-np.inf, 20, 35, np.inf],
    labels=["short", "medium", "long"]
)

# TODO: aggregate by kicker and distance_group.

fg_agg = (
    field_goals.groupby(["kicker", "distance_group"], observed=True)
    .agg(made=("fg_made", "sum"), attempts=("fg_made", "count"))
    .reset_index()
    .query("attempts >= 5")
    .assign(raw_rate=lambda df: df["made"] / df["attempts"])
)

# TODO: estimate one Beta prior within each distance_group using beta_prior_from_rates().

group_priors = (
    fg_agg.groupby("distance_group", observed=True)
    .apply(lambda g: beta_prior_from_rates(g["made"], g["attempts"]))
    .reset_index(level=0)
    .reset_index(drop=True)
)

# TODO: compute the group-specific Beta-Binomial EB estimate for each kicker-group.

fg_agg = fg_agg.merge(
    group_priors[["distance_group", "alpha", "beta"]].rename(
        columns={"alpha": "alpha_g", "beta": "beta_g"}
    ),
    on="distance_group"
)
fg_agg["eb_group"] = (fg_agg["made"] + fg_agg["alpha_g"]) / \
                     (fg_agg["attempts"] + fg_agg["alpha_g"] + fg_agg["beta_g"])
# TODO: repeat with one common Beta prior for all field-goal attempts.

common_prior = beta_prior_from_rates(fg_agg["made"], fg_agg["attempts"])
alpha_c = common_prior["alpha"].iloc[0]
beta_c  = common_prior["beta"].iloc[0]
fg_agg["eb_common"] = (fg_agg["made"] + alpha_c) / \
                      (fg_agg["attempts"] + alpha_c + beta_c)

# TODO: plot raw, group-specific EB, and common-prior EB make rates by distance_group.

fig, axes = plt.subplots(1, 3, figsize=(18, 8), sharey=False)
group_labels = {"short": "Short (ydl ≤ 20)", "medium": "Medium (20 < ydl ≤ 35)", "long": "Long (ydl > 35)"}

for ax, grp in zip(axes, ["short", "medium", "long"]):
    sub = fg_agg[fg_agg["distance_group"] == grp].sort_values("raw_rate").reset_index(drop=True)
    y = np.arange(len(sub))

    # arrows: raw -> eb_group
    for i, row in sub.iterrows():
        ax.annotate("", xy=(row["eb_group"], y[i]), xytext=(row["raw_rate"], y[i]),
                    arrowprops=dict(arrowstyle="->", color="darkorange", lw=1.0))
        ax.annotate("", xy=(row["eb_common"], y[i]), xytext=(row["raw_rate"], y[i]),
                    arrowprops=dict(arrowstyle="->", color="seagreen", lw=1.0))

    # raw dots
    ax.scatter(sub["raw_rate"], y, color="red", s=20, zorder=5, label="Raw MLE")

    # prior mean lines
    grp_center = group_priors[group_priors["distance_group"] == grp]["center"].iloc[0]
    ax.axvline(grp_center,            color="darkorange", linestyle="--", lw=1, alpha=0.7, label=f"Group prior ({grp_center:.3f})")
    ax.axvline(alpha_c/(alpha_c+beta_c), color="seagreen",   linestyle="--", lw=1, alpha=0.7, label=f"Common prior (0.845)")

    ax.set_title(group_labels[grp], fontsize=11, fontweight="bold")
    ax.set_xlabel("Make Rate Estimate")
    ax.set_ylabel("Kickers (sorted by raw rate)" if ax == axes[0] else "")
    ax.set_yticks([])
    ax.legend(fontsize=8)
    ax.grid(axis="x", alpha=0.3)
    
legend_elements = [
    Line2D([0], [0], color="red",        marker="o", linestyle="None", markersize=5, label="Raw MLE"),
    Line2D([0], [0], color="darkorange", marker=">", linestyle="-",    markersize=5, label="→ EB (Group)"),
    Line2D([0], [0], color="seagreen",   marker=">", linestyle="-",    markersize=5, label="→ EB (Common)"),
]
fig.legend(handles=legend_elements, loc="lower center", ncol=3, fontsize=9, bbox_to_anchor=(0.5, -0.02))
plt.suptitle("Shrinkage of Kicker Make Rates: Raw → EB (Group) and Raw → EB (Common)",
             fontsize=13, fontweight="bold")
plt.tight_layout()
plt.show()

# TODO: contrast which distance groups move differently under the two pooling strategies.

fg_agg["diff_group_common"] = np.abs(fg_agg["eb_group"] - fg_agg["eb_common"])
print("Mean |EB_group - EB_common| by distance group:")
print(fg_agg.groupby("distance_group", observed=True)["diff_group_common"].mean().to_string())

print("the kicker shrinkage moves differently when their point estimate is in between the two priors")
print("EX: for long fgs, the pooled data has the make percentage higher, so we can observe a kicker regressing towards a higher value for the aggregate while moving down for the categorical prior (which is lower)")
print("Categorizing is more sensible because kickers have different make rates based on distance, so we need to account for that by breaking down the distance into groups")