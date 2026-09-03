#############
### SETUP ###
#############

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import norm
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import SplineTransformer, OneHotEncoder
from sklearn.compose import ColumnTransformer
import os

np.random.seed(9)

##############################
### PART 1: EXPECTED POINTS ###
##############################

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
nfl_data = pd.read_csv("../data/09_expected-points.csv.gz")
nfl_data["log_ydstogo"] = np.log(nfl_data["ydstogo"])

score_values = np.sort(nfl_data["pts_next_score"].unique())
game_ids     = nfl_data["game_id"].unique()
FEATURE_COLS = ["yardline_100", "down", "log_ydstogo", "half_seconds_remaining"]

# Task 1:
# - Refit your preferred expected-points model from Lab 5
# - A concrete default is:
#   pts_next_score ~ bs(yardline_100, df=6) +
#                    factor(down) +
#                    log(ydstogo) +
#                    bs(half_seconds_remaining, df=5)
# - Convert fitted class probabilities into expected points

# Optional helper:
# - Takes a 2D array of predicted class probabilities (rows = observations,
#   cols aligned to score_values) and returns expected points per row
def expected_points_from_probs(prob_matrix, model_classes):
    return np.sum(prob_matrix * model_classes, axis=1)

def build_ep_model():
    preprocessor = ColumnTransformer([
        ("spline_yard", SplineTransformer(n_knots=6, degree=3, include_bias=False), ["yardline_100"]),
        ("down_onehot", OneHotEncoder(drop="first", sparse_output=False),           ["down"]),
        ("log_ydstogo", "passthrough",                                              ["log_ydstogo"]),
        ("spline_time", SplineTransformer(n_knots=6, degree=3, include_bias=False), ["half_seconds_remaining"]),
    ])
    return Pipeline([
        ("preprocessor", preprocessor),
        ("logistic", LogisticRegression(solver="lbfgs", max_iter=5000,
                                        tol=1e-3, random_state=9)),
    ])

target_state = pd.DataFrame({
    "yardline_100":           [35],
    "down":                   [1],
    "log_ydstogo":            [np.log(10)],
    "half_seconds_remaining": [1800],
})

full_model = build_ep_model()
full_model.fit(nfl_data[FEATURE_COLS], nfl_data["pts_next_score"])

full_probs  = full_model.predict_proba(target_state)
ep_original = expected_points_from_probs(full_probs, full_model.classes_)[0]

print(f"Task 1 — Fitted EP at target state: {ep_original:.4f}")

score_values = np.sort(nfl_data["pts_next_score"].unique())
game_ids = nfl_data["game_id"].unique()

# Patsy formula for design matrix (used for both full fit and bootstrap refits)
# Multinomial logistic regression via sklearn or statsmodels MNLogit
ep_formula = (
    "bs(yardline_100, df=6) + C(down) + np.log(ydstogo) + bs(half_seconds_remaining, df=5)"
)

# If your preferred model also uses posteam_spread, add:
# target_state["posteam_spread"] = 0

# Task 2:
# - Decide which bootstrap variation is most appropriate here:
#   * observation bootstrap
#   * cluster bootstrap
#   * parametric bootstrap
#   * residual bootstrap
# - State your choice in comments
# - Explain why it matches the dependence structure of this dataset

# The grouping variable that matters here is:
# game_id

print("We will choose a cluster boostrap because it captures the dependence of NFL plays within drives")
print("Also, we are not very certain that our model captures the entire trend of true EPV")

# Task 3:
# - Implement your chosen bootstrap with at least B = 200 resamples
# - For each resample:
#   * create a bootstrap dataset
#   * refit the EP model
#   * recompute expected points at target_state

B = 200
bootstrap_path = "../data/09_bootstrap_ep.npy"

if os.path.exists(bootstrap_path):
    bootstrap_ep = np.load(bootstrap_path)
    print("Task 3 — Loaded bootstrap results from disk")
else:
    bootstrap_ep = np.full(B, np.nan)

    for b in range(B):
        sampled_ids = np.random.choice(game_ids, size=len(game_ids), replace=True)

        boot_data = pd.concat(
            [nfl_data[nfl_data["game_id"] == g] for g in sampled_ids],
            ignore_index=True
        )
        boot_data["log_ydstogo"] = np.log(boot_data["ydstogo"])

        boot_model = build_ep_model()
        boot_model.fit(boot_data[FEATURE_COLS], boot_data["pts_next_score"])

        boot_probs = boot_model.predict_proba(target_state)
        bootstrap_ep[b] = expected_points_from_probs(boot_probs, boot_model.classes_)[0]

    np.save(bootstrap_path, bootstrap_ep)
    print("Task 3 — Bootstrap complete, results saved to disk")
    
# If you use a cluster bootstrap, you will likely want to:
# - sample game_ids with replacement
# - rebuild the bootstrap dataset by concatenating all rows
#   from each sampled game

# Task 4:
# - Store the bootstrap estimates in a vector  (bootstrap_ep above)
# - Make a plot of the bootstrap distribution

plt.figure(figsize=(8, 4))
plt.hist(bootstrap_ep, bins=30, color="steelblue", edgecolor="white")
plt.axvline(ep_original, color="red", linestyle="--", linewidth=1.5,
            label=f"Original EP = {ep_original:.3f}")
plt.xlabel("Expected Points")
plt.ylabel("Count")
plt.title("Bootstrap Distribution of EP at Target State\n"
          "(1st & 10, opp 35-yd line, 30:00 left in half)")
plt.legend()
plt.tight_layout()
plt.show()

# Task 5:
# - Compute:
#   * the original fitted expected-points estimate
#   * the bootstrap standard error
#   * the 95% percentile interval

bootstrap_se       = np.nanstd(bootstrap_ep, ddof=1)
ci_lower, ci_upper = np.nanpercentile(bootstrap_ep, [2.5, 97.5])

print(f"Task 5 — Original EP estimate : {ep_original:.4f}")
print(f"Task 5 — Bootstrap SE         : {bootstrap_se:.4f}")
print(f"Task 5 — 95% Percentile CI    : [{ci_lower:.4f}, {ci_upper:.4f}]")

# Task 6:
# - In comments, explain why a naive row-by-row observation bootstrap
#   is less appropriate for this dataset

print("Treats plays as independent but they are not --> plays influence the next so we should cluster off drives, which are closer to independent")

################################
### PART 2: NBA FREE THROWS ####
################################

nba_players = pd.read_csv("../data/09_nba-free-throws.csv.gz", sep=";", encoding="latin-1")

# Task 1:
# - Recreate the player-level free-throw dataset from Lab 8
# - Include:
#   * Player
#   * FT_total  = approximate total free throws made across the season
#   * FTA_total = approximate total free throws attempted across the season
#   * FT_percent
# - Basketball Reference reports FT and FTA as per-game values in this table,
#   so convert to approximate totals using G before treating them as counts
# - Filter to players with at least 25 approximate total free-throw attempts

nba_free_throws = nba_players.copy()
nba_free_throws["FT_total"]   = (nba_free_throws["FT"]  * nba_free_throws["G"]).round().astype(int)
nba_free_throws["FTA_total"]  = (nba_free_throws["FTA"] * nba_free_throws["G"]).round().astype(int)
nba_free_throws["FT_percent"] = nba_free_throws["FT_total"] / nba_free_throws["FTA_total"]

multi_team = nba_free_throws[nba_free_throws["Tm"] == "TOT"]["Player"].unique()
nba_player_level = pd.concat([
    nba_free_throws[nba_free_throws["Tm"] == "TOT"],
    nba_free_throws[~nba_free_throws["Player"].isin(multi_team)]
]).query("FTA_total >= 25").reset_index(drop=True)

print(f"Players with >= 25 FTA: {len(nba_player_level)}")

      
# Task 2:
# - For each player, construct a 95% bootstrap confidence interval
#   for free-throw percentage
# - Overlay the bootstrap intervals on your Lab 8 player plot
# - Compare bootstrap, Wald, and Agresti-Coull intervals

def bootstrap_ft_percent(ft_made, ft_attempted, B=1000):
    shots = np.array([1] * int(ft_made) + [0] * (int(ft_attempted) - int(ft_made)))
    resamples = np.random.choice(shots, size=(B, int(ft_attempted)), replace=True)
    return resamples.mean(axis=1)

def percentile_interval(bootstrap_draws, level=0.95):
    alpha = 1 - level
    return np.nanpercentile(bootstrap_draws, [alpha / 2 * 100, (1 - alpha / 2) * 100])

z = norm.ppf(0.975)
n     = nba_player_level["FTA_total"]
s     = nba_player_level["FT_total"]
p_hat = nba_player_level["FT_percent"]

# Wald
nba_player_level["wald_lo"] = p_hat - z * np.sqrt(p_hat * (1 - p_hat) / n)
nba_player_level["wald_hi"] = p_hat + z * np.sqrt(p_hat * (1 - p_hat) / n)

# Agresti-Coull
n_tilde = n + z**2
p_tilde = (s + z**2 / 2) / n_tilde
nba_player_level["ac_lo"] = p_tilde - z * np.sqrt(p_tilde * (1 - p_tilde) / n_tilde)
nba_player_level["ac_hi"] = p_tilde + z * np.sqrt(p_tilde * (1 - p_tilde) / n_tilde)

# Bootstrap
boot_cis = nba_player_level.apply(
    lambda row: percentile_interval(
        bootstrap_ft_percent(int(row["FT_total"]), int(row["FTA_total"]))
    ),
    axis=1, result_type="expand"
).rename(columns={0: "boot_lo", 1: "boot_hi"})
nba_player_level = pd.concat([nba_player_level, boot_cis], axis=1)

# plot
plot_df = nba_player_level.sort_values("FT_percent").reset_index(drop=True)
plot_df["decile"] = pd.qcut(plot_df["FT_percent"], q=10, labels=False)
sampled = (
    plot_df.groupby("decile", group_keys=False)
    .apply(lambda x: x.sample(min(5, len(x)), random_state=8))
    .sort_values("FT_percent")
    .reset_index(drop=True)
)

fig, ax = plt.subplots(figsize=(10, len(sampled) * 0.5))

offsets = {"Wald": 0.2, "Agresti-Coull": 0, "Bootstrap": -0.2}
colors  = {"Wald": "steelblue", "Agresti-Coull": "tomato", "Bootstrap": "green"}

for i, row in sampled.iterrows():
    for method, (lo, hi) in zip(
        ["Wald", "Agresti-Coull", "Bootstrap"],
        [(row["wald_lo"], row["wald_hi"]),
         (row["ac_lo"],   row["ac_hi"]),
         (row["boot_lo"], row["boot_hi"])]
    ):
        y = i + offsets[method]
        ax.plot([lo, hi], [y, y], color=colors[method], linewidth=1.5,
                label=method if i == 0 else "", zorder=2)
        ax.plot(row["FT_percent"], y, "o", color=colors[method], markersize=3, zorder=3)

ax.set_yticks(range(len(sampled)))
ax.set_yticklabels(sampled["Player"], fontsize=8)
ax.set_xlabel("FT%", fontsize=11)
ax.set_title("95% Wald vs Agresti-Coull vs Bootstrap CIs\n(sampled across FT% range)", fontsize=11)
ax.legend(fontsize=9)
ax.grid(axis="x", linestyle="--", alpha=0.4)
plt.subplots_adjust(left=0.25)
plt.tight_layout()
plt.show()

print("bootstrap gives similar ranges to AC and Wald CI")

# Task 3:
# - Revisit the Lab 8 simulation study
# - Add bootstrap confidence intervals to the same coverage framework
# - Plot bootstrap coverage probability against p
# - Compare to Wald and Agresti-Coull

sample_sizes = [10, 50, 100, 250, 500, 1000]
p_grid = np.linspace(0, 1, 1000)
M = 100
z_975 = norm.ppf(0.975)

bootstrap_coverage = pd.DataFrame()

sample_sizes = [10, 50, 100, 250, 500, 1000]
p_grid = np.linspace(0, 1, 100)  
M = 100
z_975 = norm.ppf(0.975)

records = []
for n in sample_sizes:
    for p in p_grid:
        wald_covered = ac_covered = boot_covered = 0
        for _ in range(M):
            x     = np.random.binomial(n, p)
            p_hat = x / n

            # Wald
            se      = np.sqrt(p_hat * (1 - p_hat) / n)
            wald_lo = p_hat - z_975 * se
            wald_hi = p_hat + z_975 * se

            # Agresti-Coull
            n_tilde = n + z_975**2
            p_tilde = (x + z_975**2 / 2) / n_tilde
            ac_lo   = p_tilde - z_975 * np.sqrt(p_tilde * (1 - p_tilde) / n_tilde)
            ac_hi   = p_tilde + z_975 * np.sqrt(p_tilde * (1 - p_tilde) / n_tilde)

            # Bootstrap
            boot_draws       = bootstrap_ft_percent(x, n, B=200)
            boot_lo, boot_hi = percentile_interval(boot_draws)

            wald_covered += int(wald_lo <= p <= wald_hi)
            ac_covered   += int(ac_lo   <= p <= ac_hi)
            boot_covered += int(boot_lo <= p <= boot_hi)

        records.append({
            "n":             n,
            "p":             p,
            "Wald":          wald_covered / M,
            "Agresti-Coull": ac_covered   / M,
            "Bootstrap":     boot_covered / M,
        })

coverage_df = pd.DataFrame(records)

# Task 4: plot coverage by method and sample size
cov_long = coverage_df.melt(id_vars=["n", "p"], var_name="method", value_name="coverage")

fig, axes = plt.subplots(2, 3, figsize=(15, 8), sharey=True)
for ax, n_val in zip(axes.flat, sample_sizes):
    sub = cov_long[cov_long["n"] == n_val]
    for method, color in zip(["Wald", "Agresti-Coull", "Bootstrap"],
                              ["steelblue", "tomato", "green"]):
        d = sub[sub["method"] == method]
        ax.plot(d["p"], d["coverage"], color=color, linewidth=1, label=method)
    ax.axhline(0.95, color="black", linestyle="--", linewidth=0.8, label="95% target")
    ax.set_title(f"n = {n_val}")
    ax.set_xlabel("p")
    ax.set_ylabel("Coverage")
    ax.set_ylim(0.7, 1.02)

axes.flat[0].legend(fontsize=8)
plt.suptitle("Coverage Probability: Wald vs Agresti-Coull vs Bootstrap", fontsize=12)
plt.tight_layout()
plt.show()

print("Bootstrap gives the similar undercoverage issues as Wald for confidence interval, but as n increases, we converge more towards the line") 