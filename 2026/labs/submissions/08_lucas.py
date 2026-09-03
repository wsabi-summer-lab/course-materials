#############
### SETUP ###
#############



import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy import stats

# set seed
np.random.seed(8)

#########################
### PART 1: NBA FREE THROWS ###
#########################

# load data
nba_players = pd.read_csv(
    "../data/08_nba-free-throws.csv.gz",
    delimiter=";",
    encoding="latin-1"
)


# Task 1:
# - Modify the dataset to include:
#   * Player
#   * FT_total = total free throws made across the season
#   * FTA_total = total free throws attempted across the season
#   * FT_percent = FT_total / FTA_total
# - Remember that FT and FTA are per-game values, so convert them to totals using G

nba_players["FT_total"]  = nba_players["FT"]  * nba_players["G"]
nba_players["FTA_total"] = nba_players["FTA"] * nba_players["G"]
nba_players["FT_percent"] = nba_players["FT_total"] / nba_players["FTA_total"]

# Task 2:
# - Filter the dataset to players with at least 25 total free-throw attempts
# - Decide how you want to handle players with multiple team rows
# - Make sure the final player-level dataset has one row per player-season

tot_players = nba_players[nba_players["Tm"] == "TOT"]["Player"].unique()
nba_players = nba_players[
    ~((nba_players["Player"].isin(tot_players)) & (nba_players["Tm"] != "TOT"))
].reset_index(drop=True)

nba_players = nba_players[nba_players["FTA_total"] >= 25].reset_index(drop=True)

# Task 3:
# - Construct 95% Wald confidence intervals for each player's free-throw probability
# - Construct 95% Agresti-Coull confidence intervals for each player's free-throw probability
# - Make a plot with:
#   * x-axis = FT_percent
#   * y-axis = player name
#   * both interval types overlaid
# - Comment on which intervals look most different and why

z = stats.norm.ppf(0.975)

n     = nba_players["FTA_total"]
s     = nba_players["FT_total"]
p_hat = nba_players["FT_percent"]

# Wald
nba_players["wald_lo"] = p_hat - z * np.sqrt(p_hat * (1 - p_hat) / n)
nba_players["wald_hi"] = p_hat + z * np.sqrt(p_hat * (1 - p_hat) / n)

# Agresti-Coull
n_tilde = n + z**2
p_tilde = (s + z**2 / 2) / n_tilde
nba_players["ac_lo"] = p_tilde - z * np.sqrt(p_tilde * (1 - p_tilde) / n_tilde)
nba_players["ac_hi"] = p_tilde + z * np.sqrt(p_tilde * (1 - p_tilde) / n_tilde)

# Plot
plot_df = nba_players.sort_values("FT_percent").reset_index(drop=True)

plot_df["decile"] = pd.qcut(plot_df["FT_percent"], q=10, labels=False)
sampled = (
    plot_df.groupby("decile", group_keys=False)
    .apply(lambda x: x.sample(min(5, len(x)), random_state=8))
    .sort_values("FT_percent")
    .reset_index(drop=True)
)

fig, ax = plt.subplots(figsize=(10, len(sampled) * 0.35))

for i, row in sampled.iterrows():
    ax.plot([row["wald_lo"], row["wald_hi"]], [i, i],
            color="steelblue", linewidth=1.5, label="Wald" if i == 0 else "", zorder=2)
    ax.plot([row["ac_lo"], row["ac_hi"]], [i, i],
            color="tomato", linewidth=1.5, alpha=0.7, label="Agresti-Coull" if i == 0 else "", zorder=1)
    ax.plot(row["FT_percent"], i, "o", color="black", markersize=3, zorder=3)

ax.set_yticks(range(len(sampled)))
ax.set_yticklabels(sampled["Player"], fontsize=8)
ax.set_xlabel("FT%", fontsize=11)
ax.set_title("95% Wald vs Agresti-Coull CIs — NBA Free Throws\n(sampled across FT% range)", fontsize=11)
ax.legend(fontsize=9)
ax.grid(axis="x", linestyle="--", alpha=0.4)
plt.subplots_adjust(left=0.25)
plt.tight_layout()
plt.show()

print("AC CI is wider than Wald CI; it extends towards the left if phat > 0.5 and extends right if phat < 0.5 ")
###########################
### PART 2: LLN WARM-UP ###
###########################

lln_p = 0.75
lln_n = 1000
lln_paths = 100

# Task 1:
# - Simulate one Bernoulli sequence with true probability p = 0.75 and length 1000
# - Compute the running estimate
#     phat_n = (1 / n) * sum_{i=1}^n X_i
#   for n = 1, ..., 1000

draws       = np.random.binomial(1, lln_p, lln_n)
running_est = np.cumsum(draws) / np.arange(1, lln_n + 1)

# Task 2:
# - Plot the running estimate against n
# - Add a horizontal line at p = 0.75
# - Describe what happens as n grows

fig, ax = plt.subplots(figsize=(9, 4))
ax.plot(np.arange(1, lln_n + 1), running_est, color="steelblue", linewidth=0.8)
ax.axhline(lln_p, color="red", linestyle="--", label="p = 0.75")
ax.set_xlabel("n")
ax.set_ylabel("p-hat")
ax.set_title("Running Estimate of p (single path)")
ax.legend()
plt.tight_layout()
plt.show()

print("As n grows, the running avg approaches the true p")
# Task 3:
# - Repeat the simulation 100 times
# - Overlay the 100 running-estimate paths on one figure
# - Describe how the variability changes with n

fig, ax = plt.subplots(figsize=(9, 4))

for _ in range(lln_paths):
    draws       = np.random.binomial(1, lln_p, lln_n)
    running_est = np.cumsum(draws) / np.arange(1, lln_n + 1)
    ax.plot(np.arange(1, lln_n + 1), running_est, color="steelblue", linewidth=0.4, alpha=0.3)

ax.axhline(lln_p, color="red", linestyle="--", label="p = 0.75")
ax.set_xlabel("n")
ax.set_ylabel("p-hat")
ax.set_title("Running Estimate of p (100 paths)")
ax.legend()
plt.tight_layout()
plt.show()

print("funnel shape --> variation between paths decreases as n grows")
################################
### PART 3: SIMULATION STUDY ###
################################

z_975 = stats.norm.ppf(0.975)
p_grid = np.linspace(0, 1, 1000)
sample_sizes = [10, 50, 100, 250, 500, 1000]
M = 100

# A convenient helper structure is:
# - loop over p
# - loop over n
# - simulate Binomial(n, p) data
# - compute both intervals
# - record whether each interval contains the true p

# Task 1:
# - Use the grid p_grid as your set of candidate true probabilities
# - For each p and each n in sample_sizes, generate binomial data

results = []

for p in p_grid:
    for n in sample_sizes:
        for _ in range(M):
            s = np.random.binomial(n, p)

# Task 2:
# - Compute the 95% Wald confidence interval
# - Compute the 95% Agresti-Coull confidence interval using
#     n_tilde = n + z_975^2
#     p_tilde = (S_n + z_975^2 / 2) / n_tilde
# - Interpret this as approximately adding 2 successes and 2 failures

results = []

for p in p_grid:
    for n in sample_sizes:
        for _ in range(M):
            s     = np.random.binomial(n, p)
            p_hat = s / n

            # Wald
            lo_w = p_hat - z_975 * np.sqrt(p_hat * (1 - p_hat) / n)
            hi_w = p_hat + z_975 * np.sqrt(p_hat * (1 - p_hat) / n)

            # Agresti-Coull
            n_tilde = n + z_975**2
            p_tilde = (s + z_975**2 / 2) / n_tilde
            lo_ac   = p_tilde - z_975 * np.sqrt(p_tilde * (1 - p_tilde) / n_tilde)
            hi_ac   = p_tilde + z_975 * np.sqrt(p_tilde * (1 - p_tilde) / n_tilde)

# Task 3:
# - Repeat the simulation M = 100 times for each (n, p) pair
# - For each method, estimate coverage as the fraction of intervals that contain p

results = []

for p in p_grid:
    for n in sample_sizes:
        wald_cover = 0
        ac_cover   = 0
        for _ in range(M):
            s     = np.random.binomial(n, p)
            p_hat = s / n

            # Wald
            lo_w = p_hat - z_975 * np.sqrt(p_hat * (1 - p_hat) / n)
            hi_w = p_hat + z_975 * np.sqrt(p_hat * (1 - p_hat) / n)
            wald_cover += int(lo_w <= p <= hi_w)

            # Agresti-Coull
            n_tilde = n + z_975**2
            p_tilde = (s + z_975**2 / 2) / n_tilde
            lo_ac   = p_tilde - z_975 * np.sqrt(p_tilde * (1 - p_tilde) / n_tilde)
            hi_ac   = p_tilde + z_975 * np.sqrt(p_tilde * (1 - p_tilde) / n_tilde)
            ac_cover += int(lo_ac <= p <= hi_ac)

        results.append({"p": p, "n": n, "method": "Wald",          "coverage": wald_cover / M})
        results.append({"p": p, "n": n, "method": "Agresti-Coull", "coverage": ac_cover   / M})

results_df = pd.DataFrame(results)

# Task 4:
# - Plot coverage probability vs p
# - Facet by sample size
# - Include both the Wald and Agresti-Coull methods on the same figure
# - Add a horizontal reference line at 0.95
# - Comment on where the Wald interval undercovers

fig, axes = plt.subplots(2, 3, figsize=(14, 8), sharey=True)
axes = axes.flatten()

for idx, n in enumerate(sample_sizes):
    ax = axes[idx]
    for method, color in [("Wald", "steelblue"), ("Agresti-Coull", "tomato")]:
        subset = results_df[(results_df["n"] == n) & (results_df["method"] == method)]
        ax.plot(subset["p"], subset["coverage"], color=color, linewidth=0.8, label=method)
    ax.axhline(0.95, color="black", linestyle="--", linewidth=0.8)
    ax.set_title(f"n = {n}")
    ax.set_xlabel("p")
    ax.set_ylabel("Coverage")
    ax.legend(fontsize=7)

plt.suptitle("Coverage Probability: Wald vs Agresti-Coull")
plt.tight_layout()
plt.show()

############################
### PART 4: SKITTLES DEMO ###
############################

# Replace these with your observed counts from the Skittles activity
skittles_n = 77
skittles_r = 13

# Task 1:
# - Compute the observed red proportion r / n
# - Construct a 95% Wald confidence interval for the true red probability

p_hat_sk   = skittles_r / skittles_n
lo_w_sk    = p_hat_sk - z_975 * np.sqrt(p_hat_sk * (1 - p_hat_sk) / skittles_n)
hi_w_sk    = p_hat_sk + z_975 * np.sqrt(p_hat_sk * (1 - p_hat_sk) / skittles_n)
print(f"Wald CI: ({lo_w_sk:.4f}, {hi_w_sk:.4f})")

# Task 2:
# - Construct a 95% Agresti-Coull confidence interval for the same probability

n_tilde_sk = skittles_n + z_975**2
p_tilde_sk = (skittles_r + z_975**2 / 2) / n_tilde_sk
lo_ac_sk   = p_tilde_sk - z_975 * np.sqrt(p_tilde_sk * (1 - p_tilde_sk) / n_tilde_sk)
hi_ac_sk   = p_tilde_sk + z_975 * np.sqrt(p_tilde_sk * (1 - p_tilde_sk) / n_tilde_sk)
print(f"Agresti-Coull CI: ({lo_ac_sk:.4f}, {hi_ac_sk:.4f})")

# Task 3:
# - Compare the two intervals
# - State which one seems more sensible when the observed red proportion is near 0 or 1

print("Intervals are fairly similar, but AC CI is shifted a bit right of Wald CI; sample proportion is reasonable")
print("However, if p was low, then AC would be more sensible because it brings the p away from extremes")