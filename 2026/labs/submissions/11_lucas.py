#######################
### Authors: RB, JP ###
#######################

#############
### SETUP ###
#############

# pip install pandas numpy scipy

import numpy as np
import pandas as pd
from scipy.stats import beta as beta_dist
import matplotlib.pyplot as plt

rng = np.random.default_rng(11)

########################
### HELPER FUNCTIONS ###
########################

def beta_summary(wins, attempts, alpha, beta, level=0.95):
    losses = attempts - wins
    alpha_post = alpha + wins
    beta_post = beta + losses
    tail = (1 - level) / 2

    posterior_map = (
        (alpha_post - 1) / (alpha_post + beta_post - 2)
        if alpha_post > 1 and beta_post > 1
        else None
    )

    return pd.DataFrame({
        "posterior_mean": [alpha_post / (alpha_post + beta_post)],
        "posterior_map":  [posterior_map],
        "lower":          [beta_dist.ppf(tail, alpha_post, beta_post)],
        "upper":          [beta_dist.ppf(1 - tail, alpha_post, beta_post)],
    })

def prior_from_center_strength(center, strength):
    return pd.DataFrame({
        "alpha": [center * strength],
        "beta":  [(1 - center) * strength],
    })

def beta_predictive_summary(wins, attempts, alpha, beta, future_attempts=50, draws=5000):
    alpha_post = alpha + wins
    beta_post  = beta + attempts - wins
    p_draws      = rng.beta(alpha_post, beta_post, size=draws)
    future_wins  = rng.binomial(future_attempts, p_draws)

    return pd.DataFrame({
        "predictive_mean": [np.mean(future_wins / future_attempts)],
        "analytic_mean":   [alpha_post / (alpha_post + beta_post)],
        "lower":           [np.quantile(future_wins / future_attempts, 0.025)],
        "upper":           [np.quantile(future_wins / future_attempts, 0.975)],
    })

#######################
### NBA FREE THROWS ###
#######################

nba_raw = pd.read_csv("../data/11_nba-free-throws.csv.gz", delimiter=";", encoding="latin-1")

tot_players = nba_raw[nba_raw["Tm"] == "TOT"]["Player"]

nba_players = (
    nba_raw[
        (nba_raw["Tm"] == "TOT") | (~nba_raw["Player"].isin(tot_players))
    ]
    .assign(
        wins=lambda df: (df["G"] * df["FT"]).round(),
        attempts=lambda df: (df["G"] * df["FTA"]).round(),
    )
    .assign(losses=lambda df: df["attempts"] - df["wins"])
    .assign(mle=lambda df: df["wins"] / df["attempts"])
    [["Player", "wins", "attempts", "losses", "mle"]]
    .query("attempts > 0")
    .reset_index(drop=True)
)

nba_players = nba_players[nba_players["attempts"] >= 20].reset_index(drop=True)
league_rate = nba_players["wins"].sum() / nba_players["attempts"].sum()

priors = pd.DataFrame({
    "prior": ["Weak", "League centered", "Elite shooter"],
    "alpha": [2, 30 * league_rate, 90],
    "beta":  [2, 30 * (1 - league_rate), 10],
})

# TODO: select players spanning low, medium, and high attempt totals.


low    = nba_players.nsmallest(2, "attempts")[["Player", "wins", "attempts", "mle"]]
medium = nba_players.iloc[(nba_players["attempts"] - nba_players["attempts"].median()).abs().argsort()[:2]][["Player", "wins", "attempts", "mle"]]
high   = nba_players.nlargest(2, "attempts")[["Player", "wins", "attempts", "mle"]]

selected_players = pd.concat([low, medium, high]).reset_index(drop=True)
selected_players["group"] = ["low", "low", "medium", "medium", "high", "high"]

print(f"League free throw rate: {league_rate:.3f}")
print(f"\nTotal players: {len(nba_players)}")
print(f"\nSelected players:")
print(selected_players.to_string(index=False))

# TODO: apply beta_summary() for every selected player and prior.

results = []
for _, player in selected_players.iterrows():
    for _, prior in priors.iterrows():
        summary = beta_summary(player["wins"], player["attempts"], prior["alpha"], prior["beta"])
        results.append({
            "Player":  player["Player"],
            "group":   player["group"],
            "attempts": player["attempts"],
            "mle":     player["mle"],
            "prior":   prior["prior"],
            **summary.iloc[0].to_dict()
        })

results_df = pd.DataFrame(results)
print(results_df[["Player", "group", "attempts", "mle", "prior", "posterior_mean", "posterior_map", "lower", "upper"]].to_string(index=False))

prior_colors  = {"Weak": "steelblue", "League centered": "darkorange", "Elite shooter": "seagreen"}
prior_offsets = {"Weak": -0.15, "League centered": 0.0, "Elite shooter": 0.15}

fig, axes = plt.subplots(2, 3, figsize=(16, 10), sharey=True, sharex=True)
axes = axes.flatten()

for ax, (_, player) in zip(axes, selected_players.iterrows()):
    player_results = results_df[results_df["Player"] == player["Player"]]

    for y in [-0.15, 0.0, 0.15]:
        ax.scatter(player["mle"], y, color="red", s=40, zorder=5, marker="|",
                   linewidths=2, label="MLE" if y == -0.15 else "")
    ax.axvline(player["mle"], color="red", lw=0.5, linestyle=":", alpha=0.5)
    ax.axvline(league_rate, color="gray", linestyle="--", lw=1, alpha=0.5, label="League rate")

    for _, row in player_results.iterrows():
        y = prior_offsets[row["prior"]]
        ax.scatter(row["posterior_mean"], y, color=prior_colors[row["prior"]], s=60, zorder=5)
        ax.plot([row["lower"], row["upper"]], [y, y],
                color=prior_colors[row["prior"]], lw=2, alpha=0.7)

    ax.set_title(f"{player['Player']}\n(attempts={int(player['attempts'])}, group={player['group']})",
                 fontsize=9, fontweight="bold")
    ax.set_xlabel("Free Throw Rate")
    ax.set_yticks([-0.15, 0.0, 0.15])
    ax.set_yticklabels(["Weak\nBeta(2,2)", "League\nBeta(30μ,30(1-μ))", "Elite\nBeta(90,10)"])
    ax.grid(axis="x", alpha=0.3)

handles, labels = axes[0].get_legend_handles_labels()
by_label = dict(zip(labels, handles))
fig.legend(by_label.values(), by_label.keys(), loc="lower center", ncol=4,
           fontsize=9, bbox_to_anchor=(0.5, -0.02))
plt.suptitle("MLE and Posterior Means with 95% Credible Intervals\nby Player and Prior",
             fontsize=13, fontweight="bold")
plt.tight_layout()
plt.show()

print("Designed prior: elite shooter would shoot ~90 so i chose that as the mean of my prior")
print("100 free throws was chosen as a substantive amount so that it would help adjust an elite shooter with a low shot count but if they shoot enough, it has lower influence because their results are more reflective")
print("Higher adjuststments made when volume is lower and when the means deviate more")

# TODO: verify that sequential and all-at-once updating give the same posterior.

strengths = [2, 10, 30, 100, 300]
low_player  = selected_players[selected_players["group"] == "low"].iloc[0]
high_player = selected_players[selected_players["group"] == "high"].iloc[0]

fig, ax = plt.subplots(figsize=(9, 5))

for player, style in [(low_player, "o-"), (high_player, "s--")]:
    post_means = []
    for s in strengths:
        a = league_rate * s
        b = (1 - league_rate) * s
        summary = beta_summary(player["wins"], player["attempts"], a, b)
        post_means.append(summary["posterior_mean"].iloc[0])
    ax.plot(strengths, post_means, style, label=f"{player['Player']} (n={int(player['attempts'])})")
    ax.axhline(player["mle"], linestyle=":", alpha=0.4)

ax.axhline(league_rate, color="gray", linestyle="--", lw=1, label=f"League rate ({league_rate:.3f})")
ax.set_xscale("log")
ax.set_xlabel("Prior Strength (log scale)")
ax.set_ylabel("Posterior Mean")
ax.set_title("Posterior Mean vs Prior Strength\n(center fixed at league rate)")
ax.legend()
ax.grid(alpha=0.3)
plt.tight_layout()
plt.show()

player = low_player
a0 = league_rate * 30
b0 = (1 - league_rate) * 30

half_wins     = int(player["wins"] // 2)
half_attempts = int(player["attempts"] // 2)

all_once  = beta_summary(player["wins"], player["attempts"], a0, b0)
seq_step1 = beta_summary(half_wins, half_attempts, a0, b0)
seq_final = beta_summary(
    player["wins"] - half_wins,
    player["attempts"] - half_attempts,
    alpha=a0 + half_wins,
    beta=b0 + half_attempts - half_wins
)

print(f"Sequential vs all-at-once for {player['Player']} (league-centered, strength=30):")
print(f"  All at once: mean={all_once['posterior_mean'].iloc[0]:.6f}, "
      f"CI=[{all_once['lower'].iloc[0]:.6f}, {all_once['upper'].iloc[0]:.6f}]")
print(f"  Sequential:  mean={seq_final['posterior_mean'].iloc[0]:.6f}, "
      f"CI=[{seq_final['lower'].iloc[0]:.6f}, {seq_final['upper'].iloc[0]:.6f}]")

print("For low volume --> 20 FTA, 30 FTS was enough to dominate; for high volume, even 300 FTA was not enough to dominate")
print("As we can see from Damian Jones, sequential and all at once give the same mean and CI")
# TODO: simulate posterior predictive outcomes for the next 50 attempts.

# Task 4: Posterior Prediction

print(f"{'Player':30s} {'Prior':20s} {'pred_mean':>10} {'BB_expect':>10} {'pred_low':>9} {'pred_upp':>9} {'pred_width':>11} {'CI_width':>9}")

for _, player in selected_players.iterrows():
    for _, prior in priors.iterrows():
        pred = beta_predictive_summary(player["wins"], player["attempts"], prior["alpha"], prior["beta"])
        post = beta_summary(player["wins"], player["attempts"], prior["alpha"], prior["beta"])

        # Beta-Binomial analytic expectation: 50 * (alpha + W) / (alpha + beta + N)
        alpha_post = prior["alpha"] + player["wins"]
        beta_post  = prior["beta"]  + player["attempts"] - player["wins"]
        bb_expect  = 50 * alpha_post / (alpha_post + beta_post)

        pred_width = pred["upper"].iloc[0] - pred["lower"].iloc[0]
        ci_width   = post["upper"].iloc[0] - post["lower"].iloc[0]

        print(f"{player['Player']:30s} {prior['prior']:20s} "
              f"{pred['predictive_mean'].iloc[0]:10.3f} "
              f"{bb_expect/50:10.3f} "
              f"{pred['lower'].iloc[0]:9.3f} "
              f"{pred['upper'].iloc[0]:9.3f} "
              f"{pred_width:11.3f} "
              f"{ci_width:9.3f}")
        
        
print("Assumptions:")
print("1. FTs are independent: pool consecutive free throws")
print("2. Priors are indentical for everyone: group players by make percent (logic is they are similar)")