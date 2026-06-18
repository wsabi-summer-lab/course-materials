#######################
### Authors: RB, JP ###
#######################

#############
### SETUP ###
#############

import numpy as np
import pandas as pd
import stan
import matplotlib.pyplot as plt

np.random.seed(14)

#################
### NFL GAMES ###
#################

nfl = pd.read_csv("../data/14_nfl-games.csv")
nfl = nfl[(nfl["season_type"] == "REG") & (nfl["season"] == 2023)].copy()

teams = sorted(set(nfl["home_team"]).union(set(nfl["away_team"])))
team_index = {team: i + 1 for i, team in enumerate(teams)} 

nfl["H"] = nfl["home_team"].map(team_index)
nfl["A"] = nfl["away_team"].map(team_index)

stan_data = {
    "N_games": len(nfl),
    "N_teams": len(teams),
    "y": nfl["pts_H_minus_A"].tolist(),
    "H": nfl["H"].tolist(),
    "A": nfl["A"].tolist(),
}

with open("../starter-code/14_nfl-model.stan", "r") as f:
    stan_model_code = f.read()

model = stan.build(stan_model_code, data=stan_data, random_seed=14)
fit = model.sample(num_chains=4, num_samples=1000, num_warmup=1000)

# TODO: fit with at least four chains and inspect diagnostics.
# model = stan.build(stan_model_code, data=stan_data, random_seed=14)
# fit = model.sample(num_chains=4, num_samples=1000)
# idata = az.from_pystan(fit)
# az.summary(idata)  # inspect Rhat and ESS diagnostics

n_divergent = int(np.sum(fit["divergent__"]))
print(f"Divergent transitions: {n_divergent}")

def split_rhat(draws_chains):
    """Compute split-Rhat. draws_chains shape: (N_chains, N_samples)."""
    n_chains, n_samples = draws_chains.shape
    half = n_samples // 2
    splits = np.concatenate([draws_chains[:, :half], draws_chains[:, half:]], axis=0)
    chain_means = splits.mean(axis=1)
    B = n_samples * np.var(chain_means, ddof=1)
    W = np.mean(np.var(splits, axis=1, ddof=1))
    var_hat = (n_samples - 1) / n_samples * W + B / n_samples
    return np.sqrt(var_hat / W)

def bulk_ess(draws_chains):
    """Approximate bulk ESS via rank normalization."""
    from scipy.stats import rankdata
    n_chains, n_samples = draws_chains.shape
    flat = draws_chains.flatten()
    ranked = rankdata(flat).reshape(n_chains, n_samples)
    z = (ranked - 0.5) / (n_chains * n_samples)
    z = z.clip(1e-6, 1 - 1e-6)
    from scipy.special import ndtri
    z = ndtri(z)
    rho_sum = 0.0
    for lag in range(1, n_samples):
        rho = np.mean([np.corrcoef(z[c, :-lag], z[c, lag:])[0, 1] for c in range(n_chains)])
        if lag > 1 and rho < 0:
            break
        rho_sum += rho
    ess = (n_chains * n_samples) / (1 + 2 * rho_sum)
    return ess

print("\nDiagnostics for key parameters:")
print(f"{'param':<15} {'mean':>8} {'sd':>8} {'rhat':>8} {'ess':>8}")
for param in ["home_field", "sigma_game", "sigma_team"]:
    draws = np.array(fit[param]).flatten()          
    draws_chains = draws.reshape(4, 1000)           
    mean = draws.mean()
    sd = draws.std()
    rhat = split_rhat(draws_chains)
    ess = bulk_ess(draws_chains)
    print(f"{param:<15} {mean:>8.3f} {sd:>8.3f} {rhat:>8.3f} {ess:>8.0f}")

fig, axes = plt.subplots(3, 2, figsize=(12, 8))

for row, param in enumerate(["home_field", "sigma_game", "sigma_team"]):
    draws = np.array(fit[param]).flatten()
    chain_draws = draws.reshape(4, 1000)           

    ax_trace = axes[row, 0]
    for i, chain in enumerate(chain_draws):
        ax_trace.plot(chain, alpha=0.6, linewidth=0.5, label=f"Chain {i+1}")
    ax_trace.set_ylabel(param)
    ax_trace.set_xlabel("Iteration")
    ax_trace.set_title(f"{param} — trace")
    if row == 0:
        ax_trace.legend(fontsize=7, ncol=4)

    ax_dens = axes[row, 1]
    for i, chain in enumerate(chain_draws):
        ax_dens.hist(chain, bins=40, density=True, alpha=0.4, label=f"Chain {i+1}")
    ax_dens.set_xlabel(param)
    ax_dens.set_title(f"{param} — density")

plt.suptitle("Trace Plots: home_field, sigma_game, sigma_team", y=1.02)
plt.tight_layout()
plt.show()
print("\n")

# TODO: extract posterior draws. Each row of draws["strength"] is one plausible
# football world after seeing the data.

# TODO: estimate P(beta_DAL > beta_PHI | data) by counting posterior draws.

# TODO: compute each team's probability of ranking in the top five.

strength = np.array(fit["strength"]).T

dal = teams.index("DAL")
phi = teams.index("PHI")

p_dal_gt_phi = np.mean(strength[:, dal] > strength[:, phi])
print(f"P(beta_DAL > beta_PHI | data) = {p_dal_gt_phi:.3f}")

ranks = np.argsort(np.argsort(-strength, axis=1), axis=1) + 1
top_five_prob = np.mean(ranks <= 5, axis=0)

post_mean = np.mean(strength, axis=0)
best_mean_team = teams[np.argmax(post_mean)]
prob_rank_first = np.mean(ranks == 1, axis=0)
most_likely_first_team = teams[np.argmax(prob_rank_first)]

print(f"Team with highest posterior mean: {best_mean_team} ({np.max(post_mean):.3f})")
print(f"Team most likely to rank 1st:     {most_likely_first_team} ({np.max(prob_rank_first):.3f})")
print("These don't need to be the same teams because teams may have different standard deviations")

top_five_df = pd.DataFrame({
    "team": teams,
    "posterior_mean": post_mean,
    "top_five_prob": top_five_prob,
    "prob_rank_first": prob_rank_first,
}).sort_values("top_five_prob", ascending=False)
print("\nTop-five probabilities:")
print(top_five_df.to_string(index=False))

ci_lower = np.percentile(strength, 2.5, axis=0)
ci_upper = np.percentile(strength, 97.5, axis=0)
order = np.argsort(post_mean)

fig, ax = plt.subplots(figsize=(8, 10))
y_pos = np.arange(len(teams))

for i, idx in enumerate(order):
    ax.plot([ci_lower[idx], ci_upper[idx]], [i, i], color="steelblue", linewidth=1.5, alpha=0.6)
    ax.scatter(post_mean[idx], i, color="steelblue", zorder=3, s=30)

ax.set_yticks(y_pos)
ax.set_yticklabels([teams[i] for i in order], fontsize=8)
ax.axvline(0, color="gray", linestyle="--", linewidth=0.8)
ax.set_xlabel("Team Strength (β)")
ax.set_title("Posterior Means and 95% Credible Intervals by Team")
plt.tight_layout()
plt.show()

# TODO: simulate a neutral-site Dallas vs Philadelphia game from each draw.

sigma_game = np.array(fit["sigma_game"]).flatten()

y_future = np.random.normal(
    loc=strength[:, dal] - strength[:, phi],
    scale=sigma_game
)

expected_diff = np.mean(y_future)
win_prob_predictive = np.mean(y_future > 0)
pi_lower, pi_upper = np.percentile(y_future, [2.5, 97.5])
ci_lower_diff, ci_upper_diff = np.percentile(strength[:, dal] - strength[:, phi], [2.5, 97.5])

print(f"Expected score differential (DAL - PHI): {expected_diff:.2f}")
print(f"Predictive win probability (DAL):         {win_prob_predictive:.3f}")
print(f"95% Posterior predictive interval:        [{pi_lower:.1f}, {pi_upper:.1f}]")
print(f"95% Credible interval (strength diff):    [{ci_lower_diff:.1f}, {ci_upper_diff:.1f}]")

# TODO: compare np.mean(y_future > 0) with np.mean(strength[:, dal] > strength[:, phi]).

win_prob_strength = np.mean(strength[:, dal] > strength[:, phi])
print(f"\nnp.mean(y_future > 0)                = {win_prob_predictive:.3f}")
print(f"np.mean(strength_DAL > strength_PHI) = {win_prob_strength:.3f}")

# TODO: use fit["y_rep"] for posterior predictive checks.

y_rep = np.array(fit["y_rep"]).T  
y_obs = np.array(nfl["pts_H_minus_A"].tolist())

# TODO: find a neutral-site spread with posterior predictive cover probability >= 0.55.

s_values = np.arange(0, 30, 0.5)
cover_probs = [np.mean(y_future > s) for s in s_values]
valid_s = [s for s, p in zip(s_values, cover_probs) if p >= 0.55]
best_s = max(valid_s) if valid_s else 0.0
print(f"\nLargest spread s where P(DAL covers) >= 0.55: {best_s:.1f} points")


#Task 4
y_rep = np.array(fit["y_rep"]).T 
y_obs = np.array(nfl["pts_H_minus_A"].tolist())

fig, ax = plt.subplots(figsize=(8, 5))
for i in np.random.choice(len(y_rep), size=100, replace=False):
    ax.hist(y_rep[i], bins=30, density=True, alpha=0.02, color="steelblue")
ax.hist(y_obs, bins=30, density=True, alpha=0.8, color="tomato",
        histtype="step", linewidth=2, label="Observed")
ax.set_xlabel("Score Differential (Home − Away)")
ax.set_title("PPC: Score Differential Distribution")
ax.legend()
plt.tight_layout()
plt.show()

obs_home_win_rate = np.mean(y_obs > 0)
rep_home_win_rates = np.mean(y_rep > 0, axis=1)

print(f"Observed home win rate:   {obs_home_win_rate:.3f}")
print(f"Replicated mean:          {np.mean(rep_home_win_rates):.3f}")
print(f"Replicated 95% PI:        [{np.percentile(rep_home_win_rates, 2.5):.3f}, "
      f"{np.percentile(rep_home_win_rates, 97.5):.3f}]")

obs_blowout_rate = np.mean(np.abs(y_obs) >= 21)
rep_blowout_rates = np.mean(np.abs(y_rep) >= 21, axis=1)

print(f"\nObserved blowout rate:    {obs_blowout_rate:.3f}")
print(f"Replicated mean:          {np.mean(rep_blowout_rates):.3f}")
print(f"Replicated 95% PI:        [{np.percentile(rep_blowout_rates, 2.5):.3f}, "
      f"{np.percentile(rep_blowout_rates, 97.5):.3f}]")

fig, axes = plt.subplots(1, 2, figsize=(12, 4))

for ax, rep_rates, obs_rate, title in zip(
    axes,
    [rep_home_win_rates, rep_blowout_rates],
    [obs_home_win_rate, obs_blowout_rate],
    ["Home Win Rate", "Blowout Rate (|diff| ≥ 21)"]
):
    ax.hist(rep_rates, bins=30, color="steelblue", alpha=0.7, density=True)
    ax.axvline(obs_rate, color="tomato", linewidth=2, label=f"Observed = {obs_rate:.3f}")
    ax.set_title(f"PPC: {title}")
    ax.set_xlabel("Rate")
    ax.legend()

plt.tight_layout()
plt.show()