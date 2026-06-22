###########################
### Authors: RB, JP      ###
### Python translation   ###
###########################

#############
### SETUP ###
#############

# pip install numpy pandas scipy scikit-learn matplotlib

import numpy as np
import pandas as pd
from scipy.sparse import csr_matrix
import matplotlib.pyplot as plt
from sklearn.linear_model import Ridge, RidgeCV
import re
import pyreadr

np.random.seed(15)

####################
### NBA LINEUPS  ###
####################

nba_lineups = pyreadr.read_r("../data/15_nba-lineups.rds")[None]
nba_lineups = (
    nba_lineups
    .dropna(subset=["lineup_team", "lineup_opp", "pts_poss"])
    .sort_values(["game_id", "period", "poss_num_team"])
    .reset_index(drop=True)
)

def split_lineup(series):
    return series.str.split(", ")

def player_name(token):
    return re.sub(r"^[0-9]+\s+", "", token)

off_lineups = split_lineup(nba_lineups["lineup_team"]).tolist()
def_lineups = split_lineup(nba_lineups["lineup_opp"]).tolist()

complete_lineups = [
    len(o) == 5 and len(d) == 5
    for o, d in zip(off_lineups, def_lineups)
]
nba_lineups = nba_lineups[complete_lineups].reset_index(drop=True)
off_lineups = [o for o, keep in zip(off_lineups, complete_lineups) if keep]
def_lineups = [d for d, keep in zip(def_lineups, complete_lineups) if keep]

all_tokens = []
for o, d in zip(off_lineups, def_lineups):
    all_tokens.extend(o)
    all_tokens.extend(d)
player_tokens = list(dict.fromkeys(all_tokens))  # unique, order-preserving
player_labels = [player_name(t) for t in player_tokens]

# If two players share the same name, keep the NBA ID attached.
from collections import Counter
name_counts = Counter(player_labels)
duplicate_names = {name for name, count in name_counts.items() if count > 1}
player_labels = [
    token if label in duplicate_names else label
    for token, label in zip(player_tokens, player_labels)
]
player_index = {token: i for i, token in enumerate(player_tokens)}

#######################
### DESIGN MATRICES ###
#######################

n = len(nba_lineups)
off_tokens_flat = [t for lineup in off_lineups for t in lineup]
def_tokens_flat = [t for lineup in def_lineups for t in lineup]

off_row_idx = [i for i, lineup in enumerate(off_lineups) for _ in lineup]
def_row_idx = [i for i, lineup in enumerate(def_lineups) for _ in lineup]

# Combined plus-minus design:
#   +1 when the player is on offense
#   -1 when the player is on defense
#    0 otherwise
row_idx_combined = off_row_idx + def_row_idx
col_idx_combined = (
    [player_index[t] for t in off_tokens_flat] +
    [player_index[t] for t in def_tokens_flat]
)
vals_combined = (
    [1.0] * len(off_tokens_flat) +
    [-1.0] * len(def_tokens_flat)
)
X_combined = csr_matrix(
    (vals_combined, (row_idx_combined, col_idx_combined)),
    shape=(n, len(player_tokens))
)
# Column names available via:
combined_col_names = player_labels

# Offense-defense split design:
#   offense columns get +1
#   defense columns get -1, so larger defensive coefficients are better
P = len(player_tokens)
row_idx_split = off_row_idx + def_row_idx
col_idx_split = (
    [player_index[t] for t in off_tokens_flat] +
    [P + player_index[t] for t in def_tokens_flat]
)
vals_split = (
    [1.0] * len(off_tokens_flat) +
    [-1.0] * len(def_tokens_flat)
)
X_split = csr_matrix(
    (vals_split, (row_idx_split, col_idx_split)),
    shape=(n, 2 * P)
)
split_col_names = (
    [f"{label}_off" for label in player_labels] +
    [f"{label}_def" for label in player_labels]
)

player_lookup = pd.DataFrame({
    "player_token": player_tokens,
    "player": player_labels,
    "combined_col": list(range(P)),
    "off_col": list(range(P)),
    "def_col": list(range(P, 2 * P)),
})

selected_players = [
    "Shai Gilgeous-Alexander",
    "Nikola Jokic",
    "Anthony Edwards",
    "Jalen Brunson",
    "Luka Doncic",
    "Victor Wembanyama",
    "Stephen Curry",
    "Domantas Sabonis",
    "Mikal Bridges",
    "Klay Thompson",
    "Austin Reaves",
    "Kyle Kuzma",
]
# Edit selected_players to choose who appears in your coefficient plots.

####################
### DATA SPLITS  ###
####################

games = sorted(nba_lineups["game_id"].unique())
n_games = len(games)

train_games = games[:int(np.floor(0.70 * n_games))]
valid_games = games[int(np.floor(0.70 * n_games)):int(np.floor(0.85 * n_games))]
test_games  = games[int(np.floor(0.85 * n_games)):]

idx_train = nba_lineups.index[nba_lineups["game_id"].isin(train_games)].tolist()
idx_valid = nba_lineups.index[nba_lineups["game_id"].isin(valid_games)].tolist()
idx_test  = nba_lineups.index[nba_lineups["game_id"].isin(test_games)].tolist()

y = nba_lineups["pts_poss"].to_numpy()
train_mean = y[idx_train].mean()
y_centered = y - train_mean
lambdas = np.logspace(-4, 2.5, num=50)

Xc_train = X_combined[idx_train]
Xc_valid = X_combined[idx_valid]
Xc_test  = X_combined[idx_test]

Xs_train = X_split[idx_train]
Xs_valid = X_split[idx_valid]
Xs_test  = X_split[idx_test]

y_train          = y[idx_train]
y_valid          = y[idx_valid]
y_test           = y[idx_test]
y_train_centered = y_centered[idx_train]
y_valid_centered = y_centered[idx_valid]
y_test_centered  = y_centered[idx_test]

player_possessions = np.array(np.abs(Xc_train).sum(axis=0)).flatten()

print(f"Prepared {len(nba_lineups)} possessions from {n_games} games and "
      f"{len(player_tokens)} players.")
print(f"Train / validation / test possessions: "
      f"{len(idx_train)} / {len(idx_valid)} / {len(idx_test)}")

#####################
### HELPER FUNCS  ###
#####################

def rmse(truth, prediction):
    return np.sqrt(np.mean((truth - prediction) ** 2))


def fit_ridge_path(X_train, y_train_centered, X_valid, y_valid_centered, lambdas):
    """
    Fit a ridge regression model over a grid of lambdas.
    sklearn's Ridge uses alpha = lambda * n, so we pass alpha directly.
    intercept=False and standardize=False to match glmnet behavior.
    Returns dict with fit info and best lambda by validation RMSE.
    """
    coefs = {}
    valid_rmses = []

    for lam in lambdas:
        model = Ridge(alpha=lam, fit_intercept=False)
        model.fit(X_train, y_train_centered)
        pred = model.predict(X_valid)
        valid_rmses.append(rmse(y_valid_centered, pred))
        coefs[lam] = model.coef_

    best_idx = int(np.argmin(valid_rmses))
    best_lambda = lambdas[best_idx]

    validation = pd.DataFrame({
        "lambda": lambdas,
        "valid_rmse": valid_rmses,
    })

    # Refit best model to store as a single sklearn object
    best_model = Ridge(alpha=best_lambda, fit_intercept=False)
    best_model.fit(X_train, y_train_centered)

    return {
        "coefs": coefs,
        "best_model": best_model,
        "validation": validation,
        "best_lambda": best_lambda,
        "best_valid_rmse": valid_rmses[best_idx],
    }


def predict_centered_model(model, X_new):
    """Add back the training mean to predictions from a centered model."""
    return train_mean + model.predict(X_new)


######################
### COMBINED MODEL ###
######################

# Mean-only validation RMSE
mean_valid_rmse = rmse(y_valid, np.full(len(y_valid), train_mean))
print(f"Mean-only valid RMSE: {mean_valid_rmse:.5f}")

# Raw plus-minus
poss_per_player = np.array(np.abs(Xc_train).sum(axis=0)).flatten()
pts_per_player  = np.array(Xc_train.T @ y_train_centered).flatten()
raw_pm = np.divide(pts_per_player, poss_per_player,
                   out=np.zeros_like(pts_per_player),
                   where=poss_per_player > 0)

raw_valid_rmse = rmse(y_valid, train_mean + Xc_valid @ raw_pm)
print(f"Raw PM valid RMSE: {raw_valid_rmse:.5f}")

# Unregularized APM
from scipy.sparse.linalg import lsqr
apm_coef, *_ = lsqr(Xc_train, y_train_centered, atol=1e-8, btol=1e-8, iter_lim=2000)
apm_valid_rmse = rmse(y_valid, train_mean + Xc_valid @ apm_coef)
print(f"APM valid RMSE: {apm_valid_rmse:.5f}")

# Combined RAPM
combined_ridge = fit_ridge_path(
    X_train=Xc_train,
    y_train_centered=y_train_centered,
    X_valid=Xc_valid,
    y_valid_centered=y_valid_centered,
    lambdas=lambdas,
)
print(f"Best lambda: {combined_ridge['best_lambda']:.5f}")
print(f"Comb RAPM valid RMSE: {combined_ridge['best_valid_rmse']:.5f}")

combined_valid_curve = combined_ridge["validation"]
plt.plot(combined_valid_curve["lambda"], combined_valid_curve["valid_rmse"])
plt.xscale("log")
plt.xlabel("λ (log scale)")
plt.ylabel("Validation RMSE")
plt.title("Combined RAPM – Validation RMSE vs λ")
plt.axvline(combined_ridge["best_lambda"], color="red", ls="--",
            label=f"Best λ = {combined_ridge['best_lambda']:.1f}")
plt.legend()
plt.show()

# Test evaluation
combined_test_pred = predict_centered_model(combined_ridge["best_model"], Xc_test)
combined_test_rmse = rmse(y_test, combined_test_pred)
print(f"Comb RAPM test RMSE: {combined_test_rmse:.5f}")


# Selected-player coefficient plot: Raw PM, APM, RAPM in points per 100 possessions
label_to_idx = {lab: i for i, lab in enumerate(player_labels)}
sel_idx      = [label_to_idx[p] for p in selected_players if p in label_to_idx]
sel_labels   = [player_labels[i] for i in sel_idx]

# Scale to per-100 possessions
raw_sel  = raw_pm[sel_idx]                             * 100
apm_sel  = apm_coef[sel_idx]                           * 100
rapm_sel = combined_ridge["best_model"].coef_[sel_idx] * 100

# Sort by RAPM
order    = np.argsort(rapm_sel)
sl_s     = [sel_labels[i] for i in order]
raw_s, apm_s, rapm_s = raw_sel[order], apm_sel[order], rapm_sel[order]

fig, ax = plt.subplots(figsize=(9, 7))
y_pos = np.arange(len(sl_s))
gap   = 0.22

ax.barh(y_pos + gap, raw_s,  height=0.20, color="#b0b0b0", label="Raw PM")
ax.barh(y_pos,       apm_s,  height=0.20, color="#5b9bd5", label="APM")
ax.barh(y_pos - gap, rapm_s, height=0.20, color="#e05f4a", label="RAPM (ridge)")
ax.axvline(0, color="black", lw=1.2)
ax.set_yticks(y_pos)
ax.set_yticklabels(sl_s, fontsize=10)
ax.set_xlabel("Estimated impact (points per 100 possessions)", fontsize=11)
ax.set_title("Raw PM, APM, and Combined RAPM\n(sorted by RAPM)", fontsize=13, fontweight="bold")
ax.legend(fontsize=10, loc="lower right")
ax.grid(True, axis="x", ls=":", alpha=0.5)
plt.tight_layout()
plt.show()

print("SGA and Kuzma both slide down/up respectively because of the RAPM adjustment. This is probably due to factors such as teammates that they typically play with.")
print("SGA plays with great teammates often so his Beta will likely be inflated while Kuzma has the opposite. RAPM adjusts by ridge regressing back to the center.")

#############################
### OFFENSE/DEFENSE MODEL ###
#############################

# Fit split ridge models and choose lambda using validation RMSE
split_ridge = fit_ridge_path(
    X_train=Xs_train,
    y_train_centered=y_train_centered,
    X_valid=Xs_valid,
    y_valid_centered=y_valid_centered,
    lambdas=lambdas,
)
print(f"Best lambda: {split_ridge['best_lambda']:.5f}")
print(f"Split RAPM valid RMSE: {split_ridge['best_valid_rmse']:.5f}")

# Evaluate on test set
split_test_pred = predict_centered_model(split_ridge["best_model"], Xs_test)
split_test_rmse = rmse(y_test, split_test_pred)
print(f"Split RAPM test RMSE: {split_test_rmse:.5f}")

# Selected-player coefficient plot: offense and defense separately
split_coef = split_ridge["best_model"].coef_
off_sel    = split_coef[:P][sel_idx] * 100
def_sel    = split_coef[P:][sel_idx] * 100
tot_sel    = off_sel + def_sel  # total impact

# Sort by total
order2  = np.argsort(tot_sel)
sl2     = [sel_labels[i] for i in order2]
off_s2  = off_sel[order2]
def_s2  = def_sel[order2]
tot_s2  = tot_sel[order2]

fig, ax = plt.subplots(figsize=(9, 7))
y2 = np.arange(len(sl2))

# horizontal line spanning off to def for each player
for i in range(len(sl2)):
    ax.hlines(y2[i], min(off_s2[i], def_s2[i], tot_s2[i]),
              max(off_s2[i], def_s2[i], tot_s2[i]),
              color="lightgray", lw=2, zorder=1)

ax.scatter(off_s2, y2, color="#e05f4a", s=60, zorder=2, label="Offensive RAPM")
ax.scatter(def_s2, y2, color="#4a8ae0", s=60, zorder=2, label="Defensive RAPM")
ax.scatter(tot_s2, y2, color="#888888", s=60, zorder=2, label="Total RAPM")

ax.axvline(0, color="black", lw=1.2, ls="--")
ax.set_yticks(y2)
ax.set_yticklabels(sl2, fontsize=10)
ax.set_xlabel("Estimated impact (points per 100 possessions)", fontsize=11)
ax.set_title("Split RAPM: Offensive, Defensive, and Total\n(sorted by total impact)",
             fontsize=13, fontweight="bold")
ax.legend(fontsize=10)
ax.grid(True, axis="x", ls=":", alpha=0.5)
plt.tight_layout()
plt.show()
print("Steph curry has a high offensive RAPM but a low defensive RAPM so his overall RAPM is relatively centered")


########################
### COMPARISON NOTES ###
########################

mean_test_rmse = rmse(y_test, np.full(len(y_test), train_mean))
raw_test_rmse  = rmse(y_test, train_mean + Xc_test @ raw_pm)
apm_test_rmse  = rmse(y_test, train_mean + Xc_test @ apm_coef)

model_names = ["Mean-only", "Raw PM", "APM", "Combined RAPM", "Split RAPM"]
test_rmses  = [mean_test_rmse, raw_test_rmse, apm_test_rmse,
               combined_test_rmse, split_test_rmse]

for name, val in zip(model_names, test_rmses):
    print(f"{name:20s}  test RMSE: {val:.5f}")
    
print("Combined RAPM has the lowest test RMSE; the split model showcases the value offensively and defensively of a player")
print("However, it does not appear to be that useful predicting because they have such similar RMSE, which are both barely below the mean-only (everyone at 0)")
