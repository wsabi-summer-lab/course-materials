import numpy as np
import pandas as pd

# Set seed
np.random.seed(13)

########################
### HELPER FUNCTIONS ###
########################

def positive_part_js(x, sigma2):
    # Centered shrinkage estimates one dimension through mean(x), so k >= 4.
    center = np.mean(x)
    spread = np.sum((x - center) ** 2)
    shrinkage_factor = max(0, 1 - ((len(x) - 3) * sigma2) / spread)
    return center + shrinkage_factor * (x - center)

def mse(truth, prediction):
    return np.mean((truth - prediction) ** 2)

####################
### GOLF PUTTING ###
####################

putts_train = pd.read_csv("../data/13_putts-train.csv.gz")
putts_test = pd.read_csv("../data/13_putts-test.csv.gz")

mu_hat = np.average(putts_train["X"], weights=putts_train["N"])
C_hat = mu_hat * (1 - mu_hat)
tau2_hat = max(np.var(putts_train["X"], ddof=1) - np.mean(C_hat / putts_train["N"]), 0)
sigma2_common = np.mean(C_hat / putts_train["N"])

predictions = putts_train.copy()
predictions["mean"] = predictions["X"].mean()
predictions["mle"] = predictions["X"]
predictions["empirical_bayes"] = (
    mu_hat + tau2_hat / (tau2_hat + C_hat / predictions["N"]) * (predictions["X"] - mu_hat)
)
predictions["james_stein"] = positive_part_js(predictions["X"].values, sigma2_common)

# TODO: compare the four training-data leaderboards.
print("1: Shrinks completely to the mean (lambda = 1)")
print("2: Standard MLE: no shrinkage to the mean (lambda = 0)")
print("3: Shrinkage based on normal distribution assumptions on the proportions and the sample variation")
print("4: Shrinkage based on sample variance divided by the data variance")
# TODO: explain the common-variance approximation and a standardization alternative.

estimators = ["mean", "mle", "empirical_bayes", "james_stein"]

for col in estimators:
    predictions[f"rank_{col}"] = predictions[col].rank(ascending=False).astype(int)

print("\n=== Range and StdDev ===")
print(f"\n{'Estimator':<20} {'Range':>8} {'StdDev':>8}")
print("-" * 38)
for col in estimators:
    r = predictions[col].max() - predictions[col].min()
    s = predictions[col].std()
    print(f"{col:<20} {r:>8.4f} {s:>8.4f}")

for col in estimators:
    print(f"\n=== Top 5: {col} ===")
    top5 = predictions.nsmallest(5, f"rank_{col}")[["Player", "N", col, f"rank_{col}"]]
    print(top5.to_string(index=False))

    print(f"\n=== Bottom 5: {col} ===")
    bot5 = predictions.nlargest(5, f"rank_{col}")[["Player", "N", col, f"rank_{col}"]]
    print(bot5.to_string(index=False))

print("\n=== Avg absolute move from MLE ===")
for col in ["mean", "empirical_bayes", "james_stein"]:
    avg_move = (predictions[col] - predictions["mle"]).abs().mean()
    max_move = (predictions[col] - predictions["mle"]).abs().max()
    print(f"  {col:<20}  mean_move={avg_move:.5f}  max_move={max_move:.5f}")


# TODO: join putts_test only when ready to evaluate.

results = predictions.merge(putts_test[["Player", "X"]], on="Player", suffixes=("_train", "_test"))

print("\n=== Overall MSE by Estimator ===")
mse_scores = {}
for col in estimators:
    mse_scores[col] = mse(results["X_test"], results[col])
    print(f"  {col:<20}: {mse_scores[col]:.6f}")

best_method = min(mse_scores, key=mse_scores.get)
print(f"\nBest estimator: {best_method}")

import matplotlib.pyplot as plt

fig, axes = plt.subplots(2, 2, figsize=(10, 8), sharey=True, sharex=True)
axes = axes.flatten()

for ax, col in zip(axes, estimators):
    ax.scatter(results["X_test"], results[col], alpha=0.6, edgecolors="k", linewidths=0.4)
    lo = min(results[col].min(), results["X_test"].min()) - 0.01
    hi = max(results[col].max(), results["X_test"].max()) + 0.01
    ax.plot([lo, hi], [lo, hi], "r--", linewidth=1)
    for _, row in results.iterrows():
        ax.plot([row["X_test"], row["X_test"]], [row["X_test"], row[col]], 
                color="gray", linewidth=0.5, alpha=0.5)
    ax.set_title(f"{col}\nMSE = {mse_scores[col]:.6f}")
    ax.set_xlabel("True putting rate (test)")
    ax.set_ylabel("Estimated putting rate")

fig.suptitle("Estimation Error by Estimator", fontsize=13)
plt.tight_layout()
plt.show()
# TODO: compare overall MSE and golfer-level squared errors.

for col in estimators:
    results[f"se_{col}"] = (results[col] - results["X_test"]) ** 2

best_method = min(mse_scores, key=mse_scores.get)
results["winner"] = results[[f"se_{col}" for col in estimators]].idxmin(axis=1).str.replace("se_", "")

print("\n=== Golfer-Level Winner ===")
for col in estimators:
    print(f"{col} wins for {(results['winner'] == col).sum()} golfers")

# Plot per-golfer squared errors for all 4 estimators
x = np.arange(len(results))
colors = {"mean": "green", "mle": "red", "empirical_bayes": "blue", "james_stein": "orange"}

fig, ax = plt.subplots(figsize=(14, 5))
for col in estimators:
    ax.plot(x, results[f"se_{col}"], color=colors[col], marker="o", markersize=2, label=col)

ax.set_xlabel("Player")
ax.set_ylabel("Squared Error")
ax.set_title("Player-Level Squared Error: All Estimators")
ax.legend()
plt.tight_layout()
plt.show()

print("Different methods have different MSE for individual players, so best overall method (empirical bayes) is not the best for each individual")

for col in ["mean", "mle", "empirical_bayes", "james_stein"]:
    predictions[col] = predictions[col].clip(0, 1)
    
    
    
def asymmetric_loss(truth, prediction):
    errors = prediction - truth
    loss = np.where(errors > 0, 2 * errors**2, errors**2)
    return np.mean(loss)

print("\n=== Asymmetric Loss (overrating costs 2x) ===")
asym_scores = {}
for col in estimators:
    asym_scores[col] = asymmetric_loss(results["X_test"], results[col])
    print(f"  {col:<20}: {asym_scores[col]:.6f}")

best_asym = min(asym_scores, key=asym_scores.get)
print(f"\nBest estimator under asymmetric loss: {best_asym}")
print(f"Best estimator under symmetric MSE:   {best_method}")
print("In this case, the preferred estimator doesn't change, but loss method is still important because different methods have better approaches to handling certain situations, so best result is dependent on how we evaluate loss")