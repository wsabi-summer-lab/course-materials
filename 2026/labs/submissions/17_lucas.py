##########################
### Lab 17: Neural Networks
### Python translation of R starter code
##########################

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import warnings
warnings.filterwarnings("ignore")

from sklearn.linear_model import LogisticRegression
from sklearn.neural_network import MLPClassifier
import xgboost as xgb
from itertools import product as iproduct

np.random.seed(17)

# ─────────────────────────────────────────
# Helper functions
# ─────────────────────────────────────────

def clip_probability(p, eps=1e-6):
    return np.clip(p, eps, 1 - eps)

def log_loss(actual, predicted):
    predicted = clip_probability(np.asarray(predicted, dtype=float))
    actual = np.asarray(actual, dtype=float)
    return -np.mean(actual * np.log(predicted) + (1 - actual) * np.log(1 - predicted))

def accuracy(actual, predicted):
    return np.mean((np.asarray(predicted) >= 0.5).astype(int) == np.asarray(actual))

def evaluate_predictions(model_name, split_name, actual, predicted):
    return {
        "model": model_name,
        "split": split_name,
        "log_loss": log_loss(actual, predicted),
        "accuracy": accuracy(actual, predicted),
    }

# ─────────────────────────────────────────
# Load data
# ─────────────────────────────────────────

pa = pd.read_csv("../data/17_play-action-vs-run.csv")
pa["playActionPass"] = pa["playActionPass"].astype(int)
pa["split"] = pd.Categorical(pa["split"], categories=["train", "validation", "test"], ordered=True)
for col in ["quarter", "down", "offenseFormation", "receiverAlignment"]:
    pa[col] = pa[col].astype("category")
pa["anyPreSnapMotion"] = pa["anyPreSnapMotion"].map({0: "no_motion_or_shift", 1: "motion_or_shift"}).astype("category")

snap_examples = pd.read_csv("../data/17_snap-examples.csv")
snap_examples["playActionPass"] = snap_examples["playActionPass"].astype(int)
snap_examples["side"] = pd.Categorical(snap_examples["side"], categories=["offense", "defense", "football"])

train       = pa[pa["split"] == "train"].copy()
valid       = pa[pa["split"] == "validation"].copy()
test        = pa[pa["split"] == "test"].copy()
train_valid = pa[pa["split"].isin(["train", "validation"])].copy()

print(f"Train / validation / test plays: {len(train)} / {len(valid)} / {len(test)}")
print(pa.groupby(["split", "playActionPass"]).size().reset_index(name="n"))

# ─────────────────────────────────────────
# Feature matrix (one-hot, standardize on train only)
# ─────────────────────────────────────────

model_vars = [
    "quarter", "down", "yardsToGo", "yardsToEndzone", "gameClockSeconds",
    "playClockAtSnap", "scoreDifferential", "expectedPoints",
    "offenseFormation", "receiverAlignment", "anyPreSnapMotion",
    "offenseWidth", "defenseWidth", "offenseDepth", "defenseDepth",
    "meanOffenseBackfieldDepth", "meanDefenderDepth",
    "meanOffenseSpeed", "meanDefenseSpeed", "boxDefenders",
    "meanNearestDefender", "minNearestDefender",
]

model_data = pa[["split", "playActionPass"] + model_vars].dropna()
design_all = pd.get_dummies(model_data[model_vars], drop_first=True)

tidx  = model_data["split"] == "train"
vidx  = model_data["split"] == "validation"
tidx_ = model_data["split"] == "test"
tvidx = model_data["split"].isin(["train", "validation"])

x_center = design_all[tidx].mean()
x_scale  = design_all[tidx].std().replace(0, 1)

def standardize(df):
    return (df - x_center) / x_scale

x_train       = standardize(design_all[tidx]).values
x_valid       = standardize(design_all[vidx]).values
x_test        = standardize(design_all[tidx_]).values
x_train_valid = standardize(design_all[tvidx]).values

y_train       = model_data["playActionPass"][tidx].values
y_valid       = model_data["playActionPass"][vidx].values
y_test        = model_data["playActionPass"][tidx_].values
y_train_valid = model_data["playActionPass"][tvidx].values

# ─────────────────────────────────────────
# TASK 1: Baselines
# ─────────────────────────────────────────

print("\n=== TASK 1: Class balance ===")
for sp in ["train", "validation", "test"]:
    sub = pa[pa["split"] == sp]
    n_pa    = sub["playActionPass"].sum()
    n_total = len(sub)
    print(f"  {sp}: {n_pa}/{n_total} play-action ({100*n_pa/n_total:.1f}%)")

# Training-mean baseline
mean_pred  = y_train.mean()
mean_valid = np.full(len(y_valid), mean_pred)
mean_test  = np.full(len(y_test),  mean_pred)
r_mean_valid = evaluate_predictions("Training mean", "validation", y_valid, mean_valid)
r_mean_test  = evaluate_predictions("Training mean", "test",       y_test,  mean_test)
print(f"\nTraining mean: valid_ll={r_mean_valid['log_loss']:.4f}, test_ll={r_mean_test['log_loss']:.4f}")

# Formation-rate baseline
formation_rates = (
    train.groupby(["offenseFormation", "receiverAlignment"])["playActionPass"]
    .agg(rate="mean", n="count")
    .reset_index()
)

def predict_formation_rate(df, fallback, rates):
    merged = df[["offenseFormation", "receiverAlignment"]].merge(
        rates, on=["offenseFormation", "receiverAlignment"], how="left"
    )
    return merged["rate"].fillna(fallback).values

formation_valid = predict_formation_rate(valid, mean_pred, formation_rates)
formation_test  = predict_formation_rate(test,  mean_pred, formation_rates)
r_form_valid = evaluate_predictions("Formation rate", "validation", y_valid, formation_valid)
r_form_test  = evaluate_predictions("Formation rate", "test",       y_test,  formation_test)
print(f"Formation rate: valid_ll={r_form_valid['log_loss']:.4f}, test_ll={r_form_test['log_loss']:.4f}")

# Table: PA rate by formation x alignment
pa_rate_table = (
    train.groupby(["offenseFormation", "receiverAlignment"])["playActionPass"]
    .agg(["mean", "count"])
    .rename(columns={"mean": "pa_rate", "count": "n"})
    .reset_index()
    .sort_values("pa_rate", ascending=False)
)
print("\nPlay-action rate by formation × alignment (training data):")
print(pa_rate_table.to_string(index=False))

# Formation × Alignment heatmap
pivot = pa_rate_table.pivot(index="offenseFormation", columns="receiverAlignment", values="pa_rate")
fig, ax = plt.subplots(figsize=(10, 5))
im = ax.imshow(pivot.values, aspect="auto", cmap="Blues", vmin=0,
               vmax=np.nanmax(pivot.values))
ax.set_xticks(range(len(pivot.columns)))
ax.set_xticklabels(pivot.columns, rotation=45, ha="right")
ax.set_yticks(range(len(pivot.index)))
ax.set_yticklabels(pivot.index)
plt.colorbar(im, ax=ax, label="Play-action rate")
ax.set_title("Play-action rate by Formation × Receiver Alignment (training data)")
plt.tight_layout()
plt.savefig("task1_formation_heatmap.png", dpi=150)
plt.close()
print("Saved: task1_formation_heatmap.png")

# ─────────────────────────────────────────
# TASK 2a: Logistic Regression
# ─────────────────────────────────────────

print("\n=== TASK 2: Logistic Regression ===")
lr = LogisticRegression(max_iter=1000, C=1e6, solver="lbfgs", random_state=17)
lr.fit(x_train, y_train)
lr_valid     = lr.predict_proba(x_valid)[:, 1]
r_lr_valid   = evaluate_predictions("Logistic regression", "validation", y_valid, lr_valid)
print(f"Logistic regression valid_ll={r_lr_valid['log_loss']:.4f}")

final_lr = LogisticRegression(max_iter=1000, C=1e6, solver="lbfgs", random_state=17)
final_lr.fit(x_train_valid, y_train_valid)
lr_test    = final_lr.predict_proba(x_test)[:, 1]
r_lr_test  = evaluate_predictions("Logistic regression", "test", y_test, lr_test)
print(f"Logistic regression test_ll={r_lr_test['log_loss']:.4f}")

# ─────────────────────────────────────────
# TASK 2b: Feedforward Neural Network grid search
# ─────────────────────────────────────────

print("\n=== TASK 2: Feedforward NN grid search ===")
sizes  = [2, 4, 6, 8]
decays = [0.0001, 0.001, 0.01, 0.1, 1.0]

nnet_results = []
for size in sizes:
    for decay in decays:
        mlp = MLPClassifier(
            hidden_layer_sizes=(size,),
            alpha=decay,
            activation="logistic",
            solver="lbfgs",
            max_iter=400,
            random_state=17,
        )
        mlp.fit(x_train, y_train)
        vp  = mlp.predict_proba(x_valid)[:, 1]
        vll = log_loss(y_valid, vp)
        nnet_results.append({"size": size, "decay": decay, "valid_log_loss": vll, "fit": mlp})

nnet_df = pd.DataFrame([{k: v for k, v in r.items() if k != "fit"} for r in nnet_results])
print(nnet_df.sort_values("valid_log_loss").to_string(index=False))

best_nnet = min(nnet_results, key=lambda r: r["valid_log_loss"])
print(f"\nBest NN: size={best_nnet['size']}, decay={best_nnet['decay']}, valid_ll={best_nnet['valid_log_loss']:.4f}")

final_nnet = MLPClassifier(
    hidden_layer_sizes=(best_nnet["size"],),
    alpha=best_nnet["decay"],
    activation="logistic",
    solver="lbfgs",
    max_iter=400,
    random_state=17,
)
final_nnet.fit(x_train_valid, y_train_valid)
nnet_test    = final_nnet.predict_proba(x_test)[:, 1]
r_nnet_valid = evaluate_predictions("Feedforward NN", "validation", y_valid,
                                    best_nnet["fit"].predict_proba(x_valid)[:, 1])
r_nnet_test  = evaluate_predictions("Feedforward NN", "test", y_test, nnet_test)
print(f"NN test_ll={r_nnet_test['log_loss']:.4f}")

# ─────────────────────────────────────────
# TASK 3: XGBoost
# ─────────────────────────────────────────

print("\n=== TASK 3: XGBoost grid search ===")
dtrain = xgb.DMatrix(x_train,       label=y_train)
dvalid = xgb.DMatrix(x_valid,       label=y_valid)
dtest  = xgb.DMatrix(x_test,        label=y_test)
dtv    = xgb.DMatrix(x_train_valid, label=y_train_valid)

xgb_grid    = list(iproduct([2, 4], [0.03, 0.1], [1, 5]))
xgb_results = []

for max_depth, eta, min_child_weight in xgb_grid:
    params = dict(
        objective="binary:logistic",
        eval_metric="logloss",
        max_depth=max_depth,
        eta=eta,
        min_child_weight=min_child_weight,
        subsample=0.9,
        colsample_bytree=0.9,
        nthread=2,
        seed=17,
    )
    fit = xgb.train(
        params,
        dtrain,
        num_boost_round=300,
        evals=[(dvalid, "validation")],
        early_stopping_rounds=20,
        verbose_eval=False,
    )
    vp       = fit.predict(dvalid)
    vll      = log_loss(y_valid, vp)
    best_iter = fit.best_iteration
    xgb_results.append({
        "max_depth": max_depth, "eta": eta,
        "min_child_weight": min_child_weight,
        "best_iteration": best_iter,
        "valid_log_loss": vll,
        "fit": fit,
    })
    print(f"  depth={max_depth} eta={eta} mcw={min_child_weight} → iter={best_iter} vll={vll:.4f}")

best_xgb = min(xgb_results, key=lambda r: r["valid_log_loss"])
print(f"\nBest XGB: depth={best_xgb['max_depth']}, eta={best_xgb['eta']}, "
      f"mcw={best_xgb['min_child_weight']}, iter={best_xgb['best_iteration']}, "
      f"valid_ll={best_xgb['valid_log_loss']:.4f}")

final_xgb_params = dict(
    objective="binary:logistic",
    eval_metric="logloss",
    max_depth=best_xgb["max_depth"],
    eta=best_xgb["eta"],
    min_child_weight=best_xgb["min_child_weight"],
    subsample=0.9,
    colsample_bytree=0.9,
    nthread=2,
    seed=17,
)
final_xgb = xgb.train(
    final_xgb_params,
    dtv,
    num_boost_round=best_xgb["best_iteration"],
    verbose_eval=False,
)
xgb_test_pred = final_xgb.predict(dtest)
r_xgb_valid   = evaluate_predictions("XGBoost", "validation", y_valid,
                                      best_xgb["fit"].predict(dvalid))
r_xgb_test    = evaluate_predictions("XGBoost", "test", y_test, xgb_test_pred)
print(f"XGBoost test_ll={r_xgb_test['log_loss']:.4f}")

# ─────────────────────────────────────────
# TASK 3: Model comparison table + bar plot
# ─────────────────────────────────────────

comparison = pd.DataFrame([
    {"model": "Training mean",
     "selected_tuning": "none",
     "validation_log_loss": r_mean_valid["log_loss"],
     "test_log_loss": r_mean_test["log_loss"]},
    {"model": "Formation rate",
     "selected_tuning": "formation + alignment",
     "validation_log_loss": r_form_valid["log_loss"],
     "test_log_loss": r_form_test["log_loss"]},
    {"model": "Logistic regression",
     "selected_tuning": "none",
     "validation_log_loss": r_lr_valid["log_loss"],
     "test_log_loss": r_lr_test["log_loss"]},
    {"model": "Feedforward NN",
     "selected_tuning": f"size={best_nnet['size']}, decay={best_nnet['decay']}",
     "validation_log_loss": r_nnet_valid["log_loss"],
     "test_log_loss": r_nnet_test["log_loss"]},
    {"model": "XGBoost",
     "selected_tuning": (f"depth={best_xgb['max_depth']}, eta={best_xgb['eta']}, "
                         f"mcw={best_xgb['min_child_weight']}, rounds={best_xgb['best_iteration']}"),
     "validation_log_loss": r_xgb_valid["log_loss"],
     "test_log_loss": r_xgb_test["log_loss"]},
])

print("\n=== Final model comparison ===")
print(comparison[["model", "selected_tuning", "validation_log_loss", "test_log_loss"]].to_string(index=False))

fig, ax = plt.subplots(figsize=(9, 4))
comp_sorted = comparison.sort_values("test_log_loss", ascending=False)
bars = ax.barh(comp_sorted["model"], comp_sorted["test_log_loss"], height=0.55, color="steelblue")
for bar, val in zip(bars, comp_sorted["test_log_loss"]):
    ax.text(val + 0.001, bar.get_y() + bar.get_height() / 2,
            f"{val:.4f}", va="center", fontsize=9)
ax.set_xlabel("Test log loss")
ax.set_title("Test log loss by model")
ax.set_xlim(0, comp_sorted["test_log_loss"].max() * 1.1)
plt.tight_layout()
plt.savefig("task3_model_comparison.png", dpi=150)
plt.close()
print("Saved: task3_model_comparison.png")

# ─────────────────────────────────────────
# TASK 4: Field tensor / CNN channels
# ─────────────────────────────────────────

def make_field_grid(play_data, cell_size=2):
    df = play_data[play_data["side"].isin(["offense", "defense"])].copy()
    df["x_cell"]  = np.floor(df["xStd"] / cell_size) * cell_size + cell_size / 2
    df["y_cell"]  = np.floor(df["yStd"] / cell_size) * cell_size + cell_size / 2
    df["channel"] = df["side"].map({"offense": "Offense", "defense": "Defense"})
    return (
        df.groupby(["playActionPass", "gameId", "playId", "channel", "x_cell", "y_cell"])
          .size().reset_index(name="value")
    )

for label, label_name in [(1, "play_action"), (0, "run")]:
    key = (snap_examples[snap_examples["playActionPass"] == label]
           [["gameId", "playId", "playActionPass"]]
           .drop_duplicates().iloc[0])
    example_play = snap_examples[
        (snap_examples["gameId"] == key["gameId"]) &
        (snap_examples["playId"] == key["playId"])
    ]
    example_grid = make_field_grid(example_play, cell_size=2)

    fig, axes = plt.subplots(1, 2, figsize=(14, 4), sharey=True)
    for ax, channel in zip(axes, ["Offense", "Defense"]):
        ch = example_grid[example_grid["channel"] == channel]
        sc = ax.scatter(ch["x_cell"], ch["y_cell"], c=ch["value"],
                        s=200, marker="s", cmap="Blues",
                        vmin=0, vmax=ch["value"].max() or 1)
        ax.set_xlim(0, 120)
        ax.set_ylim(0, 160 / 3)
        ax.set_title(f"{channel} channel")
        ax.set_xlabel("Standardized x")
        ax.set_ylabel("Standardized y")
        plt.colorbar(sc, ax=ax, label="Players")

    fig.suptitle(f"{'Play-action pass' if label else 'Designed run'} — snap frame channels",
                 fontsize=12, fontweight="bold")
    plt.tight_layout()
    out_path = f"task4_{label_name}_channels.png"
    plt.savefig(out_path, dpi=150)
    plt.close()
    print(f"Saved: {out_path}")

print("\nAll done.")