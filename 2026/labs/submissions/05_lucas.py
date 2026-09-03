#############
### SETUP ###
#############

# Python equivalents of the R packages used in the original script
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import SplineTransformer, OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.metrics import mean_squared_error


# set seed
RANDOM_STATE = 6
np.random.seed(RANDOM_STATE)

#######################
### EXPECTED POINTS ###
#######################

# load data
nfl_data = pd.read_csv("../data/05_expected-points.csv.gz")

#Task 1

score_values = [-7, -3, -2, 0, 2, 3, 7]

X = nfl_data[["yardline_100"]]
y = nfl_data["pts_next_score"]

model1 = LogisticRegression(solver='lbfgs', max_iter=1000, random_state=6)
model1.fit(X, y)

yard_lines = np.arange(1, 100).reshape(-1, 1)
probs = model1.predict_proba(yard_lines)
classes = model1.classes_ 

ep = probs @ classes

plt.figure(figsize=(9, 5))
plt.plot(yard_lines, ep, color='steelblue', linewidth=2)
plt.axhline(0, color='gray', linestyle='--', linewidth=0.8)
plt.xlabel("Yard Line (distance to opponent's end zone)")
plt.ylabel("Expected Points")
plt.title("EP vs Yard Line (Linear)")
plt.tight_layout()
plt.show()

print("The graph appears to be not steep enough at ends and too linear-like (only expected 4 points if you are at your opponents 1 yard line)")


spline_pipe = Pipeline([
    ("spline", SplineTransformer(n_knots=6, degree=3, include_bias=False)),
    ("logistic", LogisticRegression(solver='lbfgs', max_iter=2000, random_state=6))
])

X = nfl_data[["yardline_100"]]
y = nfl_data["pts_next_score"]

spline_pipe.fit(X, y)

yard_lines = np.arange(1, 100).reshape(-1, 1)
probs = spline_pipe.predict_proba(yard_lines)
classes = spline_pipe.classes_
ep_spline = probs @ classes

plt.figure(figsize=(9, 5))
plt.plot(yard_lines, ep_spline, color='steelblue', linewidth=2)
plt.axhline(0, color='gray', linestyle='--', linewidth=0.8)
plt.xlabel("Yard Line (distance to opponent's end zone)")
plt.ylabel("Expected Points")
plt.title("EP vs Yard Line (Spline)")
plt.tight_layout()
plt.show()

print("\nThis is an improvement because it captures the non constant patterns that emerge at different yard line phases --> much more likely to score if we move from the 5 to the 1 but not much of a change from the 50 to 45")

preprocessor = ColumnTransformer([
    ("spline", SplineTransformer(n_knots=6, degree=3, include_bias=False), ["yardline_100"]),
    ("down_onehot", OneHotEncoder(drop="first", sparse_output=False), ["down"])
])

model3 = Pipeline([
    ("preprocessor", preprocessor),
    ("logistic", LogisticRegression(solver='lbfgs', max_iter=2000, random_state=6))
])

X3 = nfl_data[["yardline_100", "down"]]
y = nfl_data["pts_next_score"]

model3.fit(X3, y)

# Predict EP for each down across all yard lines
yard_lines = np.arange(1, 100)
downs = [1, 2, 3, 4]
colors = ['steelblue', 'darkorange', 'green', 'red']

plt.figure(figsize=(9, 5))

for down, color in zip(downs, colors):
    # Build prediction dataframe: fix down, vary yard line
    pred_df = pd.DataFrame({
        "yardline_100": yard_lines,
        "down": down
    })
    probs = model3.predict_proba(pred_df)
    classes = model3.classes_
    ep = probs @ classes
    plt.plot(yard_lines, ep, color=color, linewidth=2, label=f"Down {down}")

plt.axhline(0, color='gray', linestyle='--', linewidth=0.8)
plt.xlabel("Yard Line (distance to opponent's end zone)")
plt.ylabel("Expected Points")
plt.title("EP vs Yard Line by Down")
plt.legend(title="Down")
plt.tight_layout()
plt.show()

print("""\nWe choose categorical encoding for the model because the downs reflect a change in approach 
      (e.g. first down is usually open while fourth down is almost always a punt)""")

nfl_data["log_ydstogo"] = np.log(nfl_data["ydstogo"])

preprocessor4 = ColumnTransformer([
    ("spline", SplineTransformer(n_knots=6, degree=3, include_bias=False), ["yardline_100"]),
    ("down_onehot", OneHotEncoder(drop="first", sparse_output=False), ["down"]),
    ("log_ydstogo", "passthrough", ["log_ydstogo"])  # already transformed
])

model4 = Pipeline([
    ("preprocessor", preprocessor4),
    ("logistic", LogisticRegression(solver='lbfgs', max_iter=2000, random_state=6))
])

X4 = nfl_data[["yardline_100", "down", "log_ydstogo"]]
model4.fit(X4, y)

# Plot: facet by down, color by ydstogo
ydstogo_vals = [1, 3, 5, 10, 20]
yard_lines = np.arange(1, 100)
fig, axes = plt.subplots(1, 4, figsize=(16, 5), sharey=True)

cmap = plt.cm.viridis
colors = [cmap(i / len(ydstogo_vals)) for i in range(len(ydstogo_vals))]

for ax, down in zip(axes, [1, 2, 3, 4]):
    for ytg, color in zip(ydstogo_vals, colors):
        pred_df = pd.DataFrame({
            "yardline_100": yard_lines,
            "down": down,
            "log_ydstogo": np.log(ytg)
        })
        probs = model4.predict_proba(pred_df)
        ep = probs @ model4.classes_
        ax.plot(yard_lines, ep, color=color, linewidth=2, label=f"YTG={ytg}")

    ax.axhline(0, color='gray', linestyle='--', linewidth=0.8)
    ax.set_title(f"Down {down}")
    ax.set_xlabel("Yard Line")

axes[0].set_ylabel("Expected Points")
axes[3].legend(title="Yards to Go", bbox_to_anchor=(1.05, 1), loc='upper left')
plt.suptitle("EP vs Yard Line by Down and Yards to Go")
plt.tight_layout()
plt.show()

print("\nAs the down increases, the yards-to-go impact on the EP decreases --> the curves get closer together")


X5 = nfl_data[["yardline_100", "down", "log_ydstogo", "half_seconds_remaining"]]

# --- Model 5a: linear time ---
preprocessor5a = ColumnTransformer([
    ("spline", SplineTransformer(n_knots=6, degree=3, include_bias=False), ["yardline_100"]),
    ("down_onehot", OneHotEncoder(drop="first", sparse_output=False), ["down"]),
    ("passthrough", "passthrough", ["log_ydstogo", "half_seconds_remaining"])
])

model5a = Pipeline([
    ("preprocessor", preprocessor5a),
    ("logistic", LogisticRegression(solver='lbfgs', max_iter=2000, random_state=6))
])

model5a.fit(X5, y)

# --- Model 5b: spline time ---
preprocessor5b = ColumnTransformer([
    ("spline_yard", SplineTransformer(n_knots=6, degree=3, include_bias=False), ["yardline_100"]),
    ("down_onehot", OneHotEncoder(drop="first", sparse_output=False), ["down"]),
    ("passthrough", "passthrough", ["log_ydstogo"]),
    ("spline_time", SplineTransformer(n_knots=6, degree=3, include_bias=False), ["half_seconds_remaining"])
])

model5b = Pipeline([
    ("preprocessor", preprocessor5b),
    ("logistic", LogisticRegression(solver='lbfgs', max_iter=2000, random_state=6))
])

model5b.fit(X5, y)

# --- Plot side by side ---
time_vals = [30, 120, 500, 900]
yard_lines = np.arange(1, 100)
cmap = plt.cm.plasma
colors = [cmap(i / len(time_vals)) for i in range(len(time_vals))]

fig, axes = plt.subplots(1, 2, figsize=(16, 5), sharey=True)

for ax, model, title in zip(axes, [model5a, model5b],
                             ["Linear Time", "Spline Time"]):
    for t, color in zip(time_vals, colors):
        pred_df = pd.DataFrame({
            "yardline_100": yard_lines,
            "down": 1,
            "log_ydstogo": np.log(10),
            "half_seconds_remaining": t
        })
        probs = model.predict_proba(pred_df)
        ep = probs @ model.classes_
        ax.plot(yard_lines, ep, color=color, linewidth=2, label=f"{t}s remaining")

    ax.axhline(0, color='gray', linestyle='--', linewidth=0.8)
    ax.set_xlabel("Yard Line")
    ax.set_ylabel("Expected Points")
    ax.set_title(f"EP vs Yard Line, {title} (1st & 10)")
    ax.legend(title="Time Remaining")

plt.suptitle("Task 1.5 — Linear vs Spline Time")
plt.tight_layout()
plt.show()

for model, name in zip([model5a, model5b], ["Linear Time", "Spline Time"]):
    preds = model.predict_proba(X5)
    ep_pred = preds @ model.classes_
    rmse = np.sqrt(mean_squared_error(y, ep_pred))
    print(f"{name} RMSE: {rmse:.4f}")
    
    
print("\nSpline time has lower RMSE; we will thus choose this model for task 2")

#Task 2


X_prime = nfl_data[["yardline_100", "down", "log_ydstogo", 
                     "half_seconds_remaining", "posteam_spread"]]

preprocessor_prime = ColumnTransformer([
    ("spline_yard", SplineTransformer(n_knots=6, degree=3, include_bias=False), ["yardline_100"]),
    ("down_onehot", OneHotEncoder(drop="first", sparse_output=False), ["down"]),
    ("passthrough", "passthrough", ["log_ydstogo", "posteam_spread"]),
    ("spline_time", SplineTransformer(n_knots=6, degree=3, include_bias=False), ["half_seconds_remaining"])
])

model_prime = Pipeline([
    ("preprocessor", preprocessor_prime),
    ("logistic", LogisticRegression(solver='lbfgs', max_iter=2000, random_state=6))
])

model_prime.fit(X_prime, y)

yard_lines = np.arange(1, 100)

pred_M = pd.DataFrame({
    "yardline_100": yard_lines,
    "down": 1,
    "log_ydstogo": np.log(10),
    "half_seconds_remaining": 900
})
ep_M = model5b.predict_proba(pred_M) @ model5b.classes_

pred_Mprime = pd.DataFrame({
    "yardline_100": yard_lines,
    "down": 1,
    "log_ydstogo": np.log(10),
    "half_seconds_remaining": 900,
    "posteam_spread": 0
})
ep_Mprime = model_prime.predict_proba(pred_Mprime) @ model_prime.classes_

pred_favored = pd.DataFrame({
    "yardline_100": yard_lines,
    "down": 1,
    "log_ydstogo": np.log(10),
    "half_seconds_remaining": 900,
    "posteam_spread": -7  # negative = favored
})
ep_favored = model_prime.predict_proba(pred_favored) @ model_prime.classes_

plt.figure(figsize=(9, 5))
plt.plot(yard_lines, ep_M, color='steelblue', linewidth=2, label="M (no spread)")
plt.plot(yard_lines, ep_Mprime, color='darkorange', linewidth=2, linestyle='--', label="M' (spread=0)")
plt.plot(yard_lines, ep_favored, color='green', linewidth=2, linestyle='--', label="M' (favored by 7)")
plt.axhline(0, color='gray', linestyle='--', linewidth=0.8)
plt.xlabel("Yard Line")
plt.ylabel("Expected Points")
plt.title("Task 2 — M vs M' at Spread=0 and Spread=-7")
plt.legend()
plt.tight_layout()
plt.show()

fig, axes = plt.subplots(1, 2, figsize=(16, 5))

axes[0].plot(yard_lines, ep_M, color='steelblue', linewidth=2, label="M (no spread)")
axes[0].plot(yard_lines, ep_Mprime, color='darkorange', linewidth=2, linestyle='--', label="M' (spread=0)")
axes[0].axhline(0, color='gray', linestyle='--', linewidth=0.8)
axes[0].set_xlabel("Yard Line")
axes[0].set_ylabel("Expected Points")
axes[0].set_title("Task 2 — Overlay of M and M' at Spread=0")
axes[0].legend()

axes[1].plot(yard_lines, ep_Mprime - ep_M, color='purple', linewidth=2)
axes[1].axhline(0, color='gray', linestyle='--', linewidth=0.8)
axes[1].set_xlabel("Yard Line")
axes[1].set_ylabel("EP Difference (M' - M)")
axes[1].set_title("Task 2 — Difference between M' and M")

plt.suptitle("Task 2 — Effect of Adjusting for Team Quality")
plt.tight_layout()
plt.show()

for model, name, X in zip([model5b, model_prime], ["M", "M'"], [X5, X_prime]):
    preds = model.predict_proba(X)
    ep_pred = preds @ model.classes_
    rmse = np.sqrt(mean_squared_error(y, ep_pred))
    print(f"{name} RMSE: {rmse:.4f}")
    
    
    
print("M' has a lower RMSE; when overlayed at spread = 0, there is very marginal differences between M and M'")
print("this makes sense because 0 point spread means average teams")

#Discussion 
print("The percentage of all 3 pt attempts made in the NBA should be higher because it will likely be inflated by better shooters taking more shots")

