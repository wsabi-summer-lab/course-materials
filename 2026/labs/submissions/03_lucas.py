#############
### SETUP ###
#############


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LinearRegression, LogisticRegression
import statsmodels.api as sm
import statsmodels.formula.api as smf
from sklearn.model_selection import train_test_split
from sklearn.metrics import log_loss
import numpy as np


# reproducible seed for any random operations
RANDOM_STATE = 4
np.random.seed(RANDOM_STATE)


##############
### PART 1 ###
##############

# load data
field_goals = pd.read_csv("../data/03_field-goals.csv.gz")

# Task 1:
# - Inspect the field-goal dataset
# - Compute basic summaries for the response and explanatory variables
# - Make a plot of field-goal outcome against yardline
# - Describe how make probability appears to change with distance


fig, ax = plt.subplots(figsize=(12, 4))

jitter = np.random.uniform(-0.05, 0.05, size=len(field_goals))

ax.scatter(
    field_goals["ydl"],
    field_goals["fg_made"] + jitter,
    c=field_goals["fg_made"].map({1: "green", 0: "red"}),
    alpha=0.3,
    edgecolors="none",
    s=10,
)

ax.set_xlabel("Yardline (yards from opponent's end zone)")
ax.set_ylabel("Outcome")
ax.set_yticks([0, 1])
ax.set_yticklabels(["Miss (0)", "Make (1)"])
ax.set_title("Field Goal Outcome vs. Yardline")
plt.tight_layout()
plt.show()

print("The plot shows that the probability of making a field goal is extremely high when the yardline is close to the opponent's end zone and decreases as the yardline increases.")

# Task 2:
# - Fit at least 3 competing models for field-goal success probability
# - Include at least one linear regression and at least one logistic regression
# - Consider whether kicker quality should enter the model
# - Write down each model clearly

# Model 1: Linear Regression (OLS)
model_1 = smf.ols("fg_made ~ ydl", data=field_goals).fit()
print(model_1.summary())

# Model 2: Logistic Regression
model_2 = smf.logit("fg_made ~ ydl", data=field_goals).fit()
print(model_2.summary())

# Model 3: Logistic Regression with log(ydl) and kicker quality
field_goals["log_ydl"] = np.log(field_goals["ydl"])
model_3 = smf.logit("fg_made ~ log_ydl + kq", data=field_goals).fit()
print(model_3.summary())


# Task 3:
# - Compare the models using out-of-sample predictive performance
# - Use log loss as the main metric
# - If using cross-validation, report mean test log loss and its standard error across folds
# - Select a preferred model and explain why


train, test = train_test_split(field_goals, test_size=0.2, random_state=RANDOM_STATE)

train["log_ydl"] = np.log(train["ydl"])
test["log_ydl"] = np.log(test["ydl"])

model_1 = smf.ols("fg_made ~ ydl", data=train).fit()
model_2 = smf.logit("fg_made ~ ydl", data=train).fit()
model_3 = smf.logit("fg_made ~ log_ydl + kq", data=train).fit()

pred_1 = model_1.predict(test).clip(1e-6, 1) 
pred_2 = model_2.predict(test)
pred_3 = model_3.predict(test)


ll_1 = log_loss(test["fg_made"], pred_1)
ll_2 = log_loss(test["fg_made"], pred_2)
ll_3 = log_loss(test["fg_made"], pred_3)

print(f"Model 1 (Linear):        Log Loss = {ll_1:.4f}")
print(f"Model 2 (Logistic):      Log Loss = {ll_2:.4f}")
print(f"Model 3 (Logit+log+kq):  Log Loss = {ll_3:.4f}")

print("\nModel 2 has the lowest log loss, indicating it has the best out-of-sample predictive performance among the three models.")

# Task 4:
# - Report coefficient estimates, standard errors, and 95% confidence intervals for the selected logistic model
# - Interpret the selected model's coefficients on the log-odds scale
# - When useful, exponentiate coefficients and interpret them as odds ratios

summary_df = pd.DataFrame({
    "coef":     model_2.params,
    "se":       model_2.bse,
    "ci_lower": model_2.conf_int()[0],
    "ci_upper": model_2.conf_int()[1],
})

summary_df["odds_ratio"]   = np.exp(summary_df["coef"])
summary_df["or_ci_lower"]  = np.exp(summary_df["ci_lower"])
summary_df["or_ci_upper"]  = np.exp(summary_df["ci_upper"])

print(summary_df.round(4))

print("""After exponentiating, we get odds_ratio of 0.9008 meaning that every yard back gives us an expected decrease in odds by ~10%
      """)


# Task 5:
# - Plot the selected model's predicted make probability as a function of yardline
# - Add a 95% confidence ribbon for the fitted probability
# - Bin the data by yardline and compare fitted probabilities to observed make rates
# - Comment on where the model fits well and where it misses

# 1. Predict probability for every observation
field_goals["pred_prob"] = model_2.predict(field_goals)

# 2. Bin by yardline (2-yard bins) for observed rates only
field_goals["ydl_bin"] = pd.cut(field_goals["ydl"], bins=np.arange(0, 75, 2))
binned = (
    field_goals.groupby("ydl_bin", observed=True)
    .agg(
        obs_rate=("fg_made", "mean"),
        ydl_mid=("ydl", "mean"),
    )
    .reset_index()
)

# 3. Smooth sigmoid line over grid
ydl_grid = pd.DataFrame({"ydl": np.linspace(field_goals["ydl"].min(), field_goals["ydl"].max(), 300)})
ydl_grid["pred_prob"] = model_2.predict(ydl_grid)

# 4. Plot
fig, ax = plt.subplots(figsize=(11, 5))

# Binned observed rates
ax.scatter(binned["ydl_mid"], binned["obs_rate"],
           s=12, color="coral", label="Observed rate (binned)", zorder=5)

# Sigmoid fitted line
ax.plot(ydl_grid["ydl"], ydl_grid["pred_prob"],
        color="steelblue", linewidth=2, label="Fitted probability (sigmoid)")

ax.set_xlabel("Yardline (yards from opponent's end zone)")
ax.set_ylabel("P(Field Goal Made)")
ax.set_title("Model 2: Fitted Sigmoid vs Observed Make Rate")
ax.legend()
plt.tight_layout()
plt.show()

print("The model fits very well for the yardlines that are closer to the endzone but over predicts the make probability for longer field goals (45+ from opponent endzone)")
##############
### PART 2 ###
##############

# load data
ncaab_results = pd.read_csv("../data/03_ncaab-results.csv.gz")
ncaab_team_info = pd.read_csv("../data/03_ncaab-teams.csv.gz")

# Task 1:
# - Filter the NCAA results to the 2023-2024 season
# - Recode the data into a Bradley-Terry model dataset
# - Make sure you can identify the home team, away team, and binary game outcome
# - State the identifiability convention you will use for team ratings

season_2024 = ncaab_results[ncaab_results["Season"] == 2024].copy()

def recode_game(row):
    if row["WLoc"] == "H":
        home, away, result = row["WTeamID"], row["LTeamID"], 1
    elif row["WLoc"] == "A":
        home, away, result = row["LTeamID"], row["WTeamID"], 0
    else:  
        home, away, result = row["WTeamID"], row["LTeamID"], 1
    return pd.Series({"home_team": home, "away_team": away,
                      "home_win": result, "neutral": row["WLoc"] == "N"})

bt_data = season_2024.apply(recode_game, axis=1).reset_index(drop=True)

all_teams   = sorted(set(bt_data["home_team"]).union(set(bt_data["away_team"])))
non_ref     = all_teams[1:]  
team_to_col = {t: i for i, t in enumerate(non_ref)}

n_games = len(bt_data)
n_teams = len(non_ref)

Y = bt_data["home_win"].values

X = np.zeros((n_games, 1 + n_teams))

for i, row in bt_data.iterrows():
    X[i, 0] = 0 if row["neutral"] else 1
    if row["home_team"] in team_to_col:
        X[i, 1 + team_to_col[row["home_team"]]] =  1
    if row["away_team"] in team_to_col:
        X[i, 1 + team_to_col[row["away_team"]]] = -1

print(f"Y shape: {Y.shape}")
print(f"X shape: {X.shape}")
print(f"Games: {n_games}, Teams: {n_teams}")

print("\nWe will center the team ratings by subtracing the mean rating across all teams; >0 means above average and <0 means below average.")

# Task 2:
# - Fit a Bradley-Terry logistic regression model
# - Include a home-court advantage term
# - Join team names back onto the fitted coefficients so the ratings are interpretable
# - Explain what a larger team rating means

bt_model = sm.Logit(Y, X).fit(maxiter=200)

beta_0 = bt_model.params[0] 
betas  = bt_model.params[1:] 

# Center by subtracting mean of ALL teams including reference (fixed at 0)
all_ratings  = np.append(betas, 0)
mean_rating  = all_ratings.mean()

ratings = pd.DataFrame({
    "TeamID": non_ref,
    "rating": betas - mean_rating
})

ratings = pd.concat([
    ratings,
    pd.DataFrame([{"TeamID": all_teams[0], "rating": 0 - mean_rating}])
])

ratings = (ratings
           .merge(ncaab_team_info[["TeamID", "TeamName"]], on="TeamID", how="left")
           .sort_values("rating", ascending=False)
           .reset_index(drop=True))

print(f"Home court advantage (beta_0): {beta_0:.4f}")
print(f"\nTop 10:\n{ratings.head(10)}")
print(f"\nBottom 10:\n{ratings.tail(10)}")

# Task 3:
# - Visualize the fitted team ratings
# - Add uncertainty intervals for the ratings or for rating differences
# - Explain why rating differences are often more meaningful than raw levels
# - Identify the strongest teams under your fitted model

se_lookup = {team_id: bt_model.bse[1:][i] for i, team_id in enumerate(non_ref)}
se_lookup[all_teams[0]] = 0.0
ratings["se"]       = ratings["TeamID"].map(se_lookup)
ratings["ci_lower"] = ratings["rating"] - 1.96 * ratings["se"]
ratings["ci_upper"] = ratings["rating"] + 1.96 * ratings["se"]

# --- Task 3: Plot ---
fig, ax = plt.subplots(figsize=(200, 6))  # very wide

ax.bar(range(len(ratings)), ratings["rating"], color="steelblue", alpha=0.7, width=0.6)
ax.errorbar(
    range(len(ratings)), ratings["rating"],
    yerr=1.96 * ratings["se"],
    fmt="none", color="black", capsize=2, linewidth=0.8
)

ax.axhline(0, color="red", linestyle="--", linewidth=1, label="Average team (0)")
ax.set_ylabel("Team Rating (centered, log-odds scale)")
ax.set_title("Bradley-Terry Team Ratings — 2023-24 Season (scroll to explore)")
ax.set_xticks(range(len(ratings)))
ax.set_xticklabels(ratings["TeamName"], rotation=90, fontsize=8)
ax.legend()

# Show only 30 teams at a time initially
ax.set_xlim(-1, 30)

plt.tight_layout()

def on_scroll(event):
    ax = event.inaxes
    if ax is None:
        return
    x_min, x_max = ax.get_xlim()
    width = x_max - x_min
    shift = width * 0.2 * (-1 if event.button == "up" else 1)
    ax.set_xlim(x_min + shift, x_max + shift)
    fig.canvas.draw_idle()

fig.canvas.mpl_connect("scroll_event", on_scroll)
plt.show()

print("Rating differences are useful because they capture team strengths relative to each other, so we can take their differences to project when they play H2H.")
print("After adding 95% CI, we still have Connecticut, Houston, Purdue as the highest rated teams with similar size error bars")


# Task 4:
# - Choose one or more team comparisons and compute win probabilities from the fitted model
# - For at least one matchup, quantify uncertainty in the predicted probability
# - Make sure your probability calculation matches your identifiability convention


cov_matrix = bt_model.cov_params() 

def win_prob_delta(team_j, team_k, neutral=False):
    a = np.zeros(1 + len(non_ref))
    a[0] = 0 if neutral else 1 

    j_idx = non_ref.index(ratings[ratings["TeamName"] == team_j]["TeamID"].values[0])
    k_idx = non_ref.index(ratings[ratings["TeamName"] == team_k]["TeamID"].values[0])
    a[1 + j_idx] =  1
    a[1 + k_idx] = -1

    logit = a @ bt_model.params

    se = np.sqrt(a @ cov_matrix @ a)

    prob     = 1 / (1 + np.exp(-logit))
    ci_lower = 1 / (1 + np.exp(-(logit - 1.96 * se)))
    ci_upper = 1 / (1 + np.exp(-(logit + 1.96 * se)))

    return prob, ci_lower, ci_upper, se

p, lo, hi, se = win_prob_delta("Connecticut", "Purdue", neutral=True)
print(f"UConn vs Purdue (neutral site):")
print(f"  P(UConn wins) = {p:.4f}")
print(f"  95% CI: ({lo:.4f}, {hi:.4f})")

print("This interval represents how confident we are that the true expected probability of UConn winning the game is p. Obviously, the true one game outcome is 0 or 1, and these do not fall within the bounds")

# Task 5:
# - For the Purdue vs UConn national-title game, set beta_0 = 0 for a neutral site
# - Report the estimated win probability for each team
# - Compute an approximate 95% confidence interval for the win probability
# - Convert the point estimate and both confidence-interval endpoints into moneyline prices
# - Briefly explain that this interval reflects uncertainty in the fitted probability, not certainty about one game outcome


def to_moneyline(p):
    if p >= 0.5:
        return -100 * (p / (1 - p))
    else:
        return 100 * ((1 - p) / p)

# UConn win probability (neutral site, beta_0 = 0)
print("=== Purdue vs UConn — 2024 NCAA Title Game (Neutral Site) ===\n")

print(f"UConn:  P(win) = {p:.4f},  Moneyline = {to_moneyline(p):.1f}")
print(f"Purdue: P(win) = {1-p:.4f},  Moneyline = {to_moneyline(1-p):.1f}")

# End of file — this is a Python skeleton matching the original R script structure.
