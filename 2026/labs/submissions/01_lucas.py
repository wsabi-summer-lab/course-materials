#############
### SETUP ###
#############

# pip install pandas numpy statsmodels matplotlib seaborn

import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns
import matplotlib.patches as mpatches


##############
### PART 1 ###
##############

# load data
mlb_team_seasons = pd.read_csv("../data/01_mlb-team-seasons.csv.gz")

# transformed regression data for estimating alpha
pythag_data = mlb_team_seasons.copy()

pythag_data["logit_wp"] = np.log(
    pythag_data["WP"] / (1 - pythag_data["WP"])
)

pythag_data["log_rs_ra"] = np.log(
    pythag_data["RS"] / pythag_data["RA"]
)

# no-intercept model:
# log(WP / (1 - WP)) = alpha * log(RS / RA)
X_alpha = pythag_data["log_rs_ra"]
y_alpha = pythag_data["logit_wp"]

alpha_model = sm.OLS(y_alpha, X_alpha).fit()
print(alpha_model.summary())

# alpha estimate + uncertainty
alpha_hat = alpha_model.params["log_rs_ra"]
alpha_se = alpha_model.bse["log_rs_ra"]
alpha_ci_95 = alpha_model.conf_int(alpha=0.05).loc["log_rs_ra"]


# helper for converting exponent alpha back to predicted WP
def pythag_wp(rs, ra, alpha):
    return rs**alpha / (rs**alpha + ra**alpha)

# compare fitted-alpha model vs Bill James alpha = 2
pythag_data["wp_hat_alpha"] = pythag_wp(
    pythag_data["RS"],
    pythag_data["RA"],
    alpha_hat
)

pythag_data["wp_hat_bj"] = pythag_wp(
    pythag_data["RS"],
    pythag_data["RA"],
    2
)

pythag_data["resid_alpha"] = (
    pythag_data["WP"] - pythag_data["wp_hat_alpha"]
)

pythag_data["resid_bj"] = (
    pythag_data["WP"] - pythag_data["wp_hat_bj"]
)

def rmse(y, yhat):
    return np.sqrt(np.mean((y - yhat) ** 2))

rmse_alpha = rmse(pythag_data["WP"], pythag_data["wp_hat_alpha"])
rmse_bj = rmse(pythag_data["WP"], pythag_data["wp_hat_bj"])

print("RMSE for fitted alpha model:", rmse_alpha)
print("RMSE for Bill James alpha=2 model:", rmse_bj)
print("RMSE for fitted alpha model with alpha_hat = ", alpha_hat, " is lower than RMSE for Bill James alpha=2")


# Plot: predicted vs observed (cluster/scatter) for both models side-by-side
# We'll plot predicted WP on the x-axis and observed WP on the y-axis for
# the fitted-alpha model and the Bill James alpha=2 model. Add a y = x
# reference line (45-degree, slope=1) on each plot and show RMSE in the title.
fig, axes = plt.subplots(1, 2, figsize=(12, 6), sharex=True, sharey=True)

# choose a hue column if available (e.g., yearID) to show clusters; otherwise no hue

hue_col = "yearID" if "yearID" in pythag_data.columns else None

# compute axis limits from the data (observed and both predicted series) with small padding
all_vals = pd.concat([
    pythag_data["WP"],
    pythag_data["wp_hat_alpha"],
    pythag_data["wp_hat_bj"]
])
vmin = all_vals.min()
vmax = all_vals.max()
# padding: 2% of range or at least 0.01
pad = max((vmax - vmin) * 0.02, 0.01)
lims = (max(0.0, vmin - pad), min(1.0, vmax + pad))

# Left: fitted-alpha model
ax = axes[0]
if hue_col:
    sns.scatterplot(x="wp_hat_alpha", y="WP", hue=hue_col, data=pythag_data, ax=ax, alpha=0.6, linewidth=0)
else:
    sns.scatterplot(x=pythag_data["wp_hat_alpha"], y=pythag_data["WP"], ax=ax, alpha=0.6, linewidth=0)

ax.plot(lims, lims, color="red", linestyle="--", linewidth=1)
ax.set_xlim(lims)
ax.set_ylim(lims)
ax.set_xlabel("Predicted WP (fitted alpha=" + str(alpha_hat) + ")")
ax.set_ylabel("Observed WP")
ax.set_title(f"Fitted alpha model — RMSE = {rmse_alpha:.4f}")
if hue_col:
    ax.legend(title=hue_col, bbox_to_anchor=(1.05, 1), loc="upper left")

# Right: Bill James alpha = 2 model
ax2 = axes[1]
if hue_col:
    sns.scatterplot(x="wp_hat_bj", y="WP", hue=hue_col, data=pythag_data, ax=ax2, alpha=0.6, linewidth=0, legend=False)
else:
    sns.scatterplot(x=pythag_data["wp_hat_bj"], y=pythag_data["WP"], ax=ax2, alpha=0.6, linewidth=0)

ax2.plot(lims, lims, color="red", linestyle="--", linewidth=1)
ax2.set_xlim(lims)
ax2.set_ylim(lims)
ax2.set_xlabel("Predicted WP (Bill James alpha=2)")
ax2.set_title(f"Bill James alpha=2 model — RMSE = {rmse_bj:.4f}")

plt.tight_layout()
plt.show()

print("alpha_hat:", alpha_hat)
print("Standard Error:", alpha_se)
print("95% Confidence Interval for alpha_hat:\n", alpha_ci_95)
print("Since 2 falls outside the 95% confidence interval, it is not a plausible value for the true alpha parameter")

# Residuals vs fitted (fitted alpha) — single diagnostic plot
fig, ax = plt.subplots(figsize=(8, 4))
sns.scatterplot(x=pythag_data["wp_hat_alpha"], y=pythag_data["resid_alpha"], ax=ax, alpha=0.6, linewidth=0)
ax.axhline(0, color="red", linestyle="--", linewidth=1)
ax.set_xlabel("Fitted WP (fitted alpha)")
ax.set_ylabel("Residual (Observed - Predicted)")
ax.set_title("Residuals vs Fitted (fitted alpha)")
plt.tight_layout()
plt.show()

print()
print("We would expect to see that the residuals demonstrate no clear pattern --> should seem randomly distributed")
print()
print(
    """From our residual plot, we don't seem to observe any pattern.
No apparent curvature or outliers tend to be spotted. However,
we could note that the higher positive residuals occur more frequently when the team's
model-predicted win percent is lower. Maybe this could mean that teams with lower predicted
WPs based on RS/RA have a greater potential to outperform their predicted WPs by a larger margin.
"""
)

##############
### PART 2 ###
##############

# load data
mlb_payrolls = pd.read_csv("../data/01_mlb-payrolls.csv.gz")

payroll_data = mlb_payrolls[
    mlb_payrolls["year_id"] != 2020
].copy()

X_a = sm.add_constant(payroll_data["payroll_median_ratio"])
y_a = payroll_data["wp"]

model_a = sm.OLS(y_a, X_a).fit()

# model B: WP ~ log(payroll / median)
X_b = sm.add_constant(payroll_data["log_payroll_median_ratio"])
y_b = payroll_data["wp"]

model_b = sm.OLS(y_b, X_b).fit()

# fitted values + residuals for both models
payroll_data["fitted_a"] = model_a.fittedvalues
payroll_data["resid_a"] = model_a.resid

payroll_data["fitted_b"] = model_b.fittedvalues
payroll_data["resid_b"] = model_b.resid

row_idx = 1
selected_row = payroll_data.iloc[[row_idx]]

selected_X_b = sm.add_constant(
    selected_row[["log_payroll_median_ratio"]],
    has_constant="add"
)

prediction_results_b = model_b.get_prediction(selected_X_b)
prediction_summary_b = prediction_results_b.summary_frame(alpha=0.05)

print(prediction_summary_b)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 7))
fig.suptitle("Part 2", fontsize=16, fontweight='bold')

def plot_regression(ax, x_col, y_col, model, data, xlabel, title):
    # Base scatter — all other teams
    mask_nya = data["team_id"] == "NYA"
    mask_oak = data["team_id"] == "OAK"
    mask_other = ~mask_nya & ~mask_oak

    ax.scatter(data.loc[mask_other, x_col], data.loc[mask_other, y_col],
               color="gray", alpha=0.4, s=30, zorder=2)
    ax.scatter(data.loc[mask_nya, x_col], data.loc[mask_nya, y_col],
               color="blue", alpha=0.8, s=50, zorder=3, label="NY Yankees")
    ax.scatter(data.loc[mask_oak, x_col], data.loc[mask_oak, y_col],
               color="green", alpha=0.8, s=50, zorder=3, label="Oakland Athletics")

    # Regression line
    x_range = np.linspace(data[x_col].min(), data[x_col].max(), 300)
    X_line = sm.add_constant(pd.Series(x_range, name=x_col))
    y_pred = model.predict(X_line)
    ax.plot(x_range, y_pred, color="red", linewidth=2, zorder=4, label="OLS Fit")

    ax.set_xlabel(xlabel, fontsize=12)
    ax.set_ylabel("Win Percentage (WP)", fontsize=12)
    ax.set_title(title, fontsize=13)
    ax.legend(fontsize=10)

plot_regression(
    ax1,
    x_col="payroll_median_ratio",
    y_col="wp",
    model=model_a,
    data=payroll_data,
    xlabel="Payroll / Median Ratio",
    title="Model A: WP ~ Payroll/Median Ratio"
)

plot_regression(
    ax2,
    x_col="log_payroll_median_ratio",
    y_col="wp",
    model=model_b,
    data=payroll_data,
    xlabel="Log(Payroll / Median Ratio)",
    title="Model B: WP ~ Log(Payroll/Median Ratio)"
)

plt.tight_layout()
plt.savefig("part2_plots.png", dpi=150, bbox_inches="tight")
plt.show()


# --- Residual Plots ---
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 7))
fig.suptitle("Task 2: Residual Plots", fontsize=16, fontweight='bold')

ax1.scatter(payroll_data["fitted_a"], payroll_data["resid_a"], 
            color="gray", alpha=0.4, s=30)
ax1.axhline(0, color="red", linewidth=1.5, linestyle="--")
ax1.set_xlabel("Fitted Values", fontsize=12)
ax1.set_ylabel("Residuals", fontsize=12)
ax1.set_title("Model A: Residuals vs Fitted\n(WP ~ Payroll/Median Ratio)", fontsize=13)

ax2.scatter(payroll_data["fitted_b"], payroll_data["resid_b"], 
            color="gray", alpha=0.4, s=30)
ax2.axhline(0, color="red", linewidth=1.5, linestyle="--")
ax2.set_xlabel("Fitted Values", fontsize=12)
ax2.set_ylabel("Residuals", fontsize=12)
ax2.set_title("Model B: Residuals vs Fitted\n(WP ~ Log Payroll/Median Ratio)", fontsize=13)

plt.tight_layout()
plt.show()

print("""
      The residual plot for the non-log model (Model A) seems to have a shape of a funnel,
      where a most of the points are centered at the left side of the graph (lower predicted WP) with a higher spread
      but then funnel towards the right side (higher predicted WP) with a lower spread but with fewer points. However,
      for the log model (Model B), the residuals seem to be more randomly scattered around the horizontal line with fewer
      data points on the left tail with no funnel shape --> horizontal oval shape
      """)


# --- Average Residual Per Team (in wins) ---
payroll_data["resid_a_wins"] = payroll_data["resid_a"] * 162
payroll_data["resid_b_wins"] = payroll_data["resid_b"] * 162

avg_resid = payroll_data.groupby("team_id")[["resid_a_wins", "resid_b_wins"]].mean()

avg_resid_a = avg_resid["resid_a_wins"].sort_values(ascending=False)
avg_resid_b = avg_resid["resid_b_wins"].sort_values(ascending=False)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(28, 7))
fig.suptitle("Task 2: Average Residual per Team (Wins)", fontsize=16, fontweight='bold')

# Model A
colors_a = ["steelblue" if v >= 0 else "tomato" for v in avg_resid_a]
ax1.bar(avg_resid_a.index, avg_resid_a.values, color=colors_a)
ax1.axhline(0, color="black", linewidth=1)
ax1.set_xlabel("Team", fontsize=12)
ax1.set_ylabel("Avg Residual (Wins)", fontsize=12)
ax1.set_title("Model A: WP ~ Payroll/Median Ratio", fontsize=13)
ax1.tick_params(axis='x', rotation=90)
legend_handles = [
    mpatches.Patch(color="steelblue", label="Outperformed expectation"),
    mpatches.Patch(color="tomato", label="Underperformed expectation")
]
ax1.legend(handles=legend_handles, fontsize=10)

# Model B
colors_b = ["steelblue" if v >= 0 else "tomato" for v in avg_resid_b]
ax2.bar(avg_resid_b.index, avg_resid_b.values, color=colors_b)
ax2.axhline(0, color="black", linewidth=1)
ax2.set_xlabel("Team", fontsize=12)
ax2.set_ylabel("Avg Residual (Wins)", fontsize=12)
ax2.set_title("Model B: WP ~ Log Payroll/Median Ratio", fontsize=13)
ax2.tick_params(axis='x', rotation=90)
ax2.legend(handles=legend_handles, fontsize=10)

plt.tight_layout()
plt.show()


# Task 3: LA Dodgers 2019 season
dodgers = payroll_data[(payroll_data["team_id"] == "LAN") & (payroll_data["year_id"] == 2023)]

print("Selected team-season: Dodgers 2023")

# Model A
selected_X_a = sm.add_constant(dodgers[["payroll_median_ratio"]], has_constant="add")
pred_a = model_a.get_prediction(selected_X_a)
summary_a = pred_a.summary_frame(alpha=0.05)

print("=== Model A: WP ~ Payroll/Median Ratio ===")
print(f"Fitted WP:              {summary_a['mean'].values[0]:.4f}")
print(f"95% Confidence Interval: ({summary_a['mean_ci_lower'].values[0]:.4f}, {summary_a['mean_ci_upper'].values[0]:.4f})")
print(f"95% Prediction Interval: ({summary_a['obs_ci_lower'].values[0]:.4f}, {summary_a['obs_ci_upper'].values[0]:.4f})")
print()

# Model B
selected_X_b = sm.add_constant(dodgers[["log_payroll_median_ratio"]], has_constant="add")
pred_b = model_b.get_prediction(selected_X_b)
summary_b = pred_b.summary_frame(alpha=0.05)

print("=== Model B: WP ~ Log(Payroll/Median Ratio) ===")
print(f"Fitted WP:               {summary_b['mean'].values[0]:.4f}")
print(f"95% Confidence Interval: ({summary_b['mean_ci_lower'].values[0]:.4f}, {summary_b['mean_ci_upper'].values[0]:.4f})")
print(f"95% Prediction Interval: ({summary_b['obs_ci_lower'].values[0]:.4f}, {summary_b['obs_ci_upper'].values[0]:.4f})")
print()

print(f"Actual WP: {dodgers['wp'].values[0]:.4f}")

print("""
      Intuitively, model B makes more sense because of diminishing returns --> 
      $10 million will be more impactful for a team with a lower payroll than for a team with a higher payroll. 
      Also, it is much harder to win more games when you are already winning a lot of games (e.g., going from 90 wins to 100 wins is harder than going from 60 wins to 70 wins)
      so it costs more to increase your win percentage when you are already at a higher win percentage.
      """)