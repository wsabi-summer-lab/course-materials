#############
### SETUP ###
#############

# set seed
from statistics import LinearRegression
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
from sklearn.model_selection import train_test_split
from scipy import stats

# set a deterministic seed for reproducibility of any random splits
RANDOM_STATE = 3


##############
### PART 1 ###
##############

# load data (same file the R script referenced)
nba = pd.read_csv("../data/02_nba-four-factors.csv.gz")

nba['eFG']     = (nba['FGM'] + 0.5 * nba['3PM']) / nba['FGA']
nba['opp_eFG'] = (nba['OPP_FGM'] + 0.5 * nba['OPP_3PM']) / nba['OPP_FGA']

nba['x1'] = nba['eFG'] - nba['opp_eFG']
nba['x2'] = nba['OREB%'] + nba['DREB%'] - 100
nba['x3'] = nba['OPP TOV %'] - nba['TOV%']
nba['x4'] = nba['FTA RATE'] - nba['OPP FTA RATE']

nba = nba.dropna(subset=['x1', 'x2', 'x3', 'x4', 'W'])


# Task 1:
# - Compute each variable's mean, standard deviation, minimum, and maximum
# - Plot the marginal distribution of each explanatory variable
# - Make scatterplots of wins against each of the four factors
# - Compute correlations between each pair of explanatory variables
# - Identify which variables look most strongly related to wins before fitting a model

print(nba[['x1', 'x2', 'x3', 'x4']].describe().loc[['mean', 'std', 'min', 'max']].round(3))

fig, axes = plt.subplots(2, 2, figsize=(10, 7))
for ax, col, title in zip(axes.flatten(),
                           ['x1', 'x2', 'x3', 'x4'],
                           ['Shooting adv', 'Rebounding adv',
                            'Turnover adv', 'FT-rate adv']):
    ax.hist(nba[col], bins=25, edgecolor='white', color='steelblue', alpha=0.85)
    ax.axvline(nba[col].mean(), color='crimson', linewidth=1.5, linestyle='--', label='Mean')
    ax.set_title(title)
    ax.legend()
plt.tight_layout()
plt.show()


fig, axes = plt.subplots(2, 2, figsize=(10, 7))
for ax, col, title in zip(axes.flatten(),
                           ['x1', 'x2', 'x3', 'x4'],
                           ['Shooting adv (x1)', 'Rebounding adv (x2)',
                            'Turnover adv (x3)', 'FT-rate adv (x4)']):
    ax.scatter(nba[col], nba['W'], alpha=0.4, s=18, color='steelblue')
    xline = np.linspace(nba[col].min(), nba[col].max(), 100)
    r = nba[[col, 'W']].corr().iloc[0, 1]
    ax.set_xlabel(title)
    ax.set_ylabel('Wins')
    ax.set_title(f'Wins vs {title}  (r = {r:.2f})')
plt.tight_layout()
plt.show()

print("It appears that shooting advantage is the most strongly correlated stat with wins, with rebounding advantage being the least")

# Task 2:
# - Fit the multivariable model: wins ~ x1 + x2 + x3 + x4
# - Write down the fitted regression equation
# - Interpret each coefficient in context
# - Check whether the coefficient signs make sense given the variable definitions
# - Identify which factors look strongest and weakest after adjustment

X_reg = sm.add_constant(nba[['x1', 'x2', 'x3', 'x4']])
model = sm.OLS(nba['W'], X_reg).fit()

print(model.summary())

b0, b1, b2, b3, b4 = model.params

print("\nRegression equation:")
print(f"W = {b0:.3f} + {b1:.3f}*x1 + {b2:.3f}*x2 + {b3:.3f}*x3 + {b4:.3f}*x4")

print(f"  Intercept ({b0:.3f}): predicted wins when all four advantages are zero.")
print(f"""
      b1 = {b1:.3f}: 1-unit increase in shooting advantage -> {b1:.2f} more wins,
      when others held fixed (1% EFG advantage leads to an expectation of 3.7 more wins). 
      Same logic applies to B2, B3, B4.""")
print("""\nSince all four advantages are defined so that higher values are better, it makes sense that all the coefficients are positive. Having an advantage 
      in any of the four factors should be associated with more wins, and the model confirms this intuition. Shooting advantage has the highest t value, so it
      appears to be strongly associated with wins, whereas in comparison FT rate has a lower t stat relative to the rest.""")


# Task 3:
# - Standardize the four predictors
# - Fit the standardized model
# - Rank the factors by absolute standardized coefficient size
# - Compare the original and standardized models for interpretability
# - Compare fitted values from both models and explain why they match or differ

for j in ['x1', 'x2', 'x3', 'x4']:
    mean_j = nba[j].mean()
    std_j  = nba[j].std()
    nba[f'z{j[1]}'] = (nba[j] - mean_j) / std_j
    
Z_reg = sm.add_constant(nba[['z1', 'z2', 'z3', 'z4']])
model_std = sm.OLS(nba['W'], Z_reg).fit()

print(model_std.summary())

a0, a1, a2, a3, a4 = model_std.params
print("\nregression equation for standardized:")
print(f"W = {a0:.3f} + {a1:.3f}*z1 + {a2:.3f}*z2 + {a3:.3f}*z3 + {a4:.3f}*z4")
print("""x1 > x3 > x2 > x4 in terms of standardized coefficient size.
Using standardized coefficients makes it easier to compare since they are now on the same scale.
The fitted values are identical because a linear transformation (standardization) doesn't impact the relationship.""")




# Task 4:
# - Report the residual standard error and interpret it in wins
# - Report coefficient standard errors and 95% confidence intervals
# - Identify which effects are clearly different from zero
# - Choose one team and compute a point prediction, confidence interval, and prediction interval
# - State which interval is wider and why

RSS = (model_std.resid ** 2).sum()
n   = model_std.nobs
p   = len(model_std.params)  
sigma_hat = np.sqrt(RSS / (n - p))

print("=== Residual standard error ===")
print(f"RSS = {RSS:.3f}")
print(f"n = {int(n)}, p = {int(p)}, n-p = {int(n-p)}")
print(f"sigma_hat = {sigma_hat:.3f} wins")
print(f"SD of actual wins from predicted wins is ~{sigma_hat:.1f} wins.")

print("\n95% confidence intervals")
conf_int = model_std.conf_int(alpha=0.05)
conf_int.columns = ['CI_lower', 'CI_upper']
coef_table = pd.DataFrame({
    'coef':    model_std.params,
    'std_err': model_std.bse,
    'CI_lower': conf_int['CI_lower'],
    'CI_upper': conf_int['CI_upper'],
    'p_value':  model_std.pvalues
})
print(coef_table.round(3).to_string())

print("All have impacts that are clearly different from zero given the 95% confidence intervals (that all exclude zero). However, some factors like standardized scoring are much greater than others, like standardized FT differential")

team_row = nba[(nba['TEAM'] == 'Golden State Warriors') & (nba['Season End'] == 2016)].iloc[0]
print("Team: Golden State Warriors (2015-16 season)")
print(f"Actual wins: {int(team_row['W'])}")
print(f"z1={team_row['z1']:.4f}, z2={team_row['z2']:.4f}, "
      f"z3={team_row['z3']:.4f}, z4={team_row['z4']:.4f}")

new_z = pd.DataFrame({'const': 1,
                       'z1': [team_row['z1']],
                       'z2': [team_row['z2']],
                       'z3': [team_row['z3']],
                       'z4': [team_row['z4']]})

point_pred = model_std.predict(new_z)[0]
print(f"\n(a) Model prediction:       {point_pred:.2f} wins")

ci = model_std.get_prediction(new_z).summary_frame(alpha=0.05)
print(f"(b) 95% CI for model predicted wins:   ({ci['mean_ci_lower'].values[0]:.2f}, "
      f"{ci['mean_ci_upper'].values[0]:.2f})")

print(f"(c) 95% PI for actual wins: ({ci['obs_ci_lower'].values[0]:.2f}, "
      f"{ci['obs_ci_upper'].values[0]:.2f})")

print("The actual win interval will be much wider than the expected wins interval because we have much more data points to back up our model's estimate, and the coefficients have relatively low standard errors. However, a singular team's actual results can vary more greatly")



#Task 5
print("\n")
train, test = train_test_split(nba, test_size=0.2, random_state=RANDOM_STATE)
print(f"Training set: {len(train)} observations")
print(f"Test set:     {len(test)} observations")

X_train = sm.add_constant(train[['x1', 'x2', 'x3', 'x4']])
X_test  = sm.add_constant(test[['x1', 'x2', 'x3', 'x4']])
model_train = sm.OLS(train['W'], X_train).fit()
preds_orig  = model_train.predict(X_test)
rmse_orig   = np.sqrt(((test['W'] - preds_orig) ** 2).mean())

for j in ['x1', 'x2', 'x3', 'x4']:
    train_mean = train[j].mean()
    train_std  = train[j].std()
    train[f'z{j[1]}'] = (train[j] - train_mean) / train_std
    test[f'z{j[1]}']  = (test[j]  - train_mean) / train_std

Z_train = sm.add_constant(train[['z1', 'z2', 'z3', 'z4']])
Z_test  = sm.add_constant(test[['z1', 'z2', 'z3', 'z4']])
model_std_train = sm.OLS(train['W'], Z_train).fit()
preds_std       = model_std_train.predict(Z_test)
rmse_std        = np.sqrt(((test['W'] - preds_std) ** 2).mean())

print("RMSE for test set:")
print(f"  Original model:      {rmse_orig:.3f} wins")
print(f"  Standardized model:  {rmse_std:.3f} wins")
print(f"  Difference:          {abs(rmse_orig - rmse_std):.6f} wins")
print("Both models produce the same RMSE because a linear transformation does not change our predictions")

##############
### PART 2 ###
##############

# load data (punts)
punts = pd.read_csv("../data/02_punts.csv.gz")

# Task 1:
# - Plot post-punt yard line against starting yard line
# - Bin punts by starting field position and plot average post-punt yard line in each bin
# - Describe the shape of the relationship and where it bends
# - Plot or summarize the distribution of punter quality

# --- Plot next_ydl against ydl ---
fig, ax = plt.subplots(figsize=(9, 5))
ax.scatter(punts['ydl'], punts['next_ydl'], alpha=0.25, s=10, color='steelblue')
ax.set_xlabel('Starting yard line (ydl) — yards from opponent goal line')
ax.set_ylabel('Post-punt yard line (next_ydl)')
ax.set_title('Post-punt yard line vs. Starting yard line')
plt.tight_layout()
plt.show()

# --- Average post-punt yard line for every unique starting yard line ---
ydl_means = punts.groupby('ydl')['next_ydl'].mean().reset_index()

fig, ax = plt.subplots(figsize=(9, 5))
ax.scatter(ydl_means['ydl'], ydl_means['next_ydl'],
           s=8, color='steelblue', alpha=0.7)
ax.set_xlabel('Starting yard line (ydl)')
ax.set_ylabel('Average post-punt yard line (next_ydl)')
ax.set_title('Average post-punt yard line by starting field position')
plt.tight_layout()
plt.show()

print("""Not purely linear --> looks like a horizontal oval pattern when the punt is close to the opponents endzone 
      (80-100 yards post punt) then a downwards line section that follows as we get further from the opponent endzone""")

# --- Distribution of punter quality ---
fig, ax = plt.subplots(figsize=(8, 4))
ax.hist(punts['pq'], bins=30, color='steelblue', edgecolor='white', alpha=0.85)
ax.axvline(punts['pq'].mean(), color='crimson', linestyle='--',
           linewidth=1.5, label=f"Mean = {punts['pq'].mean():.2f}")
ax.set_xlabel('Punter quality (pq)')
ax.set_ylabel('Count')
ax.set_title('Distribution of punter quality')
ax.legend()
plt.tight_layout()
plt.show()

print(punts['pq'].describe().round(3))

# Task 2:
# - Fit competing punt models: linear, quadratic, quadratic plus punter quality, and spline
# - Visualize the fitted curves from each model
# - Use train/test RMSE or cross-validation to choose a preferred model
# - Compare the linear, quadratic, and spline tradeoffs
# - Assess whether punter quality improves out-of-sample prediction
# - Interpret the punter-quality coefficient if it is included in the selected model

punts['ydl2'] = punts['ydl'] ** 2

train, test = train_test_split(punts, test_size=0.2, random_state=RANDOM_STATE)

X_train_1 = sm.add_constant(train[['ydl']], has_constant='add')
X_test_1  = sm.add_constant(test[['ydl']], has_constant='add')
m1 = sm.OLS(train['next_ydl'], X_train_1).fit()

X_train_2 = sm.add_constant(train[['ydl', 'ydl2']], has_constant='add')
X_test_2  = sm.add_constant(test[['ydl', 'ydl2']], has_constant='add')
m2 = sm.OLS(train['next_ydl'], X_train_2).fit()

X_train_3 = sm.add_constant(train[['ydl', 'ydl2', 'pq']], has_constant='add')
X_test_3  = sm.add_constant(test[['ydl', 'ydl2', 'pq']], has_constant='add')
m3 = sm.OLS(train['next_ydl'], X_train_3).fit()

def rmse(y_true, y_pred):
    return np.sqrt(((y_true - y_pred) ** 2).mean())

results = pd.DataFrame({
    'Model': ['Linear', 'Quadratic', 'Quadratic + PQ'],
    'Train RMSE': [
        rmse(train['next_ydl'], m1.predict(X_train_1)),
        rmse(train['next_ydl'], m2.predict(X_train_2)),
        rmse(train['next_ydl'], m3.predict(X_train_3)),
    ],
    'Test RMSE': [
        rmse(test['next_ydl'], m1.predict(X_test_1)),
        rmse(test['next_ydl'], m2.predict(X_test_2)),
        rmse(test['next_ydl'], m3.predict(X_test_3)),
    ]
})
print(results.round(4).to_string(index=False))

ydl_range  = np.linspace(punts['ydl'].min(), punts['ydl'].max(), 300)
pq_low     = punts['pq'].mean() - punts['pq'].std()   # one SD below average
pq_high    = punts['pq'].mean() + punts['pq'].std()   # one SD above average

pred_data_1    = sm.add_constant(pd.DataFrame({'ydl': ydl_range}), has_constant='add')
pred_data_2    = sm.add_constant(pd.DataFrame({'ydl': ydl_range, 'ydl2': ydl_range**2}), has_constant='add')
pred_data_3_lo = sm.add_constant(pd.DataFrame({'ydl': ydl_range, 'ydl2': ydl_range**2, 'pq': pq_low}),  has_constant='add')
pred_data_3_hi = sm.add_constant(pd.DataFrame({'ydl': ydl_range, 'ydl2': ydl_range**2, 'pq': pq_high}), has_constant='add')

fig, ax = plt.subplots(figsize=(10, 6))
ax.scatter(punts['ydl'], punts['next_ydl'], alpha=0.15, s=8, color='gray', label='Data')
ax.plot(ydl_range, m1.predict(pred_data_1),    color='crimson',   linewidth=2, label='Linear')
ax.plot(ydl_range, m2.predict(pred_data_2),    color='steelblue', linewidth=2, label='Quadratic')
ax.plot(ydl_range, m3.predict(pred_data_3_lo), color='green',     linewidth=2, linestyle='--', label=f'Quadratic + PQ (below avg, pq={pq_low:.2f})')
ax.plot(ydl_range, m3.predict(pred_data_3_hi), color='orange',    linewidth=2, linestyle='--', label=f'Quadratic + PQ (above avg, pq={pq_high:.2f})')
ax.set_xlabel('Starting yard line (ydl)')
ax.set_ylabel('Post-punt yard line (next_ydl)')
ax.set_title('Fitted curves: linear vs quadratic vs quadratic + punter quality')
ax.legend()
plt.tight_layout()
plt.show()

print(m3.summary())
print(f"\nPunter quality coefficient: {m3.params['pq']:.4f}, p-value: {m3.pvalues['pq']:.4f}")

print("""Tradeoffs: Linear--> assumes linear relationship which is not the case based on the average punt end postion vs start poosition.\n
    Quadratic --> captures the non-linear relationship better, but still not perfect.\n
    Quadratic + PQ --> accounts for punter quality, which improves out-of-sample prediction (barely) with a lower RMSE (11.2365 vs 11.2427).
    """)

print("coefficient 1.34 -> for every 1 unit increase in punter quality, we expect a 1.34 yard increase in post-punt yard line")

# Task 3:
# - Plot the fitted mean response for the selected punt model
# - Add a 95% confidence band for the expected response
# - Add a 95% prediction band for one individual punt
# - Explain why the prediction band is wider
# - Identify where the model is most uncertain

pq_mean = punts['pq'].mean()

pred_df = pd.DataFrame({
    'const': 1.0,
    'ydl':   ydl_range,
    'ydl2':  ydl_range**2,
    'pq':    pq_mean
})

pred = m3.get_prediction(pred_df)
pred_summary = pred.summary_frame(alpha=0.05)

fig, ax = plt.subplots(figsize=(10, 6))

# raw data
ax.scatter(punts['ydl'], punts['next_ydl'], alpha=0.15, s=8, color='gray', label='Data')

# fitted mean
ax.plot(ydl_range, pred_summary['mean'], color='steelblue', linewidth=2, label='Fitted mean (quadratic + PQ)')

# 95% confidence band (uncertainty around the mean)
ax.fill_between(ydl_range,
                pred_summary['mean_ci_lower'],
                pred_summary['mean_ci_upper'],
                alpha=0.4, color='steelblue', label='95% Confidence band')

# 95% prediction band (uncertainty around individual punt)
ax.fill_between(ydl_range,
                pred_summary['obs_ci_lower'],
                pred_summary['obs_ci_upper'],
                alpha=0.15, color='orange', label='95% Prediction band')

ax.set_xlabel('Starting yard line (ydl)')
ax.set_ylabel('Post-punt yard line (next_ydl)')
ax.set_title('Fitted mean with confidence and prediction bands (quadratic + PQ, at mean PQ)')
ax.legend()
plt.tight_layout()
plt.show()

print("""The prediction band is wider than the confidence band because individual points have larger variance 
      whereas for the CI for expectation we have a lot of data points so we can be more certain about the true mean.
      """)
# Task 4:
# - Define punt yards over expected so that positive values are better punts
# - Compute PYOE for each punt
# - For each punter, compute average PYOE, number of punts, and standard error of average PYOE
# - Rank punters by average PYOE
# - Visualize punter rankings with uncertainty intervals
# - Identify which punters look clearly above average and which rankings are unstable

X_all = pd.DataFrame({
    'const': 1.0,
    'ydl':   punts['ydl'],
    'ydl2':  punts['ydl2'],
    'pq':    punts['pq']
})
punts['y_hat'] = m3.predict(X_all)
punts['PYOE']  = punts['y_hat'] - punts['next_ydl']

punter_stats = punts.groupby('punter').agg(
    avg_PYOE = ('PYOE', 'mean'),
    n_punts  = ('PYOE', 'count'),
    se_PYOE  = ('PYOE', lambda x: x.std() / np.sqrt(len(x)))
).reset_index()

punter_stats['t_crit']   = punter_stats['n_punts'].apply(lambda n: stats.t.ppf(0.975, df=n-1))
punter_stats['ci_lower'] = punter_stats['avg_PYOE'] - punter_stats['t_crit'] * punter_stats['se_PYOE']
punter_stats['ci_upper'] = punter_stats['avg_PYOE'] + punter_stats['t_crit'] * punter_stats['se_PYOE']

punter_stats = punter_stats.sort_values('avg_PYOE', ascending=False).reset_index(drop=True)

# --- Plot 1: all punters ---
x_pos = np.arange(len(punter_stats))

fig, ax = plt.subplots(figsize=(len(punter_stats) * 0.4 + 2, 7))
ax.vlines(x_pos,
          punter_stats['ci_lower'],
          punter_stats['ci_upper'],
          color='steelblue', alpha=0.5, linewidth=1.5)
ax.scatter(x_pos, punter_stats['avg_PYOE'], color='steelblue', s=30, zorder=3)
ax.axhline(0, color='crimson', linewidth=1.5, linestyle='--', label='League average')
ax.set_xticks(x_pos)
ax.set_xticklabels(punter_stats['punter'], rotation=90, fontsize=7)
ax.set_ylabel('Average PYOE (yards)')
ax.set_title('Punter rankings by PYOE with 95% CIs (all punters)')
ax.legend()
plt.tight_layout()
plt.show()

# --- Plot 2: min 30 punts ---
punter_stats_filtered = punter_stats[punter_stats['n_punts'] >= 30].reset_index(drop=True)
x_pos = np.arange(len(punter_stats_filtered))

fig, ax = plt.subplots(figsize=(len(punter_stats_filtered) * 0.5 + 2, 7))
ax.vlines(x_pos,
          punter_stats_filtered['ci_lower'],
          punter_stats_filtered['ci_upper'],
          color='steelblue', alpha=0.5, linewidth=1.5)
ax.scatter(x_pos, punter_stats_filtered['avg_PYOE'], color='steelblue', s=30, zorder=3)
ax.axhline(0, color='crimson', linewidth=1.5, linestyle='--', label='League average')
ax.set_xticks(x_pos)
ax.set_xticklabels(punter_stats_filtered['punter'], rotation=90, fontsize=8)
ax.set_ylabel('Average PYOE (yards)')
ax.set_title('Punter rankings by PYOE with 95% CIs (min 30 punts)')
ax.legend()
plt.tight_layout()
plt.show()

above_avg = punter_stats_filtered[punter_stats_filtered['ci_lower'] > 0]
print("\nPunters clearly above average (95% CI entirely above 0):")
print(above_avg[['punter','avg_PYOE','n_punts','se_PYOE']].round(3).to_string(index=False))

unstable = punter_stats.nlargest(5, 'se_PYOE')
print("\nMost unstable rankings (top 5 largest SE, all punters):")
print(unstable[['punter','avg_PYOE','n_punts','se_PYOE']].round(3).to_string(index=False))

# Final reflection:
# - Explain how adding columns changed what the model could fit
# - Explain when flexibility helped and when it could hurt
# - Interpret the residual standard error in this setting
# - Explain why prediction intervals are wider than confidence intervals
# - Note one coefficient, prediction, or ranking you would interpret cautiously

print("""1. Adding columns gives the model a greater ability to capture nonlinear relationships (like the quadratic term) 
      and to account for other factors (like punter quality) that might impact data (reduce MSE)/
      """)

print("""2/3. Flexibility helps when the true relationship is non-linear or is influenced by multiple factors, 
      but it can hurt when the model overfits the training data.
      """)

print("""4. The residual standard error (RSE) estimates the standard deviation of our error term;
    in this context it gives us an idea of how much actual the post-punt yard line deviates from our model's predictions.
      """)

print("""5. Prediction intervals are wider than confidence intervals because they are widely impacted by the 
      variability of an individual. Confidence intervals have more data points to 
      support the estimate of the mean, so we are more confident that our prediction is accurate compared to the true prediction.
      """)

print("""6. I would interpret the punter quality coefficient cautiously because the improvement in RMSE is very small. 
      I would also interpret the rankings of punters with very few punts cautiously because some of their standard errors
      are very large, so we cannot be too sure on the correctness of the order.
      """)
# End of file — this is a direct Python skeleton of the original R script.
