#############
### SETUP ###
#############

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import gmean

np.random.seed(10)

bankroll0 = 1000

##########################
### HELPER FUNCTIONS #####
##########################

# These low-level pricing helpers are provided for you.
# You do not need to rewrite them unless you want to.

def american_to_decimal(a):
    a = np.asarray(a, dtype=float)
    return np.where(a > 0, 1 + a / 100, 1 + 100 / np.abs(a))

def contract_to_decimal(price):
    price = np.asarray(price, dtype=float)
    return 1 / price

def break_even_prob(decimal_odds):
    decimal_odds = np.asarray(decimal_odds, dtype=float)
    return 1 / decimal_odds

def expected_value_per_dollar(p, decimal_odds):
    p = np.asarray(p, dtype=float)
    decimal_odds = np.asarray(decimal_odds, dtype=float)
    return decimal_odds * p - 1

def kelly_fraction(p, decimal_odds):
    p = np.asarray(p, dtype=float)
    decimal_odds = np.asarray(decimal_odds, dtype=float)
    return np.maximum(0, (decimal_odds * p - 1) / (decimal_odds - 1))

def remove_vig_2way(decimal_a, decimal_b):
    q_a = 1 / decimal_a
    q_b = 1 / decimal_b
    return pd.DataFrame({
        "raw_a": [q_a],
        "raw_b": [q_b],
        "hold": [q_a + q_b - 1],
        "novig_a": [q_a / (q_a + q_b)],
        "novig_b": [q_b / (q_a + q_b)],
    })

########################
### PART A DATA ########
########################

part_a = pd.DataFrame({
    "bet": [
        "Team A moneyline",
        "Team B moneyline",
        "Spread bet",
        "Prediction contract",
        "Underdog",
        "Favorite",
    ],
    "quote_type": ["american", "american", "american", "contract", "american", "american"],
    "quote": [140, -160, -110, 0.63, 250, -220],
    "model_p": [0.46, 0.59, 0.55, 0.68, 0.31, 0.66],
})

def solve_part_a(part_a, bankroll0=1000):
    df = part_a.copy()

    df["decimal_odds"] = np.where(
        df["quote_type"] == "american",
        american_to_decimal(df["quote"]),
        contract_to_decimal(df["quote"])
    )

    df["break_even_p"] = break_even_prob(df["decimal_odds"])

    df["ev_per_dollar"] = expected_value_per_dollar(df["model_p"], df["decimal_odds"])

    df["kelly_full"] = kelly_fraction(df["model_p"], df["decimal_odds"])
    df["kelly_half"] = df["kelly_full"] / 2
    df["kelly_quarter"] = df["kelly_full"] / 4

    df["stake_full"] = df["kelly_full"] * bankroll0
    df["stake_half"] = df["kelly_half"] * bankroll0
    df["stake_quarter"] = df["kelly_quarter"] * bankroll0

    eps = 0.01
    df["ev_label"] = np.where(
        df["ev_per_dollar"] > eps, "positive EV",
        np.where(df["ev_per_dollar"] < -eps, "negative EV", "roughly neutral")
    )

    df["ev_rank"] = df["ev_per_dollar"].rank(ascending=False).astype(int)

    return df.sort_values("ev_rank").reset_index(drop=True)

result_a = solve_part_a(part_a, bankroll0=1000)

print(result_a[["bet", "quote", "decimal_odds", "break_even_p",
                 "model_p", "ev_per_dollar", "ev_label", "ev_rank"]])


########################
### PART B DATA ########
########################

two_sided = pd.DataFrame({
    "market": ["Game 1", "Game 2", "Game 3"],
    "side_a": [-150, -110, 105],
    "side_b": [130, -110, -125],
    "model_p_side_a": [0.60, 0.53, 0.49],
})

def solve_part_b(two_sided):
    # TODO:
    # 1. Convert side_a and side_b to decimal odds.
    # 2. Compute raw implied probabilities, hold, and no-vig probabilities.
    # 3. Compare model_p_side_a to the no-vig probability for side_a.

    df = two_sided.copy()

    df["decimal_a"] = american_to_decimal(df["side_a"])
    df["decimal_b"] = american_to_decimal(df["side_b"])

    vig_results = df.apply(
        lambda row: remove_vig_2way(row["decimal_a"], row["decimal_b"]),
        axis=1
    )
    vig_df = pd.concat(vig_results.values, ignore_index=True)
    df = pd.concat([df.reset_index(drop=True), vig_df], axis=1)

    df["model_vs_novig_diff"] = df["model_p_side_a"] - df["novig_a"]

    return df

result_b = solve_part_b(two_sided)

print(result_b[["market", "decimal_a", "decimal_b", "raw_a", "raw_b",
                 "hold", "novig_a", "novig_b", "model_p_side_a", "model_vs_novig_diff"]])

############################
### PART C EXTENSIONS ######
############################

parlay = pd.DataFrame({
    "leg1_p": [0.58],
    "leg2_p": [0.54],
    "american_odds": [290],
})

correlated_example = pd.DataFrame({
    "bet": ["Eagles -3.5", "QB over 1.5 pass TD"],
    "american_odds": [-110, 120],
    "model_p": [0.55, 0.49],
})

def analyze_parlay(leg1_p, leg2_p, american_odds):
    # TODO:
    # 1. Compute the parlay win probability under independence.
    # 2. Convert the offered odds to decimal odds.
    # 3. Compute break-even probability, EV, and full Kelly.

    parlay_p = leg1_p * leg2_p

    decimal_odds = american_to_decimal(american_odds)
    be_p = break_even_prob(decimal_odds)

    ev = expected_value_per_dollar(parlay_p, decimal_odds)

    kelly_full = kelly_fraction(parlay_p, decimal_odds)

    return pd.DataFrame({
        "parlay_p": [parlay_p],
        "decimal_odds": [decimal_odds],
        "break_even_p": [be_p],
        "ev_per_dollar": [ev],
        "kelly_full": [kelly_full],
    })

parlay_result = analyze_parlay(
    parlay["leg1_p"].iloc[0],
    parlay["leg2_p"].iloc[0],
    parlay["american_odds"].iloc[0]
)
print(parlay_result)

pos_ev = result_a[result_a["ev_label"] == "positive EV"].copy()

pos_ev["stake_flat1"] = 0.01 * bankroll0
pos_ev["kelly_rank"] = pos_ev["kelly_full"].rank(ascending=False).astype(int)

result_c = pos_ev[["bet", "model_p", "decimal_odds", "ev_per_dollar",
                    "kelly_full", "kelly_half", "kelly_quarter",
                    "stake_full", "stake_half", "stake_quarter", "stake_flat1",
                    "kelly_rank"]].sort_values("kelly_rank").reset_index(drop=True)

print(result_c)

############################
### PART D SIMULATION ######
############################

def sample_bet(board):
    return board.sample(n=1)

def strategy_fraction(strategy, p, decimal_odds):
    if strategy == "flat1":
        return 0.01
    elif strategy == "kelly":
        return float(kelly_fraction(p, decimal_odds))
    elif strategy == "half":
        return float(kelly_fraction(p, decimal_odds)) / 2
    elif strategy == "quarter":
        return float(kelly_fraction(p, decimal_odds)) / 4
    elif strategy == "reckless10":
        return 0.10
    else:
        raise ValueError(f"Unknown strategy: {strategy}")


def simulate_path(board, n_steps=500, bankroll0=1000, strategy="flat1"):
    valid_strategies = {"flat1", "kelly", "half", "quarter", "reckless10"}
    if strategy not in valid_strategies:
        raise ValueError(f"strategy must be one of {valid_strategies}")

    bankroll = bankroll0
    bankrolls = [bankroll0]

    for _ in range(n_steps):
        bet = sample_bet(board).iloc[0]
        p_true = bet["p_true"]
        p_hat = bet["p_hat"]
        decimal_odds = bet["decimal_odds"]

        f = strategy_fraction(strategy, p_hat, decimal_odds)
        f = min(max(f, 0.0), 1.0) 

        stake = f * bankroll

        win = np.random.rand() < p_true
        if win:
            bankroll += stake * (decimal_odds - 1)
        else:
            bankroll -= stake

        bankroll = max(bankroll, 0.0) 
        bankrolls.append(bankroll)

    return pd.DataFrame({"step": range(0, n_steps + 1), "bankroll": bankrolls})


def simulate_many_paths(board, n_paths=1000, n_steps=500, bankroll0=1000, strategy="flat1"):
    valid_strategies = {"flat1", "kelly", "half", "quarter", "reckless10"}
    if strategy not in valid_strategies:
        raise ValueError(f"strategy must be one of {valid_strategies}")

    paths = []
    for path_id in range(n_paths):
        path_df = simulate_path(board, n_steps=n_steps, bankroll0=bankroll0, strategy=strategy)
        path_df["path_id"] = path_id
        paths.append(path_df)

    return pd.concat(paths, ignore_index=True)


board_singles = result_c[["bet", "model_p", "decimal_odds"]].rename(columns={"model_p": "p_true"})
board_singles["p_hat"] = board_singles["p_true"]

if parlay_result["ev_per_dollar"].iloc[0] > 0:
    parlay_row = pd.DataFrame({
        "bet": ["Parlay"],
        "p_true": [float(parlay_result["parlay_p"].iloc[0])],
        "p_hat": [float(parlay_result["parlay_p"].iloc[0])],
        "decimal_odds": [float(parlay_result["decimal_odds"].iloc[0])],
    })
    board = pd.concat([board_singles, parlay_row], ignore_index=True)
else:
    board = board_singles.copy()

print(board)

strategies = ["flat1", "kelly", "half", "quarter", "reckless10"]
n_paths = 1000
n_steps = 500

all_results = {}
for strat in strategies:
    all_results[strat] = simulate_many_paths(
        board, n_paths=n_paths, n_steps=n_steps, bankroll0=bankroll0, strategy=strat
    )


plt.figure(figsize=(10, 6))
for strat in strategies:
    summary = all_results[strat].groupby("step")["bankroll"].median()
    plt.plot(summary.index, summary.values, label=strat)
plt.xlabel("Step")
plt.ylabel("Median bankroll")
plt.title("Median bankroll over time by strategy")
plt.legend()
plt.show()

for strat in strategies:
    df = all_results[strat]
    summary = df.groupby("step")["bankroll"].agg(
        median="median",
        p10=lambda x: x.quantile(0.10),
        p90=lambda x: x.quantile(0.90),
    )
    plt.figure(figsize=(8, 5))
    plt.plot(summary.index, summary["median"], label="median")
    plt.plot(summary.index, summary["p10"], linestyle="--", label="10th pct")
    plt.plot(summary.index, summary["p90"], linestyle="--", label="90th pct")
    plt.title(f"Bankroll paths - {strat}")
    plt.xlabel("Step")
    plt.ylabel("Bankroll")
    plt.legend()
    plt.show()
    
prob_loss_50 = {}
for strat in strategies:
    df = all_results[strat]
    min_per_path = df.groupby("path_id")["bankroll"].min()
    prob_loss_50[strat] = (min_per_path <= 0.5 * bankroll0).mean()

print("\nP(lose >= 50% of bankroll at some point):")
print(pd.Series(prob_loss_50))

geo_mean_final = {}
for strat in strategies:
    df = all_results[strat]
    final = df[df["step"] == n_steps]["bankroll"]
    final_nonzero = final[final > 0]  
    geo_mean_final[strat] = gmean(final_nonzero)

print("\nGeometric mean final bankroll:")
print(pd.Series(geo_mean_final))


############################
### PART E UNCERTAINTY #####
############################

def add_model_noise(p_true, sd_eps=0.03):
    p_true = np.asarray(p_true, dtype=float)
    p_hat = p_true + np.random.normal(loc=0, scale=sd_eps, size=p_true.shape)
    return np.minimum(0.99, np.maximum(0.01, p_hat))
def add_model_noise(p_true, sd_eps=0.03):
    p_true = np.asarray(p_true, dtype=float)
    p_hat = p_true + np.random.normal(loc=0, scale=sd_eps, size=p_true.shape)
    return np.minimum(0.99, np.maximum(0.01, p_hat))

board_noisy = board.copy()
board_noisy["p_hat"] = add_model_noise(board_noisy["p_true"].values, sd_eps=0.03)

print(board_noisy)

kelly_strategies = ["kelly", "half", "quarter"]

all_results_noisy = {}
for strat in kelly_strategies:
    all_results_noisy[strat] = simulate_many_paths(
        board_noisy, n_paths=n_paths, n_steps=n_steps, bankroll0=bankroll0, strategy=strat
    )

plt.figure(figsize=(10, 6))
for strat in kelly_strategies:
    clean_summary = all_results[strat].groupby("step")["bankroll"].median()
    noisy_summary = all_results_noisy[strat].groupby("step")["bankroll"].median()
    plt.plot(clean_summary.index, clean_summary.values, label=f"{strat} (clean p)")
    plt.plot(noisy_summary.index, noisy_summary.values, linestyle="--", label=f"{strat} (noisy p_hat)")
plt.xlabel("Step")
plt.ylabel("Median bankroll")
plt.title("Median bankroll: clean vs. noisy probability estimates")
plt.legend()
plt.show()

prob_loss_50_noisy = {}
for strat in kelly_strategies:
    df = all_results_noisy[strat]
    min_per_path = df.groupby("path_id")["bankroll"].min()
    prob_loss_50_noisy[strat] = (min_per_path <= 0.5 * bankroll0).mean()

print("\nP(lose >= 50% of bankroll at some point):")
compare_loss = pd.DataFrame({
    "clean": {strat: prob_loss_50[strat] for strat in kelly_strategies},
    "noisy": prob_loss_50_noisy,
})
print(compare_loss)

geo_mean_final_noisy = {}
for strat in kelly_strategies:
    df = all_results_noisy[strat]
    final = df[df["step"] == n_steps]["bankroll"]
    final_nonzero = final[final > 0]
    geo_mean_final_noisy[strat] = gmean(final_nonzero)

print("\nGeometric mean final bankroll:")
compare_gmean = pd.DataFrame({
    "clean": {strat: geo_mean_final[strat] for strat in kelly_strategies},
    "noisy": geo_mean_final_noisy,
})
print(compare_gmean)