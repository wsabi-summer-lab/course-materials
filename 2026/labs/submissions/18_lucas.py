#############
### SETUP ###
#############

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score, silhouette_samples
from scipy.spatial.distance import cdist
import warnings
warnings.filterwarnings("ignore")

np.random.seed(18)

################
### NBA DATA ###
################

data_path = "../data/18_nba-clusters.csv.gz"
import os
if not os.path.exists(data_path):
    data_path = "2026/labs/data/18_nba-clusters.csv.gz"

nba_data = pd.read_csv(data_path)

feature_cols = [
    "FGA_freq_Spotup_OG",
    "FGA_freq_PRRollMan_OG",
    "FGA_freq_Postup_OG",
    "FGA_freq_Cut_OG",
    "FGA_freq_OffRebound_OG",
    "FGA_freq_PRBallHandler_OG",
    "FGA_freq_Isolation_OG",
    "FGA_freq_OffScreenOrHandoff_OG",
]

feature_labels = {
    "FGA_freq_Spotup_OG": "Spot-up",
    "FGA_freq_PRRollMan_OG": "P&R roll man",
    "FGA_freq_Postup_OG": "Post-up",
    "FGA_freq_Cut_OG": "Cut",
    "FGA_freq_OffRebound_OG": "Off. rebound",
    "FGA_freq_PRBallHandler_OG": "P&R ball handler",
    "FGA_freq_Isolation_OG": "Isolation",
    "FGA_freq_OffScreenOrHandoff_OG": "Off-screen/handoff",
}

nba_data = nba_data.dropna(subset=feature_cols).reset_index(drop=True)
nba_data["row_id"] = nba_data.index  # 0-indexed

print(nba_data.dtypes)
print(nba_data.shape)

#################################
### EXPLORATORY VISUALIZATION ###
#################################

# Player-seasons by year
season_counts = nba_data.groupby("szn").size().reset_index(name="n")

fig, ax = plt.subplots()
ax.bar(season_counts["szn"], season_counts["n"])
ax.set_xticks(sorted(nba_data["szn"].unique()))
ax.set_xlabel("Season")
ax.set_ylabel("Player-seasons")
ax.set_title("Player-Seasons By Year")
plt.tight_layout()
plt.show()

# Distribution of play-type frequencies
melted = (
    nba_data[["player_szn", "szn"] + feature_cols]
    .melt(id_vars=["player_szn", "szn"], var_name="play_type", value_name="frequency")
)
melted["play_type"] = melted["play_type"].map(feature_labels)
ordered_labels = list(feature_labels.values())
melted["play_type"] = pd.Categorical(melted["play_type"], categories=ordered_labels, ordered=True)

fig, ax = plt.subplots(figsize=(8, 5))
grouped = [melted[melted["play_type"] == pt]["frequency"].dropna().values for pt in ordered_labels]
ax.boxplot(grouped, vert=False, labels=ordered_labels)
ax.set_xlabel("FGA frequency")
ax.set_title("Distribution Of Offensive Play Types")
plt.tight_layout()
plt.show()

#####################
### ROLE FEATURES ###
#####################

role_features = nba_data[feature_cols].copy()

scaler = StandardScaler()
role_scaled = scaler.fit_transform(role_features)

####################
### TARGET CASES ###
####################

target_cases = pd.DataFrame({
    "team": ["Detroit Pistons", "San Antonio Spurs", "Utah Jazz"],
    "target_player_szn": ["Jerami Grant 2022", "Dejounte Murray 2022", "Donovan Mitchell 2022"],
    "context": [
        "Grant was traded to Portland after the 2022 season.",
        "Murray was traded to Atlanta after the 2022 season.",
        "Mitchell was traded to Cleveland after the 2022 season.",
    ],
})

print(target_cases)

missing_targets = set(target_cases["target_player_szn"]) - set(nba_data["player_szn"])
if missing_targets:
    raise ValueError(f"Missing target player-seasons: {', '.join(missing_targets)}")

##############################
### PRINCIPAL COMPONENTS #####
##############################

pca_fit = PCA(n_components=None)
pca_fit.fit(role_scaled)

pca_var = pca_fit.explained_variance_ratio_

pca_scores_arr = pca_fit.transform(role_scaled)
pca_scores = pd.concat([
    nba_data[["player_szn", "szn", "PLAYER_NAME"]].reset_index(drop=True),
    pd.DataFrame(pca_scores_arr[:, :4], columns=["PC1", "PC2", "PC3", "PC4"]),
], axis=1)

# Scree plot
fig, ax = plt.subplots()
components = np.arange(1, len(pca_var) + 1)
ax.plot(components, pca_var, marker="o")
ax.set_xticks(components)
ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f"{round(100 * x)}%"))
ax.set_xlabel("Principal component")
ax.set_ylabel("Variance explained")
ax.set_title("NBA Role PCA Scree Plot")
plt.tight_layout()
plt.show()

# PCA scatter with target players highlighted
target_scores = pca_scores.merge(
    target_cases, left_on="player_szn", right_on="target_player_szn"
)

fig, ax = plt.subplots(figsize=(8, 6))
ax.scatter(pca_scores["PC1"], pca_scores["PC2"], color="gray", alpha=0.35, s=20)
ax.scatter(target_scores["PC1"], target_scores["PC2"], color="#D95D39", s=60, zorder=5)
for _, row in target_scores.iterrows():
    ax.text(row["PC1"], row["PC2"] + 0.35, row["team"], color="#D95D39", fontsize=8, ha="center")
ax.set_xlabel(f"PC1 ({round(100 * pca_var[0])}%)")
ax.set_ylabel(f"PC2 ({round(100 * pca_var[1])}%)")
ax.set_title("Three Replacement Targets In PCA Role Space")
plt.tight_layout()
plt.show()

# PCA loadings
loadings = pd.DataFrame(
    pca_fit.components_[:2].T,
    index=feature_cols,
    columns=["PC1", "PC2"],
)
loadings.index = [feature_labels[f] for f in loadings.index]
loadings["abs_PC1"] = loadings["PC1"].abs()
pca_loadings = loadings.sort_values("abs_PC1", ascending=False).drop(columns="abs_PC1")
print(pca_loadings)

# TODO: What does PC1 seem to measure? What does PC2 seem to measure?

print("PC1: Off-ball finisher (cuts, putbacks, roll man) vs. perimeter player (P&R ball handler, spot-up, isolation).")
print("PC2: Passive spacer/spot-up shooter (high) vs. isolation/self-creation scorer (low).")

###########################
### K-MEANS CLUSTERING ####
###########################

# Elbow plot
elbow_results = []
for k in range(1, 11):
    km = KMeans(n_clusters=k, n_init=50, max_iter=100, random_state=18)
    km.fit(role_scaled)
    elbow_results.append({"k": k, "total_withinss": km.inertia_})
elbow_df = pd.DataFrame(elbow_results)

fig, ax = plt.subplots()
ax.plot(elbow_df["k"], elbow_df["total_withinss"], marker="o")
ax.set_xticks(range(1, 11))
ax.set_xlabel("Number of clusters k")
ax.set_ylabel("Total within-cluster sum of squares")
ax.set_title("Elbow Plot")
plt.tight_layout()
plt.show()

# Silhouette scores
silhouette_results = []
for k in range(2, 11):
    km = KMeans(n_clusters=k, n_init=50, max_iter=100, random_state=18)
    labels = km.fit_predict(role_scaled)
    avg_sil = silhouette_score(role_scaled, labels)
    silhouette_results.append({"k": k, "avg_silhouette": avg_sil})
sil_df = pd.DataFrame(silhouette_results)

fig, ax = plt.subplots()
ax.plot(sil_df["k"], sil_df["avg_silhouette"], marker="o")
ax.set_xticks(range(2, 11))
ax.set_xlabel("Number of clusters k")
ax.set_ylabel("Average silhouette width")
ax.set_title("Silhouette Scores")
plt.tight_layout()
plt.show()

# Fit chosen k
chosen_k = 5
kmeans_fit = KMeans(n_clusters=chosen_k, n_init=100, max_iter=100, random_state=18)
kmeans_fit.fit(role_scaled)

nba_clustered = pca_scores.copy()
nba_clustered["cluster"] = kmeans_fit.labels_.astype(str)
nba_clustered["player_type"] = nba_clustered["player_szn"].apply(
    lambda x: "Targets" if x in target_cases["target_player_szn"].values else "Other player-seasons"
)

# PCA plot with clusters
target_clustered = nba_clustered[nba_clustered["player_type"] == "Targets"].merge(
    target_cases, left_on="player_szn", right_on="target_player_szn"
)

fig, ax = plt.subplots(figsize=(8, 6))
for cluster_id, group in nba_clustered.groupby("cluster"):
    ax.scatter(group["PC1"], group["PC2"], label=f"Cluster {cluster_id}", alpha=0.65, s=20)
ax.scatter(target_clustered["PC1"], target_clustered["PC2"], color="#D95D39", s=60, zorder=5)
for _, row in target_clustered.iterrows():
    ax.text(row["PC1"], row["PC2"] + 0.35, row["team"], color="#D95D39", fontsize=8, ha="center")
ax.set_xlabel("PC1")
ax.set_ylabel("PC2")
ax.set_title("NBA K-means Clusters And Replacement Targets")
ax.legend(title="Cluster", fontsize=7)
plt.tight_layout()
plt.show()

# Cluster profiles
cluster_profiles = (
    role_features.copy()
    .assign(cluster=kmeans_fit.labels_.astype(str))
    .groupby("cluster")
    .agg(n=("FGA_freq_Spotup_OG", "count"), **{col: (col, "mean") for col in feature_cols})
    .reset_index()
)
print(cluster_profiles)

# Target cluster assignments
target_cluster_df = target_cases.merge(
    nba_clustered[["player_szn", "cluster"]],
    left_on="target_player_szn",
    right_on="player_szn",
    how="left",
)
print(target_cluster_df)

######################################
### RECOMMENDATION CANDIDATES ########
######################################

recent_window = 1

def find_neighbors(target_player_szn, n_neighbors=10):
    target_idx = nba_data.index[nba_data["player_szn"] == target_player_szn][0]
    target_player = nba_data.loc[target_idx, "PLAYER_NAME"]
    target_season = nba_data.loc[target_idx, "szn"]
    target_vec = role_scaled[target_idx].reshape(1, -1)

    distances = np.sqrt(((role_scaled - target_vec) ** 2).sum(axis=1))

    candidates = nba_data.copy()
    candidates["distance"] = distances
    candidates = candidates[
        (candidates["row_id"] != target_idx)
        & (candidates["PLAYER_NAME"] != target_player)
        & (candidates["szn"] >= target_season - recent_window)
        & (candidates["szn"] <= target_season)
    ]
    candidates = candidates.sort_values("distance")
    return (
        candidates[["player_szn", "PLAYER_NAME", "szn", "distance"] + feature_cols]
        .head(n_neighbors)
    )

neighbor_frames = []
for _, row in target_cases.iterrows():
    neighbors = find_neighbors(row["target_player_szn"])
    neighbors = neighbors.copy()
    neighbors["team"] = row["team"]
    neighbors["target_player_szn"] = row["target_player_szn"]
    neighbors["context"] = row["context"]
    neighbor_frames.append(neighbors)

nearest_players = pd.concat(neighbor_frames, ignore_index=True)

nearest_with_clusters = nearest_players.merge(
    nba_clustered[["player_szn", "cluster"]],
    on="player_szn",
    how="left",
).sort_values(["team", "distance"])

print(nearest_with_clusters[["team", "player_szn", "PLAYER_NAME", "szn", "distance", "cluster"]])

# TODO: For each team, which 2-3 candidates would you investigate further?
# Use the nearest-neighbor table, PCA map, k-means clusters, and basketball
# context to explain your recommendations and limitations.

print("Detroit Pistons: OG Anunoby 2021 (0.62) and Miles Bridges 2021 (0.76); model misses defensive versatility.")
print("San Antonio Spurs: Kevin Porter Jr. 2022 (0.20) and Reggie Jackson 2021 (0.26); can't distinguish two-way guards from pure scorers.")
print("Utah Jazz: Jalen Green 2022 (0.25) and Darius Garland 2021 (0.26); unclear if either can handle Mitchell's first-option volume.")