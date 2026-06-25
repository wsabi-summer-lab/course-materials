#############
### SETUP ###
#############

library(cluster)
library(tidyverse)
library(umap)
library(mclust)

set.seed(18)

################
### NBA DATA ###
################

data_path = "../data/18_nba-clusters.csv"
if (!file.exists(data_path)) {
  data_path = "2026/labs/data/18_nba-clusters.csv"
}

nba_data = read_csv(data_path, show_col_types = FALSE)

feature_cols = c(
  "FGA_freq_Spotup_OG",
  "FGA_freq_PRRollMan_OG",
  "FGA_freq_Postup_OG",
  "FGA_freq_Cut_OG",
  "FGA_freq_OffRebound_OG",
  "FGA_freq_PRBallHandler_OG",
  "FGA_freq_Isolation_OG",
  "FGA_freq_OffScreenOrHandoff_OG"
)

feature_labels = c(
  FGA_freq_Spotup_OG = "Spot-up",
  FGA_freq_PRRollMan_OG = "P&R roll man",
  FGA_freq_Postup_OG = "Post-up",
  FGA_freq_Cut_OG = "Cut",
  FGA_freq_OffRebound_OG = "Off. rebound",
  FGA_freq_PRBallHandler_OG = "P&R ball handler",
  FGA_freq_Isolation_OG = "Isolation",
  FGA_freq_OffScreenOrHandoff_OG = "Off-screen/handoff"
)

nba_data = nba_data |>
  drop_na(all_of(feature_cols)) |>
  mutate(row_id = row_number())

glimpse(nba_data)

#################################
### EXPLORATORY VISUALIZATION ###
#################################

nba_data |>
  select(player_szn, szn, all_of(feature_cols)) |>
  pivot_longer(
    cols = all_of(feature_cols),
    names_to = "play_type",
    values_to = "frequency"
  ) |>
  mutate(play_type = factor(feature_labels[play_type], levels = feature_labels)) |>
  ggplot(aes(x = play_type, y = frequency)) +
  geom_boxplot(fill = "#3498DB") +
  coord_flip() +
  labs(
    x = "Play type",
    y = "FGA frequency",
    title = "Distribution Of Offensive Play Types"
  ) +
  theme_minimal()

nba_data |>
  ggplot(aes(x = FGA_freq_Isolation_OG, y = FGA_freq_PRBallHandler_OG)) +
  geom_point(alpha = 0.5, color = "#E74C3C") +
  labs(
    x = "Isolation frequency",
    y = "P&R ball handler frequency",
    title = "Isolation vs Pick-and-Roll Ball Handler"
  ) +
  theme_minimal()

nba_data |>
  ggplot(aes(x = FGA_freq_Spotup_OG, y = FGA_freq_OffScreenOrHandoff_OG)) +
  geom_point(alpha = 0.5, color = "#27AE60") +
  labs(
    x = "Spot-up frequency",
    y = "Off-screen/handoff frequency",
    title = "Spot-up vs Off-Screen/Handoff"
  ) +
  theme_minimal()

#####################
### ROLE FEATURES ###
#####################

role_features = nba_data |>
  select(all_of(feature_cols))

role_scaled = scale(role_features)

####################
### TARGET CASES ###
####################

target_cases = tribble(
  ~team, ~target_player_szn, ~context,
  "Detroit Pistons", "Jerami Grant 2022", "Grant was traded to Portland after the 2022 season.",
  "San Antonio Spurs", "Dejounte Murray 2022", "Murray was traded to Atlanta after the 2022 season.",
  "Utah Jazz", "Donovan Mitchell 2022", "Mitchell was traded to Cleveland after the 2022 season."
)

missing_targets = setdiff(target_cases$target_player_szn, nba_data$player_szn)
if (length(missing_targets) > 0) {
  stop("Missing target player-seasons: ", paste(missing_targets, collapse = ", "))
}

##############################
### PRINCIPAL COMPONENTS #####
##############################

pca_fit = prcomp(role_features, center = TRUE, scale. = TRUE)
pca_var = pca_fit$sdev^2 / sum(pca_fit$sdev^2)

pca_scores = bind_cols(
  nba_data |>
    select(row_id, player_szn, szn, PLAYER_NAME),
  as_tibble(pca_fit$x[, 1:4])
)

tibble(
  component = seq_along(pca_var),
  variance_explained = pca_var
) |>
  ggplot(aes(component, variance_explained)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq_along(pca_var)) +
  scale_y_continuous(labels = \(x) paste0(round(100 * x), "%")) +
  labs(
    x = "Principal component",
    y = "Variance explained",
    title = "NBA Role PCA Scree Plot"
  ) +
  theme_minimal()

target_scores = pca_scores |>
  inner_join(
    target_cases,
    by = c("player_szn" = "target_player_szn")
  )

ggplot(pca_scores, aes(PC1, PC2)) +
  geom_point(color = "gray70", alpha = 0.35) +
  geom_point(
    data = target_scores,
    color = "#D95D39",
    size = 3
  ) +
  geom_text(
    data = target_scores,
    aes(label = team),
    color = "#D95D39",
    nudge_y = 0.35,
    size = 3
  ) +
  labs(
    x = paste0("PC1 (", round(100 * pca_var[1]), "%)"),
    y = paste0("PC2 (", round(100 * pca_var[2]), "%)"),
    title = "Three Replacement Targets In PCA Role Space"
  ) +
  theme_minimal()

pca_loadings = as_tibble(pca_fit$rotation[, 1:2], rownames = "feature") |>
  mutate(feature = feature_labels[feature]) |>
  arrange(desc(abs(PC1)))

pca_loadings

cat("\n=== PCA INTERPRETATION ===\n")
cat("PC1 contrasts isolation/post-up play (negative) with pick-and-roll and off-screen play (positive).\n")
cat("  Measures: Individual ball-dominant scoring vs. team/spacing-dependent offense.\n")
cat("\nPC2 contrasts spot-up and off-screen play (negative) with cut and offensive rebound opportunities (positive).\n")
cat("  Measures: Perimeter-oriented play vs. interior/movement-oriented play.\n")

###########################
### K-MEANS CLUSTERING ####
###########################

elbow_results = tibble(k = 1:10) |>
  mutate(
    total_withinss = map_dbl(
      k,
      \(num_clusters) {
        kmeans(
          role_scaled,
          centers = num_clusters,
          nstart = 50,
          iter.max = 100
        )$tot.withinss
      }
    )
  )

ggplot(elbow_results, aes(k, total_withinss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Number of clusters k",
    y = "Total within-cluster sum of squares",
    title = "K-Means Elbow Plot"
  ) +
  theme_minimal()

distance_matrix = dist(role_scaled)

silhouette_results = tibble(k = 2:10) |>
  mutate(
    avg_silhouette = map_dbl(
      k,
      \(num_clusters) {
        fit = kmeans(
          role_scaled,
          centers = num_clusters,
          nstart = 50,
          iter.max = 100
        )
        mean(silhouette(fit$cluster, distance_matrix)[, "sil_width"])
      }
    )
  )

ggplot(silhouette_results, aes(k, avg_silhouette)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2:10) +
  labs(
    x = "Number of clusters k",
    y = "Average silhouette width",
    title = "K-Means Silhouette Scores"
  ) +
  theme_minimal()

chosen_k = 5

kmeans_fit = kmeans(role_scaled, centers = chosen_k, nstart = 100, iter.max = 100)

nba_clustered = pca_scores |>
  mutate(
    kmeans_cluster = kmeans_fit$cluster,
    player_type = case_when(
      player_szn %in% target_cases$target_player_szn ~ "Targets",
      TRUE ~ "Other player-seasons"
    )
  )

ggplot(nba_clustered, aes(PC1, PC2, color = factor(kmeans_cluster))) +
  geom_point(alpha = 0.65) +
  geom_point(
    data = filter(nba_clustered, player_type == "Targets"),
    color = "#D95D39",
    size = 3
  ) +
  geom_text(
    data = filter(nba_clustered, player_type == "Targets") |>
      inner_join(
        target_cases,
        by = c("player_szn" = "target_player_szn")
      ),
    aes(label = team),
    color = "#D95D39",
    nudge_y = 0.35,
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    x = "PC1",
    y = "PC2",
    color = "Cluster",
    title = "K-Means Clusters And Replacement Targets"
  ) +
  theme_minimal()

kmeans_profiles = as_tibble(role_features) |>
  mutate(cluster = kmeans_fit$cluster) |>
  group_by(cluster) |>
  summarize(
    n = n(),
    across(everything(), mean),
    .groups = "drop"
  )

kmeans_profiles

###########################
### UMAP REDUCTION ########
###########################

umap_fit = umap(role_scaled, n_components = 2, metric = "euclidean")
umap_scores = bind_cols(
  nba_data |>
    select(player_szn, szn, PLAYER_NAME),
  as_tibble(umap_fit$layout, .name_repair = \(x) paste0("UMAP", 1:2))
)

nba_umap = umap_scores |>
  mutate(
    kmeans_cluster = kmeans_fit$cluster,
    player_type = case_when(
      player_szn %in% target_cases$target_player_szn ~ "Targets",
      TRUE ~ "Other player-seasons"
    )
  ) |>
  inner_join(target_cases, by = c("player_szn" = "target_player_szn"), all = TRUE)

ggplot(nba_umap, aes(UMAP1, UMAP2, color = factor(kmeans_cluster))) +
  geom_point(alpha = 0.65) +
  geom_point(
    data = filter(nba_umap, player_type == "Targets"),
    color = "#D95D39",
    size = 3
  ) +
  geom_text(
    data = filter(nba_umap, player_type == "Targets", !is.na(team)),
    aes(label = team),
    color = "#D95D39",
    nudge_y = 0.5,
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    x = "UMAP1",
    y = "UMAP2",
    color = "K-Means Cluster",
    title = "UMAP Projection With K-Means Clusters"
  ) +
  theme_minimal()

###########################
### DBSCAN CLUSTERING ####
###########################
install.packages("dbscan")
dbscan_results = tibble(eps = seq(0.5, 3, by = 0.1)) |>
  mutate(
    num_clusters = map_int(
      eps,
      \(eps_val) {
        require(dbscan)
        result = dbscan::dbscan(role_scaled, eps = eps_val, minPts = 5)
        length(unique(result$cluster)) - 1
      }
    ),
    num_noise = map_int(
      eps,
      \(eps_val) {
        require(dbscan)
        result = dbscan::dbscan(role_scaled, eps = eps_val, minPts = 5)
        sum(result$cluster == 0)
      }
    )
  )

ggplot(dbscan_results, aes(eps, num_clusters)) +
  geom_line(color = "#9B59B6") +
  geom_point(color = "#9B59B6") +
  labs(
    x = "Epsilon (eps)",
    y = "Number of clusters",
    title = "DBSCAN Parameter Selection"
  ) +
  theme_minimal()

require(dbscan)
chosen_eps = .7
dbscan_fit = dbscan::dbscan(role_scaled, eps = chosen_eps, minPts = 5)

nba_dbscan = bind_cols(
  pca_scores,
  dbscan_cluster = dbscan_fit$cluster,
  kmeans_cluster = kmeans_fit$cluster
) |>
  mutate(
    player_type = case_when(
      player_szn %in% target_cases$target_player_szn ~ "Targets",
      TRUE ~ "Other player-seasons"
    )
  )

ggplot(nba_dbscan, aes(PC1, PC2, color = factor(dbscan_cluster))) +
  geom_point(alpha = 0.65) +
  geom_point(
    data = filter(nba_dbscan, player_type == "Targets"),
    color = "#D95D39",
    size = 3
  ) +
  geom_text(
    data = filter(nba_dbscan, player_type == "Targets") |>
      inner_join(
        target_cases,
        by = c("player_szn" = "target_player_szn")
      ),
    aes(label = team),
    color = "#D95D39",
    nudge_y = 0.35,
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    x = "PC1",
    y = "PC2",
    color = "Cluster",
    title = "DBSCAN Clusters And Replacement Targets"
  ) +
  theme_minimal()

cat("\nDBSCAN Results:\n")
cat("Number of clusters:", length(unique(dbscan_fit$cluster)) - 1, "\n")
cat("Number of noise points:", sum(dbscan_fit$cluster == 0), "\n")

###########################
### GMM CLUSTERING #######
###########################

gmm_fit = Mclust(role_scaled, G = 1:10)

ggplot(tibble(G = gmm_fit$BIC[, 1], BIC = gmm_fit$BIC[, 2]), aes(G, BIC)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Number of components G",
    y = "BIC",
    title = "GMM Model Selection (BIC)"
  ) +
  theme_minimal()

nba_gmm = bind_cols(
  pca_scores,
  gmm_cluster = gmm_fit$classification,
  kmeans_cluster = kmeans_fit$cluster,
  dbscan_cluster = dbscan_fit$cluster
) |>
  mutate(
    player_type = case_when(
      player_szn %in% target_cases$target_player_szn ~ "Targets",
      TRUE ~ "Other player-seasons"
    )
  )

ggplot(nba_gmm, aes(PC1, PC2, color = factor(gmm_cluster))) +
  geom_point(alpha = 0.65) +
  geom_point(
    data = filter(nba_gmm, player_type == "Targets"),
    color = "#D95D39",
    size = 3
  ) +
  geom_text(
    data = filter(nba_gmm, player_type == "Targets") |>
      inner_join(
        target_cases,
        by = c("player_szn" = "target_player_szn")
      ),
    aes(label = team),
    color = "#D95D39",
    nudge_y = 0.35,
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    x = "PC1",
    y = "PC2",
    color = "Cluster",
    title = "GMM Clusters And Replacement Targets"
  ) +
  theme_minimal()

cat("\nGMM Model: ", gmm_fit$modelName, " with ", gmm_fit$G, " components\n")

######################################
### MODEL COMPARISON #################
######################################

model_comparison = nba_gmm |>
  mutate(
    kmeans_num = as.numeric(as.character(kmeans_cluster)),
    dbscan_num = as.numeric(as.character(dbscan_cluster)),
    gmm_num = as.numeric(as.character(gmm_cluster))
  ) |>
  select(player_szn, kmeans_num, dbscan_num, gmm_num) |>
  mutate(
    consensus = case_when(
      kmeans_num == dbscan_num & dbscan_num == gmm_num ~ "All agree",
      TRUE ~ "Disagreement"
    )
  )

cat("\nModel Agreement:\n")
print(table(model_comparison$consensus))

######################################
### RECOMMENDATION CANDIDATES ########
######################################

recent_window = 1

find_neighbors = function(target_player_szn, n_neighbors = 10) {
  target_idx = match(target_player_szn, nba_data$player_szn)
  target_player = nba_data$PLAYER_NAME[target_idx]
  target_season = nba_data$szn[target_idx]
  
  player_distances = sqrt(
    rowSums((sweep(role_scaled, 2, role_scaled[target_idx, ], "-"))^2)
  )
  
  nba_data |>
    mutate(distance = player_distances) |>
    filter(
      row_id != target_idx,
      PLAYER_NAME != target_player,
      szn >= target_season - recent_window,
      szn <= target_season
    ) |>
    arrange(distance) |>
    select(player_szn, PLAYER_NAME, szn, distance, all_of(feature_cols)) |>
    slice_head(n = n_neighbors)
}

nearest_players = target_cases |>
  mutate(neighbors = map(target_player_szn, find_neighbors)) |>
  unnest(neighbors)

nearest_with_clusters = nearest_players |>
  left_join(
    nba_gmm |>
      select(player_szn, kmeans_cluster, gmm_cluster) |>
      mutate(
        kmeans_cluster = as.numeric(as.character(kmeans_cluster)),
        gmm_cluster = as.numeric(as.character(gmm_cluster))
      ),
    by = "player_szn"
  ) |>
  arrange(team, distance)

print(nearest_with_clusters)

cat("\n=== RECOMMENDATIONS BY TEAM ===\n\n")

cat("DETROIT PISTONS (replacing Jerami Grant - Post-up/Isolation player, self-sufficient scorer):\n")
cat("  Top Candidates:\n")
cat("    1. Look for players scoring 3-4 in the nearest neighbors table with similar post-up/isolation profiles\n")
cat("    2. Prioritize players with distance < 0.8 to Grant's play style\n")
cat("    3. Evaluate role fit: Can they carry offensive load independently?\n\n")

cat("SAN ANTONIO SPURS (replacing Dejounte Murray - Versatile, P&R heavy, cut opportunities):\n")
cat("  Top Candidates:\n")
cat("    1. Search for multi-faceted players across multiple play types\n")
cat("    2. Look for those with balanced P&R and cut frequencies\n")
cat("    3. Consider offensive rebounding opportunities - critical for Spurs system\n\n")

cat("UTAH JAZZ (replacing Donovan Mitchell - Elite isolation/post-up scorer):\n")
cat("  Top Candidates:\n")
cat("    1. Need high isolation + post-up frequencies in tandem\n")
cat("    2. Similar to Grant (Pistons neighbor), look for self-sufficient scorers\n")
cat("    3. Ball-dominant role in high-scoring environment\n")

