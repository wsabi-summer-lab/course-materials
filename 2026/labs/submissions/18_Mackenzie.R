#############
### SETUP ###
#############

# install.packages(c("cluster", "tidyverse", "scales"))

library(cluster)
library(tidyverse)
library(scales)

set.seed(18)

################
### NBA DATA ###
################

data_path = "../data/18_nba-clusters.csv"

if (!file.exists(data_path)) {
  data_path = "2026/labs/data/18_nba-clusters.csv"
}

nba_data = read_csv(data_path, show_col_types = FALSE)

# Print column names so you can check the dataset structure
print(names(nba_data))

# Standardize important column names so the rest of the code works

# Fix player name column
if (!"PLAYER_NAME" %in% names(nba_data)) {
  if ("player_name" %in% names(nba_data)) {
    nba_data = nba_data |>
      rename(PLAYER_NAME = player_name)
  } else if ("Player" %in% names(nba_data)) {
    nba_data = nba_data |>
      rename(PLAYER_NAME = Player)
  } else if ("player" %in% names(nba_data)) {
    nba_data = nba_data |>
      rename(PLAYER_NAME = player)
  } else {
    stop("Could not find a player name column. Look at names(nba_data).")
  }
}

# Fix season column
if (!"szn" %in% names(nba_data)) {
  if ("season" %in% names(nba_data)) {
    nba_data = nba_data |>
      rename(szn = season)
  } else if ("Season" %in% names(nba_data)) {
    nba_data = nba_data |>
      rename(szn = Season)
  } else {
    stop("Could not find a season column. Look at names(nba_data).")
  }
}

# Fix or create player-season column
if (!"player_szn" %in% names(nba_data)) {
  if ("PLAYER_SZN" %in% names(nba_data)) {
    nba_data = nba_data |>
      rename(player_szn = PLAYER_SZN)
  } else if ("player_season" %in% names(nba_data)) {
    nba_data = nba_data |>
      rename(player_szn = player_season)
  } else if ("PlayerSeason" %in% names(nba_data)) {
    nba_data = nba_data |>
      rename(player_szn = PlayerSeason)
  } else {
    nba_data = nba_data |>
      mutate(player_szn = paste(PLAYER_NAME, szn))
  }
}

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

# Check that all feature columns exist
missing_features = setdiff(feature_cols, names(nba_data))

if (length(missing_features) > 0) {
  stop("Missing feature columns: ", paste(missing_features, collapse = ", "))
}

nba_data = nba_data |>
  drop_na(all_of(feature_cols)) |>
  mutate(row_id = row_number())

glimpse(nba_data)

#################################
### EXPLORATORY VISUALIZATION ###
#################################

# Visualization 1: number of player-seasons by year

nba_data |>
  count(szn) |>
  ggplot(aes(x = szn, y = n)) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(breaks = sort(unique(nba_data$szn))) +
  labs(
    x = "Season",
    y = "Player-seasons",
    title = "Player-Seasons By Year"
  ) +
  theme_minimal()

# Visualization 2: distribution of play-type frequencies

nba_data |>
  select(player_szn, szn, all_of(feature_cols)) |>
  pivot_longer(
    cols = all_of(feature_cols),
    names_to = "play_type",
    values_to = "frequency"
  ) |>
  mutate(
    play_type = unname(feature_labels[play_type]),
    play_type = factor(play_type, levels = unname(feature_labels))
  ) |>
  ggplot(aes(x = play_type, y = frequency)) +
  geom_boxplot(fill = "gray85", color = "gray25") +
  coord_flip() +
  labs(
    x = "Play type",
    y = "FGA frequency",
    title = "Distribution Of Offensive Play Types"
  ) +
  theme_minimal()

# Visualization 3: on-ball creation comparison

nba_data |>
  ggplot(aes(
    x = FGA_freq_Isolation_OG,
    y = FGA_freq_PRBallHandler_OG
  )) +
  geom_point(alpha = 0.35) +
  labs(
    x = "Isolation FGA frequency",
    y = "P&R ball-handler FGA frequency",
    title = "Isolation vs. Pick-and-Roll Ball-Handler Frequency"
  ) +
  theme_minimal()

# Visualization 4: off-ball shooting comparison

nba_data |>
  ggplot(aes(
    x = FGA_freq_Spotup_OG,
    y = FGA_freq_OffScreenOrHandoff_OG
  )) +
  geom_point(alpha = 0.35) +
  labs(
    x = "Spot-up FGA frequency",
    y = "Off-screen/handoff FGA frequency",
    title = "Spot-Up vs. Off-Screen/Handoff Frequency"
  ) +
  theme_minimal()

#####################
### ROLE FEATURES ###
#####################

# These features describe offensive role: how a player's shot attempts are created.
# They do not measure salary, contract status, defense, health, age, or efficiency.
# The assignment asks for role-comparison lists, not full roster rankings.

role_features = nba_data |>
  select(all_of(feature_cols))

# Standardize features so each play type contributes fairly to distance and clustering.

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

target_cases

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

# Scree plot

scree_plot = tibble(
  component = seq_along(pca_var),
  variance_explained = pca_var
) |>
  ggplot(aes(component, variance_explained)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq_along(pca_var)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Principal component",
    y = "Variance explained",
    title = "NBA Role PCA Scree Plot"
  ) +
  theme_minimal()

scree_plot

target_scores = pca_scores |>
  inner_join(
    target_cases,
    by = c("player_szn" = "target_player_szn")
  )

# PCA map with targets highlighted

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

# PCA loadings

pca_loadings = as_tibble(pca_fit$rotation[, 1:2], rownames = "feature") |>
  mutate(feature = unname(feature_labels[feature])) |>
  arrange(desc(abs(PC1)))

pca_loadings

pca_loadings_long = pca_loadings |>
  pivot_longer(
    cols = c(PC1, PC2),
    names_to = "component",
    values_to = "loading"
  )

ggplot(pca_loadings_long, aes(x = reorder(feature, loading), y = loading)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ component, scales = "free_x") +
  labs(
    x = "Play type",
    y = "Loading",
    title = "PCA Loadings For First Two Components"
  ) +
  theme_minimal()

# Print strongest positive and negative loadings for interpretation

pc1_positive = pca_loadings |>
  arrange(desc(PC1)) |>
  slice_head(n = 3)

pc1_negative = pca_loadings |>
  arrange(PC1) |>
  slice_head(n = 3)

pc2_positive = pca_loadings |>
  arrange(desc(PC2)) |>
  slice_head(n = 3)

pc2_negative = pca_loadings |>
  arrange(PC2) |>
  slice_head(n = 3)

cat("\nPCA interpretation guide:\n")
cat("PC1 is high for:", paste(pc1_positive$feature, collapse = ", "), "\n")
cat("PC1 is low for:", paste(pc1_negative$feature, collapse = ", "), "\n")
cat("PC2 is high for:", paste(pc2_positive$feature, collapse = ", "), "\n")
cat("PC2 is low for:", paste(pc2_negative$feature, collapse = ", "), "\n\n")

###########################
### K-MEANS CLUSTERING ####
###########################

# Elbow plot

elbow_results = tibble(k = 1:10) |>
  mutate(
    total_withinss = map_dbl(
      k,
      function(num_clusters) {
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
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Number of clusters k",
    y = "Total within-cluster sum of squares",
    title = "Elbow Plot"
  ) +
  theme_minimal()

elbow_results

# Silhouette scores

distance_matrix = dist(role_scaled)

silhouette_results = tibble(k = 2:10) |>
  mutate(
    avg_silhouette = map_dbl(
      k,
      function(num_clusters) {
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
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2:10) +
  labs(
    x = "Number of clusters k",
    y = "Average silhouette width",
    title = "Silhouette Scores"
  ) +
  theme_minimal()

silhouette_results

# Choose k.
# k = 5 is a reasonable balance between fit and interpretability.
# You can change this if your elbow/silhouette plots suggest another value.

chosen_k = 5

kmeans_fit = kmeans(
  role_scaled,
  centers = chosen_k,
  nstart = 100,
  iter.max = 100
)

nba_clustered = pca_scores |>
  mutate(
    cluster = factor(kmeans_fit$cluster),
    player_type = case_when(
      player_szn %in% target_cases$target_player_szn ~ "Targets",
      TRUE ~ "Other player-seasons"
    )
  )

# PCA plot colored by k-means cluster

ggplot(nba_clustered, aes(PC1, PC2, color = cluster)) +
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
    title = "NBA K-means Clusters And Replacement Targets"
  ) +
  theme_minimal()

################################
### CLUSTER INTERPRETATION #####
################################

cluster_profiles = as_tibble(role_features) |>
  mutate(cluster = factor(kmeans_fit$cluster)) |>
  group_by(cluster) |>
  summarize(
    n = n(),
    across(all_of(feature_cols), mean),
    .groups = "drop"
  )

cluster_profiles

cluster_profiles_long = cluster_profiles |>
  pivot_longer(
    cols = all_of(feature_cols),
    names_to = "play_type",
    values_to = "avg_frequency"
  ) |>
  mutate(
    play_type = unname(feature_labels[play_type]),
    play_type = factor(play_type, levels = unname(feature_labels))
  )

ggplot(cluster_profiles_long, aes(x = play_type, y = avg_frequency, fill = cluster)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ cluster) +
  labs(
    x = "Play type",
    y = "Average FGA frequency",
    title = "Average Offensive Role Profile By Cluster"
  ) +
  theme_minimal()

# Standardized cluster profiles: easier to see above/below-average tendencies

cluster_profiles_scaled = as_tibble(role_scaled) |>
  mutate(cluster = factor(kmeans_fit$cluster)) |>
  group_by(cluster) |>
  summarize(
    n = n(),
    across(everything(), mean),
    .groups = "drop"
  )

cluster_profiles_scaled_long = cluster_profiles_scaled |>
  pivot_longer(
    cols = all_of(feature_cols),
    names_to = "play_type",
    values_to = "standardized_avg"
  ) |>
  mutate(
    play_type = unname(feature_labels[play_type]),
    play_type = factor(play_type, levels = unname(feature_labels))
  )

ggplot(cluster_profiles_scaled_long, aes(x = play_type, y = standardized_avg, fill = cluster)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ cluster) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Play type",
    y = "Standardized cluster average",
    title = "Above/Below Average Play-Type Tendencies By Cluster"
  ) +
  theme_minimal()

# Print top features for each cluster

cluster_top_features = cluster_profiles_scaled_long |>
  group_by(cluster) |>
  slice_max(order_by = standardized_avg, n = 3) |>
  arrange(cluster, desc(standardized_avg))

cluster_top_features

cat("\nCluster interpretation guide:\n")

for (cl in levels(factor(kmeans_fit$cluster))) {
  top_features = cluster_top_features |>
    filter(cluster == cl) |>
    pull(play_type)
  
  cat(
    "Cluster", cl, "is highest in:",
    paste(top_features, collapse = ", "),
    "\n"
  )
}

# Target clusters

target_clusters = target_cases |>
  left_join(
    nba_clustered |>
      select(player_szn, cluster),
    by = c("target_player_szn" = "player_szn")
  )

target_clusters

######################################
### RECOMMENDATION CANDIDATES ########
######################################

# All targets are from 2022, so use 2021 and 2022 as the recent comparison window.

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
    select(
      player_szn,
      PLAYER_NAME,
      szn,
      distance,
      all_of(feature_cols)
    ) |>
    slice_head(n = n_neighbors)
}

nearest_players = target_cases |>
  mutate(neighbors = map(target_player_szn, find_neighbors)) |>
  unnest(neighbors) |>
  left_join(
    nba_clustered |>
      select(player_szn, cluster),
    by = "player_szn"
  ) |>
  arrange(team, distance)

nearest_players

# Separate small tables for each team

detroit_table = nearest_players |>
  filter(team == "Detroit Pistons") |>
  select(team, PLAYER_NAME, player_szn, szn, cluster, distance)

san_antonio_table = nearest_players |>
  filter(team == "San Antonio Spurs") |>
  select(team, PLAYER_NAME, player_szn, szn, cluster, distance)

utah_table = nearest_players |>
  filter(team == "Utah Jazz") |>
  select(team, PLAYER_NAME, player_szn, szn, cluster, distance)

cat("\nDetroit Pistons: 10 closest recent player-seasons to Jerami Grant 2022\n")
print(detroit_table)

cat("\nSan Antonio Spurs: 10 closest recent player-seasons to Dejounte Murray 2022\n")
print(san_antonio_table)

cat("\nUtah Jazz: 10 closest recent player-seasons to Donovan Mitchell 2022\n")
print(utah_table)

# Automatic top 3 recommendations by distance

recommendations = nearest_players |>
  group_by(team, target_player_szn) |>
  slice_min(order_by = distance, n = 3, with_ties = FALSE) |>
  ungroup() |>
  select(
    team,
    target_player_szn,
    recommended_player = PLAYER_NAME,
    recommended_player_szn = player_szn,
    szn,
    cluster,
    distance
  )

recommendations

#################################
### WRITE-UP ANSWERS ###########
#################################

cat("\n\n================ WRITE-UP ANSWERS ================\n")

cat("
Feature choice:
I used the eight play-type frequency variables because they describe how a player's
shot attempts were created. These features compare offensive role, not overall
quality. Two players are considered similar if they take similar shares of their
shots from spot-ups, pick-and-roll actions, post-ups, cuts, offensive rebounds,
isolations, and off-screen or handoff actions. I standardized the features because
k-means clustering, PCA, and Euclidean distance are scale-sensitive.

PCA interpretation:
The scree plot shows how much variation each principal component explains. The
first two principal components give a lower-dimensional map of offensive roles.
To interpret PC1 and PC2, look at the PCA loading table. Features with large
positive loadings pull players in the positive direction, while features with
large negative loadings pull players in the opposite direction. If a component
loads strongly on isolation and pick-and-roll ball-handler frequency, it likely
represents on-ball self-creation. If it loads strongly on cuts, roll-man usage,
post-ups, or offensive rebounds, it likely represents more interior or off-ball
finishing activity.

Choice of k:
I chose k = 5 because it gives a reasonable balance between statistical fit and
basketball interpretability. The elbow plot helps show where additional clusters
stop reducing within-cluster variation as much. The silhouette plot helps show
how cleanly separated the clusters are. I did not choose k only mechanically;
I also wanted clusters that could be interpreted as meaningful basketball role
groups.

Cluster interpretation:
The clusters are not official positions. K-means only returns statistical
similarity groups based on play-type frequencies. I interpreted the clusters by
looking at which play types were above average in each cluster. For example, a
cluster high in spot-ups can be described as off-ball shooting players, a cluster
high in isolation and pick-and-roll ball-handler usage can be described as
on-ball creators, and a cluster high in cuts, roll-man usage, post-ups, or
offensive rebounds can be described as interior finishers or bigs.

Recommendations:
The nearest-neighbor tables show the 10 closest recent player-seasons to each
target using Euclidean distance on standardized offensive role features. The top
three listed in the recommendations table are the players I would investigate
further as role replacements. They are plausible comparisons because their shot
attempts came from similar offensive play types in 2021 or 2022.

Limitations:
This is only a role-comparison model. It does not measure salary, contract status,
trade availability, age, injury history, defense, shooting efficiency, passing,
team fit, or future development. A real front office would need all of that
additional information before making a roster move. Therefore, these results
should be treated as a first-pass list of offensive role comparisons, not a final
ranking of the best replacement players.
")

cat("\n\nTop 3 automatic recommendations by standardized role distance:\n")
print(recommendations)

cat("\n================ END WRITE-UP ANSWERS ================\n")