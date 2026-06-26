#############
### SETUP ###
#############

# install.packages(c("cluster", "tidyverse"))

library(cluster)
library(tidyverse)

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

# TODO: Make at least two exploratory visualizations before clustering.
# Ideas:
#   * number of player-seasons by year
#   * distribution of play-type frequencies
#   * isolation frequency vs. pick-and-roll ball-handler frequency
#   * spot-up frequency vs. off-screen/handoff frequency

nba_data |>
    count(szn) |>
    ggplot(aes(x = szn, y = n)) +
    geom_col() +
    scale_x_continuous(breaks = sort(unique(nba_data$szn))) +
    labs(
        x = "Season",
        y = "Player-seasons",
        title = "Player-Seasons By Year"
    )

nba_data |>
    select(player_szn, szn, all_of(feature_cols)) |>
    pivot_longer(
        cols = all_of(feature_cols),
        names_to = "play_type",
        values_to = "frequency"
    ) |>
    mutate(play_type = factor(feature_labels[play_type], levels = feature_labels)) |>
    ggplot(aes(x = play_type, y = frequency)) +
    geom_boxplot() +
    coord_flip() +
    labs(
        x = "Play type",
        y = "FGA frequency",
        title = "Distribution Of Offensive Play Types"
    )

# P&R ball-handler frequency vs. isolation frequency
nba_data |>
    ggplot(aes(x = FGA_freq_PRBallHandler_OG, y = FGA_freq_Isolation_OG)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "#D95D39") +
    labs(
        x = "P&R ball-handler FGA frequency",
        y = "Isolation FGA frequency",
        title = "P&R Ball-Handler vs. Isolation Frequency"
    )


#####################
### ROLE FEATURES ###
#####################

role_features = nba_data |>
    select(all_of(feature_cols))

# Standardize features so that one high-variance feature does not dominate
# Euclidean distance.
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
        select(row_idx, player_szn, szn, PLAYER_NAME),
    as_tibble(pca_fit$x[, 1:4])
)

scree_plot = tibble(
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
    )

scree_plot

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
    )

pca_loadings = as_tibble(pca_fit$rotation[, 1:2], rownames = "feature") |>
    mutate(feature = feature_labels[feature]) |>
    arrange(desc(abs(PC1)))

pca_loadings

# TODO: What does PC1 seem to measure? What does PC2 seem to measure?

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
        title = "Elbow Plot"
    )

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
        title = "Silhouette Scores"
    )

# Use the elbow plot, silhouette scores, and interpretability to choose k.
chosen_k = 5

kmeans_fit = kmeans(role_scaled, centers = chosen_k, nstart = 100, iter.max = 100)

nba_clustered = pca_scores |>
    mutate(
        cluster = factor(kmeans_fit$cluster),
        player_type = case_when(
            player_szn %in% target_cases$target_player_szn ~ "Targets",
            TRUE ~ "Other player-seasons"
        )
    )

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
    )

cluster_profiles = as_tibble(role_features) |>
    mutate(cluster = factor(kmeans_fit$cluster)) |>
    group_by(cluster) |>
    summarize(
        n = n(),
        across(everything(), mean),
        .groups = "drop"
    )

cluster_profiles

target_cases |>
    left_join(
        nba_clustered |>
            select(player_szn, cluster),
        by = c("target_player_szn" = "player_szn")
    )

######################################
### RECOMMENDATION CANDIDATES ########
######################################

# After fitting PCA and k-means, restrict the final recommendation pool to
# recent player-seasons. All three target cases are from 2022, so this compares
# against 2021 and 2022 player-seasons.
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

nearest_players |>
    left_join(
        nba_clustered |>
            select(player_szn, cluster),
        by = "player_szn"
    ) |>
    arrange(team, distance)

# TODO: For each team, which 2-3 candidates would you investigate further?
# Use the nearest-neighbor table, PCA map, k-means clusters, and basketball
# context to explain your recommendations and limitations.
