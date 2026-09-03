#############
### SETUP ###
#############

install.packages(c("cluster", "factoextra", "flexclust", "ggdendro", "ggplot2", "mclust", "tidyverse"))
library(cluster)      # for hierarchical clustering and silhouette analysis
library(factoextra)   # for clustering visualization and elbow method
library(flexclust)    # for k-means clustering
library(ggdendro)     # for dendrogram visualization
library(ggplot2)      # for plotting
library(mclust)       # for Gaussian Mixture Models
library(tidyverse)    # for data manipulation and visualization

# set seed
set.seed(19)

####################
### SPOTIFY DATA ###
####################

# read in data
spotify_data = read_csv("../data/19_spotify-train.csv.gz")

# preview data
head(spotify_data)

colnames(spotify_data)

# Plot distribution of top 10 genre's
top_genres <- spotify_data %>% 
  count(Genre, sort = TRUE) %>% 
  slice_head(n = 10) %>% 
  pull(Genre)
spotify_filter = spotify_data %>%
  filter(Genre %in% top_genres)

ggplot(spotify_filter, aes(x = Genre, fill = Genre)) +
  geom_bar() +
  labs(
    title = "Distribution of Genres by Person Who Added",
    x     = "Genre",
    y     = "Number of Tracks"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Plot Danceability of top 10 genres
# Rename afrobeats; azonto; ... as just afrobeats in the genere column
spotify_filter <- spotify_filter %>%
  mutate(Genre = ifelse(Genre == "afrobeats; azonto; afrobeat; afropop; hiplife; bongo flava", "afrobeats", Genre)) %>% 
  mutate(Genre = ifelse(Genre == "classic rock; yacht rock; soft rock", "classic rock; yacht rock", Genre))

ggplot(spotify_filter, aes(x = Genre, y = Danceability, fill = Genre)) +
  geom_boxplot() +
  labs(
    title = "Danceability of Top 10 Genres",
    x     = "Genre",
    y     = "Danceability"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 80, hjust = 1),
    legend.position = "none"
  )
colnames(spotify_data)
# Filter to numeric columns and standardize those columns
spotify_standard = spotify_data %>% 
  select(`Duration (ms)`,Popularity, Explicit, Danceability,Energy,Key,Loudness,Mode,Speechiness,Acousticness,Instrumentalness,Liveness,Valence,Tempo,`Time Signature`) %>% 
  mutate(across(everything(), scale)) 

spotify_rest = spotify_data %>% 
  select(-c(`Duration (ms)`,Popularity, Explicit, Danceability,Energy,Key,Loudness,Mode,Speechiness,Acousticness,Instrumentalness,Liveness,Valence,Tempo,`Time Signature`))

colnames(spotify_standard)
head(spotify_standard)
colnames(spotify_rest)

# Combine the standardized data with the rest of the data
spotify_combined <- spotify_data %>%
  mutate(
    across(
      c(
        `Duration (ms)`, Popularity, Explicit, Danceability, Energy, Key,
        Loudness, Mode, Speechiness, Acousticness, Instrumentalness,
        Liveness, Valence, Tempo, `Time Signature`
      ),
      scale
    )
  )

colnames(spotify_combined)

head(spotify_combined)

# K-means clustering
num_mat <- as.matrix(spotify_standard)
num_mat
wss <- map_dbl(1:10, function(k) {
  kmeans(num_mat, centers = k, nstart = 25)$tot.withinss
})

# Elbow plot
elbow_df <- tibble(k = 1:10, WSS = wss)
ggplot(elbow_df, aes(x = k, y = WSS)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Elbow Method: Total WSS vs. Number of Clusters",
    x     = "Number of clusters k",
    y     = "Total within-cluster sum of squares"
  ) +
  theme_minimal()

# Fitting k-means
km_res <- kmeans(num_mat, centers = 4, nstart = 25)

# Add cluster results to the original data
spotify_clustered <- spotify_combined %>%
  mutate(cluster = factor(km_res$cluster))

# Centriods
centroids <- as.data.frame(km_res$centers) %>%
  rownames_to_column(var = "cluster") %>%
  mutate(cluster = factor(cluster))

print(centroids)

centroids_long <- centroids %>%
  pivot_longer(-cluster, names_to = "feature", values_to = "z_score")

# Summarize clusters
cluster_summaries <- spotify_clustered %>%
  group_by(cluster) %>%
  summarise(
    across(
      c(`Duration (ms)`, Popularity, Explicit, Danceability, Energy, Key,
        Loudness, Mode, Speechiness, Acousticness, Instrumentalness,
        Liveness, Valence, Tempo, `Time Signature`),
      mean, .names = "mean_{.col}"
    ),
    .groups = "drop"
  )
print(cluster_summaries)

# Plots:
cluster_genre_tbl <- spotify_clustered %>%
  count(cluster, Genre) %>%
  group_by(cluster) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

print(cluster_genre_tbl)

# 1) Scatter of two features (Danceability vs. Energy) colored by cluster
ggplot(spotify_clustered, aes(x = Danceability, y = Energy, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "Clusters on Danceability vs. Energy",
    x     = "Danceability (z-score)",
    y     = "Energy (z-score)",
    color = "Cluster"
  ) +
  theme_minimal()

# 2) Bar chart of genre counts within each cluster
#    (assumes you have cluster_genre_tbl from `count(cluster, Genre)`)
ggplot(cluster_genre_tbl, aes(x = cluster, y = n, fill = Genre)) +
  geom_col(position = "stack") +
  labs(
    title = "Genre Composition by Cluster",
    x     = "Cluster",
    y     = "Number of Tracks",
    fill  = "Genre"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12))
