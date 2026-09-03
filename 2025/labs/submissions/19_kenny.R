library(cluster)      # for hierarchical clustering and silhouette analysis
library(factoextra)   # for clustering visualization and elbow method
library(flexclust)    # for k-means clustering
library(ggdendro)     # for dendrogram visualization
library(ggplot2)      # for plotting
library(mclust)       # for Gaussian Mixture Models
library(tidyverse)    # for data manipulation and visualization

set.seed(19)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

spotify_data = read_csv("19_spotify-train.csv.gz")

head(spotify_data)

# Visualizations

ggplot(spotify_data, aes(x = Valence, y = Popularity)) +
  geom_point(size = 3, alpha = 0.7) +
  facet_wrap(~ `Added by`) +
  labs(title = "Popularity vs. Valence by Genre and User",
       x = "Valence (Positivity of Song)",
       y = "Popularity",
       color = "Genre") +
  theme_minimal()

ggplot(spotify_data, aes(x = Danceability, y = Energy, color = `Added by`)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Danceability vs. Energy by User",
       x = "Danceability",
       y = "Energy",
       color = "Genre") +
  theme_minimal()

# Filtering and Standardizing

spotify_numeric <- spotify_data %>%
  select(where(is.numeric))

spotify_scaled <- as.data.frame(scale(spotify_numeric))

# Clustering

length(unique(spotify_data$`Added by`))

kmeans_result <- kmeans(spotify_scaled, centers = 10, nstart = 25)

spotify_clustered <- spotify_scaled %>%
  mutate(Cluster = as.factor(kmeans_result$cluster))

# Cluster examining

ggplot(spotify_clustered, aes(x = Danceability, y = Energy, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "K-Means Clustering of Tracks",
       x = "Danceability",
       y = "Energy",
       color = "Cluster") +
  theme_minimal()

cluster_summary <- spotify_clustered %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")

cluster_summary

spotify_clustered <- cbind(Genre = spotify_data$Genre, spotify_scaled)
spotify_clustered$Cluster <- as.factor(kmeans_result$cluster)

spotify_clustered %>%
  separate_rows(Genre, sep = ";\\s*") %>%
  group_by(Cluster, Genre) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Cluster) %>%
  slice_max(n, n = 3) %>%
  arrange(Cluster, desc(n))

cluster_summary_long <- cluster_summary %>%
  pivot_longer(-Cluster, names_to = "Feature", values_to = "Mean")

ggplot(cluster_summary_long, aes(x = Feature, y = Mean, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Average Feature Values per Cluster") +
  theme_minimal()
