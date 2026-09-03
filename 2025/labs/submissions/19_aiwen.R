#############
### SETUP ###
#############

rm(list=ls())

# install.packages(c("cluster", "factoextra", "flexclust", "ggdendro", "ggplot2", "mclust", "tidyverse"))
library(cluster)      # for hierarchical clustering and silhouette analysis
library(factoextra)   # for clustering visualization and elbow method
library(flexclust)    # for k-means clustering
library(ggdendro)     # for dendrogram visualization
library(ggplot2)      # for plotting
library(mclust)       # for Gaussian Mixture Models
library(tidyverse)    # for data manipulation and visualization
# install.packages("mclust")

# set seed
set.seed(19)

####################
### SPOTIFY DATA ###
####################

# read in data
spotify_data = read_csv("../data/19_spotify-train.csv.gz")

# preview data
head(spotify_data)
names(spotify_data)

##########################
# part 1 eda

# distn of genres
spotify_data %>% 
  separate_rows(Genre, sep = ";\\s*") %>%
  mutate(Genre = str_trim(Genre)) %>% 
  count(Genre, sort = TRUE) %>% 
  slice_max(n, n = 20) %>% 
  ggplot(aes(fct_reorder(Genre, n), n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Individual Genres in Collaborative Playlist",
       x = NULL, y = "Number of Songs")

# most common key signatures: using "key" column & "mode" (1 = major, 0 = minor)
spotify_data <- spotify_data %>% 
  mutate(key_letter = factor(Key, levels = 0:11, labels = c("C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B")),
         mode_label = factor(Mode, levels = c(1, 0),
                             labels = c("Major", "Minor")))

spotify_data %>% 
  count(key_letter, mode_label) %>% 
  ggplot(aes(x = key_letter, y = n, fill = mode_label)) +
  geom_col(position = "dodge") +  
  labs(title = "Distribution of Key Signatures by Mode",
       x = "Key signature", y = "Number of songs",
       fill = "Mode") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##########################
# part 2 standardize numerical values
names(spotify_data)

num_mat <- spotify_data %>% 
  select(where(is.numeric)) %>%
  select(-c("Key", "Mode", "Time Signature")) %>% 
  scale()  
head(num_mat)

##########################
# part 3 k-means clustering

# elbow plot
wss <- map_dbl(1:10, ~ kmeans(num_mat, centers = .x, nstart = 25)$tot.withinss)
elbow_df <- tibble(k = 1:10, WSS = wss)

ggplot(elbow_df, aes(k, WSS)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = 3, linetype = "dashed") +
  labs(title = "Elbow Plot for k-means on Playlist Songs",
       x = "Number of Clusters (k)", y = "Total Within-Cluster SS")

# 9 clusters?
best_k <- 9 
km_fit <- kmeans(num_mat, centers = best_k, nstart = 50) # nstart = how many different random sets of starting centroids to try

##########################
# interpret clusters
spotify_clust <- spotify_data %>% mutate(cluster = factor(km_fit$cluster))

# PCA scatter with clusters
pca_2d <- prcomp(num_mat, center = TRUE)$x[, 1:2] %>% 
  as_tibble() %>% 
  set_names(c("PC1", "PC2"))
spotify_clust <- bind_cols(spotify_clust, pca_2d)

ggplot(spotify_clust, aes(PC1, PC2, colour = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "k-means Clusters in PCA Space")

# cluster-level feature means 
cluster_profiles <- spotify_clust %>% 
  select(cluster, where(is.numeric)) %>% 
  group_by(cluster) %>% 
  summarise(across(everything(), mean), .groups = "drop")

print(cluster_profiles)

# append cluster to og dataset
spotify_data <- spotify_data %>% 
  mutate(cluster = factor(km_fit$cluster)) 

# genres in each cluster
genre_counts <- spotify_data %>%
  separate_rows(Genre, sep = ";\\s*") %>%
  mutate(Genre = str_trim(Genre)) %>% 
  count(cluster, Genre, sort = TRUE) 

top_genres <- genre_counts %>%                 
  group_by(cluster) %>%
  slice_max(n, n = 5, with_ties = FALSE) %>% # keep 5 most common genres
  mutate(rank = row_number()) %>% 
  summarise(
    top_genres = str_c(Genre, collapse = ", "),
    .groups = "drop"
  )
## haha looks like most of cluster 2 is audrey, cluster 8 could be me
## 4 is JP? only two songs tho