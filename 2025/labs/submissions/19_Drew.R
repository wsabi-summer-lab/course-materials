```{r}
#############
### SETUP ###
#############

# install.packages(c("cluster", "factoextra", "flexclust", "ggdendro", "ggplot2", "mclust", "tidyverse"))
library(cluster)      # for hierarchical clustering and silhouette analysis
library(factoextra)   # for clustering visualization and elbow method
library(flexclust)    # for k-means clustering
library(ggdendro)     # for dendrogram visualization
library(ggplot2)      # for plotting
library(mclust)       # for Gaussian Mixture Models
library(tidyverse)    # for data manipulation and visualization

set.seed(19)

####################
### SPOTIFY DATA ###
####################

# read in data
spotify_data <- read_csv("../data/19_spotify-train.csv.gz")

# peek at structure
glimpse(spotify_data)


#######
# Task 1: Exploratory Visualizations
#
# Make at least two visualizations to understand the playlist’s songs and features.
#######

# 1a) Distribution of genres (if you have a genre column)
#plot the distribution of most popular Genres
top_genres <- spotify_data %>%
  filter(!is.na(Genre)) %>%          # drop any missing genres
  count(Genre, name = "n") %>%       # count rows per genre
  arrange(desc(n)) %>%               # sort descending
  slice_head(n = 10)                 # keep only top 10

# 2. Plot
ggplot(top_genres, aes(x = reorder(Genre, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Most Popular Genres",
    x     = "Genre",
    y     = "Number of Songs"
  ) +
  theme_minimal(base_size = 14)



 
# 1b) Plot Danceability vs. Popularity colored by genre

top30_dance <- spotify_data %>%
  filter(!is.na(Danceability), !is.na(Popularity)) %>%
  arrange(desc(Danceability)) %>%
  slice_head(n = 30)

# 2. Plot Danceability vs. Popularity for those top‐30 tracks
ggplot(top30_dance, aes(x = Danceability, y = Popularity)) +
  geom_point(size = 3, alpha = 0.8, color = "darkgreen") +
  labs(
    title = "Top 30 Most Danceable Songs: Danceability vs Popularity",
    x     = "Danceability",
    y     = "Popularity (0–100)"
  ) +
  theme_minimal(base_size = 14)







```



Task 2: Filtering Dataset
```{r}
filtered_data <- read_csv("../data/19_spotify-train.csv.gz")

# 2. Keep only numeric columns
spotify_num <- spotify_data %>%
  select_if(is.numeric)

# 3. Standardize each numeric feature: (x - mean(x)) / sd(x)
spotify_scaled <- spotify_num %>%
  mutate(across(
    .cols = everything(),
    .fns  = ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)
  ))

# 4. Inspect the result
glimpse(spotify_scaled)
head(spotify_scaled)
```


Task 3: Clustering K-means
```{r}
# 3a) Choose k by Elbow (WSS) & Silhouette
fviz_nbclust(spotify_scaled, kmeans, method = "wss") +
  labs(
    title = "Elbow Method for Optimal k",
    x     = "Number of Clusters (k)",
    y     = "Within-Cluster Sum of Squares (WSS)"
  ) +
  theme_minimal(base_size = 14)

#3b run K means
k <- 5  # assuming k=5 based on elbow method
kmeans_result <- kmeans(spotify_scaled, centers = k, nstart = 25)
# add cluster labels back to original
spotify_data <- spotify_data %>%
  mutate(Cluster = as.factor(kmeans_result$cluster))

```




Task 4: Interpret & Visualize Clusters
```{r}
# 4a) Visualize clusters in 2D via PCA
pca_result <- prcomp(spotify_scaled, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x) %>%
  mutate(Cluster = spotify_data$Cluster)
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "PCA of Spotify Clusters",
    x     = "Principal Component 1",
    y     = "Principal Component 2"
  ) +
  theme_minimal(base_size = 14)

# 4b) Examine song examples from each cluster
cluster_examples <- spotify_data %>%
  group_by(Cluster) %>%
  slice_head(n = 3) %>%  # get top 3 songs per cluster
  ungroup() %>%
  select(Cluster, track, `Artist Name(s)`, Danceability, Popularity)

```



