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

# set seed
set.seed(19)

####################
### SPOTIFY DATA ###
####################

# read in data
spotify_data = read_csv("../data/19_spotify-train.csv.gz")

# preview data
head(spotify_data)

spotify_data = spotify_data %>%
  mutate(`Genre List` = str_split(Genre, ";\\s*"))

spotify_data_cleaned = spotify_data %>%
  select_if(is.numeric) %>%
  mutate(across(everything(), ~ scale(.) %>% as.vector()))

# Hierarchical clustering
dissimilarity_matrix = dist(spotify_data_cleaned, method = "euclidean")
hc = hclust(dissimilarity_matrix, method = "ward.D2")
# Plot dendrogram
fviz_dend(hc, k = 5, rect = TRUE, show_labels = FALSE) +
  labs(title = "Hierarchical Clustering Dendrogram") +
  theme_minimal()

spotify_long = spotify_data %>%
  select(-Genre) %>%
  unnest(`Genre List`)

spotify_wide = spotify_long %>%
  mutate(has_genre = 1) %>%
  pivot_wider(
    id_cols = `Track URI`,
    names_from = `Genre List`,
    values_from = has_genre,
    values_fill = list(has_genre = 0),
    values_fn = list(has_genre = max)
  )

genres = colnames(spotify_wide)[-1]

