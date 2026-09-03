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

library(xgboost)

# set seed
set.seed(19104)

####################
### SPOTIFY DATA ###
####################

# read in data
spotify_data = read_csv("../data/19_spotify-train.csv.gz")

# preview data
head(spotify_data)

spotify_data = spotify_data %>%
  mutate(`Genre List` = str_split(Genre, ";\\s*")) %>%
  mutate(`Artist List` = str_split(`Artist Name(s)`, ",\\s*"))

spotify_data_cleaned = spotify_data %>%
  select_if(is.numeric) %>%
  mutate(across(everything(), ~ scale(.) %>% as.vector()))

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

# Join spotify_wide to spotify_data_cleaned
spotify_data_cleaned = spotify_data_cleaned %>%
  mutate(`Track URI` = spotify_data$`Track URI`) %>%
  left_join(spotify_wide, by = "Track URI") %>%
  mutate(`Added by` = spotify_data$`Added by`) %>%
  group_by(`Added by`) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(Added_by_label = as.numeric(as.factor(`Added by`)) - 1)

#---

spotify_long = spotify_data %>%
  select(-`Artist Name(s)`) %>%
  unnest(`Artist List`)

spotify_wide = spotify_long %>%
  mutate(has_artist = 1) %>%
  pivot_wider(
    id_cols = `Track URI`,
    names_from = `Artist List`,
    values_from = has_artist,
    values_fill = list(has_artist = 0),
    values_fn = list(has_artist = max)
  )

# Join spotify_wide to spotify_data_cleaned
spotify_data_cleaned = spotify_data_cleaned %>%
  mutate(`Track URI` = spotify_data$`Track URI`) %>%
  left_join(spotify_wide, by = "Track URI") %>%
  mutate(`Added by` = spotify_data$`Added by`) %>%
  group_by(`Added by`) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(Added_by_label = as.numeric(as.factor(`Added by`)) - 1)

X <- as.matrix(spotify_data_cleaned[, !names(spotify_data_cleaned) %in% c("Track URI", "Added by", "Added_by_label")])
y <- spotify_data_cleaned$Added_by_label

quick_model <- xgboost(data = X, label = y, nrounds = 20, objective = "multi:softprob", num_class = length(unique(y)), verbose = 0)

importance <- xgb.importance(model = quick_model)

top_features <- importance$Feature[1:30]
X_top <- X[, top_features]

params <- list(
  objective = "multi:softmax",
  eval_metric = "mlogloss",
  num_class = num_class,
  max_depth = 6,
  eta = 0.3,
  subsample = 0.8,
  colsample_bytree = 0.8
)

dtrain <- xgb.DMatrix(data = X_top, label = y)
xgb_cv <- xgb.cv(params, dtrain, nrounds = 100, nfold = 5, early_stopping_rounds = 10, verbose = 1, maximize = FALSE)
final_model <- xgb.train(params, dtrain, nrounds = xgb_cv$best_iteration, verbose = 0)