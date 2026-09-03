# Spotify Prediction Model

library(MLmetrics)
library(tidyverse)
library(dplyr)

set.seed(80085)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

spotify_data = read_csv("19_spotify-train.csv.gz")

head(spotify_data)

# At least 10 songs added

spotify_data <- spotify_data %>%
  group_by(`Added by`) %>%
  filter(n() >= 10) %>%
  ungroup()

# Preparing Data

spotify_data$`Release Date` <- as.Date(spotify_data$`Release Date`, format = "%m/%d/%y")

spotify_data$Years_Since_Release <- as.numeric(difftime(Sys.Date(), 
                                    spotify_data$`Release Date`, 
                                    units = "days")) / 365.25

factor_columns <- c("Album Name", "Artist Name(s)", "Explicit", "Record Label", "Genre")
spotify_data[factor_columns] <- lapply(spotify_data[factor_columns], as.factor)

numeric_columns <- c("Duration (ms)", "Popularity", "Danceability", "Energy", "Key", "Loudness", "Mode", 
                     "Speechiness", "Acousticness", "Instrumentalness", "Liveness", "Valence", "Tempo")
spotify_data[numeric_columns] <- lapply(spotify_data[numeric_columns], as.numeric)

# Data Inspection

# Numeric

long_data <- spotify_data %>%
  select(all_of(numeric_columns), `Added by`) %>%
  pivot_longer(cols = -`Added by`, names_to = "feature", values_to = "value")

ggplot(long_data, aes(x = `Added by`, y = value, fill = `Added by`)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  facet_wrap(~ feature, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Clearly not predictive Variables: Mode, Loudness, Key, Liveness

# Non-numeric

long_data_factors <- spotify_data %>%
  select(all_of(factor_columns), `Added by`) %>%
  pivot_longer(cols = -`Added by`, names_to = "feature", values_to = "value")

ggplot(long_data_factors, aes(x = value, fill = `Added by`)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ feature, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Factor Level", y = "Count", fill = "Added by")

# Too many unique Genres/Artists/Albums/Record Labels

# Release Date

long_data_release <- spotify_data %>%
  select(Years_Since_Release, `Added by`) %>%
  mutate(feature = "Years_Since_Release") %>%
  rename(value = Years_Since_Release)

ggplot(long_data_release, aes(x = `Added by`, y = value, fill = `Added by`)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(y = "Years_Since_Release") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Ridge Regression for numeric variable selection

library(glmnet)
library(caret)
library(dplyr)
library(tidyr)

spotify_data$Explicit_binary <- ifelse(spotify_data$Explicit == "TRUE", 1, 0)

numeric_columns <- c("Duration (ms)", "Popularity", "Danceability", "Energy",
                     "Speechiness", "Acousticness", 
                     "Instrumentalness", "Valence", "Tempo",
                     "Explicit_binary", "Years_Since_Release")

X <- spotify_data[, numeric_columns]

X <- as.data.frame(lapply(X, as.numeric))

X_mat <- as.matrix(do.call(cbind, lapply(X, function(col) {
  col[is.na(col)] <- mean(col, na.rm = TRUE)
  col
})))

y <- as.factor(spotify_data$`Added by`)

train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- as.matrix(X_mat[train_idx, ])
X_test  <- as.matrix(X_mat[-train_idx, ])
y_train <- y[train_idx]
y_test  <- y[-train_idx]

# Ensemble Model

ridge_fit <- cv.glmnet(
  X_train, y_train, 
  alpha = 0,
  family = "multinomial"
)

plot(ridge_fit)

lasso_fit <- cv.glmnet(
  X_train, y_train, 
  alpha = 1,
  family = "multinomial"
)

plot(lasso_fit)

# Cross Entropy Loss (Ridge)

pred_probs <- predict(ridge_fit, newx = X_test, s = "lambda.min", type = "response")

prob_matrix <- as.matrix(pred_probs[, , 1])

colnames(prob_matrix) <- levels(y_test)

ce_loss <- MLmetrics::MultiLogLoss(
  y_pred = prob_matrix,
  y_true = y_test
)

cat("Cross-Entropy Loss:", ce_loss, "\n")

# Cross Entropy Loss (Lasso)

pred_probs_lasso <- predict(lasso_fit, newx = X_test, s = "lambda.min", type = "response")

prob_matrix_lasso <- as.matrix(pred_probs_lasso[, , 1])

colnames(prob_matrix_lasso) <- levels(y)

ce_loss_lasso <- MLmetrics::MultiLogLoss(
  y_pred = prob_matrix_lasso,
  y_true = y_test
)

cat("Cross-Entropy Loss:", ce_loss_lasso, "\n")

######################
# Actual Final Model #
######################


final_model <- cv.glmnet(
  X_mat, y, 
  alpha = 1,
  family = "multinomial"
)

#predict(lasso_fit, newx = , s = "lambda.min", type = "response")

#ce_loss <- MLmetrics::MultiLogLoss(
#  y_pred = ,
#  y_true = 
#)

#cat("Cross-Entropy Loss:", ce_loss_lasso, "\n")


