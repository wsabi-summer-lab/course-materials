# Victoria's Spotify Prediction Model Evaluation
# Trains on 19_spotify-train.csv.gz and evaluates on 19_spotify-test.csv.gz

import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import log_loss
import warnings
warnings.filterwarnings('ignore')

# Set seed
np.random.seed(42)

# Load training data
spotify_train = pd.read_csv("../data/19_spotify-train.csv.gz")

# Filter to users with at least 10 songs
user_counts = spotify_train['Added by'].value_counts()
valid_users = user_counts[user_counts >= 10].index
spotify_train = spotify_train[spotify_train['Added by'].isin(valid_users)]

# Select numeric features
numeric_features = ['Duration (ms)', 'Popularity', 'Danceability', 'Energy', 'Loudness', 
                   'Speechiness', 'Acousticness', 'Instrumentalness', 'Liveness', 
                   'Valence', 'Tempo']

# Prepare training data
X_train = spotify_train[numeric_features].copy()
y_train = spotify_train['Added by'].copy()

# Handle missing values
X_train = X_train.fillna(X_train.mean())

# Scale features
scaler = StandardScaler()
X_train_scaled = scaler.fit_transform(X_train)

# Train Random Forest model
rf_model = RandomForestClassifier(
    n_estimators=200,
    max_depth=10,
    min_samples_split=5,
    min_samples_leaf=2,
    random_state=42,
    n_jobs=-1
)

rf_model.fit(X_train_scaled, y_train)

# Load test data
spotify_test = pd.read_csv("../data/19_spotify-test.csv.gz")

# Process test data the same way
spotify_test = spotify_test[spotify_test['Added by'].isin(valid_users)]

# Prepare test data
X_test = spotify_test[numeric_features].copy()
y_test = spotify_test['Added by'].copy()

# Handle missing values in test data
X_test = X_test.fillna(X_test.mean())

# Scale test features using training scaler
X_test_scaled = scaler.transform(X_test)

# Predict on test data
pred_probs = rf_model.predict_proba(X_test_scaled)

# Get true labels for test data
y_test_actual = y_test.values

# Calculate log loss
log_loss_score = log_loss(y_test_actual, pred_probs)

print("=== Victoria's Random Forest Model Test Log Loss ===")
print(f"Log Loss: {log_loss_score}")
print(f"Test rows processed: {len(spotify_test)}") 