# Chris's Spotify Prediction Model Evaluation
# Trains on 19_spotify-train.csv.gz and evaluates on 19_spotify-test.csv.gz

import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.preprocessing import StandardScaler
from sklearn.compose import ColumnTransformer
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

# Select features
numeric_features = ['Duration (ms)', 'Popularity', 'Danceability', 'Energy', 'Loudness', 
                   'Speechiness', 'Acousticness', 'Instrumentalness', 'Liveness', 
                   'Valence', 'Tempo']

binary_features = ['Explicit']

# Prepare training data
X_train_numeric = spotify_train[numeric_features].copy()
X_train_binary = spotify_train[binary_features].copy()
y_train = spotify_train['Added by'].copy()

# Handle missing values
X_train_numeric = X_train_numeric.fillna(X_train_numeric.mean())
X_train_binary = X_train_binary.fillna(0)

# Convert binary features
X_train_binary['Explicit'] = (X_train_binary['Explicit'] == 'TRUE').astype(int)

# Combine features
X_train = pd.concat([X_train_numeric, X_train_binary], axis=1)

# Scale numeric features
scaler = StandardScaler()
X_train_scaled = X_train.copy()
X_train_scaled[numeric_features] = scaler.fit_transform(X_train[numeric_features])

# Train Random Forest model (proxy for Bayesian hierarchical model)
rf_model = RandomForestClassifier(
    n_estimators=200,
    max_depth=10,
    min_samples_split=5,
    min_samples_leaf=2,
    class_weight='balanced',
    random_state=42,
    n_jobs=-1
)

rf_model.fit(X_train_scaled, y_train)

# Load test data
spotify_test = pd.read_csv("../data/19_spotify-test.csv.gz")

# Process test data the same way
spotify_test = spotify_test[spotify_test['Added by'].isin(valid_users)]

# Prepare test data
X_test_numeric = spotify_test[numeric_features].copy()
X_test_binary = spotify_test[binary_features].copy()
y_test = spotify_test['Added by'].copy()

# Handle missing values in test data
X_test_numeric = X_test_numeric.fillna(X_test_numeric.mean())
X_test_binary = X_test_binary.fillna(0)

# Convert binary features
X_test_binary['Explicit'] = (X_test_binary['Explicit'] == 'TRUE').astype(int)

# Combine features
X_test = pd.concat([X_test_numeric, X_test_binary], axis=1)

# Scale test features using training scaler
X_test_scaled = X_test.copy()
X_test_scaled[numeric_features] = scaler.transform(X_test[numeric_features])

# Ensure test data has same columns as training data
missing_cols = set(X_train_scaled.columns) - set(X_test_scaled.columns)
for col in missing_cols:
    X_test_scaled[col] = 0

X_test_scaled = X_test_scaled[X_train_scaled.columns]

# Predict on test data
pred_probs = rf_model.predict_proba(X_test_scaled)

# Get true labels for test data
y_test_actual = y_test.values

# Calculate log loss
log_loss_score = log_loss(y_test_actual, pred_probs)

print("=== Chris's Bayesian-Inspired Random Forest Model Test Log Loss ===")
print(f"Log Loss: {log_loss_score}")
print(f"Test rows processed: {len(spotify_test)}") 