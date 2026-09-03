"""
evaluate_final_model.py

Evaluate the saved final Bayesian softmax model on a new out-of-sample CSV
of the same format as the training data.

Usage:
    python evaluate_final_model.py path/to/new_data.csv.gz

    python3 evaluate_final_model.py {csv_path}

    python3 evaluate_final_model.py /Users/chrisbugs/summer-lab/2025/labs/data/19_spotify-train.csv.gz
"""

import sys
import pandas as pd
import numpy as np
from sklearn.metrics import log_loss, accuracy_score
from model.chrisbugs_competition import predict_proba
from data import load as data_loader

def main(csv_path: str):
    # 1) Load new data
    df_new = pd.read_csv(csv_path)
    # sanity check columns
    expected = set(data_loader.load_df().columns)
    assert set(df_new.columns) == expected, "Column mismatch vs. original data"

    # 2) True labels (encode to 0..K-1)
    y_true = df_new["Added by"].astype("category")
    # ensure same category ordering as used in training
    y_true = y_true.cat.set_categories(data_loader.load_df()["Added by"].astype("category").cat.categories)
    y = y_true.cat.codes.to_numpy()

    # 3) Predict probabilities
    probs = predict_proba(df_new)  # shape (n_new, K)

    # 4) Compute log-loss
    loss = log_loss(y, probs, labels=np.arange(probs.shape[1]))
    print(f"Out-of-sample cross-entropy (log-loss): {loss:.4f}")

    # 5) Compute accuracy (most likely class)
    y_pred = np.argmax(probs, axis=1)
    acc    = accuracy_score(y, y_pred)
    print(f"Out-of-sample accuracy: {acc:.4%}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python evaluate_final_model.py <new_data.csv.gz>")
        sys.exit(1)
    main(sys.argv[1])
