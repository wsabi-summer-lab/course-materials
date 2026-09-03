"""
Hi JP - this is the file for loading and preparing the data.

Run this once to create `preprocessor.joblib`, then import the helper
functions from other scripts.

- Chris

TODO: This is what to do to run the model
Here is what to do to run this:

Change input path to your data file, then run this command:
cd data
python3 load.py

After that, run this:
cd ..
python3 model/chrisbugs_competition.py

then evaluate:
python3 model/evaluate_final_model.py {your csv path here}

ex: python3 evaluate_final_model.py /Users/chrisbugs/summer-lab/2025/labs/data/19_spotify-train.csv.gz
"""
import pandas as pd
import numpy as np
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.pipeline import Pipeline
import joblib
from pathlib import Path

# Replace with your data path
INPUT_PATH = "../../../data/19_spotify-train.csv.gz"
# Preprocessor path for saving the preprocessing pipeline
PREPROCESSOR_PATH = Path(__file__).resolve().parent / "preprocessor.joblib"

# Column type split (for standardizing)
continuous_cols = [
    "Duration (ms)", "Popularity", "Loudness",
    "Danceability", "Energy", "Speechiness", "Acousticness",
    "Instrumentalness", "Liveness", "Valence", "Tempo"
]
binary_cols = ["Explicit"]
categorical_cols = ["Genre"]


# Helper func to load the data
def load_df(path: Path = INPUT_PATH) -> pd.DataFrame:
    df = pd.read_csv(path)
    return df


# Function to build and save the preprocessor
def build_preprocessor() -> ColumnTransformer:
    # Data preprocessing: standardize continuous features, encode categorical features, and leave the binary
    preproc = ColumnTransformer(
        transformers=[
            ("num", StandardScaler(), continuous_cols),
            ("cat", OneHotEncoder(handle_unknown="ignore", sparse_output=False), categorical_cols),
            ("passthrough", "passthrough", binary_cols),
        ],
        remainder="drop",
    )
    return preproc


# Function to prepare the data for modeling
def prepare_data(df: pd.DataFrame, *, fit_transform: bool = True):
    # Target variable
    y = df["Added by"].astype("category")
    class_names = list(y.cat.categories)
    y_idx = y.cat.codes.to_numpy()

    if fit_transform:
        preproc = build_preprocessor()
        X = preproc.fit_transform(df).astype("float32")
        joblib.dump(preproc, PREPROCESSOR_PATH)  # persist for reuse
    else:
        preproc = joblib.load(PREPROCESSOR_PATH)
        X = preproc.transform(df).astype("float32")

    return X, y_idx, class_names, preproc


if __name__ == "__main__":
    # Test
    df = load_df()
    print("Columns:", df.columns.tolist())

    X, y_idx, class_names, _ = prepare_data(df, fit_transform=True)
    print(f"Prepared X shape = {X.shape}  |  y shape = {y_idx.shape}\n")
    print(f"Users (classes): {class_names}")

    # Save df to csv
    # output_path = "/Users/chrisbugs/Documents/Projects/spotify-competition.csv"
    # df.to_csv(output_path, index=False)
