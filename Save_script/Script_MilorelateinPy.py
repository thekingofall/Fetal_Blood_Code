import os
import pandas as pd
import numpy as np
import anndata
import pandas as pd

def save_data_to_files(test_adata, path='test'):
    expr = test_adata.X.toarray()
    covariate = test_adata.obs[["test_label"]]
    gene_names = test_adata.var_names

    # Create the output directory if it doesn't exist
    os.makedirs(path, exist_ok=True)

    # Save expr, covariate, and gene_names to CSV files in the specified path
    np.savetxt(os.path.join(path, "expr.csv"), expr, delimiter=",")
    covariate.to_csv(os.path.join(path, "covariate.csv"), index=False)
    gene_names_series = pd.Series(gene_names)
    gene_names_series.to_csv(os.path.join(path, "gene_names.csv"), index=False)



def label_by_logfc_spatialfdr(row):
    logfc = row['logFC']
    spatialfdr = row['SpatialFDR']
    
    if spatialfdr < 0.1:
        if logfc < 0:
            return "early"
        elif logfc > 0:
            return "late"
        else:
            return "neither"
    else:
        return "not_significant"


def process_adata(adata, label):
    nhood_adata2 = adata.uns["nhood_adata"].copy()
    anno_nhood_adata2 = nhood_adata2[nhood_adata2.obs["nhood_annotation"].isin([label])]
    anno_nhood_adata2.obs['test_label'] = anno_nhood_adata2.obs.apply(label_by_logfc_spatialfdr, axis=1)
    X_mat2 = anno_nhood_adata2.obsm["expr"]
    test_adata2 = anndata.AnnData(X=X_mat2, obs=anno_nhood_adata2.obs, var=adata.var)
    test_adata2 = test_adata2[~test_adata2.obs["test_label"].isin(['not_significant'])]
    return test_adata2


import scanpy as sc
import pandas as pd
import numpy as np
import seaborn as sns
from sklearn.preprocessing import MinMaxScaler

def Meanexpscale(adata, group_by='AdjustedWeek', plot_heatmap=False ,output_path="/home/maolp/data5/Gaofeng_All_matrix/Allcount/All_scanpyData/Milo_Data/Stage_exp.csv"):
    """
    Process AnnData object, scale the data, and save to a CSV file.

    Parameters:
    - adata: AnnData object to be processed.
    - group_by: The column name in adata.obs to group by (e.g., 'AdjustedWeek').
    - output_path: The path to save the scaled data CSV file.
    - plot_heatmap: If True, plot a heatmap of the scaled data.
    """
    # Calculate the mean expression for each group
    df_mean = adata.to_df().groupby(adata.obs[group_by]).mean()

    # Initialize the MinMaxScaler
    scaler = MinMaxScaler()

    # Scale the data
    df_scaled = df_mean.apply(lambda x: scaler.fit_transform(x.values.reshape(-1, 1)).flatten(), axis=0)

    # Save the scaled data to a CSV file
    df_scaled.to_csv(output_path)

    # Optionally plot a heatmap
    if plot_heatmap:
        sns.heatmap(df_scaled.T)
        plt.show()

    return df_scaled.T

