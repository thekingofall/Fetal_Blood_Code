import pandas as pd

def sort_categories(adata, column):
    categories = pd.Series(adata.obs[column].unique(), dtype='str')
    df = categories.str.extract('(\D)(\d+\.\d+)(_.*)')
    df.columns = ['letter', 'number', 'suffix']
    df['number'] = df['number'].astype(float)
    cat_type = pd.CategoricalDtype(categories=['B', 'L', 'T', 'S'], ordered=True)
    df['letter'] = df['letter'].astype(cat_type)
    df.sort_values(by=['letter', 'number'], inplace=True)
    df['sorted_category'] = df.apply(lambda row: row['letter'] + str(row['number']) + row['suffix'], axis=1)
    sorted_categories = df['sorted_category']
    adata.obs[column] = pd.Categorical(adata.obs[column], categories=sorted_categories, ordered=True)
    return adata


def save_ranked_genes_groups(borgan, path_prefix, file_prefix):
    df_ranked_genes = sc.get.rank_genes_groups_df(borgan, group=None)
    borgan.obs.to_csv(f"{path_prefix}/{file_prefix}_subB.csv")
    
    dataframes = []
    
    for group in ["Liver", "Spleen", "PBMC", "Thymus"]:
        df_group = df_ranked_genes[df_ranked_genes.group == group]
        df_group.to_csv(f"{path_prefix}/{file_prefix}_{group}.csv")
        dataframes.append(df_group)
    
    return dataframes

import scanpy as sc
import matplotlib.pyplot as plt

def SubBheatmap(adata, cell_type, output_file):

    sub_adata = adata[adata.obs["Last_cell_type"].isin(cell_type)]
    sub_adata.obs['New_Body'] = sub_adata.obs['New_Body'].astype('str')
    sc.tl.rank_genes_groups(sub_adata, groupby='New_Body', method='wilcoxon')
    sub_adata.layers['scaled'] = sc.pp.scale(sub_adata, copy=True).X
    sc.tl.dendrogram(sub_adata, groupby='New_Body')
    plt.style.use('default')
    plt.rcParams['figure.dpi'] = 300
    sc.pl.rank_genes_groups_matrixplot(sub_adata, n_genes=20, use_raw=False, vmin=-1, vmax=1, cmap='RdBu_r', layer='scaled', swap_axes=True, show=False, figsize=(5,12))
    plt.savefig(output_file, bbox_inches='tight')
    return sub_adata