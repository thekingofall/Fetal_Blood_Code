from collections import Counter
import pandas as pd
import numpy as np
import seaborn as sb
import matplotlib.pyplot as plt

def calculate_TCRVDJ_heatmap(input_adata, input_TRBVlist_values, input_IR='IR_VDJ_1_v_call', input_cell_type='Naïve CD4 T', input_adjusted_id='T12.P25'):
    selected_adata = input_adata[input_adata.obs['Last_cell_type'].isin([input_cell_type])]
    selected_adata.obs['Organ_cell_type'] = selected_adata.obs['Last_cell_type'].astype('str') + "_" + selected_adata.obs['AdjustedID'].astype('str')
    
    TCR_chain_data = None
    selected_ct = sorted(selected_adata.obs['Organ_cell_type'].unique())
    input_dict = {
       input_IR : input_TRBVlist_values}
    
    for dict_name in list(input_dict.keys()):
        intermediate_df = pd.DataFrame(columns = input_dict[dict_name],  index = selected_ct)
        
        for cell in intermediate_df.index:
            counter = Counter(selected_adata.obs.loc[selected_adata.obs['Organ_cell_type'] == cell, dict_name])
            for chain_type in intermediate_df.columns:
                intermediate_df.loc[cell, chain_type] = counter[chain_type]
            
            intermediate_df.loc[cell, :] = intermediate_df.loc[cell, :] / np.sum(intermediate_df.loc[cell, :]) * 100
        
        if TCR_chain_data is None:
            TCR_chain_data = intermediate_df.copy()
        else:
            TCR_chain_data = pd.concat([TCR_chain_data, intermediate_df], axis=1)
    
    fig, ax = plt.subplots(figsize=(TCR_chain_data.shape[1]/2, TCR_chain_data.shape[0]/2))
    uniform_data = TCR_chain_data.astype('float')
    ax = sb.heatmap(uniform_data, cmap='RdYlBu_r', yticklabels=True)
    _, xlabels = plt.xticks()
    _, ylabels = plt.yticks()
    ax.set_xticklabels(xlabels, size=20, rotation=90)
    ax.set_yticklabels(ylabels, size=20, rotation=0)
    
    return TCR_chain_data, fig


from collections import Counter
import pandas as pd
import numpy as np
import seaborn as sb
import matplotlib.pyplot as plt

def calculate_TCRVDJ_heatmap2(input_adata, input_TRBVlist_values, input_IR='IR_VDJ_1_v_call', input_cell_type=['Naïve CD4 T'], input_adjusted_id='T12.P25'):
    selected_adata = input_adata[input_adata.obs['Last_cell_type'].isin(input_cell_type)]
    selected_adata.obs['Organ_cell_type'] = selected_adata.obs['Last_cell_type'].astype('str') + "_" + selected_adata.obs['AdjustedID'].astype('str')
    
    TCR_chain_data = None
    selected_ct = sorted(selected_adata.obs['Organ_cell_type'].unique())
    input_dict = {
       input_IR : input_TRBVlist_values}
    
    for dict_name in list(input_dict.keys()):
        intermediate_df = pd.DataFrame(columns = input_dict[dict_name],  index = selected_ct)
        
        for cell in intermediate_df.index:
            counter = Counter(selected_adata.obs.loc[selected_adata.obs['Organ_cell_type'] == cell, dict_name])
            for chain_type in intermediate_df.columns:
                intermediate_df.loc[cell, chain_type] = counter[chain_type]
            
            intermediate_df.loc[cell, :] = intermediate_df.loc[cell, :] / np.sum(intermediate_df.loc[cell, :]) * 100
        
        if TCR_chain_data is None:
            TCR_chain_data = intermediate_df.copy()
        else:
            TCR_chain_data = pd.concat([TCR_chain_data, intermediate_df], axis=1)
    
    fig, ax = plt.subplots(figsize=(TCR_chain_data.shape[1]/2, TCR_chain_data.shape[0]/2))
    uniform_data = TCR_chain_data.astype('float')
    ax = sb.heatmap(uniform_data, cmap='RdYlBu_r', yticklabels=True)
    _, xlabels = plt.xticks()
    _, ylabels = plt.yticks()
    ax.set_xticklabels(xlabels, size=20, rotation=90)
    ax.set_yticklabels(ylabels, size=20, rotation=0)
    
    return TCR_chain_data, fig



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