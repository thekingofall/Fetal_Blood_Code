## ----setup, include=FALSE----------------------------------------------------------
# knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------------------------
adata1 = readRDS("/home/maolp/data5/Gaofeng_All_matrix/Allcount/All_scanpyData/Data/scdata.rds")
adata1@meta.data$Last_cell_type2=adata1@meta.data$Last_cell_type

# 替换左括号 "(" 为一个空格
adata1@meta.data$Last_cell_type <- gsub("\\(", "", adata1@meta.data$Last_cell_type)

# 替换右括号 ")" 为一个空格
adata1@meta.data$Last_cell_type <- gsub("\\)", "", adata1@meta.data$Last_cell_type)

## ----------------------------------------------------------------------------------
# %%R
library(CellChat)
process_cellchat2 <- function(adata, adjusted_id) {
  subset_data <- subset(adata, AdjustedID == adjusted_id)
  data.pbmc <- subset_data@assays$RNA@data
  cellmeta <- data.frame(group = as.character(subset_data$Last_cell_type), row.names = names(subset_data$Last_cell_type))
  
  cellchat <- createCellChat(object = data.pbmc)
  cellchat <- addMeta(cellchat, meta = cellmeta, meta.name = "labels")
  cellchat <- setIdent(cellchat, ident.use = "labels")
  
  CellChatDB <- CellChatDB.human
  CellChatDB.use <- subsetDB(CellChatDB, search = "Cell-Cell Contact", key = "annotation")
  cellchat@DB <- CellChatDB.use
  
  cellchat <- subsetData(cellchat)
  cellchat <- identifyOverExpressedGenes(cellchat)
  cellchat <- identifyOverExpressedInteractions(cellchat)
  cellchat <- projectData(cellchat, PPI.human)

  cellchat <- computeCommunProb(cellchat)
  cellchat <- computeCommunProbPathway(cellchat)
  cellchat <- aggregateNet(cellchat)
  
  return(cellchat)
}

# Get a list of all unique AdjustedID values
adjusted_ids <- levels(unique(adata1$AdjustedID))

# Run the function on all AdjustedID values and store the results in a list
results2 <- lapply(adjusted_ids, function(id) process_cellchat2(adata1, id))
print("start")
# Save the results as an RDS file
saveRDS(results2, file = "/home/maolp/Allcount/All_scanpyData/Data/Allsample_cellchat_results2.rds")

system("python /home/maolp/Email.py cellchat")

