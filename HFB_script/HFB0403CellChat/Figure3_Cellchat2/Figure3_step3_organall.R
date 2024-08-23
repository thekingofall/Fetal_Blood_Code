rm(list = ls())
library(dplyr)
library(stringr)
library(CellChat)
adata1 = readRDS("/home/maolp/data5/Gaofeng_All_matrix/Allcount/All_scanpyData/Data/scdata.rds")
# Add a new column to the metadata
adata1@meta.data <- adata1@meta.data %>% mutate(
  Category = case_when(
    # Identify the initial letter
    str_detect(AdjustedID, "^B") ~ "PBMC",
    str_detect(AdjustedID, "^L") ~ "Liver",
    str_detect(AdjustedID, "^T") ~ "Thymus",
    str_detect(AdjustedID, "^S") ~ "Spleen",
    TRUE ~ "Other"  # for any other cases
  ),
  NumericPart = as.numeric(str_extract(AdjustedID, "\\d+\\.\\d+")),  # extract the numeric part
  Stage = ifelse(NumericPart <= 26, "Early", "Late")  # classify as "Early" or "Late"
)

 adata1@meta.data$GroupStat<-paste0(adata1@meta.data$Category,"_",adata1@meta.data$Stage)
unique( adata1@meta.data$Last_cell_type)
# 假设你的数据框叫做 df，而你想修改的列名为 column_name
adata1@meta.data$Last_cell_type2=adata1@meta.data$Last_cell_type

# 替换左括号 "(" 为一个空格
adata1@meta.data$Last_cell_type <- gsub("\\(", "", adata1@meta.data$Last_cell_type)

# 替换右括号 ")" 为一个空格
adata1@meta.data$Last_cell_type <- gsub("\\)", "", adata1@meta.data$Last_cell_type)
process_cellchat233 <- function(adata,  id_value) {
  subset_data <- subset(adata, GroupStat== id_value)
  data.pbmc <- subset_data@assays$RNA@data
  # Use cell_type_label as the name of the column to use for group labels
  cellmeta <- data.frame(group = as.character(subset_data$Last_cell_type), row.names = names(subset_data$Last_cell_type))
  #   print("test3")
  print(paste("Cellmeta rows: ", nrow(cellmeta)))
  print(paste("Subset data cells: ", length(subset_data@meta.data$Last_cell_type)))
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

GroupStat_ids <- c( "PBMC_Early","PBMC_Late" ,"Liver_Early","Thymus_Early", "Spleen_Early")
Organ_results <- lapply(GroupStat_ids, function(id) process_cellchat233(adata1, id))
saveRDS(Organ_results, file = "/home/maolp/Allcount/All_scanpyData/Cellchat/Organ_results_rmcl2.rds")