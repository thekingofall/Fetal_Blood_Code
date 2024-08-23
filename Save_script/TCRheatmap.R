library(tidyverse)
library(scales)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ComplexHeatmap)

TCR_Data_heatmap <- function(Data_chainlong, DataVlist_values) {
  Data <- Data_chainlong
  Data$variable <- factor(Data$variable, levels = DataVlist_values)
  
  split_Data <- strsplit(Data$Sample, "_")
  Data$CellType <- sapply(split_Data, "[", 1)
  Data$AdjusteID <- sapply(split_Data, "[", 2)
  Data$Organ <- substr(Data$AdjusteID, 1, 1)
  Data$Sample <- factor(Data$Sample, levels = rev(sort(unique(Data$Sample))))
  Data2 <- Data %>% group_by(variable) %>% filter(sum(as.numeric(value)) != 0) %>% ungroup()
  
  Data2$CellType_Organ <- paste(Data2$CellType, Data2$Organ, sep = "_")
  wide_data <- spread(Data2, variable, value)
  rownames(wide_data) <- wide_data$Sample
  wide_data$Sample <- NULL
  
  annotations <- Data2 %>% 
    select(Sample, CellType, Organ) %>% 
    distinct() %>% 
    column_to_rownames("Sample")
  
  annotations[] <- lapply(annotations, as.factor)
  
  wide_data <- wide_data %>%
    mutate(AdjustedID_num = as.numeric(str_extract(AdjusteID, "\\d+\\.\\d")))
  
  wide_data$Organ <- factor(wide_data$Organ, levels = c("B", "L", "T", "S"))
  wide_data <- wide_data %>% arrange(CellType, Organ, AdjustedID_num)
  wide_data <- wide_data %>% select(-AdjustedID_num)
  
  mat <- sapply(wide_data[,-c(1:4)], as.numeric)
  rownames(mat) <- wide_data$AdjusteID
  
  annotation_colors <- list(
    CellType = c('Naïve CD4 T' = "#fa6e01", 'Naïve CD8 T' = "#993333"),
    Organ = c(B = "#F6313E", L = "#fb862b", T = "#0eb0c8", S = "#6a73cf")
  )
  
  color_vector <- rev(colorRampPalette(brewer.pal(11, "RdYlBu"))(100))
  row_anno <- rowAnnotation(df = annotations, col = annotation_colors)
  
  row_split_vector <- c(rep(1, 28), rep(2, nrow(mat) - 28))
  
  Data_heatmap <- Heatmap(mat, 
                              name = "value",
                              col = color_vector,
                              cluster_rows = FALSE, row_title = NULL,
                              cluster_columns = FALSE,
                              rect_gp = gpar(col = "white", lwd = 0.5), column_title = "TCRAV",
                              row_split = row_split_vector, show_row_names = TRUE
  )      
  rt=list(row_anno,Data_heatmap)
  return(rt)
}




TCRgene_heatmap <- function(TCRH_chainlong, IGHlistvalues,titlename='IGHV') {
  library(tidyverse)
  library(scales)
  library(ggplot2)
  library(dplyr)
  library(RColorBrewer)
  library(ComplexHeatmap)
  
  TCRH = TCRH_chainlong
  TCRH$variable <- factor(TCRH$variable, level = IGHlistvalues)
  
  split_TCRH <- strsplit(TCRH$Sample, "_")
  
  TCRH$CellType <- sapply(split_TCRH, "[", 1)
  TCRH$AdjusteID <- sapply(split_TCRH, "[", 2)
  
  TCRH$Organ <- substr(TCRH$AdjusteID, 1, 1)
  TCRH$Sample <- factor(TCRH$Sample, level = rev(sort(unique(TCRH$Sample))))
  
  TCRH2 <- TCRH %>% group_by(variable) %>% filter(sum(as.numeric(value)) != 0) %>% ungroup()
  
  TCRH2$CellType_Organ <- paste(TCRH2$CellType, TCRH2$Organ, sep = "_")
  
  wide_data <- spread(TCRH2, variable, value)
  rownames(wide_data) <- wide_data$Sample
  wide_data$Sample <- NULL 
  
  annotations2 <- TCRH2 %>% 
    select(Sample, CellType, Organ) %>% 
    distinct() %>% 
    column_to_rownames("Sample")
  
  wide_data <- wide_data %>%
    mutate(AdjustedID_num = as.numeric(str_extract(AdjusteID, "\\d+\\.\\d")))
  
  wide_data$Organ <- factor(wide_data$Organ, levels = c("B", "L", "T", "S"), ordered = TRUE)
  wide_data <- wide_data %>% arrange(CellType, Organ, AdjustedID_num)
  
  wide_data <- wide_data %>% 
    select(-AdjustedID_num)
  
  mat <- sapply(wide_data[,-c(1:4)], as.numeric)
  
  rownames(mat) <- wide_data$AdjusteID
  
  annotations2$Organ = factor(annotations2$Organ, levels = c("B", "L", "T", "S"), labels = c("PBMC", "Liver", "Thymus", "Spleen"), ordered = TRUE)
  
  sorted_annotations <- annotations2 %>%
    arrange(CellType, Organ)
  
  annotation_colors2 <- list(
    CellType =  c('Naïve CD4 T' = "#fa6e01", 'Naïve CD8 T' = "#993333"),
    Organ = c(PBMC = "#F6313E", Liver = "#fb862b", SThymus = "#6a73cf", Thymus = "#0eb0c8", Spleen = "#6a73cf")
    #     CellType = c('IGKC+Naïve B' = "#0081C9", 'IGKC-Naïve B' = "#0f6657")
  )
  
  row_anno3 <- rowAnnotation(df = sorted_annotations, col = annotation_colors2)
  
  row_split_vector <- c(rep(1, 28), rep(2, nrow(mat) - 28))
  row_split_vector <- c(rep(1, 21), rep(2, 2),rep(3, 3),rep(4, 2),rep(5, 21),rep(6,2),rep(7,3),rep(8,nrow(mat)-21-2-3-2-21-2-3))
  TCRH_heatmap <- Heatmap(mat, 
                          name = "value",
                          col = c("#313695", "#649AC7", "#FFFFBF", "#FDBE70", "#EA5839", "#A50026"),
                          cluster_rows = FALSE, 
                          row_title = NULL ,
                          cluster_columns = FALSE,
                          
                          column_title = titlename,
                          row_split = row_split_vector,
                          show_row_names = FALSE)
  
  return(list(row_anno3 , TCRH_heatmap))
}
# TCRHVheatmap <- TCRgene_heatmap(TCRH_chainlong, TRBVlist_values,titlename='TRBV')
# # print(heatmap)
# TCRHVheatmap[[1]]+TCRHVheatmap[[2]]
# row_anno3+TCRH_heatmap

