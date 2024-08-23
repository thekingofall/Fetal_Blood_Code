# %%R  -w 3200 -h 2400 -r 300 -i BCRH_chainlong -i IGHlistvalues 

BCRTR_heatmap <- function(BCRH_chainlong, IGHlistvalues,titlename='IGHV') {
  library(tidyverse)
  library(scales)
  library(ggplot2)
  library(dplyr)
  library(RColorBrewer)
  library(ComplexHeatmap)
  
  BCRH = BCRH_chainlong
  BCRH$variable <- factor(BCRH$variable, level = IGHlistvalues)

  split_BCRH <- strsplit(BCRH$Sample, "_")

  BCRH$CellType <- sapply(split_BCRH, "[", 1)
  BCRH$AdjusteID <- sapply(split_BCRH, "[", 2)

  BCRH$Organ <- substr(BCRH$AdjusteID, 1, 1)
  BCRH$Sample <- factor(BCRH$Sample, level = rev(sort(unique(BCRH$Sample))))

  BCRH2 <- BCRH %>% group_by(variable) %>% filter(sum(as.numeric(value)) != 0) %>% ungroup()
  
  BCRH2$CellType_Organ <- paste(BCRH2$CellType, BCRH2$Organ, sep = "_")

  wide_data <- spread(BCRH2, variable, value)
  rownames(wide_data) <- wide_data$Sample
  wide_data$Sample <- NULL 

  annotations2 <- BCRH2 %>% 
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
    Organ = c(PBMC = "#F6313E", Liver = "#fb862b", SThymus = "#6a73cf", Thymus = "#0eb0c8", Spleen = "#6a73cf"),
    CellType = c('Naïve B' = "#0081C9")
  )

  row_anno3 <- rowAnnotation(df = sorted_annotations, col = annotation_colors2)

#   row_split_vector <- c(rep(1, 29), rep(2, nrow(mat) - 29))
#     row_split_vector <- c(rep(1, 21), rep(2, 4),rep(3, 2),rep(4, 2),rep(5, 21),rep(6,4),rep(7,2),rep(8,nrow(mat)-21-4-2-2-21-4-2))
  BCRH_heatmap <- Heatmap(mat, 
    name = "value",
   col = c("#313695", "#649AC7", "#FFFFBF", "#FDBE70", "#EA5839", "#A50026"),
    cluster_rows = FALSE, 
    row_title = NULL ,
    cluster_columns = FALSE,
    # rect_gp = gpar(col = "white", lwd = 0.01),
    column_title = titlename,

    show_row_names = T)

  return(list(row_anno3 , BCRH_heatmap))
}

# # BCRH_chainlong$value[which(as.numeric(BCRH_chainlong$value)==30)] <- median(BCRH_chainlong$value)
# BCRH_chainlong$value[which(as.numeric(BCRH_chainlong$value)>=25)] <-  median(BCRH_chainlong$value)
# BCRHVheatmap <- BCRTR_heatmap(BCRH_chainlong, IGHlistvalues,titlename='IGHV')
# # print(heatmap)
# BCRHVheatmap[[1]]+BCRHVheatmap[[2]]
# # row_anno3+BCRH_heatmap