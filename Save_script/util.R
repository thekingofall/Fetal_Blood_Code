split_core_enrichment <- function(data, id_column, enrichment_column) {
  split_list <- strsplit(data[[enrichment_column]], split = "/")
  lengths <- sapply(split_list, length)
  replicated_ids <- rep(data[[id_column]], times = lengths)
  flat_vector <- unlist(split_list)
  new_df <- data.frame(ID = replicated_ids, core_enrichment = flat_vector)
  return(new_df)
}

## Usage
# new_df <- split_core_enrichment(df, 'ID', 'core_enrichment')
# print(new_df)

library(clusterProfiler)
library(org.Hs.eg.db)
library(fgsea)
library(msigdbr)

library(clusterProfiler)
library(org.Hs.eg.db)
library(DOSE)

perform_enrichment_analysis <- function(gene_df) {
  
    map_df <- bitr(gene_df$names, fromType = "SYMBOL", toType = c("ENTREZID"), OrgDb = org.Hs.eg.db)
    

    merged_df <- merge(gene_df, map_df, by.x = "names", by.y = "SYMBOL")
    
    enrichKEGG_result <- enrichKEGG(gene = merged_df$ENTREZID, organism = "hsa", 
                                    keyType = "kegg",
                                    pvalueCutoff = 0.05,
                                    pAdjustMethod = "BH",
                                    minGSSize = 10, maxGSSize = 500, 
                                    qvalueCutoff = 0.05,
                                    use_internal_data = FALSE)
    

    enrichGO_result <- enrichGO(merged_df$names, OrgDb = org.Hs.eg.db, ont = 'ALL', 
                                pAdjustMethod = 'BH', pvalueCutoff = 0.05, 
                                qvalueCutoff = 0.05, keyType = 'SYMBOL')
    
    
    return(list("KEGG" = enrichKEGG_result, "GO" = enrichGO_result))
}


perform_GO<- function(gene_df) {
  

    

    enrichGO_result <- enrichGO(gene_df, OrgDb = org.Hs.eg.db, ont = 'ALL', 
                                pAdjustMethod = 'BH', pvalueCutoff = 0.05, 
                                qvalueCutoff = 0.05, keyType = 'SYMBOL')
    
    
    return(enrichGO_result)
}

library(ggplot2)
library(ggrepel)
library(dplyr)

Interaction_plot <- function(data, plot_title) {
  # Filter the data
  da1 <- subset(data, outgoing >= 0 & incoming >= 0)
  da12 <- subset(da1, outgoing > 0.3 | incoming > 0.3)
  
  # Create the plot
  p <- ggplot(da1, aes(x = outgoing, y = incoming)) +
    geom_point(color= "black", fill="#490C65", size=3, shape=21) +
    theme_linedraw() +
    theme(panel.grid = element_blank()) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", size = 0.25) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.25) +
    theme(text = element_text(size = 10),
          legend.key.height = grid::unit(0.15, "in")) +
    labs(title = plot_title,
         x = "Differential outcoming interaction strength",
         y = "Differential incoming interaction strength") +
    theme(plot.title = element_text(size = NULL, hjust = 0.5, face = "plain")) +
    theme(axis.line.x = element_line(size = 0.25),
          axis.line.y = element_line(size = 0.25)) +
    theme(legend.title = element_blank()) +
    ggrepel::geom_text_repel(data = da12,
                             aes(label = labels),
                             color="#C71000FF",
                             size = 3,
                             show.legend = FALSE,
                             segment.size = 0.2,
                             segment.alpha = 0.5) +
    theme(aspect.ratio = 2)
  
  return(p)
}

# Example usage:
# Assuming 'your_data' is a data frame with 'outgoing', 'incoming', and 'labels' columns
# your_plot <- create_interaction_plot(your_data, "Your Plot Title")
# print(your_plot)




perform_wilcox_tests <- function(data, variable_col, organ_stage_col, value_col) {
  results <- data.frame(variable = character(),
                        OrganStageComparison = character(),
                        p_value = numeric(),
                        stringsAsFactors = FALSE) # Avoid automatic conversion to factors
  
  variables <- unique(data[[variable_col]])
  
  for (var in variables) {
    subset_data <- data[data[[variable_col]] == var, ]
    organ_stages <- unique(subset_data[[organ_stage_col]])
    
    for (i in 1:(length(organ_stages) - 1)) {
      for (j in (i + 1):length(organ_stages)) {
        group1 <- subset_data[subset_data[[organ_stage_col]] == organ_stages[i], ][[value_col]]
        # print(group1)
        group2 <- subset_data[subset_data[[organ_stage_col]] == organ_stages[j], ][[value_col]]
        
        # Ensure that group1 and group2 are numeric vectors
        if (is.numeric(group1) && is.numeric(group2)) {
          test_result <- wilcox.test(group1, group2)
          
          comparison_label <- paste(organ_stages[i], "vs", organ_stages[j])
          results <- rbind(results, data.frame(variable = var,
                                               OrganStageComparison = comparison_label,
                                               p_value = test_result$p.value))
        } else {
          warning(paste("Non-numeric data encountered for variable:", var,
                        "and organ stage comparison:", comparison_label))
        }
      }
    }
  }
  
  return(results)
}
# BCRH3results <- perform_wilcox_tests(BCRH3, "variable", "Organ_Stage", "value")