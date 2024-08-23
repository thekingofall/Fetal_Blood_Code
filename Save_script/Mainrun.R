library(dplyr)
library(purrr)
library(tidyr)
library(broom)
##_______________TCR
### spearman
Dfcalculate_correlation <- function(df, group_var, var1, var2) {
  results_df <- df %>%
    group_by(.data[[group_var]]) %>%
    nest() %>%
    mutate(correlation = map(data, ~cor.test(.x[[var1]], .x[[var2]], method = "spearman")) %>% map(broom::tidy)) %>%
    unnest(correlation) %>%
    select(.data[[group_var]], estimate, p.value)
colnames(results_df)<-c('lengths','R_value', 'p_value')
  return(results_df)
}

### example: TRAresults_df <- calculate_correlation(TRAlong_ratio, "lengths", "week", "proportion")

plot_Cell_sum_ratio_celltype <- function(Cell_data) {

  Cell_sum_ratio <- melt(Cell_data, id = c("lengths"))
  Cell_sum_ratio$lengths <- factor(Cell_sum_ratio$lengths, level = unique(Cell_sum_ratio$lengths))

  colnames(Cell_sum_ratio) <- c('lengths', 'Sample', 'ratio')
  Cell_sum_ratio$ratio <- Cell_sum_ratio$ratio * 100
Cell_sum_ratio$proportion<- Cell_sum_ratio$ratio * 100
   Cell_sum_ratio$week <- as.numeric(substring(Cell_sum_ratio$Sample,2,4))
Cell_sum_ratio$time <-as.numeric(Cell_sum_ratio$week)
  
  return(Cell_sum_ratio)
}

###  create_pval_plot
create_pval_plot <- function(df,titlex) {
 
  df$pvalue_cat <- cut(df$p_value,
                       breaks = c(0, 0.0001, 0.001, 0.01, 0.05, Inf),
                       
                       labels = c('< 0.0001', '0.0001-0.001', '0.001-0.01',
                                  '0.01-0.05', '> 0.05'),
                       include.lowest = TRUE)

  colors <- c('< 0.0001' = "#C71000B2", '0.0001-0.001' =  "#FF6348B2", 
              '0.001-0.01' = "#FF95A8B2", '0.01-0.05' = "#8A4198B2" ,
              '> 0.05' ="#008EA0B2" )

 
  plot <- ggplot(df, aes(x = factor(lengths), y = R_value, fill = pvalue_cat)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = colors) +
      labs(x = "Lengths", y = "R Value(spearman) ", fill = "P Value") +
      theme_bw() +
      ggtitle(titlex) + 
      theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
           panel.border = element_rect(linetype = "solid", colour = "black", size = 1.5)) + 
      scale_y_continuous(breaks = seq(-1, 1, by = 0.2), labels = seq(-1, 1, by = 0.2))
  
  return(plot)
}


##TRApval_plot <- create_pval_plot(TRAresults_df,titlex="Percentage of TRA Lengths Over Time")
##print(TRApval_plot)


library(tidyverse)
library(reshape2)
library(gridExtra)
plot_PBMC_sum_ratio_celltype <- function(PBMC_data, plot_title = 'BCR IGH CDR3(aa) in PBMC') {

  PBMC_sum_ratio <- melt(PBMC_data, id = c("lengths"))
  PBMC_sum_ratio$lengths <- factor(PBMC_sum_ratio$lengths, level = unique(PBMC_sum_ratio$lengths))

  colnames(PBMC_sum_ratio) <- c('Length', 'Sample', 'ratio')
  PBMC_sum_ratio$ratio <- PBMC_sum_ratio$ratio * 100
   PBMC_sum_ratio$Week <- as.numeric(substring(PBMC_sum_ratio$Sample,2,4))
    p <- ggplot(PBMC_sum_ratio, aes(Length, ratio, color = Week, group=Sample)) +
  geom_point(shape=1,size=1) +
#   geom_line() +
#   geom_smooth(se=F, method='gam') +
     geom_smooth(se=F,size=0.3) +
  ggtitle(plot_title) +
  theme_linedraw() +
  xlab('Length') +
  ylab('ratio(%)') +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        panel.border = element_rect(linetype = "solid", colour = "black", size = 1.5))+
    scale_color_gradientn(colours = rev(colorRampPalette(brewer.pal(9, "Spectral"))(100)))
  
  return(p)
}

library(dplyr)
library(ggplot2)
library(gridExtra)

library(gridExtra)
library(ggplot2)

# Function to process data, calculate correlation, create plots, and arrange them in a grid
Immusig_plot <- function(data, regions, data_type, file_path) {
  plt_list <- list()  # Initialize an empty list to store plots


  for (region in regions) {
    print(region)
    if (data_type == "TRB") {
      processed_data <- process_TRB_data(data, region)
    } else if (data_type == "TRA") {
      processed_data <- process_TRA_data(data, region)
    } else {
      stop("Invalid data type")
    }

    plot <- Dfcalculate_correlation(processed_data, "Ntlen", "Week", "Freq_ratio") %>%
      create_pval_plot(titlex = region) +
      xlab("lengths(nt)")
    plt_list[[region]] <- plot
  }

  # Arrange plots in a single column grid
  grid_plots <- do.call(grid.arrange, c(plt_list, ncol = 1))

  # Save the grid of plots to the specified file path
  save_plot_grid(grid_plots, file_path)
}

# Function to save the grid of plots to a file
save_plot_grid <- function(grid_plots, file_path) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(plot = grid_plots, filename = file_path, width = 8, height = length(grid_plots) * 3, dpi = 600)
}


library(gridExtra)
library(ggplot2)


Immue_sigplot_General <- function(data, regions, process_data_function, file_path) {
  plt_list <- list()  

 
  for (region in regions) {
    print(region)
    processed_data <- process_data_function(data, region)
    
    plot <- Dfcalculate_correlation(processed_data, "Ntlen", "Week", "Freq_ratio") %>%
      create_pval_plot(titlex = region) +
      xlab("lengths(nt)")
    plt_list[[region]] <- plot
  }


  grid_plots <- do.call(grid.arrange, c(plt_list, ncol = 1))


  save_plot_grid(grid_plots, file_path)
}


# save_plot_grid <- function(grid_plots, file_path) {
#   dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
#   ggsave(plot = grid_plots, filename = file_path, width = 6, height = length(grid_plots) * 3, dpi = 600)
# }


#Immue_sigplot_General(TRB_data2, c("N1-REGION", "N2-REGION", "3'V-REGION", "5'J-REGION", "D-REGION"), 
#                         process_TRB_data, "/home/maolp/Main_Gao_ScanpyProject20231130/HFB_Figure_Plot/Sup_Figure4/S24sig/CD8TRBsig.pdf")


#Immue_sigplot_General(TRA_data2, c("N-REGION", "3'V-REGION", "5'J-REGION"), 
 #                        process_TRA_data, "/home/maolp/Main_Gao_ScanpyProject20231130/HFB_Figure_Plot/Sup_Figure4/S24sig/AllTRAsig.pdf")

# Example usage:
# Process and plot TRB data
# Immusig_plot(TRB_data2, c("N1-REGION", "N2-REGION", "3'V-REGION", "5'J-REGION", "D-REGION"), "TRB",
#                          "/home/maolp/Main_Gao_ScanpyProject20231130/HFB_Figure_Plot/Sup_Figure4/S24sig/CD8TRBsig.pdf")

# # Process and plot TRA data
# Immusig_plot(TRA_data2, c("N-REGION", "3'V-REGION", "5'J-REGION"), "TRA",
#                          "/home/maolp/Main_Gao_ScanpyProject20231130/HFB_Figure_Plot/Sup_Figure4/S24sig/AllTRAsig.pdf")