library(readr)

perform_lognormal_dge_from_files <- function(expr_file, covariate_file, gene_names_file){
    # Read data
    exprs.data=expr_file
    covariate=covariate_file
    gene_names=gene_names_file
    design = "~test_label"
    gene.offset = TRUE
    model.contrasts = NULL
    n.coef = NULL

    covariate_df <- as.data.frame(covariate)
    colnames(covariate_df) <- "test_label"
    
    test.model = model.matrix(as.formula(design), data = covariate_df)

    if(isTRUE(gene.offset)){
        n.gene <- apply(exprs.data, 2, function(X) sum(X > 0))
        old.col <- colnames(test.model)
        if(all(test.model[, 1] == 1)){
            test.model <- cbind(test.model[, 1], n.gene, test.model[, c(2:ncol(test.model))])
            colnames(test.model) <- c(old.col[1], "NGenes", old.col[c(2:length(old.col))])
        } else{
            test.model <- cbind(n.gene, test.model)
            colnames(test.model) <- c("NGenes", old.col)
        }
    }

    i.fit <- lmFit(exprs.data, test.model)
    if(!is.null(model.contrasts)){
        mod.contrast <- makeContrasts(contrasts=model.contrasts, levels=test.model)
        i.fit <- contrasts.fit(i.fit, contrasts=mod.contrast)
        i.fit <- eBayes(i.fit, trend=TRUE)
        i.res <- as.data.frame(topTreat(i.fit, number = Inf, sort.by = "p", p.value = 1))
    } else{
        i.fit <- eBayes(i.fit, trend=TRUE)
        if(is.null(n.coef)){
            n.coef <- ncol(test.model)
        }

        i.res <- as.data.frame(topTreat(i.fit, coef=ncol(test.model), number = Inf, sort.by = "none", p.value = 1))
    }
    
    i.res <- data.frame(Gene = gene_names, i.res)
    i.res <- i.res[order(i.res$adj.P.Val), ]
    return(i.res)
}



load_data_from_files <- function(path) {
  exprs.data1 <- as.matrix(read_csv(file.path(path, "expr.csv"), col_names = FALSE))
  covariate1 <- read_csv(file.path(path, "covariate.csv"))
  gene_names <- read_csv(file.path(path, "gene_names.csv"), col_names = FALSE)
  gene_names <- gene_names[-1,]

  return(list(exprs.data1=t(exprs.data1), covariate1=covariate1, gene_names=gene_names))
}