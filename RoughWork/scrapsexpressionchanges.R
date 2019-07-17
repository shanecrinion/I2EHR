library(GOexpress)
sub.genomic_data <- subEset(
  eSet=Ellsworth_final,
  subset=list(
    title=c("baseline","year1")))

i_case <- individual[disease == "Case"]

design_Ellsworth_Case <- model.matrix(~ 0 + tissue[disease == "Case"] + i_case)

colnames(design_Ellsworth_Case)[1:2] <- c("baseline", "year1")
rownames(design_Ellsworth_Case) <- i_case

i_control <- individual[disease == "Control"]
design_Ellsworth_Control <- model.matrix(~ 0 + tissue[disease == "Control"] + i_control )
colnames(design_Ellsworth_Control)[1:2] <- c("baseline", "year1")
rownames(design_Ellsworth_Control) <- i_control 

tissue_Case <- tissue[disease == "Case"]
tissue_Control <- tissue[disease == "Control"]

tissue <- Biobase::pData(sub.genomic_data)$title
#######

individual <- str_replace_all(Biobase::pData(sub.genomic_data)$FULLNAME,
                              c("'"), "_")
individual <- str_replace_all(individual,
                              c(" "), "_")


tissue <- str_replace_all(Biobase::pData(sub.genomic_data)$title,
                          " ", "_")

contrast_matrix_Case <- makeContrasts(baseline-year1, levels = design_Ellsworth_Case)

Ellsworth_fit_Case <- eBayes(contrasts.fit(lmFit(sub.genomic_data[,disease == "Case"],
                                                 design = design_Ellsworth_Case),
                                           contrast_matrix_Case))

contrast_matrix_Control <- makeContrasts(baseline-year1, levels = design_Ellsworth_Control)

Ellsworth_fit_Control <- eBayes(contrasts.fit(lmFit(sub.genomic_data[,disease == "Control"],
                                                    design = design_Ellsworth_Control),
                                              contrast_matrix_Control))

table_Case <- topTable(Ellsworth_fit_Case, number = Inf)
DE_genes_Case <- subset(table_Case, adj.P.Val < 0.1)$PROBEID
back_genes_idx <- genefilter::genefinder(sub.genomic_data, 
                                         as.character(DE_genes_Case), 
                                         method = "manhattan", scale = "none")

back_genes <- featureNames(sub.genomic_data)[back_genes_idx]
back_genes <- setdiff(back_genes, DE_genes_Case)


multidensity(list(
  all = table_Case[,"AveExpr"] ,
  fore = table_Case[DE_genes_Case , "AveExpr"],
  back = table_Case[rownames(table_Case) %in% back_genes, "AveExpr"]),
  col = c("#e46981", "#ae7ee2", "#a7ad4a"),
  xlab = "mean expression",
  main = "DE genes for Case-background-matching")