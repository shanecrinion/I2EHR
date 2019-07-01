
tabPanel("Valid gene list",
         dataTableOutput("valid_genes")),


box(title="Differential Expression",
    width=12, status="success",
    plotOutput("GSE115313_Differential_Expression")),



output$GSE115313_Differential_Expression <- renderPlot({
  
  raw_data <- GSE115313[[1]]
  
  minguez_eset <- raw_data
  minguez_eset_norm <- raw_data
  
  
  man_threshold <- 2.5
  
  no_of_samples <- 
    table(paste0(pData(minguez_eset_norm)$'diabetes_status:ch1', "_", 
                 pData(minguez_eset_norm)$'tissue_type:ch1'))
  no_of_samples 
  
  samples_cutoff <- min(no_of_samples)
  
  idx_man_threshold <- apply(Biobase::exprs(minguez_eset_norm), 1,
                             function(x){
                               sum(x > man_threshold) >= samples_cutoff})
  
  minguez_manfiltered <- subset(minguez_eset_norm, idx_man_threshold)
  
  
  anno_minguez <- AnnotationDbi::select(hugene20sttranscriptcluster.db,
                                        keys = (featureNames(minguez_manfiltered)),
                                        columns = c("SYMBOL", "GENENAME"),
                                        keytype = "PROBEID")
  
  anno_minguez <- subset(anno_minguez, !is.na(SYMBOL))
  
  anno_grouped <- group_by(anno_minguez, PROBEID)
  anno_summarized <- 
    dplyr::summarize(anno_grouped, no_of_matches = n_distinct(SYMBOL))
  
  anno_filtered <- filter(anno_summarized, no_of_matches > 1)
  
  probe_stats <- anno_filtered 
  
  ids_to_exlude <- (featureNames(minguez_manfiltered) %in% probe_stats$PROBEID)
  
  minguez_final <- subset(minguez_manfiltered, !ids_to_exlude)
  
  individual <- 
    minguez_final$geo_accession
  
  tissue <- str_replace_all(Biobase::pData(minguez_final)$'tissue_type:ch1',
                            " ", "_")
  
  tissue <- ifelse(tissue == "Colon_cancer_Tumor",
                   "CC", "nC")
  
  disease <- str_replace_all(Biobase::pData(minguez_final)$'diabetes_status:ch1',
                             " ", "_")
  
  disease <- ifelse(disease == "diabetic_patient",
                    "T2D", "nD")
  
  tissue_T2D <- tissue[disease == "T2D"]
  TCFL2_expr <- Biobase::exprs(minguez_final)["16709333", disease == "T2D"]
  TCFL2_data <- as.data.frame(TCFL2_expr)
  
  colnames(TCFL2_data)[1] <- "org_value"
  TCFL2_data <- mutate(TCFL2_data, 
                       individual = i_T2D, 
                       tissue_T2D)
  
  
  TCFL2_data$tissue_T2D <- factor(TCFL2_data$tissue_T2D, 
                                  levels = c("CC", "nC"))
  
  TCFL2_EC <-
    
    ggplot(data = TCFL2_data, aes(x = tissue_T2D, y = org_value, 
                                  group = tissue_T2D, fill= tissue_T2D)) +
    geom_violin() +
    ggtitle("TCFL2 gene expression dispersal")
  
  grid.arrange(TCFL2_plot, TCFL2_EC, nrow = 1)
  
  
})

output$gene_features <- renderDataTable({
  
  ids_to_exlude <- (featureNames(gse_norm) %in% probe_stats$PROBEID)
  gse_final <- subset(gse_norm, !ids_to_exlude)
  
  fData(gse_final)
})

output$valid_genes <- renderDataTable({
  
  
  gse_final <- subset(gse_norm, !ids_to_exlude)
  
  fData(gse_final)$PROBEID <- rownames(fData(gse_final))
  fData(gse_final) <- left_join(fData(gse_final), anno_gse)
  rownames(fData(gse_final)) <- fData(gse_final)$PROBEID 
  fData(gse_final)
  
})