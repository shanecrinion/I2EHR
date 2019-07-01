
row_medians_assayData <- 
  Biobase::rowMedians(as.matrix(
    log2(Biobase::exprs(gse25462[[1]]))))

RLE_data <- sweep(log2(Biobase::exprs(gse25462[[1]])), 1, 
                  row_medians_assayData)


# class for the fill
RLE_class <- data.frame(patient_array = rownames(pData(gse25462[[1]])), 
                        disease_cat=gse25462[[1]]$disease_cat)

RLE_data <- as.data.frame(RLE_data)


RLE_data_gathered <- 
  tidyr::gather(RLE_data, 
                patient_array, 
                log2_expression_deviation)

RLE_data_gathered_diagnosis <- 
  merge(RLE_data_gathered, 
        RLE_class, 
        by="patient_array")

ggplot2::ggplot(RLE_data_gathered_diagnosis, 
                aes(patient_array,
                    log2_expression_deviation, 
                    fill=disease_cat)) + 
  geom_boxplot(outlier.shape = NA) + 
  
  ylim(c(-2, 2)) + 
  theme(axis.text.x = element_text(colour = "aquamarine4", 
                                   angle = 60, size = 6.5, hjust = 1 ,
                                   face = "bold"))


