# diabetes patient subset data
diabetes_samples_names <- c("GSM624961", 
                            "GSM624962", 
                            "GSM624963", 
                            "GSM624964", 
                            "GSM624965", 
                            "GSM624966", 
                            "GSM624967", 
                            "GSM624968", 
                            "GSM624969", 
                            "GSM624970")

diabetes_patients <- gse25462[[1]][,diabetes_samples_names]

dim(diabetes_row_max_assayData)
diabetes_row_medians_assayData <- 
  Biobase::rowMedians(as.matrix(
    log2(Biobase::exprs(diabetes_patients))))

diabetes_row_max_assayData <- 
  Biobase::rowMax(as.matrix(
    log2(Biobase::exprs(diabetes_patients))))

diabetes_row_min_assayData <- 
  Biobase::rowMin(as.matrix(
    log2(Biobase::exprs(diabetes_patients))))


# control data


# synthetic expression data


# finish the steps of the differential expression data

