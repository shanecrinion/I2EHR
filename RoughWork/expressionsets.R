
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


i_T2D <- individual[disease == "T2D"]
design_minguez_T2D <-
  model.matrix(~ 0 + tissue[disease == "T2D"] + i_T2D)
colnames(design_minguez_T2D)[1:2] <- c("T2D", "nD")
rownames(design_minguez_T2D) <- i_T2D

i_nD <- individual[disease == "nD"]
design_minguez_nD <- 
  model.matrix(~ 0 + tissue[disease == "nD"] + i_nD )
colnames(design_minguez_nD)[1:2] <- c("T2D", "nD")
rownames(design_minguez_nD) <- i_nD 

head(design_minguez_T2D[, 1:6])

head(design_minguez_nD[, 1:6])


### differential expression based on a single gene
tissue_T2D <- tissue[disease == "T2D"]
TCFL2_expr <- Biobase::exprs(minguez_final)["16709333", disease == "T2D"]
TCFL2_data <- as.data.frame(TCFL2_expr)
colnames(TCFL2_data)[1] <- "org_value"
TCFL2_data <- mutate(TCFL2_data, 
                     individual = i_T2D, 
                     tissue_T2D)


TCFL2_data$tissue_T2D <- factor(TCFL2_data$tissue_T2D, 
                                levels = c("CC", "nC"))

ggplot(data = TCFL2_data, aes(x = tissue_T2D, y = org_value, 
                              group = 1, color = individual)) +
  geom_line() +
  ggtitle("Expression changes for the TCFL2 gene")


TCFL2_data_CC_median <- median(TCFL2_data_CC$org_value)
TCFL2_data_nC_median <- median(TCFL2_data_nC$org_value)
TCFL2_data_sum <- data.table(org_value = c(TCFL2_data_CC_median, 
                                           TCFL2_data_nC_median),
                             tissue_T2D = c("CC", "nC"))

ggplot(data = TCFL2_data_sum, 
       aes(x = tissue_T2D, 
           y = org_value, 
           color = tissue_T2D,
           group=1)) + 
  geom_line() +
  ggtitle("Expression changes for the TCFL2 gene")

