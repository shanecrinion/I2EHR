exp_raw <- Biobase::exprs(genomic_data[[1]])
PCA_raw <- prcomp(t(exp_raw), scale. = TRUE)
percentVar <- round(100*PCA_raw$sdev^2/sum(PCA_raw$sdev^2),1)
sd_ratio <- sqrt(percentVar[2] / percentVar[1])

dataGG <- data.frame(PC1 = PCA_raw$x[,1], PC2 = PCA_raw$x[,2],
                     Sample = pData(genomic_data[[1]])$`group:ch1`,
                     Diabetes = pData(genomic_data[[1]])$`diabetes:ch1`,
                     Individual = pData(genomic_data[[1]])$PATIENT,
                     BMI_Stat = pData(genomic_data[[1]])$`ch1:highBMI_stat`,
                     Coronary_Artery_Disease = pData(genomic_data[[1]])$`cad:ch1`,
                     Systolic_BP =pData(genomic_data[[1]])$`ch1:SBP`,
                     Diastolic_BP =pData(genomic_data[[1]])$`ch1:DBP`)

Phenotype <- input$PCA_colour

if (Phenotype == "Coronary Artery Disease (G)"){
  Phenotype <- pData(genomic_data[[1]])$`cad:ch1`
} else if (Phenotype == "BMI (C)"){
  Phenotype <-  pData(genomic_data[[1]])$`ch1:highBMI_stat`
} else if (Phenotype == "Diabetes (G)"){
  Phenotype <- pData(genomic_data[[1]])$`diabetes:ch1`
} else if (Phenotype == "Systolic Blood Pressure (C)"){
  Phenotype <- pData(genomic_data[[1]])$`cad:ch1`
} else if (Phenotype == "Diastolic Blood Pressure (C)"){
  Phenotype <- pData(genomic_data[[1]])$`ch1:DBP`
}

library(ggplot2)
ggplot(dataGG, aes(PC1, PC2)) +
  geom_point(aes(shape = Sample, colour = Phenotype)) +
  ggtitle("PCA plot of the log-transformed raw expression data") +
  xlab(paste0("PC1, VarExp: ", percentVar[1], "%")) +
  ylab(paste0("PC2, VarExp: ", percentVar[2], "%")) +
  theme(plot.title = element_text(hjust = 0.5))+
  coord_fixed(ratio = sd_ratio) +
  scale_shape_manual(values = c(4,15)) + 
  scale_color_manual(values = c("darkorange2", "dodgerblue4"))