
# PCA 
exp_raw <- log2(Biobase::exprs(genomic_data[[1]]))
PCA_raw <- prcomp(t(exp_raw), scale. = FALSE)

percentVar <- round(100*PCA_raw$sdev^2/sum(PCA_raw$sdev^2),1)
sd_ratio <- sqrt(percentVar[2] / percentVar[1])


# run the below only once or all will be assigned as controls
pData(genomic_data[[1]])$`group:ch1` <- 
  ifelse(str_detect(Biobase::pData(genomic_data[[1]])$`group:ch1`, 
                    "Matched Ornish"), "Case", "Control")


dataGG <- data.frame(PC1 = PCA_raw$x[,1], PC2 = PCA_raw$x[,2],
                     Disease = pData(genomic_data[[1]])$`group:ch1`,
                     Phenotype = pData(genomic_data[[1]])$`diabetes:ch1`,
                     Individual = pData(genomic_data[[1]])$FULLNAME)

library(ggplot2)
ggplot(dataGG, aes(PC1, PC2)) +
  geom_point(aes(shape = Disease, colour = Phenotype)) +
  ggtitle("PCA plot of the log-transformed raw expression data") +
  xlab(paste0("PC1, VarExp: ", percentVar[1], "%")) +
  ylab(paste0("PC2, VarExp: ", percentVar[2], "%")) +
  theme(plot.title = element_text(hjust = 0.5))+
  coord_fixed(ratio = sd_ratio) +
  scale_shape_manual(values = c(4,15)) + 
  scale_color_manual(values = c("darkorange2", "dodgerblue4"))
