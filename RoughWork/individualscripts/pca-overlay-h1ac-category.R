gse25462 <- getGEO("GSE25462", GSEMatrix = TRUE)
raw_data <- gse25462[[1]]
exp_raw <- exprs(raw_data)  

# make the empty column 
gse25462[[1]]$diabetes_status <- 0
# assign each column to its appropriate bin 
gse25462[[1]]$diabetes_status[gse25462[[1]]$`hemoglobin a1c:ch1`>=6.5] <- "diabetic levels"
gse25462[[1]]$diabetes_status[(gse25462[[1]]$`hemoglobin a1c:ch1`< 6.5 
                               & gse25462[[1]]$`hemoglobin a1c:ch1`> 6)] <- "pre-diabetic levels"
gse25462[[1]]$diabetes_status[gse25462[[1]]$`hemoglobin a1c:ch1`< 6] <- "normal levels"
#log 2
IR_raw <- log2(Biobase::exprs(gse25462[[1]]))
#pca
PCA_raw <- prcomp(t(exp_raw), scale. = FALSE)

percentVar <- round(100*PCA_raw$sdev^2/sum(PCA_raw$sdev^2),1)

sd_ratio <- sqrt(percentVar[2] / percentVar[1])

dataGG <- data.frame(PC1 = PCA_raw$x[,1], PC2 = PCA_raw$x[,2],
                     Disease = pData(gse25462[[1]])$characteristics_ch1.3,# disease state
                     Phenotype = pData(gse25462[[1]])$diabetes_status, #fasting glucose levels
                     Individual = pData(gse25462[[1]])$title)

ggplot(dataGG, aes(PC1, PC2)) +
  geom_point(
    aes(shape = Disease, 
        colour = Phenotype)) +
  
  ggtitle("PCA plot of the log-transformed raw expression data") +
  
  xlab(paste0("PC1, VarExp: ", percentVar[1], "%")) +
  
  ylab(paste0("PC2, VarExp: ", percentVar[2], "%")) +
  
  theme(plot.title = element_text(hjust = 0.5))+
  
  coord_fixed(ratio = sd_ratio) 