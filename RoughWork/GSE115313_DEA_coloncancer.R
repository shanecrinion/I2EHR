##GSE115313 -  COLON CANCER TUMOUR SAMPLES
## 50 DIABETIC AND 50 NON DIABETIC SAMPLES
# TYPES: Normal colonic mucosa AND Colon cancer Tumor

#General Bioconductor packages
library(Biobase)
library(oligoClasses)

#Annotation and data import packages
library(GEOquery)
library(pd.hugene.1.0.st.v1)
library(hugene10sttranscriptcluster.db)

#Quality control and pre-processing packages
library(oligo)
library(arrayQualityMetrics)

#Analysis and statistics packages
library(limma)
library(topGO)
library(ReactomePA)
library(clusterProfiler)

#Plotting and color options packages
library(gplots)
library(ggplot2)
library(geneplotter)
library(RColorBrewer)
library(pheatmap)

#Formatting/documentation packages
#library(rmarkdown)
#library(BiocStyle)
library(dplyr)
library(tidyr)

#Helpers:
library(stringr)
library(matrixStats)
library(genefilter)
library(openxlsx)


#### 1. Import the raw data from GEO as an ExpressionSet

##Contains the 
# assayData (expression data), 
# metaData (phenoData: sample descriptions, featureData: chip data)
# experimentData (describes the experimetn)

#GSE115313 <- getGEO("GSE115313", GSEMatrix = TRUE)

# lists first entries for the assay, meta and experimental data

show(GSE115313)
show(pData(phenoData(GSE115313[[1]])))
colnames(pData(phenoData(GSE115313[[1]])))

#

#### 2. Limit ExpressionSet to the needed data

raw_data <- GSE115313[[1]]
pData(raw_data) <- pData(raw_data)[,c("geo_accession", 
                               "characteristics_ch1.1", 
                               "description", 
                               "diabetes_status:ch1", 
                               "tissue_type:ch1")]


#### 3. Quality control of the microarray data
## It has been noted that the data has already been calibrated and normalised
## This is evident from observing values "CEL files are background corrected and normalized using the RMA method implemented in the oligo R package and annotated with hugene20sttranscriptcluster R package"
## The RMA function gives the expression in log form.

## Principal Component Analysis 
# 1. Generate values 
exp_raw <- exprs(raw_data)
PCA_raw <- prcomp(t(exp_raw), scale. = FALSE)
# 2. Get the percentage variance- understand how many components we should use
percentVar <- round(100*PCA_raw$sdev^2/sum(PCA_raw$sdev^2),1)
sd_ratio <- sqrt(percentVar[2] / percentVar[1])
dataGG <- data.frame(PC1 = PCA_raw$x[,1], PC2 = PCA_raw$x[,2],
                     Disease = pData(raw_data)$'diabetes_status:ch1',
                     Phenotype = pData(raw_data)$'tissue_type:ch1',
                     Individual = pData(raw_data)$geo_accession)

ggplot(dataGG, aes(PC1, PC2)) +
  geom_point(aes(shape = Disease, colour = Phenotype)) +
  ggtitle("PCA plot of the log-transformed raw expression data") +
  xlab(paste0("PC1, VarExp: ", percentVar[1], "%")) +
  ylab(paste0("PC2, VarExp: ", percentVar[2], "%")) +
  theme(plot.title = element_text(hjust = 0.5))+
  coord_fixed(ratio = sd_ratio) +
  scale_shape_manual(values = c(4,15)) + 
  scale_color_manual(values = c("darkorange2", "dodgerblue4"))

