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
library(hugene20sttranscriptcluster.db)
library(data.table)

#### 1. Import the raw data from GEO as an ExpressionSet

##Contains the 
# assayData (expression data), 
# metaData (phenoData: sample descriptions, featureData: chip data)
# experimentData (describes the experimetn)

GSE115313 <- getGEO("GSE115313", GSEMatrix = TRUE)

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

oligo::boxplot(raw_data, target = "core", 
               main = "Boxplot of log2-intensitites for the raw data")


# arrayQualityMetrics(expressionset = raw_data,
#                     outdir = tempdir(),
#                     force = TRUE, do.logtransform = TRUE,
#                     intgroup = c('diabetes_status:ch1', 'tissue_type:ch1'))


head(ls("package:hugene20sttranscriptcluster.db"))

## because it's already been corrected
minguez_eset <- raw_data
minguez_eset_norm <- raw_data

row_medians_assayData <- 
  Biobase::rowMedians(as.matrix(Biobase::exprs(minguez_eset)))

RLE_data <- sweep(Biobase::exprs(minguez_eset), 1, row_medians_assayData)

RLE_data <- as.data.frame(RLE_data)
RLE_data_gathered <- 
  tidyr::gather(RLE_data, patient_array, log2_expression_deviation)

ggplot2::ggplot(RLE_data_gathered, aes(patient_array,
                                       log2_expression_deviation)) + 
  geom_boxplot(outlier.shape = NA) + 
  ylim(c(-2, 2)) + 
  theme(axis.text.x = element_text(colour = "aquamarine4", 
                                   angle = 60, size = 6.5, hjust = 1 ,
                                   face = "bold"))


phenotype_names <- ifelse(str_detect(pData
                                   (minguez_eset_norm)$'tissue_type:ch1',
                                   "Colon"), "CC", "nC")



disease_names <- ifelse(str_detect(pData
                                   (minguez_eset_norm)$'diabetes_status:ch1',
                                   "non"), "non_diabetic", "diabetic")


annotation_for_heatmap <- 
  data.frame(Phenotype = phenotype_names,  Disease = disease_names)

row.names(annotation_for_heatmap) <- row.names(pData(minguez_eset_norm))


### heatmap

dists <- as.matrix(dist(t(exp_raw), method = "manhattan"))


rownames(dists) <- row.names(pData(minguez_eset_norm))
hmcol <- rev(colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(255))
colnames(dists) <- NULL
diag(dists) <- NA

ann_colors <- list(
  Phenotype = c(CC = "chartreuse4", nC = "burlywood3"),
  Disease = c(diabetic = "blue4", non_diabetic = "cadetblue2")
)


pheatmap(dists, col = (hmcol), 
         annotation_row = annotation_for_heatmap,
         annotation_colors = ann_colors,
         legend = TRUE, 
         treeheight_row = 0,
         legend_breaks = c(min(dists, na.rm = TRUE), 
                           max(dists, na.rm = TRUE)), 
         legend_labels = (c("small distance", "large distance")),
         main = "Clustering heatmap for the calibrated samples")

## filtering based on intensity

minguez_medians <- rowMedians(Biobase::exprs(minguez_eset_norm))

hist_res <- hist(minguez_medians, 100, col = "cornsilk1", freq = FALSE, 
                 main = "Histogram of the median intensities", 
                 border = "antiquewhite4",
                 xlab = "Median intensities")

man_threshold <- 2.5

hist_res <- hist(minguez_medians, 100, col = "cornsilk", freq = FALSE, 
                 main = "Histogram of the median intensities",
                 border = "antiquewhite4",
                 xlab = "Median intensities")

abline(v = man_threshold, col = "coral4", lwd = 2)


no_of_samples <- 
  table(paste0(pData(minguez_eset_norm)$'diabetes_status:ch1', "_", 
               pData(minguez_eset_norm)$'tissue_type:ch1'))
no_of_samples 

samples_cutoff <- min(no_of_samples)

idx_man_threshold <- apply(Biobase::exprs(minguez_eset_norm), 1,
                           function(x){
                             sum(x > man_threshold) >= samples_cutoff})
table(idx_man_threshold)

minguez_manfiltered <- subset(minguez_eset_norm, idx_man_threshold)


anno_minguez <- AnnotationDbi::select(hugene20sttranscriptcluster.db,
                                       keys = (featureNames(minguez_manfiltered)),
                                       columns = c("SYMBOL", "GENENAME"),
                                       keytype = "PROBEID")

anno_minguez <- subset(anno_minguez, !is.na(SYMBOL))

anno_grouped <- group_by(anno_minguez, PROBEID)
anno_summarized <- 
  dplyr::summarize(anno_grouped, no_of_matches = n_distinct(SYMBOL))

head(anno_summarized)

anno_filtered <- filter(anno_summarized, no_of_matches > 1)

head(anno_filtered)

probe_stats <- anno_filtered 

nrow(probe_stats)

ids_to_exlude <- (featureNames(minguez_manfiltered) %in% probe_stats$PROBEID)

table(ids_to_exlude)

minguez_final <- subset(minguez_manfiltered, !ids_to_exlude)

validObject(minguez_final)

head(minguez_final)

fData(minguez_final)$PROBEID <- rownames(fData(minguez_final))

fData(minguez_final) <- left_join(fData(minguez_final), anno_minguez)

rownames(fData(minguez_final)) <- fData(minguez_final)$PROBEID 

validObject(minguez_final)

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

TCFL2_data_nC <- TCFL2_data[TCFL2_data$tissue_T2D == "nC",]
TCFL2_data_CC <- TCFL2_data[TCFL2_data$tissue_T2D == "CC",]

TCFL2_data_CC_median <- median(TCFL2_data_CC$org_value)
TCFL2_data_nC_median <- median(TCFL2_data_nC$org_value)
library(data.table)
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

