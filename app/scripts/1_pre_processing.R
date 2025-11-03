## Preprocessing of Expression and Clinical data from GEO

# LIBRARIES
library(GEOquery) # load GEO data
library(multiClust) # Run clustering
library(dplyr) # Clean GSE data
library(stringr) # Clean GSE data
library(hgu133plus2.db)   
library(AnnotationDbi)

# Import the Series Matrix, downloaded directly from GEO
gse <- getGEO('GSE46097')[[1]]

# Extract and append the participant ID
id <- ifelse(pData(gse)$`group:ch1`=='Matched Ornish Participant',
             substring(pData(gse)$title,29,36),
             substring(pData(gse)$title,24,31))
pData(gse)$patient_id <- id


# Clean errors in the data
typos <- c("basleline" = "baseline",
           "3month" = "month3",
           "3moths" = "month3",
           'month3s' = 'month3',
           '11year' = '1 year1',
           '1year' = 'year1',
           '1 year' = 'year1',
           '1_year' = 'year1')

# Edit typos
pData(gse) <- pData(gse) %>%
  mutate(title = str_replace_all(title, typos))

# Modify non-conforming patient ID
pData(gse)[str_detect(pData(gse)$title, 'cv000613R'),'patient_id'] <- 'cv000613R'

# Replace the original labels with the cleaned version and factor for later analysis
saveRDS(object = gse, file = 'app/data/GSE46097_series_matrix.rds') # for efficient data loading

# Extract expression, clinical and feature data
exprs_data <- exprs(gse)
clinical_data <- pData(gse)

# Observe the number of variables to inspect
cat("Expression dimensions:", dim(exprs_data), "\n")
cat("Clinical dimensions:", dim(clinical_data), "\n")

# Extract sequencing, QC and other useful info
processing_info <- clinical_data[1,c('extract_protocol_ch1', 'label_protocol_ch1', 'hyb_protocol', 'data_processing')]
processing_info <- as.data.frame(t(processing_info))
colnames(processing_info) <- 'Information'
rownames(processing_info) <- c('Extract Protocol', 'Label Protocol', 'Hybridization', 'Data Processing')

# Filter to retain only statistically useful columns
clinical_data <- clinical_data[,c('title', 'patient_id', 'group:ch1', 'age:ch1', 'gender:ch1', '1 year weight loss (%):ch1', 'cad:ch1', 'diabetes:ch1') ]

# append timepoint info
clinical_data$sample <- NULL
clinical_data[str_detect(clinical_data$title, 'year'),'sample'] <- 'year1'
clinical_data[str_detect(clinical_data$title, 'month'),'sample'] <- 'month3'
clinical_data[str_detect(clinical_data$title, 'baseline'),'sample'] <- 'baseline'

# Convert columns classes
# numeric
cols.num <- c("1 year weight loss (%):ch1","age:ch1")
clinical_data[cols.num] <- sapply(clinical_data[cols.num],as.numeric)
sapply(clinical_data, class)

# factor
cols.factor <- c('group:ch1', 'gender:ch1', 'cad:ch1', 'diabetes:ch1', 'sample')
clinical_data[,cols.factor] <- lapply(clinical_data[,cols.factor] , factor)

# rename columns for ease
names(clinical_data) <- c('title', 'patient_id',  'group', 'age', 'gender', 'year_weight_loss', 'cad', 'diabetes', 'sample')

# Write the gene expression and clinical data to text files (needed for multiClust)
# Row names need to be removed so extract probe list
probe_list <- row.names(exprs_data)
row.names(exprs_data) <- NULL # formatting for MultiClust

WriteMatrixToFile(tmpMatrix=exprs_data, tmpFileName="app/data/GSE46097.expression.txt",
                  blnRowNames=T, blnColNames=T) # **If re-run needed, delete previous file**

WriteMatrixToFile(tmpMatrix=clinical_data, tmpFileName="app/data/GSE46097.clinical.txt",
                  blnRowNames=TRUE, blnColNames=TRUE)

# Get gene names 
gene_symbols <- mapIds(
  hgu133plus2.db,          # or hgu133a.db
  keys = probe_list,
  column = "SYMBOL",       # or "GENENAME", "ENTREZID", "ENSEMBL"
  keytype = "PROBEID",
  multiVals = "first")

saveRDS(gene_symbols, file='app/data/gene_symbols.rds')

# Add probe names again
row.names(exprs_data) <- probe_list

# Add gene name column as a new df
exprs_data <- as.data.frame(exprs_data)
exprs_data$gene_symbol <- gene_symbols[rownames(exprs_data)]
exprs_data <- exprs_data[,c(379, 1:378)]

# Extract case/control info for quick analysis
cases <- rownames(subset(clinical_data, group=='Matched Ornish Participant'))
controls <- rownames(subset(clinical_data, group=='Matched Control Group'))


# Format expression data for multiClust
saveRDS(object = exprs_data, 'app/data/GSE46097.expression.rds')
saveRDS(object= clinical_data, 'app/data/GSE46097.clinical.rds')

rm(cols.factor, cols.num, id, typos)



