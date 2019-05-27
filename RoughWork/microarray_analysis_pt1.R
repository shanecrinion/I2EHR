list.of.packages <- c("GEOquery", "oligo", "hgu133plus2.db", "limma", "gplots", "topGO")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


#### GEO EXTRACTION ####


library(GEOquery)
## get the GEO accession for type 2 diabetes patients
# this contains the R structure for GDS3884
gse <- getGEO("GSE25462", GSEMatrix = TRUE)
show(gse)

#accessing the raw data from GEO (the CEL files)
filePaths = getGEOSuppFiles("GSE25462")
filePaths

#accessing GSE data tables from GEO
dim(pData(gse[[1]]))
head(pData(gse[[1]])[, 1:3])

#### AFTER THE ABOVE STEP I DONE A MANUAL ANNOTATION OF THE FILES AND LABELLED BY GROUP

# if GSEs are in separate data tables, use the below command (unsure if it's needed)
df1 <- getGSEDataTables("GSE25462")
lapply(df1, head)
####output suggests not :)

#### next I moved my annotated text file to the location of CEL files


#### MICROARRAY ANALYSIS ####

library(oligo)
library(hgu133plus2.db)
library(hugene20sttranscriptcluster.db) # might need to double check this with pilib
library(limma)
library(gplots)
library(topGO)

# file reading
#read the annotated file
setwd("/media/shane/Data/github/I2EHR/GEO/GSE25462/")
pd <- read.AnnotatedDataFrame("pData.txt",sep="\t")

#format for reading
celfiles <- paste("./", rownames(pd),sep="")

#read in the raw data CEL files
rawData <- read.celfiles(celfiles, phenoData=pd)
CELS <- list.celfiles("~/GSE25462/", full.names=TRUE)

#read the norm data using rma
#no feature selection as it is the only option
normData <- rma(rawData)

##### ---- biomart annotation
# require(GEOquery)
# require(Biobase)
# gset <- getGEO("GSE25462", GSEMatrix =TRUE, getGPL=FALSE)
# if (length(gset) > 1) idx <- grep("GPL570", attr(gset, "names")) else idx <- 1
# gset <- gset[[idx]]
# expression set object
# dim(exprs(gset))
# rownames(exprs(gset))[1:50]
# require("biomaRt")
# mart <- useMart("ENSEMBL_MART_ENSEMBL")
# mart <- useDataset("hsapiens_gene_ensembl", mart)
# annotLookup <- getBM(
#   mart = mart,
#   attributes = c(
#     "affy_hg_u133_plus_2",
#     "ensembl_gene_id",
#     "gene_biotype",
#     "external_gene_name"),
#   filter = "affy_hg_u133_plus_2",
#   values = rownames(exprs(gset))[1:50],
#   uniqueRows=TRUE)
##### ---- biomart annotation

##### ----  RAW DATA RANGE
lab <- paste(pd$studyGroup)
boxplot(rawData, target="core", 
        main="Raw Data",
        names=lab,
        ylab="log2(exprs)",
        las=2)

##### ----  NORMALIZED DATA RANGE
boxplot(normData, 
        main="Norm Data", 
        ylab="log2(exprs)", 
        names=lab, las=2)


##### ----  NORMALISED EXPRESSION DENSITY
hist(normData, 
     main="Normalised Expression Density")


##### ----  HEATMAP
heatmap.2(cor(exprs(normData))^2,
          trace="none", 
          scale="none", 
          margins=c(9,9), 
          labRow=lab,
          labCol=lab)

##### ----  MULTIDIMENSIONAL SCALING
colors <- as.numeric(factor(pd$studyGroup))+1
plotMDS(normData, 
        labels=lab, 
        col=colors)



##### ---- Linear Modelling and Gene Ontology Enrichment Analysis

