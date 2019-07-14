
source("www/install_packages.R")
#source("www/clinicaldata_filter.R")
source("www/genomic_clinical_merge.R")

exp_raw <- exprs(genomic_data[[1]])
PCA_raw <- prcomp(t(exp_raw), scale. = FALSE)

percentVar <- round(100*PCA_raw$sdev^2/sum(PCA_raw$sdev^2),1)
sd_ratio <- sqrt(percentVar[2] / percentVar[1])

dataGG <- data.frame(PC1 = PCA_raw$x[,1], PC2 = PCA_raw$x[,2],
                     Disease = pData(genomic_data[[1]])$`group:ch1`,
                     Phenotype = pData(genomic_data[[1]])$`diabetes:ch1`,
                     Individual = pData(genomic_data[[1]])$FULLNAME)
