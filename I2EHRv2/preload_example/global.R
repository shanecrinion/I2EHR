### Global file for preloaded version


library(ggplot2); library(plotly)
library(plyr); library(dplyr)
library(dendextend) ; library(ctc)
library(survival) ; library(splines)
library(d3heatmap); library(limma) ; library(GEOquery); library(EnhancedVolcano)
library(org.Hs.eg.db)
library(topGO)
library(annotate)
library(hgu133a2.db)

geo <- getGEO(filename = '/Users/shanecrinion/GSE46097_series_matrix.txt.gz')

### clustering 
# upload the gene/probe expression matrix generated in multiClust
exprs.filepath <- 
  '/Users/shanecrinion/Documents/business/projects/I2EHR/I2EHRv2/www/GSE46097.expression.txt'
exprs <- multiClust::input_file(exprs.filepath)
#exprs_numeric <- 
#  (dplyr::mutate_if(exprs, is.character, ~ as.numeric(.x)))

# upload clinical data
clinical.filepath <- 
  '/Users/shanecrinion/Documents/business/projects/I2EHR/I2EHRv2/www/GSE46097.clinical.txt'

# upload outcome data
outcome.filepath <- 
  '/Users/shanecrinion/Documents/business/projects/I2EHR/I2EHR_APP/GSE46097-Clinical-Outcome.txt'

surv_df <-
  read.csv('/Users/shanecrinion/Documents/business/projects/I2EHR/I2EHRv2/preload_example/clinical-outcome.csv')

### clinical integration

samps1year <- rownames(subset(pData(geo), 
                              grepl(paste('1year', 
                                          collapse = "|"), 
                                    title)))

#eset1year <- geo[, sampleNames(geo) %in% samps1year]

individual_id <- ifelse(pData(geo)$`group:ch1`=='Matched Ornish Participant',
                        substring(pData(geo)$title,29,36),
                        substring(pData(geo)$title,24,31))

pData(geo)$indiv <- individual_id 
pData(geo)
