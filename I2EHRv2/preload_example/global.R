### Global file for preloaded version


library(ggplot2)
library(plotly)
#library(dplyr)

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

