### run after clinicaldata_filter.R
### combining clinical and genomic data

# import genomic  data
library(GEOquery)
genomic_data <- getGEO("GSE46097", GSEMatrix = TRUE)

# clean genomic data
library(stringr)

pData(genomic_data[[1]])$title <- as.character(pData(genomic_data[[1]])$title)

x <- replace(pData(genomic_data[[1]])$title, str_detect(Biobase::pData(genomic_data[[1]])$title, 
                                                        "baseline"), "baseline")
x <- replace(pData(genomic_data[[1]])$title, str_detect(Biobase::pData(genomic_data[[1]])$title, 
                                                        "3month"), "month3")
x <- replace(x, str_detect(Biobase::pData(genomic_data[[1]])$title, 
                           "baseline"), "baseline")
x <- replace(x, str_detect(Biobase::pData(genomic_data[[1]])$title, 
                           "1year"), "year1")

# correcting mistakes in the published data
x <- replace(x, str_detect(Biobase::pData(genomic_data[[1]])$title, 
                           "basleline"), "baseline")
x <- replace(x, str_detect(Biobase::pData(genomic_data[[1]])$title, 
                           "3moths"), "month3")
x <- replace(x, str_detect(Biobase::pData(genomic_data[[1]])$title, 
                           "1_year"), "year1")
x <- replace(x, str_detect(Biobase::pData(genomic_data[[1]])$title, 
                           "1 year"), "year1")


### match by the number of individuals 
## GEO data contains 216 (72) females , 162 (54 male) 
## 36 each female ,  27 each male

# inspect data 
sort(patients.csv$GENDER)
library(plyr)
count(patients.csv$GENDER)

# currently 74 females and 86 male so removing those with least observations
# filter last 2 female
patients.csv <- patients.csv[-c(153,154),]

#filter 32 males
patients.csv <-with(patients.csv , patients.csv[order(GENDER),]) 


