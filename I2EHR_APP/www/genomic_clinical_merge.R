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


Biobase::pData(genomic_data[[1]])$title <- x
Biobase::pData(genomic_data[[1]])$title <- as.factor(Biobase::pData(genomic_data[[1]])$title)


### match by the number of individuals 
## GEO data contains 216 (72) females , 162 (54 male) 
## 36 each female ,  27 each male

# inspect data 
sort(patients.csv$GENDER)
library(plyr)
plyr::count(patients.csv$GENDER)

# currently 74 females and 86 male so removing those with least observations
# filter last 2 female
patients.csv <- patients.csv[-c(153,154),]
plyr::count(patients.csv$GENDER)
#filter 32 males
patients.csv <-with(patients.csv , 
                    patients.csv[order(GENDER),]) 
patients.csv <- patients.csv[-c(73:104),]

# grab the patient ID for addition to the expression set
patient_id_list <- rep(patients.csv$PATIENT, each=3)


# order both by sex to assign appropriate gender

pData(genomic_data[[1]]) <- pData(genomic_data[[1]])[, c("geo_accession",
                                                         "1 year weight loss (%):ch1",
                                                         "age:ch1", 
                                                         "cad:ch1",
                                                         "diabetes:ch1",
                                                         "gender:ch1",
                                                         "group:ch1")]


with(pData(genomic_data[[1]]) , pData(genomic_data[[1]])[order(`gender:ch1`),]) 
pData(genomic_data[[1]]) <- with(pData(genomic_data[[1]]), pData(genomic_data[[1]])[order(`gender:ch1`),]) 

# the below assigns a patient ID of someone of the same sex
genomic_data[[1]]$PATIENT <- NULL
genomic_data[[1]]$PATIENT <- rep(patients.csv$PATIENT, each=3)

#paste the full name for search feature
patients.csv$FULLNAME <- paste(patients.csv$FIRST,  patients.csv$LAST)
genomic_data[[1]]$FULLNAME <- rep(patients.csv$FULLNAME, each = 3)

library(lubridate)
#calculate the age from dob
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

#assign the age
patients.csv$AGE <- 0
patients.csv$AGE <- age(patients.csv$BIRTHDATE)


## limit to the interesting data

pData(genomic_data[[1]]) <- pData(genomic_data[[1]])[, c("geo_accession",
                                                         "title",
                                                         "1 year weight loss (%):ch1",
                                                         "age:ch1", 
                                                         "cad:ch1",
                                                         "diabetes:ch1",
                                                         "gender:ch1",
                                                         "group:ch1", 
                                                         "PATIENT", 
                                                         "FULLNAME")]

