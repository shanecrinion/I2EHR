# Author: SHANE CRINION #
# Title: Interactive Integrated Electronic Health Records (I2EHR) #
# Version: 2 
# Description: v2 = improved features & improved usability

library(shiny)
#library(shinyFiles)

# to do list
# ---------
# move from csv to json
# create input box for local gse file location and go button (radio button for local, not local) - note that online slows down system
# add some summary stats for the clinical and genomic data
# improve the title parsing - maybe with a for loop
# genome annotation - go button
# filtering selection and go buttons
# machine learning approach to predict diabetes status by their bp and bmi measurements
# patient prediction - predict the patient's output dependent on their variants 
# remove controls from each row name

# ---- import data

# import the clinical data
# data: Synthetic clinical data from Synthea synthetic patient generator (https://synthetichealth.github.io/synthea/)
## this data mimics a cohort of cardiovascular disease patients
# file format: csv
clinical_path = '/Users/shanecrinion/Documents/business/projects/I2EHR/I2EHR_APP/'  # -- make this user entry 
temp = gsub("\\.csv$","", list.files(path=clinical_path, pattern="\\.csv$"))
for (i in 1:length(temp)) assign(temp[i], read.csv(paste0(clinical_path,temp[i],'.csv')))
message("Finished: Importing clinical data")

# import the genomic data 
# data: microarray data from GEO (https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=gse46097).
## this includes data for 63 cardiovascular disease patients and 63 matched control
## plaform: [HG-U133A_2] Affymetrix Human Genome U133A 2.0 Array
# file format: series matrix - series matrix is data that is preprocessed 
## for normalisation, summarisation and filtering

#BiocManager::install('GEOquery')
library(GEOquery)
# genomic_data <- getGEO("GSE46097", GSEMatrix = TRUE) - this command would extract directly from the website
# genomic_data <- genomic_data[[1]] - if downloaded from online
# downloading series matrix locally greatly improves app start up

genomic_path <- "/Users/shanecrinion/Documents/business/projects/I2EHR/I2EHRv2/GSE46097_series_matrix.txt.gz"
genomic_data <- getGEO(filename=genomic_path, GSEMatrix=TRUE) # if local, no need for [[1]]


# --- clean and parse data

# clinical data - no cleaning needed
# genomic data - data entry errors
library(stringr)
# replace full title with info we need 
# (eg. "Matched Ornish Participant [cv000288 baseline]" -> "baseline")
# note: ** need to improve this by making a function

# baseline entry
pData(genomic_data)$title <- replace(
  as.character(pData(genomic_data)$title), 
             str_detect(
               as.character(pData(genomic_data)$title), 
               "baseline"), "baseline")
pData(genomic_data)$title <- replace(
  as.character(pData(genomic_data)$title),
             str_detect(
               as.character(pData(genomic_data)$title), 
              "basleline"), "baseline") # formatting error

# month 3 entry
pData(genomic_data)$title <- replace(
  as.character(pData(genomic_data)$title),
              str_detect(
                as.character(pData(genomic_data)$title),
              "3month"), "month3")
pData(genomic_data)$title <- replace(
  as.character(pData(genomic_data)$title), 
              str_detect(
                as.character(pData(genomic_data)$title), 
              "3moths"), "month3") # formatting error

# year 1 entry
pData(genomic_data)$title <- replace(
  as.character(pData(genomic_data)$title), 
  str_detect(
    as.character(pData(genomic_data)$title), 
                           "1year"), "year1")
pData(genomic_data)$title <- replace(
  as.character(pData(genomic_data)$title), 
  str_detect(as.character(pData(genomic_data)$title), 
                           "1_year"), "year1") # formatting error
pData(genomic_data)$title <- replace(
  as.character(pData(genomic_data)$title), 
  str_detect(as.character(pData(genomic_data)$title), 
                           "1 year"), "year1")


# --- merge clinical and genomic data (part 1)
#assigns a patient and p-id to genomic data
Biobase::pData(genomic_data)$title <- # as factor for plotting
  as.factor(Biobase::pData(
    genomic_data)$title)

pData(genomic_data) <- # order by gender for align w/ clinical data 
  with(
    pData(genomic_data), 
    pData(genomic_data)[
      order(`gender:ch1`),]) 

# plyr::count(genomic_data$`gender:ch1`) / 3 ; plyr::count(patients$GENDER) 
## count = 72 F, 54 M in both datasets (think I manually matched clinical data)

# assign to genomic data a gender-matched name and patient id 
genomic_data$PATIENT <- rep(patients$PATIENT, each=3) 
genomic_data$FULLNAME <- rep(patients$FULLNAME, each = 3)

pData(genomic_data) <- # extract useful columns
  pData(genomic_data)[, c(
    "geo_accession", "PATIENT", "FULLNAME",
    "title", "age:ch1", "cad:ch1", 
    "diabetes:ch1", "gender:ch1","group:ch1")]

names(pData(genomic_data)) <- # change ugly names
  c('geo_accession', 'patient', 'full_name', 
    'title', 'age', 'cad_status', 
    'diabetes_status', 'gender', 'phenotype')

# -- filter genomic data 
## (by exprs and no. of matched probes)

# 1. filter the samples by minimum exprs of 12
genomic_data <- # get subset samp w exprs > 12
  subset(genomic_data,
         (apply(exprs(genomic_data), 
                1, function(x){
                  sum(x > 12) >= 
                    (min(plyr::count( # count samples
                      genomic_data$diabetes_status) 
                      $freq)/2) }))) 
                          # / 2 because 1/2 are controls


# 2. filter by gene annotation to only probes w 1 match

# get the full annotation data
# - Affymetrix Human Genome U133A 2.0 Array   
library(hgu133a2.db)
array_annotation <- hgu133a2.db # array used
genomic_annotation <-  # extract symbol and gene name
  AnnotationDbi::select(array_annotation,
  keys = (featureNames(genomic_data)),
  columns = c("SYMBOL", "GENENAME"),
  keytype = "PROBEID")

# find probes in annot data that match to multiple genes
genomic_annotation_probe_matches <- 
  # find how many probe each matches to
  dplyr::summarize(
  dplyr::group_by(genomic_annotation, PROBEID), 
  no_of_matches = dplyr::n_distinct(SYMBOL))


# extract list of single matching probes
# avoids false results
genomic_annotation_single_matches <- 
dplyr::filter(
  genomic_annotation_probe_matches, 
         no_of_matches == 1)$PROBEID

# 3. filter array features from 348 messy -> 260 accurate 
genomic_data <- 
  subset(
  genomic_data, 
  featureNames(genomic_data) %in% 
    genomic_annotation_single_matches)

# 4. filter the "month3" because I don't use these samples
# might be useful if you're interested in looking @ progression
# BiocManager::install('GOexpress')
library(GOexpress)
genomic_data <- subEset(
  eSet=genomic_data,
  subset=list(
    title=c("baseline","year1")))


# get list of p-id for each case & control
cases <- pData(subEset(
  eSet=genomic_data,
  subset=list(
    phenotype =c("Matched Ornish Participant"))))$patient

controls <- pData(subEset(
  eSet=genomic_data,
  subset=list(
    phenotype =c("Matched Control Group"))))$patient

# --- merge clinical and genomic data (part 2)

# 1. assign simulated values to clinical data
# phenotype: sourced from genomic data
observations$`group:ch1` <- 0

observations[
  observations$PATIENT %in% 
    cases,]$`group:ch1` <- "Matched Ornish Participant"

observations[
  observations$PATIENT %in% 
    controls,]$`group:ch1` <- "Matched Control Group" 

# bmi: simulate bmi measurement 
observations$bmi <- 0

observations[
  observations$DESCRIPTION 
  == "Body Mass Index" 
  & observations$`group:ch1` 
  == "Matched Control Group",]$
  bmi <- 
  rnorm(1941, mean=23) 

observations[
  observations$DESCRIPTION 
  ==  "Body Mass Index" 
  & observations$`group:ch1` 
  == "Matched Ornish Participant",]$
  bmi <- 
  rnorm(1788, mean = 29)

# bmi stat: categorise the patients by their bmi stat
observations$bmi_stat = 0 

observations$bmi_stat<- 
  ifelse(observations$bmi >= 25 
         & observations$DESCRIPTION 
         == "Body Mass Index",
         'overweight', 
  ifelse(observations$bmi < 25
         & observations$DESCRIPTION
         == 'Body Mass Index',
         'healthy', NA))

# systolic bp: simulate sbp measurement
observations$sbp = 0

observations[
  observations$DESCRIPTION 
  == "Systolic Blood Pressure" 
  & observations$`group:ch1` 
  == "Matched Control Group",]$
  sbp <- 
  rnorm(2504, mean=117)

observations[
  observations$DESCRIPTION 
  == "Systolic Blood Pressure" 
  & observations$`group:ch1` 
  == "Matched Ornish Participant",]$
  sbp <- rnorm(2355, mean = 123) # need to unhard code 

# systolic bp stat: categorise the patients by their bmi stat
observations$sbp_stat = 0 

observations$sbp_stat<- 
  ifelse(observations$sbp >= 25 
         & observations$DESCRIPTION 
         == "Systolic Blood Pressure",
         'high', 
         ifelse(observations$sbp < 25
                & observations$DESCRIPTION
                == 'Systolic Blood Pressure',
                'healthy', NA))

# diastolic bp: simulate bp measurement
observations$dbp = 0

observations[
  observations$DESCRIPTION 
  == "Diastolic Blood Pressure"
  & observations$`group:ch1` 
  == "Matched Control Group",]$
  dbp <- 
  rnorm(dim(observations[
    observations$DESCRIPTION 
        == "Diastolic Blood Pressure"
        & observations$`group:ch1` 
        == "Matched Control Group",])[1], mean=73)

observations[
  observations$DESCRIPTION 
  == "Diastolic Blood Pressure" 
  & observations$`group:ch1` 
  == "Matched Ornish Participant",]$
  dbp <- rnorm(dim(observations[
    observations$DESCRIPTION 
    == "Diastolic Blood Pressure" 
    & observations$`group:ch1` 
    == "Matched Ornish Participant",])[1], mean = 84)

dim(observations)
# diastolic bp stat: categorise the patients by their bmi stat
observations$sbp_stat = 0 

observations$sbp_stat<- 
  ifelse(observations$sbp < 80 
         & observations$DESCRIPTION 
         == "Diastolic Blood Pressure",
         'high', 
         ifelse(observations$sbp >= 80
                & observations$DESCRIPTION
                == 'Diastolic Blood Pressure',
                'healthy', NA))

# 2. generate a matrix for patient name: tissue sample
# - w/ genomic samples @ baseline and 1 year.

# matrix values
patient_phenotype <- 
  ifelse(str_detect(
    Biobase::pData(genomic_data)
    $phenotype, 
    "Matched Ornish"), 
    "Case", "Control") # simpler phenotype labelling

patient_name <- str_replace_all(
  Biobase::pData(
    genomic_data)$full_name,
  " ", "_") # list of names

patient_tissue <- 
  Biobase::pData(
    genomic_data)$title # tissue labels 

# matrix construction
cases_list <- patient_name[patient_phenotype == "Case"] # use value indexes in matrix construction
cases_matrix_design <- 
  model.matrix(~ 0 + 
  patient_tissue[
    patient_phenotype == "Case"] 
  + cases_list) # todo: remove cases from each row name

controls_list <- patient_name[patient_phenotype == "Control"] # use value indexes in matrix construction
controls_matrix_design <- 
  model.matrix(~ 0 + 
  patient_tissue[
   patient_phenotype == "Control"] 
   + controls_list) # todo: remove controls from each row name

# assign col / row names
colnames(controls_matrix_design)[1:2] <- 
  colnames(cases_matrix_design)[1:2] <- 
  c("baseline", "year1")

rownames(cases_matrix_design) <- cases_list
rownames(controls_matrix_design) <- controls_list


### --- UI 
# 1. Structure of the app

