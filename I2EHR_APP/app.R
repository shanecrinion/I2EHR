## --------features to complete 
## TABS FOR PATIETS AND COHORT BASED ANALYSIS
## BUTTON TO NORMALISE AND ANALYSE THE DATA
## limit the data to patient only samples
## radio buttons for which dataset to use


#### SHANE CRINION #####

#This script installs and loads packages required for a particular analysis.  You
#can add packages to it by including them in the lists of 'requiredCRANPackages' and
#'requiredBioconductorPackages'.  
#add the Rstudio CRAN mirror to avoid prompting
options(repos = c(CRAN = "http://cran.rstudio.com"))

#check that the packages required for the analysis are installed, and prompt to install them
#if not
#check for CRAN packages
currentInstalledPackages = installed.packages(priority=NULL)[,'Package']
requiredCRANPackages = c("shiny",
                         "shinydashboard",
                         "shinyWidgets",
                         "ggridges",
                         "ggplot2",
                         "lattice",
                         "viridis",
                         "plotly",
                         "gridExtra",
                         "gplots",
                         "RColorBrewer",
                         "pheatmap",
                         "dplyr",
                         "tidyr",
                         "plyr",
                         "stringr",
                         "matrixStats",
                         "openxlsx",
                         "data.table")


missingCRANPackages = setdiff(requiredCRANPackages,currentInstalledPackages)
if (length(missingCRANPackages)==0){
  message("All required CRAN packages are installed")
} else {
  message("Installing the following required CRAN packages")
  print(missingCRANPackages)
  install.packages(missingCRANPackages)
}
#check for Bioconductor packages
requiredBioconductorPackages = c("Biobase", 
                                 "GEOquery", 
                                 "affyPLM",
                                 "GOexpress",
                                 "oligoClasses",
                                 "pd.hugene.1.0.st.v1", 
                                 "pd.hugene.2.0.st",
                                 "hugene10sttranscriptcluster.db",
                                 "hugene20sttranscriptcluster.db",
                                 "oligo",
                                 "arrayQualityMetrics",
                                 "limma",
                                 "topGO",
                                 "ReactomePA",
                                 "clusterProfiler",
                                 "geneplotter",
                                 "genefilter")


missingBioconductorPackages = setdiff(requiredBioconductorPackages,currentInstalledPackages)
if (length(missingBioconductorPackages)==0){
  message("All required Bioconductor packages are installed")
} else {
  message("Installing the following required Bioconductor packages")
  print(missingBioconductorPackages)
  source("http://bioconductor.org/biocLite.R")
  biocLite(missingBioconductorPackages)
}

#load the required packages
lapply(requiredCRANPackages, require, character.only = T)
lapply(requiredBioconductorPackages, require, character.only = T)
message("Finished: Package installation and set up")

#importing the clinical files 
message("Importing clinical data")

#import all csv files containing clinical data from Synthea
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))
message("Finished: Importing clinical data")

#importing the clinical files 
message("Importing, and integrating genomic data")

# *** unhash source if you need to limit your large cohort files
#source("www/genomic_clinical_merge.R")
message("Finished:Importing, and integrating genomic data")










#set up non-changing variables
message("Setup Library loading")
hasSetupScriptRun = TRUE

suppressPackageStartupMessages(library(shiny)) 
suppressPackageStartupMessages(library(shinydashboard)) 
suppressPackageStartupMessages(library(shinyWidgets)) 
suppressPackageStartupMessages(library(ggridges)) 
suppressPackageStartupMessages(library(ggplot2)) 
suppressPackageStartupMessages(library(lattice)) 
suppressPackageStartupMessages(library(viridis)) 
suppressPackageStartupMessages(library(GEOquery)) 
suppressPackageStartupMessages(library(plotly)) 
suppressPackageStartupMessages(library(affyPLM)) 

### Microarray Libraries
#General Bioconductor packages
suppressPackageStartupMessages(library(Biobase))
suppressPackageStartupMessages(library(oligoClasses))
#Annotation and data import packages
suppressPackageStartupMessages(library(GEOquery))
suppressPackageStartupMessages(library(pd.hugene.1.0.st.v1))
suppressPackageStartupMessages(library(pd.hugene.2.0.st))
suppressPackageStartupMessages(library(hgu133a2.db))
suppressPackageStartupMessages(library(hugene10sttranscriptcluster.db))
suppressPackageStartupMessages(library(hugene20sttranscriptcluster.db))
suppressPackageStartupMessages(library(gridExtra)) 
#Quality control and pre-processing packages
suppressPackageStartupMessages(library(oligo))
suppressPackageStartupMessages(library(arrayQualityMetrics))
#Analysis and statistics packages
suppressPackageStartupMessages(library(limma)) 
suppressPackageStartupMessages(library(topGO)) 
suppressPackageStartupMessages(library(ReactomePA)) 
suppressPackageStartupMessages(library(clusterProfiler)) 
suppressPackageStartupMessages(library(GOexpress)) 
#Plotting and color options packages
suppressPackageStartupMessages(library(gplots))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(geneplotter)) 
suppressPackageStartupMessages(library(RColorBrewer)) 
suppressPackageStartupMessages(library(pheatmap)) 
suppressPackageStartupMessages(library(d3heatmap))
#Formatting/documentation packages
#suppressPackageStartupMessages(library(rmarkdown)
#suppressPackageStartupMessages(library(BiocStyle)
suppressPackageStartupMessages(library(dplyr)) 
suppressPackageStartupMessages(library(tidyr)) 
suppressPackageStartupMessages(library(plyr)) 

#Helpers:
suppressPackageStartupMessages(library(stringr)) 
suppressPackageStartupMessages(library(matrixStats)) 
suppressPackageStartupMessages(library(genefilter))
suppressPackageStartupMessages(library(openxlsx)) 
suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(devtools))
suppressPackageStartupMessages(library(lubridate))
message("Libraries loaded")

## FILE IMPORTING


## CLINICAL

#import all csv files containing clinical data from Synthea
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))
message("Finished: Importing clinical data")
message("Set up complete")


#source("global.R")


# order to obtains with most detailed info
observations.csv <-with(observations.csv, observations.csv[order(PATIENT),])

# list 64 patients with most obs (match no of GEO samples)
patients_most_obs <- unique(observations.csv$PATIENT)[0:126]

# format patients.csv from Id to PATIENT to match other files
names(patients.csv)[names(patients.csv) == "Id"] <- "PATIENT"

# limit the info in each other csv file to these patients
#allergies.csv <- allergies.csv[allergies.csv$PATIENT %in% patients_most_obs,]
#careplans.csv <- careplans.csv[careplans.csv$PATIENT %in% patients_most_obs,]
#conditions.csv <- conditions.csv[conditions.csv$PATIENT %in% patients_most_obs,]
#encounters.csv <- encounters.csv[encounters.csv$PATIENT %in% patients_most_obs,]
#immunizations.csv <- immunizations.csv[immunizations.csv$PATIENT %in% patients_most_obs,]
#observations.csv <- observations.csv[observations.csv$PATIENT %in% patients_most_obs,]
#organizations.csv <- organizations.csv[organizations.csv$PATIENT %in% patients_most_obs,]
#patients.csv <- patients.csv[patients.csv$PATIENT %in% patients_most_obs,]
#procedures.csv <- procedures.csv[procedures.csv$PATIENT %in% patients_most_obs,]
#providers.csv <- providers.csv[providers.csv$PATIENT %in% patients_most_obs,]


### match by the number of individuals 
## GEO data contains 216 (72) females , 162 (54 male) 
## 36 each female ,  27 each male

# inspect data 
sort(patients.csv$GENDER)
library(plyr)
plyr::count(patients.csv$GENDER)

# currently 74 females and 86 male so removing those with least observations
# filter last 2 female
#patients.csv <- patients.csv[-c(153,154),]
#plyr::count(patients.csv$GENDER)
#filter 32 males
#patients.csv <-with(patients.csv , 
#                    patients.csv[order(GENDER),]) 
#patients.csv <- patients.csv[-c(73:104),]
# grab the patient ID for addition to the expression set
patient_id_list <- rep(patients.csv$PATIENT, each=3)


## GENOMIC


# import genomic  data
library(GEOquery)
genomic_data <- getGEO("GSE46097", GSEMatrix = TRUE)

# clean genomic data\

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



## INTEGRATE


# order both by sex to assign appropriate gender




#View(Biobase::pData(genomic_data[[1]])[, c("geo_accession",
#                                      "1 year weight loss (%):ch1",
#                                      "age:ch1", 
#                                      "cad:ch1",
#                                      "diabetes:ch1",
#                                      "gender:ch1",
#                                      "group:ch1", 
#                                      "PATIENT", 
#                                      "FULLNAME")])

with(pData(genomic_data[[1]]) , pData(genomic_data[[1]])[order(`gender:ch1`),]) 
pData(genomic_data[[1]]) <- with(pData(genomic_data[[1]]), pData(genomic_data[[1]])[order(`gender:ch1`),]) 

# the below assigns a patient ID of someone of the same sex
genomic_data[[1]]$PATIENT <- NULL
genomic_data[[1]]$PATIENT <- rep(patients.csv$PATIENT, each=3)

#paste the full name for search feature
#patients.csv$FULLNAME <- paste(patients.csv$FIRST,  patients.csv$LAST)
genomic_data[[1]]$FULLNAME <- rep(patients.csv$FULLNAME, each = 3)


#library(lubridate)
#calculate the age from dob
#age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
#  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
#  if (floor) return(as.integer(floor(calc.age)))
#  return(calc.age)
#}

#assign the age
#patients.csv$AGE <- 0
#patients.csv$AGE <- age(patients.csv$BIRTHDATE)

#patients.csv <- patients.csv[,c("PATIENT", "FULLNAME", "GENDER", "ETHNICITY", "RACE")]

pData(genomic_data[[1]]) <- pData(genomic_data[[1]])[, c("geo_accession",
                                                         "PATIENT", 
                                                         "FULLNAME",
                                                         "title",
                                                         "age:ch1", 
                                                         "cad:ch1",
                                                         "diabetes:ch1",
                                                         "gender:ch1",
                                                         "group:ch1")]
### ANALYSIS SPECIFIC FILES



######### RLE
exp_data <- Biobase::exprs(genomic_data[[1]])
row_medians_assayData <- 
  Biobase::rowMedians(as.matrix(exp_data))

RLE_data <- sweep(exp_data, 1, row_medians_assayData)
RLE_data <- as.data.frame(RLE_data)

row_medians_assayData <- 
  Biobase::rowMedians(as.matrix(exp_data))



######### PCA FILES

exp_raw <- Biobase::exprs(genomic_data[[1]])
PCA_raw <- prcomp(t(exp_raw), scale. = TRUE)
percentVar <- round(100*PCA_raw$sdev^2/sum(PCA_raw$sdev^2),1)
sd_ratio <- sqrt(percentVar[2] / percentVar[1])


##filtering following inspection of intensity
man_threshold <- 12
no_of_samples <- 
  table(paste0(pData(genomic_data[[1]])$`group:ch1`, "_", 
               pData(genomic_data[[1]])$`diabetes:ch1`))

samples_cutoff <- min(no_of_samples)
idx_man_threshold <- apply(exp_raw, 1,
                           function(x){
                             sum(x > man_threshold) >= samples_cutoff})
Ellsworth_medians <- rowMedians(Biobase::exprs(genomic_data[[1]]))

Ellsworth_manfiltered <- subset(genomic_data[[1]], idx_man_threshold)
anno_Ellsworth <- AnnotationDbi::select(hgu133a2.db,
                                        keys = (featureNames(Ellsworth_manfiltered)),
                                        columns = c("SYMBOL", "GENENAME"),
                                        keytype = "PROBEID")

anno_grouped <- group_by(anno_Ellsworth, PROBEID)
anno_summarized <- 
  dplyr::summarize(anno_grouped, no_of_matches = n_distinct(SYMBOL))
anno_filtered <- filter(anno_summarized, no_of_matches > 1)
probe_stats <- anno_filtered 
ids_to_exlude <- (featureNames(Ellsworth_manfiltered) %in% probe_stats$PROBEID)
Ellsworth_final <- subset(Ellsworth_manfiltered, !ids_to_exlude)
fData(Ellsworth_final)$PROBEID <- rownames(fData(Ellsworth_final))
fData(Ellsworth_final) <- left_join(fData(Ellsworth_final), anno_Ellsworth)

Ellsworth_final_info <- AnnotationDbi::select(hgu133a2.db,
                      keys = (featureNames(Ellsworth_final)),
                      columns = c("SYMBOL", "GENENAME"),
                      keytype = "PROBEID")

# restore rownames after left_join
rownames(fData(Ellsworth_final)) <- fData(Ellsworth_final)$PROBEID ## make the threshold a slider input

###### SET UP FOR LIT

library(GOexpress)
sub.genomic_data <- subEset(
  eSet=Ellsworth_final,
  subset=list(
    title=c("baseline","year1")))



####### DISEASE CATEOGORIES

sub.genomic_data.control <- subEset(
  eSet=genomic_data[[1]],
  subset=list(
    `group:ch1` =c("Matched Ornish Participant")))
control_patient <- sub.genomic_data.control$PATIENT 

sub.genomic_data.case <- subEset(
  eSet=genomic_data[[1]],
  subset=list(
    `group:ch1` =c("Matched Control Group")))

case_patient <- sub.genomic_data.case$PATIENT

'%!in%' <- function(x,y)!('%in%'(x,y))

observations.csv$`group:ch1` <- 0
observations.csv[observations.csv$PATIENT %in% case_patient,]$`group:ch1` <- "Matched Control Group"
observations.csv[observations.csv$PATIENT %in% control_patient,]$`group:ch1` <- "Matched Ornish Participant" 

observations.csv$PCA_sim_BMI <- 0
observations.csv[observations.csv$DESCRIPTION == "Body Mass Index" 
                 & observations.csv$`group:ch1` == "Matched Control Group",]$PCA_sim_BMI <- rnorm(1941, mean=23)

observations.csv[observations.csv$DESCRIPTION == "Body Mass Index" 
                 & observations.csv$`group:ch1` == "Matched Ornish Participant",]$PCA_sim_BMI <- rnorm(1788, mean = 29)

obs_bmi_overweight <- subset(x=observations.csv, DESCRIPTION == "Body Mass Index" & PCA_sim_BMI >= 25)
obs_bmi_healthy <- subset(x=observations.csv, DESCRIPTION == "Body Mass Index" & PCA_sim_BMI < 25)


genomic_data[[1]]$`ch1:highBMI_stat` <- NA
genomic_data[[1]]$`ch1:highBMI_stat`[genomic_data[[1]]$PATIENT %in% obs_bmi_overweight$PATIENT] <- "Overweight"
genomic_data[[1]]$`ch1:highBMI_stat`[genomic_data[[1]]$PATIENT %in% obs_bmi_healthy$PATIENT] <- "Healthy"


observations.csv$PCA_sim_SBP <- 0
observations.csv[observations.csv$DESCRIPTION == "Systolic Blood Pressure" 
                 & observations.csv$`group:ch1` == "Matched Control Group",]$PCA_sim_SBP <- rnorm(2504, mean=117)

observations.csv[observations.csv$DESCRIPTION == "Systolic Blood Pressure" 
                 & observations.csv$`group:ch1` == "Matched Ornish Participant",]$PCA_sim_SBP <- rnorm(2355, mean = 123)


obs_sbp_high_bp <- subset(x=observations.csv, DESCRIPTION == "Systolic Blood Pressure" & PCA_sim_SBP >= 120)
obs_sbp_healthy <- subset(x=observations.csv, DESCRIPTION == "Systolic Blood Pressure" & PCA_sim_SBP < 120)


genomic_data[[1]]$`ch1:SBP` <- NA
genomic_data[[1]]$`ch1:SBP`[genomic_data[[1]]$PATIENT %in% obs_sbp_high_bp$PATIENT] <- "High Systolic"
genomic_data[[1]]$`ch1:SBP`[genomic_data[[1]]$PATIENT %in% obs_sbp_healthy$PATIENT] <- "Healthy"


observations.csv$PCA_sim_DBP <- 0
observations.csv[observations.csv$DESCRIPTION == "Diastolic Blood Pressure" 
                 & observations.csv$`group:ch1` == "Matched Control Group",]$PCA_sim_DBP <- rnorm(2504, mean=73)

observations.csv[observations.csv$DESCRIPTION == "Diastolic Blood Pressure" 
                 & observations.csv$`group:ch1` == "Matched Ornish Participant",]$PCA_sim_DBP <- rnorm(2355, mean = 84)


obs_dbp_high_bp <- subset(x=observations.csv, DESCRIPTION == "Diastolic Blood Pressure" & PCA_sim_DBP < 80)
obs_dbp_healthy <- subset(x=observations.csv, DESCRIPTION == "Diastolic Blood Pressure" & PCA_sim_DBP >= 80)


genomic_data[[1]]$`ch1:DBP` <- NA
genomic_data[[1]]$`ch1:DBP`[genomic_data[[1]]$PATIENT %in% obs_dbp_high_bp$PATIENT] <- "High Diastolic"
genomic_data[[1]]$`ch1:DBP`[genomic_data[[1]]$PATIENT %in% obs_dbp_healthy$PATIENT] <- "Healthy"


#### INDIVIDUAL GENE SELECT MODEL

library(GOexpress)
sub.genomic_data <- subEset(
  eSet=Ellsworth_final,
  subset=list(
    title=c("baseline","year1")))

tissue <- Biobase::pData(sub.genomic_data)$title
#######

individual <- 
  as.character(Biobase::pData(sub.genomic_data)$FULLNAME)

individual <- str_replace_all(Biobase::pData(sub.genomic_data)$FULLNAME,
                              " ", "_")

tissue <- str_replace_all(Biobase::pData(sub.genomic_data)$title,
                          " ", "_")

disease <- 
  ifelse(str_detect(Biobase::pData(sub.genomic_data)$`group:ch1`, 
                    "Matched Ornish"), "Case", "Control")
i_case <- individual[disease == "Case"]

design_Ellsworth_Case <- model.matrix(~ 0 + tissue[disease == "Case"] + i_case)

colnames(design_Ellsworth_Case)[1:2] <- c("baseline", "year1")
rownames(design_Ellsworth_Case) <- i_case

i_control <- individual[disease == "Control"]
design_Ellsworth_Control <- model.matrix(~ 0 + tissue[disease == "Control"] + i_control )
colnames(design_Ellsworth_Control)[1:2] <- c("baseline", "year1")
rownames(design_Ellsworth_Control) <- i_control 









### UI

### --- 1 Structure layout
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "I2EHR"),
  dashboardSidebar(
    
    ### --- 2 Sidebar items 
    sidebarMenu(
      menuItem("Overview", 
               tabName = "overview", 
               icon = icon("id-card")),
      menuItem("Patient Analysis",
               tabName="Patient_Tab",
               icon=icon("id-card")),
      menuItem("Cohort Analysis", 
               icon = icon("poll"), 
               tabName = "Cohort_Tab"))),
  
  ### 3 Body items 
  dashboardBody(
    
    #    tags$style(type="text/css",
    #               ".shiny-output-error { visibility: hidden; }",
    #               ".shiny-output-error:before { visibility: hidden; }"),
    
    
    tags$head(
      # Font set-up
      tags$link(rel = "stylesheet", 
                type = "text/css", 
                href = "custom.css")),
    
    tabItems(
      tabItem(tabName = "overview", 
              box(title = "Welcome to the Interactive Integrated 
                  Electronic Health Record (I2EHR)", 
                  width=12,
                  tabsetPanel(
                    tabPanel(title = "Project Aims", 
                                img(src="flowchart.png", 
                                 width=400, 
                                 height=400),
                             br(),
                             h5("This project involves the development of a Shiny application 
                                  to analyse and interact with clinical data. Synthea will be
                                  used to model a disease cohort and perform predictive analytics. 
                                  Gene expression data will then be downloaded to provide proof-of-concept 
                                  for clinical and molecular analytics. The objective of the project is to develop an
                                  interactive genomic health record that can be used to obtain statistical 
                                  data at a patient and cohort level.")), # close project aims tabpanel 
                    tabPanel(title="Clinical data",
                             img(src="synthea_logo.png", 
                                 width = 650, 
                                 height = 150),
                             br(),
                            h5("Synthea (https://synthetichealth.github.io/synthea) is an open-source package
                                containing synthetic EHRs encoded in FHIR standard. Synthea models the
                                lifespan of patients based on the top 10 chronic conditions and reasons for medical
                                care encounters in the US. The objective of Synthea is to address the legal and ethical 
                                limitations that has caused lagging in health record technology [40]. The framework for 
                                Synthea is based on the Publicly 
                                Available Data Approach to the Realistic Synthetic Data (PADARSER). The model uses publicly available 
                                health statistics as inputs to generate data from clinical workflow and
                                disease progression. Finally, the model includes a temporal model to provide a
                                complete profile for the patient beyond the disease of interest. The longitu
                                dinal model is ideal for modelling disease progression and performing population analysis.")
                    ), # close clinical data tabpanel
                    tabPanel(title = "Genomic data",
                             img(src="geo_logo.png",
                                 width = 600, 
                                 height = 250),
                             br(),
                             h5("The Gene Expression Omnibus is an online public repository containing gene
                              expression data that is publicly available for clinical research [60]. GEO accepts
                              data of many forms and specifies criteria to allow an integrative design for large
                              scale analysis of raw and processed data. The reusing of GEO facilitates genomic
                              data integration and is useful in identifying gene expression to phenotype patterns.
                              The heterogenous nature of T2D means that many patients do not respond well
                              to certain drugs. Genetic variants associated with positive drug response may be
                              identifiable by disease modelling using Synthea and GEO [46]. GEO has been
                              used to study gene expression and methylation patterns in T2D patterns and
                              identified 47 upregulated and 56 downregulated genes associated with fatty acid
                              and glucose metabolic pathways [61]. shinyGEO is a web application that allows
                              gene expression data analysis including differential expression analysis [62].
                              Gene expression data will be integrated with Synthea generated patients to
                              model gene expression variation associated with disease. The project will provide
                              a framework for combined clinical and molecular analytics without legal or ethical
                              restrictions.")
                    ), # close genomic data tabpanel
                    tabPanel("Contact Details",
                                    h4("Shane Crinion"),
                                    h4("shanecrinion@gmail.com"), 
                                    h4("+ 353 858018212"),
                                    img(src="nui-galway.jpg", width=120, height=40)
                  ) # close tabpanel contact details
                  ), # close tabset panel
                  solidHeader = TRUE) # close 0
              
      ), # close overview tab
      
      tabItem(tabName = "Patient_Tab",
              icon=icon("id-card"),
              box(title = "Enter Patient Information:",
                  status="info",
                  width = 12,
                  fluidRow(
                    column(8,
                           searchInput(
                             value = "0cd0592a-774a-4ece-806a-5384a15af9b4",
                             inputId = "search", 
                             label = "Patient search",
                             placeholder = "Enter Patient ID number",
                             btnSearch = icon("search"),
                             btnReset = icon("remove"),
                             width = "450px"),
                           strong(em("For example, use the following identifiers:")),
                           br(), em("0cd0592a-774a-4ece-806a-5384a15af9b4"), br(), 
                           em("088efea2-9017-4b31-a25b-4bbc802025a1"), br(),
                           em("1e48d3da-d4f0-4680-8106-2304c9d1426e")
                    ), #close searchinput and column
                    
                    column(4, h4("Patient clinical information has been integrated below with a similar sample from GEO")
                    ) # close column
                  ) # close the fluid row
              ), # close box
              
              box(solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("Patient_Clinical",
                             shiny::tableOutput("patient_info_table"),
                             fluidRow(column(3, 
                                             selectInput(inputId= "observation_selection",
                                                         label = "Select an observation for comparison",
                                                         selected = "Body Mass Index",
                                                         choices = c(sort(as.character(unique(observations.csv$DESCRIPTION))))
                                             ) # close select input 
                             ), # close column
                             column(9, plotlyOutput("observation_plot")) #close column         
                             ), # close plotly fluidrow,
                             br(),
                             h5("Additional information:"),
                             selectInput(label = "Patient Data:",
                                         inputId="patient_select_dt",
                                         choices = c("Conditions",
                                                     "Encounters",
                                                     "Imaging Studies",
                                                     "Immunizations",
                                                     "Medications",
                                                     "Observations",
                                                     "Organizations",
                                                     "Procedures",
                                                     "Providers")),
                             DT::dataTableOutput("patient_dt")), # close tab panel clinical 
                    
                    tabPanel("Patient_Genomic",
                             #  fileInput("file1", "Choose CEL File",
                             #             multiple = FALSE,
                             #             accept = c(".CEL")),
                             
                             h4("Patient gene expression samples"),
                             tableOutput("patient_genomic_table"),
                             
                             h5("Quality Control"),
                             plotOutput("patient_RLE"),
                             h4("Individual gene query"),
                             h5("Gene/Probe Query:"),
                             DT::dataTableOutput("patient_topgenes_list"),
                             selectInput(inputId = "patient_genomic_gene_select",
                                         label = "Find the probe for your selected gene",
                                         selected = "200002_at",
                                         choices=c(unique(sort(as.character(unique(Ellsworth_final_info$PROBEID)))))
                             ), # close selectinput
                             plotOutput("patient_genomic_gene_level_expression"),
                             verbatimTextOutput("patient_genomic_gene_ttest")
                    ) # close tabpanel genomic
                ) # closes tabsetpanel
              ) # closes box 
      ), #close tabitem patient-tab
      
      tabItem(tabName="Cohort_Tab",
              tabsetPanel(type = "tabs", 
                          tabPanel("Clinical",
                                   br(),
                                   box(title = "Clinical Observation Plots",
                                       solidHeader = TRUE,
                                       width=12,
                                       radioButtons(inline = TRUE,
                                                    inputId = "cohort_clinical_stats",
                                                    label = "Frequency:",
                                                   choices = c("Conditions",
                                                     "Immunizations",
                                                     "Medications",
                                                     "Procedures")),
                                       plotOutput("cohort_clinical_disease_prevalence")),
                                   box(title = "Demographic Features",
                                       collapsible = TRUE,
                                       width = 12,
                                       fluidRow(column(6,
                                                       radioButtons(inline = TRUE,
                                                                    inputId = "cohort_patient_demographics_plotted",
                                                                    choices = c("ETHNICITY", "RACE"),
                                                                    label = "Feature selection:")
                                       )), # close fluidrow
                                       plotOutput("cohort_patient_demographics")
                                   ), # close box
                                   
                                   textOutput("patient_clinical_summary"), 
                                   plotlyOutput("patient_clinical_plot_obs")
                          ), # close tabpanel clinical 
                          
                          tabPanel("Genomic",
                                   box(title = "CVD samples by Diabetes status",
                                       width = 4,
                                       collapsible=TRUE,
                                       tableOutput("cohort_data_sample_numbers")),
                                   box(width = 8,
                                       title = "Principal Component Analysis", 
                                       collapsible = TRUE,
                                       selectInput(inputId = "PCA_colour",
                                                   label = "Phenotype Selection:",
                                                   selected = "Diabetes (G)",
                                                   choices= c("Coronary Artery Disease (G)",
                                                              "BMI (C)",
                                                              "Diabetes (G)",
                                                              "Systolic Blood Pressure (C)",
                                                              "Diastolic Blood Pressure (C)")),
                                       plotOutput("cohort_qc_PCA")),
                                   box(width = 12, 
                                       title = "Differential Expression Analysis",
                                       fluidRow(
                                         column(4, 
                                                selectInput(inputId = "subgroup_select",
                                                            label = "Phenotype Selection:",
                                                            selected = "Coronary Artery Disease (G)",
                                                            choices= c("Coronary Artery Disease (G)",
                                                                       "BMI (C)",
                                                                       "Diabetes (G)",
                                                                       "Systolic Blood Pressure (C)",
                                                                       "Diastolic Blood Pressure (C)")),
                                                selectInput(inputId = "probe_select", 
                                                            selected = "200002_at",
                                                            label = "Probe Selection:",
                                                            choices = c(unique(sort(as.character(unique(Ellsworth_final_info$PROBEID))))))
                                         ), # close column
                                         column(6,
                                                plotOutput("Density_Plot"))
                                       )), # close fluid row
                                   box(
                                       title = "Heatmap",
                                       width=12,
                                       collapsed = TRUE,
                                       collapsible = TRUE,
                                       d3heatmapOutput("cohort_bioint_heatmap")),
                                   box(title = "Gene Query",
                                       width = 12,
                                              DT::dataTableOutput("cohort_topgenes_list")
                                       ) # close box
                                   ), # close tabpanel genomic
                          
                      tabPanel("Analysis",
                           box(title = "Volcano Plot", img(src="volcanoplot.png", width = 200,height=200), width = 6),
                           box(title = "Multi Dimensional Scaling", img(src="MDS.png", width = 200, height=200), width = 6),
                           box(title = "Gene Ontology", img(src="topGO.png", width=200,height=200), width = 6),
                           box(title = "Enriched Pathways", img(src="reactome_enriched.png",width=200,height=200), width=6),
                           box(title = "Major Enrichments", img(src="reactome_enriched_barchart.png",width=300,height=300))
                      ) # close tabpanel analysis 
                  ) # close tabset panel
              ) # close tab item cohort-tab
          ) # close tabitems 
  ) # close dashboard body
) # close dashboard page

message("Finished: Reading UI")

## server
server <- function(input, output, session) {

  
#### PATIENT OUTPUTS ####0
## file set up list
# patient_dt, 
# patient_genomic_table,
# patient_RLE, 
  

# patient_PCA, 
# patient_topgenes_list, 
# patient_log2foldchange, 
# patient_genomic_gene_ttest, 
# patient_gene_selection, 
# patient_integrated_plot

  
  

## --------- open CLINICAL   

output$patient_info_table <- renderTable({
  
  #selection <-input$search_by()
  
  library(data.table)
  patient_data <- subset(x=patients.csv, subset = patients.csv$PATIENT==input$search)
  #if(selection() != "ID_select"){
  #  patient_data <- patients.csv[patients.csv$FULLNAME == input$search,]
  #}
  patient_data <- as.data.frame(patient_data)
  patient_data

  }) # close patient_info_table

  
output$observation_plot <- renderPlotly({
  
  ## ---- data set up
  observations_with_names <- merge(observations.csv,patients.csv, by  = "PATIENT") 
  patient <- input$search
  patient_data <- observations_with_names[observations_with_names$PATIENT == patient,]
  observation <- input$observation_selection
  patient_observation_data <- patient_data[patient_data$DESCRIPTION == observation,]
  ## ---- data set up
  
  
  ## --- plotly set up
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "Date of Observation",
    titlefont = f
  )
  y <- list(
    title = paste(patient_observation_data$DESCRIPTION[1], "(",
                  patient_observation_data$UNITS[1], ")"),
    titlefont = f
  )
  patient_observation_data  %>% 
    plot_ly(
      x = ~DATE, y =~as.numeric(as.character(VALUE)), 
      type = "scatter")  %>% 
    layout(xaxis = x, yaxis = y, 
           title= paste(patient_observation_data$DESCRIPTION[1], "for",
                        patient_observation_data$FULLNAME[[1]]))
  
  ## --- plotly set up
}) # close observation_plot 



output$patient_dt <- DT::renderDataTable({
    
    selected_data <- input$patient_select_dt
  
    if (selected_data == "Conditions"){
      select_datatable <- subset(conditions.csv, subset=conditions.csv$PATIENT == input$search)
    } else if (selected_data == "Encounters"){
      select_datatable <- subset(encounters.csv, subset=encounters.csv$PATIENT == input$search)
    } else if (selected_data  == "Imaging Studies"){
      select_datatable <- subset(imaging_studies.csv, subset=imaging_studies.csv$PATIENT == input$search)
    } else if (selected_data == "Immunizations"){
      select_datatable <- subset(immunizations.csv, subset=immunizations.csv$PATIENT == input$search)
    } else if (selected_data == "Medications"){
      select_datatable <- subset(medications.csv, subset=medications.csv$PATIENT == input$search)
    } else if (selected_data == "Observations"){
      select_datatable <- subset(observations.csv, subset=observations.csv$PATIENT == input$search)
    } else if (selected_data == "Organizations"){
      select_datatable <- subset(organizations.csv, subset=organizations.csv$PATIENT == input$search)
    } else if (selected_data == "Procedures"){
      select_datatable <- subset(procedures.csv, subset=procedures.csv$PATIENT == input$search)
    } else if (selected_data == "Providers"){
      select_datatable <- subset(providers.csv, subset=providers.csv$PATIENT == input$search)
    }
  
    #select_datatable <- select_datatable[, colSums(select_datatable != "") != 0]
    
    
    select_datatable <- select_datatable[,colSums(is.na(select_datatable))<nrow(select_datatable)]
    select_datatable
    
    })

## --------- close PATIENT CLINICAL  




## --------- open PATIENT GENOMIC 

output$patient_genomic_table <- shiny::renderTable({ 

# create expression set subset
subset.patient <- subEset(
    eSet=genomic_data[[1]],
     subset=list(
     PATIENT=c(input$search)))

#View(Biobase::pData(genomic_data[[1]])[, c("geo_accession",
#                                      "1 year weight loss (%):ch1",
#                                      "age:ch1", 
#                                      "cad:ch1",
#                                      "diabetes:ch1",
#                                      "gender:ch1",
#                                      "group:ch1", 
#                                      "PATIENT", 
#                                      "FULLNAME")])

pData(subset.patient)

# display table
pData(subset.patient)
})


output$patient_RLE <- renderPlot({

  
  
  ######## PATIENT SET UP
  subset.patient <- subEset(
    eSet=genomic_data[[1]],
    subset=list(
      PATIENT=c(input$search)))
  ###### END PATIENT
  
  
  ###### CONTROL SET UP
  diabetes_state <- subset.patient$`diabetes:ch1`[1]
  gender_state <- subset.patient$`gender:ch1`[1]
  cad_state <- subset.patient$`cad:ch1`[1]
  age_range <- c((as.numeric(subset.patient$`age:ch1`) - 5 ),
                 (as.numeric(subset.patient$`age:ch1`) + 5 ))
  age_range <- age_range[c(1,5)]
  
  
  subset.control <- genomic_data[[1]][,genomic_data[[1]]$`diabetes:ch1` == diabetes_state]
  subset.control <- subset.control[,subset.control$`gender:ch1` == gender_state]
  subset.control <- subset.control[,subset.control$`cad:ch1` == cad_state]
  subset.control <- subset.control[,subset.control$`age:ch1` > age_range[1] 
                                   & subset.control$`age:ch1` < age_range[2]]
  subset.control <- subset.control[,subset.control$`cad:ch1` == cad_state]
  subset.control <- subset.control[,subset.control$`group:ch1` != "Matched Ornish Participant"]
  subset.control <- subset.control[1]
  subset.control.samples <- sampleNames(subset.control)[1:3]
  
  
  
  ### PATIENT RLE
  subset.patient.RLE <-
    RLE_data[sampleNames(subset.patient)]
  
  # medians 
  subset.patient.row_medians_assayData <- 
    Biobase::rowMedians(as.matrix(subset.patient.RLE))
  
  # sweep
  subset.patient.RLE_data <- sweep(subset.patient.RLE, 1, 
                                   subset.patient.row_medians_assayData)
  
  # as dataframe
  subset.patient.RLE_data <- as.data.frame(subset.patient.RLE_data)
  
  # gathered
  subset.patient.RLE_data_gathered <- 
    tidyr::gather(subset.patient.RLE_data, patient_array, log2_expression_deviation)
  
  
  p1 <- ggplot2::ggplot(subset.patient.RLE_data_gathered, aes(patient_array,log2_expression_deviation)) + 
    geom_boxplot(outlier.shape = NA) + 
    ylim(c(-1, 1)) + 
    theme(axis.text.x = element_text(colour = "aquamarine4", 
                                     angle = 60, size = 6.5, hjust = 1 ,
                                     face = "bold"))
  
  
  ### COHORT RLE
  
  subset.control.RLE <-
    RLE_data[subset.control.samples]
  
  # medians 
  subset.control.row_medians_assayData <- 
    Biobase::rowMedians(as.matrix(subset.control.RLE))
  
  # sweep
  subset.control.RLE_data <- sweep(subset.control.RLE, 1, 
                                   subset.control.row_medians_assayData)
  
  # as dataframe
  subset.control.RLE_data <- as.data.frame(subset.control.RLE_data)
  
  # gathered
  subset.control.RLE_data_gathered <- 
    tidyr::gather(subset.control.RLE_data, patient_array, log2_expression_deviation)
  
  
   p2 <- ggplot2::ggplot(subset.control.RLE_data_gathered, aes(patient_array,log2_expression_deviation)) + 
    geom_boxplot(outlier.shape = NA) + 
    ylim(c(-1, 1)) + 
    theme(axis.text.x = element_text(colour = "aquamarine4", 
                                     angle = 60, size = 6.5, hjust = 1 ,
                                     face = "bold"))
  
  ### PLOT
  
  grid.arrange(p1, p2, ncol=2) })
  

output$patient_topgenes_list <- DT::renderDataTable({
  DT::datatable(anno_Ellsworth, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
})


output$cohort_topgenes_list <- DT::renderDataTable({
  DT::datatable(anno_Ellsworth, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
})


output$patient_genomic_gene_ttest <- renderPrint({


  tissue_Case <- tissue[disease == "Case"]
  RPL35_expr <- Biobase::exprs(sub.genomic_data)[input$patient_genomic_gene_select, disease == "Case"]
  RPL35_data <- as.data.frame(RPL35_expr)
  colnames(RPL35_data)[1] <- "org_value"
  

  RPL35_data <- mutate(RPL35_data, individual = i_case, tissue_Case)
  RPL35_data$tissue_Case <- factor(RPL35_data$tissue_Case, levels = c("baseline", "year1"))
  
  
  tissue_Control <- tissue[disease == "Control"]
  RPL35_expr <- Biobase::exprs(sub.genomic_data)[input$patient_genomic_gene_select, disease == "Control"]
  RPL35_data <- as.data.frame(RPL35_expr)
  colnames(RPL35_data)[1] <- "org_value"
  RPL35_data <- mutate(RPL35_data, individual = i_control, tissue_Control)
  RPL35_data$tissue_Control <- factor(RPL35_data$tissue_Control, levels = c("baseline", "year1"))
  
  
  RPL35_year1 <- na.exclude(RPL35_data$org_value[tissue == "year1"])
  RPL35_baseline <- na.exclude(RPL35_data$org_value[tissue == "baseline"])
  res_t <- t.test(RPL35_year1 ,RPL35_baseline , paired = TRUE)
  res_t

    
})

## --------- close PATIENT GENOMIC  



#### COHORT OUTPUTS #### 


output$cohort_patient_demographics <- renderPlot({

if (input$cohort_patient_demographics_plotted == "ETHNICITY"){
  demographic <-patients.csv$ETHNICITY
} else if (input$cohort_patient_demographics_plotted == "RACE"){
  demographic <- patients.csv$RACE
} 


ggplot(as.data.frame(demographic),
       aes(x=demographic, 
           color=patients.csv$GENDER)) +
  
  geom_histogram(fill = "white", 
                 alpha = 0.3, 
                 position = "identity", 
                 stat = "count") +
  #  geom_vline(data=as.data.frame(mu), 
  # aes(xintercept=mu, colour=patients.csv$GENDER), 
  #             linetype="dashed", alpha=0.5) +
  scale_colour_manual(values = c("#0571b0", 
                                 "#ca0020")) +
  theme_classic() +
  theme(legend.position = "top", 
        axis.text.x = element_text(face = "bold", 
                                   size = 8, 
                                   angle = 90),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#d3d3d3"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) })


output$cohort_qc_IF <- renderPlot({
  Ellsworth_medians <- rowMedians(Biobase::exprs(genomic_data[[1]]))
  hist_res <- hist(Ellsworth_medians, 100, col = "cornsilk1", freq = FALSE, 
                   main = "Histogram of the median intensities", 
                   border = "antiquewhite4",
                   xlab = "Median intensities")
})

output$cohort_data_sample_numbers <- renderTable({
  no_of_samples <- 
    table(paste0(pData(genomic_data[[1]])$'diabetes:ch1', "_", 
                 pData(genomic_data[[1]])$'group:ch1'))
  no_of_samples 
})


#output$cohort_qc_RLE <- renderPlot({
  
  ## DATA
#  row_medians_assayData <- 
#    Biobase::rowMedians(as.matrix(exprs(genomic_data[[1]])))
  
#  RLE_data <- sweep(log2(Biobase::exprs(genomic_data[[1]])), 1, 
#                    row_medians_assayData)
  
  # class for the fill
#  RLE_class <- data.frame(patient_array = rownames(pData(genomic_data[[1]])), 
#                          disease_cat = str_detect(Biobase::pData(genomic_data[[1]])$PATIENT, 
#                                                   input$search))
  
#  RLE_data <- as.data.frame(RLE_data)
#  RLE_data_gathered <- 
#    tidyr::gather(RLE_data, 
#                  patient_array, 
#                  log2_expression_deviation)
  
#  RLE_data_gathered_diagnosis <- 
#    merge(RLE_data_gathered, 
#          RLE_class, 
#          by="patient_array")
  
#  ggplot2::ggplot(RLE_data_gathered_diagnosis, 
#                  aes(patient_array,
#                      log2_expression_deviation, 
#                      fill=disease_cat)) + 
#    geom_boxplot(outlier.shape = NA) + 
#    ylim(c(-2, 2)) + 
      #    theme(axis.text.x = element_text(colour = "aquamarine4", 
#                                     angle = 60, size = 6.5, hjust = 1 ,
#                                     face = "bold"))
  
#}) # close cohort_qc_RLE

output$cohort_clinical_disease_prevalence <- renderPlot({

  
  if (input$cohort_clinical_stats == "Conditions"){
    selection <- conditions_full.csv$DESCRIPTION
  } else if (input$cohort_clinical_stats == "Immunizations"){
    selection <- immunizations.csv$DESCRIPTION
  } else if (input$cohort_clinical_stats == "Medications"){
    selection <- medications.csv$DESCRIPTION
  } else if (input$cohort_clinical_stats == "Procedures"){
    selection <- procedures.csv$DESCRIPTION
  }
  
  disorders_vector <- as.vector(plyr::count(selection))
  #disorders_vector$freqs <- as.numeric(disorders_vector$freqs)
  
  par(mar=c(2, 28, 5, 5))
  
  xlim <- c(0, 1.1*max(disorders_vector$freq))
  
  xx <- barplot(disorders_vector$freq,
                xaxt = 'n',
                horiz = TRUE,
                col=viridis(12),
                width = 12,
                xlim = xlim,
                main = "Frequency for each clinical measurement",
                xlab = "Frequency")
  
  ## Add text at top of bars
  text(y = xx, x = disorders_vector$freq,
       label = disorders_vector$freq,
       pos = 4,
       cex = 0.5,
       col = magma(1))
  ## Add x-axis labels
  axis(side = 2, at=xx,
       labels=disorders_vector$x,
       tick=FALSE,
       las=2,
       line=-0.5,
       cex.axis=0.5)
})


output$patient_genomic_gene_level_expression <- renderPlot({
 
  
  tissue_Case <- tissue[disease == "Case"]
  RPL35_expr <- Biobase::exprs(sub.genomic_data)[input$patient_genomic_gene_select, disease == "Case"]
  RPL35_data <- as.data.frame(RPL35_expr)
  colnames(RPL35_data)[1] <- "org_value"
  RPL35_data <- mutate(RPL35_data, individual = i_case, tissue_Case)
  RPL35_data$tissue_Case <- factor(RPL35_data$tissue_Case, levels = c("baseline", "year1"))
  
  
  tissue_Control <- tissue[disease == "Control"]
  RPL35_expr <- Biobase::exprs(sub.genomic_data)[input$patient_genomic_gene_select, disease == "Control"]
  RPL35_data <- as.data.frame(RPL35_expr)
  colnames(RPL35_data)[1] <- "org_value"
  RPL35_data <- mutate(RPL35_data, individual = i_control, tissue_Control)
  RPL35_data$tissue_Control <- factor(RPL35_data$tissue_Control, levels = c("baseline", "year1"))
  
  
  p1 <-
    ggplot(data = RPL35_data, aes(x = tissue_Case, y = org_value, 
                                  group = individual, color = individual)) +
    theme(legend.position = "none") +
    geom_line() + 
    ggtitle("Expression changes for CVD sample after 1 year")
  
 
  
  p2 <- 
    ggplot(data = RPL35_data, aes(x = tissue_Control, y = org_value, 
                                  group = individual, color = individual)) +
    theme(legend.position = "none") +
    geom_line() +
    ggtitle("Expression changes for Control sample after 1 year")

  library(gridExtra)
  grid.arrange(p1, p2, nrow = 1)
  
})
  



output$cohort_qc_PCA <- renderPlot({
  
  dataGG <- data.frame(PC1 = PCA_raw$x[,1], PC2 = PCA_raw$x[,2],
                       Sample = pData(genomic_data[[1]])$`group:ch1`,
                       Diabetes = pData(genomic_data[[1]])$`diabetes:ch1`,
                       Individual = pData(genomic_data[[1]])$PATIENT,
                       BMI_Stat = pData(genomic_data[[1]])$`ch1:highBMI_stat`,
                       Coronary_Artery_Disease = pData(genomic_data[[1]])$`cad:ch1`,
                       Systolic_BP =pData(genomic_data[[1]])$`ch1:SBP`,
                       Diastolic_BP =pData(genomic_data[[1]])$`ch1:DBP`)
  
  Phenotype <- input$PCA_colour
  
  if (Phenotype == "Coronary Artery Disease (G)"){
    Phenotype <- pData(genomic_data[[1]])$`cad:ch1`
  } else if (Phenotype == "BMI (C)"){
    Phenotype <-  pData(genomic_data[[1]])$`ch1:highBMI_stat`
  } else if (Phenotype == "Diabetes (G)"){
    Phenotype <- pData(genomic_data[[1]])$`diabetes:ch1`
  } else if (Phenotype == "Systolic Blood Pressure (C)"){
    Phenotype <- pData(genomic_data[[1]])$`cad:ch1`
  } else if (Phenotype == "Diastolic Blood Pressure (C)"){
    Phenotype <- pData(genomic_data[[1]])$`ch1:DBP`
  }
  
  library(ggplot2)
  ggplot(dataGG, aes(PC1, PC2)) +
    geom_point(aes(shape = Sample, colour = Phenotype)) +
    ggtitle("PCA plot of the log-transformed raw expression data") +
    xlab(paste0("PC1, VarExp: ", percentVar[1], "%")) +
    ylab(paste0("PC2, VarExp: ", percentVar[2], "%")) +
    theme(plot.title = element_text(hjust = 0.5))+
    coord_fixed(ratio = sd_ratio) +
    scale_shape_manual(values = c(4,15)) + 
    scale_color_manual(values = c("darkorange2", "dodgerblue4"))
})
  
output$cohort_bioint_heatmap <- renderD3heatmap({
    
    phenotype_names <- ifelse(str_detect(pData
                                         (genomic_data[[1]])$`diabetes:ch1`,
                                         "No"), "non_diabetic", "diabetic")
    
    disease_names <- ifelse(str_detect(pData
                                       (genomic_data[[1]])$`group:ch1`,
                                       "Matched Ornish"), "case", "control")
    
    annotation_for_heatmap <- 
      data.frame(Phenotype = phenotype_names,  Disease = disease_names)
    
    row.names(annotation_for_heatmap) <- row.names(pData(genomic_data[[1]]))
    
    dists <- as.matrix(dist(t(exprs(genomic_data[[1]])), method = "manhattan"))
    
    rownames(dists) <- row.names(pData(genomic_data[[1]]))
    hmcol <- rev(colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(255))
    colnames(dists) <- NULL
    diag(dists) <- NA
    
    ann_colors <- list(
      Phenotype = c(non_diabetic = "chartreuse4", diabetic = "burlywood3"),
      Disease = c(case = "blue4", control = "cadetblue2")
    )
    
    #### add the diabetes sidebar
    library(d3heatmap)
    d3heatmap(dists, color = (hmcol), 
              annotation_row = annotation_for_heatmap,
              annotation_colors = ann_colors,
              legend = TRUE, 
              treeheight_row = 0,
              legend_breaks = c(min(dists, na.rm = TRUE), 
                                max(dists, na.rm = TRUE)), 
              legend_labels = (c("small distance", "large distance")),
              main = "Clustering heatmap for the calibrated samples")
  })


output$TopTable <- DT::renderDataTable ({
  
  
  ## set up the subset
  # Subset it to only samples of "CN" and "MB" treatments, and also only "2H",
  # "6H", and "24H" time-points
  library(GOexpress)
  sub.genomic_data <- subEset(
    eSet=Ellsworth_final,
    subset=list(
      title=c("baseline","year1")))
  
  
  
  tissue <- Biobase::pData(sub.genomic_data)$title
  #######
  
  individual <- str_replace_all(Biobase::pData(sub.genomic_data)$FULLNAME,
                                c("'"), "_")
  individual <- str_replace_all(individual,
                                c(" "), "_")
  
  tissue <- str_replace_all(Biobase::pData(sub.genomic_data)$title,
                            " ", "_")
  
  
  # tissue <- ifelse(phenotype == "No","non_diabetic", "diabetic")
  
  # disease <- 
  #  str_replace_all(Biobase::pData(sub.Ellsworth_final)$`group:ch1`,
  #                  " ", "_")
  
  
  disease <- 
    ifelse(str_detect(Biobase::pData(sub.genomic_data)$`group:ch1`, 
                      "Matched Ornish"), "Case", "Control")
  
  
  i_case <- individual[disease == "Case"]
  
  design_Ellsworth_Case <- model.matrix(~ 0 + tissue[disease == "Case"] + i_case)
  
  colnames(design_Ellsworth_Case)[1:2] <- c("baseline", "year1")
  rownames(design_Ellsworth_Case) <- i_case
  
  
  contrast_matrix_Case <- makeContrasts(baseline-year1, levels = design_Ellsworth_Case)
  
  
  Ellsworth_fit_Case <- eBayes(contrasts.fit(lmFit(sub.genomic_data[,disease == "Case"],
                                                   design = design_Ellsworth_Case),
                                             contrast_matrix_Case))
  
  table_Case <- topTable(Ellsworth_fit_Case, number = Inf)
  DT::datatable(table_Case, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
})

output$volcanoplot <- renderPlot({
  
  
  ## set up the subset
  # Subset it to only samples of "CN" and "MB" treatments, and also only "2H",
  # "6H", and "24H" time-points
  library(GOexpress)
  sub.genomic_data <- subEset(
    eSet=Ellsworth_final,
    subset=list(
      title=c("baseline","year1")))
  
  
  
  tissue <- Biobase::pData(sub.genomic_data)$title
  #######
  
  individual <- str_replace_all(Biobase::pData(sub.genomic_data)$FULLNAME,
                                c("'"), "_")
  individual <- str_replace_all(individual,
                                c(" "), "_")
  
  
  tissue <- str_replace_all(Biobase::pData(sub.genomic_data)$title,
                            " ", "_")
  
  
  # tissue <- ifelse(phenotype == "No","non_diabetic", "diabetic")
  
  # disease <- 
  #  str_replace_all(Biobase::pData(sub.Ellsworth_final)$`group:ch1`,
  #                  " ", "_")
  
  
  disease <- 
    ifelse(str_detect(Biobase::pData(sub.genomic_data)$`group:ch1`, 
                      "Matched Ornish"), "Case", "Control")
  
  
  i_case <- individual[disease == "Case"]
  
  design_Ellsworth_Case <- model.matrix(~ 0 + tissue[disease == "Case"] + i_case)
  
  colnames(design_Ellsworth_Case)[1:2] <- c("baseline", "year1")
  rownames(design_Ellsworth_Case) <- i_case
  
  
  contrast_matrix_Case <- makeContrasts(baseline-year1, levels = design_Ellsworth_Case)
  
  
  Ellsworth_fit_Case <- eBayes(contrasts.fit(lmFit(sub.genomic_data[,disease == "Case"],
                                                   design = design_Ellsworth_Case),
                                             contrast_matrix_Case))
  
  volcano_names_subset <- Ellsworth_fit_Case$coefficients[Ellsworth_fit_Case$coefficients>=1,]
  
  volcano_names <- ifelse(abs(Ellsworth_fit_Case$coefficients) >=1 , 
                          Ellsworth_fit_Case$genes$SYMBOL, NA)
  
  
  volcanoplot(Ellsworth_fit_Case, coef = 1L, style = "p-value", highlight = 100, 
              names = volcano_names,
              xlab = "Log2 Fold Change", ylab = NULL, pch=16, cex=0.35)
  
})

output$MDS <- renderPlot ({
  library(GOexpress)
  sub.genomic_data <- subEset(
    eSet=Ellsworth_final,
    subset=list(
      title=c("baseline","year1")))
  
  i_case <- individual[disease == "Case"]
  
  design_Ellsworth_Case <- model.matrix(~ 0 + tissue[disease == "Case"] + i_case)
  
  colnames(design_Ellsworth_Case)[1:2] <- c("baseline", "year1")
  rownames(design_Ellsworth_Case) <- i_case
  
  i_control <- individual[disease == "Control"]
  design_Ellsworth_Control <- model.matrix(~ 0 + tissue[disease == "Control"] + i_control )
  colnames(design_Ellsworth_Control)[1:2] <- c("baseline", "year1")
  rownames(design_Ellsworth_Control) <- i_control 
  
  tissue_Case <- tissue[disease == "Case"]
  tissue_Control <- tissue[disease == "Control"]
  
  tissue <- Biobase::pData(sub.genomic_data)$title
  #######
  
  individual <- str_replace_all(Biobase::pData(sub.genomic_data)$FULLNAME,
                                c("'"), "_")
  individual <- str_replace_all(individual,
                                c(" "), "_")
  
  
  tissue <- str_replace_all(Biobase::pData(sub.genomic_data)$title, control, " ", "_")
  
  contrast_matrix_Case <- makeContrasts(baseline-year1, levels = design_Ellsworth_Case)
  
  Ellsworth_fit_Case <- eBayes(contrasts.fit(lmFit(sub.genomic_data[,disease == "Case"],
                                                   design = design_Ellsworth_Case),
                                             contrast_matrix_Case))
  
  contrast_matrix_Control <- makeContrasts(baseline-year1, levels = design_Ellsworth_Control)
  
  Ellsworth_fit_Control <- eBayes(contrasts.fit(lmFit(sub.genomic_data[,disease == "Control"],
                                                      design = design_Ellsworth_Control),
                                                contrast_matrix_Control))
  
  table_Case <- topTable(Ellsworth_fit_Case, number = Inf)
  DE_genes_Case <- subset(table_Case, adj.P.Val < 0.1)$PROBEID
  back_genes_idx <- genefilter::genefinder(sub.genomic_data, 
                                           as.character(DE_genes_Case), 
                                           method = "manhattan", scale = "none")
  
  back_genes <- featureNames(sub.genomic_data)[back_genes_idx]
  back_genes <- setdiff(back_genes, DE_genes_Case)
  
  
  multidensity(list(
    all = table_Case[,"AveExpr"] ,
    fore = table_Case[DE_genes_Case , "AveExpr"],
    back = table_Case[rownames(table_Case) %in% back_genes, "AveExpr"]),
    col = c("#e46981", "#ae7ee2", "#a7ad4a"),
    xlab = "mean expression",
    main = "DE genes for Case-background-matching")
})

output$cohort_gene_ontology <- renderPlot({
  
  
  library(GOexpress)
  sub.genomic_data <- subEset(
    eSet=Ellsworth_final,
    subset=list(
      title=c("baseline","year1")))
  
  
  
  tissue <- Biobase::pData(sub.genomic_data)$title
  #######
  
  individual <- str_replace_all(Biobase::pData(sub.genomic_data)$FULLNAME,
                                c("'"), "_")
  individual <- str_replace_all(individual,
                                c(" "), "_")
  
  
  tissue <- str_replace_all(Biobase::pData(sub.genomic_data)$title,
                            " ", "_")
  
  
  # tissue <- ifelse(phenotype == "No","non_diabetic", "diabetic")
  
  # disease <- 
  #  str_replace_all(Biobase::pData(sub.Ellsworth_final)$`group:ch1`,
  #                  " ", "_")
  
  
  disease <- 
    ifelse(str_detect(Biobase::pData(sub.genomic_data)$`group:ch1`, 
                      "Matched Ornish"), "Case", "Control")
  
  
  i_case <- individual[disease == "Case"]
  
  design_Ellsworth_Case <- model.matrix(~ 0 + tissue[disease == "Case"] + i_case)
  
  
  contrast_matrix_Case <- makeContrasts(baseline-year1, levels = design_Ellsworth_Case)
  
  
  Ellsworth_fit_Case <- eBayes(contrasts.fit(lmFit(sub.genomic_data[,disease == "Case"],
                                                   design = design_Ellsworth_Case),
                                             contrast_matrix_Case))
  
  table_Case <- topTable(Ellsworth_fit_Case, number = Inf)
  
  
  
  DE_genes_Case <- subset(table_Case, adj.P.Val < 0.1)$PROBEID
  
  back_genes_idx <- genefilter::genefinder(sub.genomic_data, 
                                           as.character(DE_genes_Case), 
                                           method = "manhattan", scale = "none")
  back_genes <- featureNames(sub.genomic_data)[back_genes_idx]
  back_genes <- setdiff(back_genes, DE_genes_Case)
  
  gene_IDs <- rownames(table_Case)
  in_universe <- gene_IDs %in% c(DE_genes_Case, back_genes)
  in_selection <- gene_IDs %in% DE_genes_Case 
  
  all_genes <- in_selection[in_universe]
  all_genes <- factor(as.integer(in_selection[in_universe]))
  names(all_genes) <- gene_IDs[in_universe] 
  
  
  top_GO_data <- new("topGOdata", ontology = "BP", allGenes = all_genes,
                     nodeSize = 10, annot = annFUN.db, affyLib = "hgu133a2.db")
  result_top_GO_elim <- 
    runTest(top_GO_data, algorithm = "elim", statistic = "Fisher")
  result_top_GO_classic <- 
    runTest(top_GO_data, algorithm = "classic", statistic = "Fisher")
  
  res_top_GO <- GenTable(top_GO_data, Fisher.elim = result_top_GO_elim,
                         Fisher.classic = result_top_GO_classic,
                         orderBy = "Fisher.elim" , topNodes = 100)
  
  genes_top_GO <- printGenes(top_GO_data, whichTerms = res_top_GO$GO.ID,
                             chip = "hgu133a2.db", geneCutOff = 1000)
  
  res_top_GO$sig_genes <- sapply(genes_top_GO, function(x){
    str_c(paste0(x[x$'raw p-value' == 2, "Symbol.id"],";"), 
          collapse = "")
  })
  
  
  showSigOfNodes(top_GO_data, score(result_top_GO_elim), firstSigNodes = 3,
                 useInfo = 'def')
  
})

output$Density_Plot <- renderPlot({
  
  if(input$subgroup_select=="Coronary Artery Disease (G)"){
    subset.patient <- subEset(
      eSet=genomic_data[[1]],
      subset=list(
        "cad:ch1"=c("Yes")))
    subset.control <- subEset(
      eSet=genomic_data[[1]],
      subset=list(
        "cad:ch1"=c("No")))
  } else if (input$subgroup_select=="BMI (C)"){
    subset.patient <- subEset(
      eSet=genomic_data[[1]],
      subset=list(
        "ch1:highBMI_stat"=c("Overweight")))
    subset.control <- subEset(
      eSet=genomic_data[[1]],
      subset=list(
        "ch1:highBMI_stat"=c("Healthy")))
  } else if (input$subgroup_select=="Diabetes (G)"){
    subset.patient <- subEset(
      eSet=genomic_data[[1]],
      subset=list(
        "diabetes:ch1"=c("Yes")))
    subset.control <- subEset(
      eSet=genomic_data[[1]],
      subset=list(
        "diabetes:ch1"=c("No")))
  } else if (input$subgroup_select=="Systolic Blood Pressure (C)"){
    subset.patient <- subEset(
      eSet=genomic_data[[1]],
      subset=list(
        "ch1:SBP"=c("High Systolic")))
    subset.control <- subEset(
      eSet=genomic_data[[1]],
      subset=list(
        "ch1:SBP"=c("Healthy")))
  } else if (input$subgroup_select == "Diastolic Blood Pressure (C)"){
    subset.patient <- subEset(
      eSet=genomic_data[[1]],
      subset=list(
        "ch1:DBP"=c("High Diastolic")))
    subset.control <- subEset(
      eSet=genomic_data[[1]],
      subset=list(
        "ch1:DBP"=c("Healthy")))}
  
  subset.patient.exprs <- data.frame(x= as.numeric(exprs(subset.patient)[input$probe_select,]))
  subset.control.exprs <- data.frame(x= as.numeric(exprs(subset.control)[input$probe_select,]))
  
  library(ggplot2)
  
  p <- ggplot() + geom_density(aes(x=x), colour="red", data=subset.patient.exprs) + 
    geom_density(aes(x=x), colour="blue", data=subset.control.exprs) +
    labs(
      x = "Expression value",
      y = "Density",
      color = "Class"
    )
  theme(legend.position="right")
  
  print(p)
})
  
} # close server

shinyApp(ui, server)
