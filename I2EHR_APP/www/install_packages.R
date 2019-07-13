## *** adapted from https://github.com/uomsystemsbiology/budden2014predictive_reference_environment/data/install_packages.R 

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
message("Importing, and integrating genomic data")

# *** unhash source if you need to limit your large cohort files
# source("www/genomic_clinical_merge.R")

message("Finished:Importing, and integrating genomic data")



#set up non-changing variables
message("Setup complete")
hasSetupScriptRun = TRUE

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
#Plotting and color options packages
suppressPackageStartupMessages(library(gplots))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(geneplotter)) 
suppressPackageStartupMessages(library(RColorBrewer)) 
suppressPackageStartupMessages(library(pheatmap)) 

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

#import csv containing FHIR format patient data
#temp = list.files(pattern="*.csv")
#for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))


# merge attaches the name to each patient ID
#observations_merge <- merge(x = patients.csv, 
#                            y = observations.csv, 
#                            by.x = "Id", 
#                            by.y= "PATIENT")



