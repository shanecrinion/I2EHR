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

#set up non-changing variables
message("Setup complete")
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
#suppressPackageStartupMessages(library(devtools)

message("Libraries loaded")

#import csv containing FHIR format patient data
#temp = list.files(pattern="*.csv")
#for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))


# merge attaches the name to each patient ID
#observations_merge <- merge(x = patients.csv, 
#                            y = observations.csv, 
#                            by.x = "Id", 
#                            by.y= "PATIENT")

message("File import and merge complete")

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
    tags$head(
      # Font set-up
      tags$link(rel = "stylesheet", 
                type = "text/css", 
                href = "custom.css")),
    
    tabItems(
      tabItem(tabName = "overview", 
              box(title = "Welcome to the Interactive Integrated 
                  Electronic Health Record (I2EHR)", 
                  width=8, 
                  status = "success", 
                  solidHeader = TRUE) # close box
              ), # close overview tab

      tabItem(tabName = "Patient_Tab",
              icon=icon("id-card"),
              h5("Enter patient ID below to query current records in each dataset"),
              searchInput(value = "1425bcf6-2853-4c3a-b4c5-64fbe03d43d2",
                          inputId = "search", 
                          label = "Patient search",
                          placeholder = "Enter Patient ID number",
                          btnSearch = icon("search"),
                          btnReset = icon("remove"),
                          width = "450px"), #close searchinput
              tabsetPanel(type = "tabs",
                     tabPanel("Patient_Clinical",
                              textOutput("patient_clinical_summary"), # PUT FUNCTION HERE TO PRINT PATIENT DATA IN PLAIN TEXT FROM PATIENT.CSV
                              plotlyOutput("patient_clinical_plot_obs")
                              ), # close tabpanel clinical 
                     tabPanel("Patient_Genomic",
                              h5("Genomic data for the patient"),
                              #   fileInput("file1", "Choose CEL File",
                              #             multiple = FALSE,
                              #             accept = c(".CEL")),
                              radioButtons(inputId = "patient_genomic_data_selection",
                                           label = "Select the dataset to use for the example",
                                           c("Simulated data" = "simulated",
                                             "Insulin resistance" = "IR",
                                             "Colon cancer" = "CC")),
                              selectInput("patient_genomic_step",
                                          "Genomic analysis step:",
                                          c("Quality Control"="patient_qc",
                                          "Normalisation"="patient_normalisation",   # plot expression log2 exprs
                                          "Differential Expression Analysis" = "patient_dea", # put plot of expression for that one sample 
                                          "Biological Interpretation" = "patient_bioint")    # put expression values here for top genes
                                          ),  # close select input
                              actionButton("button", "Go"),
                              
                              # patient_qc outputs
                              conditionalPanel(
                                condition = "input.tabs == 'Patient_Genomic' & input.patient_genomic_step=='patient_qc'",
                                              # plotOutput("patient_array_intensity"),
                                                img(src="calibrated_PCA.png")),

                              # patient_normalisation outputs
                              conditionalPanel("input.tabs == 'Patient_Genomic' & input.patient_genomic_step=='patient_normalisation'"),
                              
                              # patient_dea outputs
                              conditionalPanel("input.tabs == 'Patient_Genomic' & input.patient_genomic_step=='patient_dea'"),
                                               
                              # patient_bioint outputs
                              conditionalPanel("input.tabs == 'Patient_Genomic' & input.patient_genomic_step=='patient_bioint'")
                              
                              ), # close tabpanel genomic
                     
                     tabPanel("Integrated",
                              radioButtons(inputId = "patient_integration_selection",
                                           label = "Select the dataset to use for the example",
                                           c("Simulated data" = "simulated",
                                             "Insulin resistance" = "IR",
                                             "Colon cancer" = "CC")),
                              selectInput("patient_gene_selection",
                                          "Select a gene for further analysis",
                                          choices= "Gene X"), 
                              plotOutput("patient_integrated_plot")
                              ) # closes tabpanel integrated
            ) # closes tabsetpanel
              ), #close tabitem patient-tab
      
      tabItem(tabName="Cohort_Tab",
              tabsetPanel(type = "tabs",
                          tabPanel("Clinical",
                                   textOutput("patient_clinical_summary"), # PUT FUNCTION HERE TO PRINT PATIENT DATA IN PLAIN TEXT FROM PATIENT.CSV
                                   plotlyOutput("patient_clinical_plot_obs")
                          ), # close tabpanel clinical 
                          tabPanel("Genomic",
                                   h5("Genomic data for the cohort"),
                                   #   fileInput("file1", "Choose CEL File",
                                   #             multiple = FALSE,
                                   #             accept = c(".CEL")),
                                   selectInput("cohort_genomic_step",
                                               "Genomic analysis step:",
                                               c("Quality Control"="cohort_qc",
                                                 "Normalisation"="cohort_normalisation",   # plot expression log2 exprs
                                                 "Differential Expression Analysis" = "cohort_dea", # put plot of expression for that one sample 
                                                 "Biological Interpretation" = "cohort_bioint")    # put expression values here for top genes
                                   ),  # close select input
                                   
                                   # cohort_qc outputs
                                   conditionalPanel('input.cohort_genomic_step=="cohort_qc"',
                                                    plotOutput("cohort_array_intensity"),
                                                    plotOutput("cohort_PCA_plot")),
                                   
                                   # cohort_normalisation outputs
                                   conditionalPanel('input.cohort_genomic_step=="cohort_normalisation"',
                                                    tableOutput("")),
                                   
                                   # cohort_dea outputs
                                   conditionalPanel('input.cohort_genomic_step=="cohort_dea"'),
                                   
                                   # cohort_bioint outputs
                                   conditionalPanel('input.cohort_genomic_step=="cohort_bioint"')
                          ), # close tabpanel genomic
                          
                          tabPanel("Integrated"
                          ) # closes tabpanel integrated
                          
                  ) # close tabsetpanel
                          
              ), # close tabitem cohort-tab
      
      tabItem(tabName="Predictive_Analytics")
      
      ) # close tabitems 
  ) # close dashboard body
) # close dashboard page


## server
server <- function(input, output, session) {
  
  
}    
shinyApp(ui, server)