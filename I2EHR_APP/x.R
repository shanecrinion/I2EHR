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

#importing the clinical files 
message("Importing clinical data")
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))


#import the genomic data
message("Importing genomic data")
genomic_data <- getGEO("GSE46097", GSEMatrix = TRUE)

## set up genomic data - move to global when complete
pData(genomic_data)$title <- as.character(pData(genomic_data)$title)
x <- replace(pData(genomic_data)$title, str_detect(Biobase::pData(genomic_data)$title, 
                                                      "baseline"), "baseline")
x <- replace(pData(genomic_data)$title, str_detect(Biobase::pData(genomic_data)$title, 
                                                      "3month"), "month3")
x <- replace(x, str_detect(Biobase::pData(genomic_data)$title, 
                           "baseline"), "baseline")
x <- replace(x, str_detect(Biobase::pData(genomic_data)$title, 
                           "1year"), "year1")

####### correcting mistakes in the published data
x <- replace(x, str_detect(Biobase::pData(genomic_data)$title, 
                           "basleline"), "baseline")


x <- replace(x, str_detect(Biobase::pData(genomic_data)$title, 
                           "3moths"), "month3")

x <- replace(x, str_detect(Biobase::pData(genomic_data)$title, 
                           "1_year"), "year1")

x <- replace(x, str_detect(Biobase::pData(genomic_data)$title, 
                           "1 year"), "year1")

Biobase::pData(genomic_data)$title <- x
Biobase::pData(genomic_data)$title <- as.factor(Biobase::pData(genomic_data)$title)


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
suppressPackageStartupMessages(library(devtools))

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
                                 height = 150)
                             ), # close clinical data tabpanel
                    tabPanel(title = "Genomic data",
                             img(src="geo_logo.png",
                                 width = 600, 
                                 height = 250)
                             ) # close genomic data tabpanel
                             ), # close tabset panel
                  solidHeader = TRUE) # close box
              ), # close overview tab

      tabItem(tabName = "Patient_Tab",
              icon=icon("id-card"),
              box(title = "Enter Patient Information:",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(8,
                  searchInput(
                          inputId = "search", 
                          label = "Patient search",
                          placeholder = "Enter Patient ID number",
                          btnSearch = icon("search"),
                          btnReset = icon("remove"),
                          width = "450px"),
                  strong(em("For example, use the following identifiers:")),
                  br(), em("dcda72ab-0371-41bc-a0a6-05034f5304d7")
                  ), #close searchinput and column
                  
                    column(4,
                  radioButtons(inputId = "search_by", 
                           label = "Select the search term",
                            choices = c( 
                              "Patient ID number"= "ID_select", 
                              "Patient Name" = "patient_select"),
                           selected = "ID_select"
                          ) # close radio buttons   
                    ) # close column
                  ) # close the fluid row
              ), # close box
              
              box(solidHeader = TRUE, width = 12,
              tabsetPanel(
                     tabPanel("Patient_Clinical",
                              tableOutput("patient_info_table"),
                              fluidRow(column(3, 
                                       selectInput(inputId= "observation_selection",
                                                   label = "Select an observation for comparison",
                                                   choices = c(sort(as.character(unique(observations.csv$DESCRIPTION)))),
                                                   selected = "Body Mass Index")
                                       ), # close column
                              column(8, plotlyOutput("observation_plot")) #close column         
                              ) # close plotly fluidrow
                              # plotlyOutput("patient_clinical_plot_obs")
                              ), # close tabpanel clinical 
                     
                     tabPanel("Patient_Genomic",
                              h5("Genomic data for the patient"),
                                 fileInput("file1", "Choose CEL File",
                                           multiple = FALSE,
                                           accept = c(".CEL")),
                              selectInput("patient_genomic_step",
                                          "Genomic analysis step:",
                                          c("Quality Control"="patient_qc",
                                          "Normalisation"="patient_normalisation",   # plot expression log2 exprs
                                          "Differential Expression Analysis" = "patient_dea", # put plot of expression for that one sample 
                                          "Biological Interpretation" = "patient_bioint"))  # put expression values here for top genes
                              # patient_qc outputs
                              # conditionalPanel(
                              #  condition = "input.tabs == 'Patient_Genomic' & input.patient_genomic_step=='patient_qc'",
                                              # plotOutput("patient_array_intensity"),
                              #                  img(src="calibrated_PCA.png")),

                              # patient_normalisation outputs
                              # conditionalPanel("input.tabs == 'Patient_Genomic' & input.patient_genomic_step=='patient_normalisation'"),
                              
                              # patient_dea outputs
                              # conditionalPanel("input.tabs == 'Patient_Genomic' & input.patient_genomic_step=='patient_dea'"),
                                               
                              # patient_bioint outputs
                              #conditionalPanel("input.tabs == 'Patient_Genomic' & input.patient_genomic_step=='patient_bioint'")
                               
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
              ) # closes box 
              ), #close tabitem patient-tab
      
      tabItem(tabName="Cohort_Tab",
              tabsetPanel(type = "tabs", 
                          tabPanel("Clinical",
                                   textOutput("patient_clinical_summary"), 
                                   plotlyOutput("patient_clinical_plot_obs")
                          ), # close tabpanel clinical 
                          tabPanel("Genomic",
                                   h5("Genomic data for the cohort"),
                                   #   fileInput("file1", "Choose CEL File",
                                   #             multiple = FALSE,
                                   #             accept = c(".CEL")),
                                   plotOutput("cohort_qc_RLE"),
                                   plotOutput("cohort_qc_PCA"),
                                   plotOutput("cohort_normalisation"),
                                   plotOutput("cohort_dea"),
                                   plotOutput("cohort_bioint")
                                   # cohort_qc outputs
                                   #conditionalPanel('input.cohort_genomic_step=="cohort_qc"',
                                   #                plotOutput("cohort_array_intensity"),
                                   #                  plotOutput("cohort_PCA_plot")),
                                   
                                   # cohort_normalisation outputs
                                   #conditionalPanel('input.cohort_genomic_step=="cohort_normalisation"',
                                   #                  tableOutput("")),
                                   
                                   # cohort_dea outputs
                                   #conditionalPanel('input.cohort_genomic_step=="cohort_dea"'),
                                   
                                   # cohort_bioint outputs
                                   #conditionalPanel('input.cohort_genomic_step=="cohort_bioint"')
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
  
#### PATIENT OUTPUTS ####
selection <- 
  reactive({input$search_by})

output$selection_datatype <- 
  renderText({
  paste("Search using", selection)
})
  
output$patient_info_table <- renderTable({
    
#    if (selection == ID_select) {
#    paste('Identifying using patient ID number:')
#    
    patient_ID <- input$search
    patient_data <- patients.csv[patients.csv$Id == patient_ID,]
    patient_data <- patient_data[, colSums(patient_data != "") != 0]
    patient_data
    
#  } else {
#    print('Identifying using patient name:')
#    patient_name <- input$search
#    patient <-  unlist(strsplit(patient, " "))
#    patient_data <- patients.csv[patients.csv$FIRST == patient[1] & patients.csv$LAST == patient[2],]
#    patient_data <- patient_data[, colSums(patient_data != "") != 0]
#    patient_data
#  }
}) # close patient_info_table

  
output$observation_plot <- renderPlotly({
  ## ---- data set up
  ## create merger to grab patient names
  patients.csv$PATIENT <- patients.csv$Id 
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
                        patient_observation_data$FIRST[[1]],
                        patient_observation_data$LAST[[1]]))
  ## --- plotly set up
}) # close observation_plot 

#### COHORT OUTPUTS ####

output$cohort_qc_RLE <- renderPlot({
  
## DATA
  row_medians_assayData <- 
  Biobase::rowMedians(as.matrix(
    log2(Biobase::exprs(genomic_data))))

RLE_data <- sweep(log2(Biobase::exprs(genomic_data)), 1, 
                  row_medians_assayData)

# class for the fill
RLE_class <- data.frame(patient_array = rownames(pData(genomic_data)), 
                        disease_cat=genomic_data$disease_cat)

RLE_data <- as.data.frame(RLE_data)
RLE_data_gathered <- 
  tidyr::gather(RLE_data, 
                patient_array, 
                log2_expression_deviation)

RLE_data_gathered_diagnosis <- 
  merge(RLE_data_gathered, 
        RLE_class, 
        by="patient_array")

ggplot2::ggplot(RLE_data_gathered_diagnosis, 
                aes(patient_array,
                    log2_expression_deviation, 
                    fill=disease_cat)) + 
  geom_boxplot(outlier.shape = NA) + 
  ylim(c(-2, 2)) + 
  theme(axis.text.x = element_text(colour = "aquamarine4", 
                                   angle = 60, size = 6.5, hjust = 1 ,
                                   face = "bold"))

}) # close cohort_qc_RLE

} # close server

shinyApp(ui, server)