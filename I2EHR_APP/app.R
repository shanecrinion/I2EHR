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
# source("www/genomic_clinical_merge.R")

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

message("Set up complete")

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
                           br(), em("0cd0592a-774a-4ece-806a-5384a15af9b4"), br(), 
                           em("088efea2-9017-4b31-a25b-4bbc802025a1"), br(),
                           em("1e48d3da-d4f0-4680-8106-2304c9d1426e")
                    ), #close searchinput and column
                    
                    column(4,
                           radioButtons(inputId = "search_by", 
                                        label = "Select the search term",
                                        choices = c( 
                                          "Patient ID number"="ID_select", 
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
                                                         choices = c(sort(as.character(unique(observations.csv$DESCRIPTION))))
                                             ) # close select input 
                             ), # close column
                             column(9, plotlyOutput("observation_plot")) #close column         
                             ), # close plotly fluidrow,
                             br(),
                             h5("Additional information:"),
                             selectInput(label = "Select patient data to inspect:",
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
                             tableOutput("patient_dt")), # close tab panel clinical 
                    
                    tabPanel("Patient_Genomic",
                             h5("Genomic data for the patient"),
                             #  fileInput("file1", "Choose CEL File",
                             #             multiple = FALSE,
                             #             accept = c(".CEL")),
                             
                             h5("Phenotypic data"),
                             tableOutput("patient_genomic_table"),
                             
                             h5("Quality Control"),
                             plotOutput("patient_RLE"), # colour == the patient
                             plotOutput("patient_PCA"), # PCA without sweep 
                             
                             h5("Top genes"),
                             tableOutput("patient_topgenes_list"),
                             plotOutput("patient_log2foldchange"),
                             
                             
                             h5("Individual gene query"),
                             selectInput(inputId = "patient_genomic_gene_select",
                                         label = "Select the gene to analyse",
                                         choices=c(unique(sort(as.character(unique(anno_genomic_data$SYMBOL)))))
                             ), # close selectinput
                             textOutput("patient_genomic_gene_ttest")
                             
                    ), # close tabpanel genomic
                    
                    tabPanel("Integrated",
                             radioButtons(inputId = "patient_integration_selection",
                                          label="Please select a clinical observation as the plot colours:",
                                          choices=c("BMI"="bmi",
                                                    "diabetes status"= "diabetes_status",
                                                    "CAD")),
                             selectInput("patient_gene_selection",
                                         "Select a gene for further analysis",
                                         choices= c("Gene X")), 
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
                                   plotOutput("cohort_qc_RLE"), # add for control
                                   plotOutput("cohort_qc_PCA"), # add for control
                                   plotOutput("cohort_normalisation"), # add for control
                                   plotOutput("cohort_dea"), # add for control
                                   plotOutput("cohort_bioint") # add for control
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
                          
                          tabPanel("Integrated",
                                   textOutput("Select the colour coordinating with the clinical observation of query"),
                                   "cohort_integrated_volcanoplot_RNA") # closes tabpanel integrated
                          
                  ) # close tabset panel
              ), # close tab item cohort-tab
      
      tabItem(tabName="Predictive_Analytics")
      
      ) # close tabitems 
  ) # close dashboard body
) # close dashboard page

message("Finished: Reading UI")

## server
server <- function(input, output, session) {

  
#### PATIENT OUTPUTS ####0
## to do list
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
  
  selection <- 
    reactive({input$search_by})
  
  patient_data <- patients.csv[patients.csv$PATIENT == input$search]
  
  if(selection() != "ID_select"){
    patient_data <- patients.csv[patients.csv$FULLNAME == input$search,]
  }
  
  patient_data <- patient_data[, colSums(patient_data != "") != 0]

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
                        patient_observation_data$FIRST[[1]],
                        patient_observation_data$LAST[[1]]))
  
  ## --- plotly set up
}) # close observation_plot 



output$patient_dt <- renderDataTable({
  
    if (input$patient_select_dt == "Conditions"){
      select_datatable <- subset(conditions.csv, subset=conditions.csv$PATIENT == input$search)
    } else if (input$patient_select_dt == "Encounters"){
      select_datatable <- subset(encounters.csv, subset=encounters.csv$PATIENT == input$search)
    } else if (input$patient_select_dt  == "Imaging Studies"){
      select_datatable <- subset(imaging_studies.csv, subset=imaging_studies.csv$PATIENT == input$search)
    } else if (input$patient_select_dt == "Immunizations"){
      select_datatable <- subset(immunizations.csv, subset=immunizations.csv$PATIENT == input$search)
    } else if (input$patient_select_dt == "Medications"){
      select_datatable <- subset(medications.csv, subset=medications.csv$PATIENT == input$search)
    } else if (input$patient_select_dt == "Observations"){
      select_datatable <- subset(observations.csv, subset=observations.csv$PATIENT == input$search)
    } else if (input$patient_select_dt == "Organizations"){
      select_datatable <- subset(organizations.csv, subset=organizations.csv$PATIENT == input$search)
    } else if (input$patient_select_dt == "Procedures"){
      select_datatable <- subset(procedures.csv, subset=procedures.csv$PATIENT == input$search)
    } else if (input$patient_select_dt == "Providers"){
      select_datatable <- subset(providers.csv, subset=providers.csv$PATIENT == input$search)
    }
  
    select_datatable  
    
    })

## --------- close PATIENT CLINICAL  



## --------- open PATIENT GENOMIC 

output$patient_genomic_table <- renderTable({ 

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
  
  output$cohort_qc_RLE <- renderPlot({
    
    ## DATA
    row_medians_assayData <- 
      Biobase::rowMedians(as.matrix(exprs(genomic_data[[1]])))
    RLE_data <- sweep(log2(Biobase::exprs(genomic_data[[1]])), 1, 
                      row_medians_assayData)
    # class for the fill
    RLE_class <- data.frame(patient_array = rownames(pData(genomic_data[[1]])), 
                            disease_cat = str_detect(Biobase::pData(genomic_data[[1]])$PATIENT, 
                                                     input$search))
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

})

## --------- close PATIENT GENOMIC  



#### COHORT OUTPUTS ####

output$cohort_qc_RLE <- renderPlot({
  
  ## DATA
  row_medians_assayData <- 
    Biobase::rowMedians(as.matrix(exprs(genomic_data[[1]])))
  
  RLE_data <- sweep(log2(Biobase::exprs(genomic_data[[1]])), 1, 
                    row_medians_assayData)
  
  # class for the fill
  RLE_class <- data.frame(patient_array = rownames(pData(genomic_data[[1]])), 
                          disease_cat = str_detect(Biobase::pData(genomic_data[[1]])$PATIENT, 
                                                   input$search))
  
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


output$cohort_qc_PCA <- renderPlot({

}) # close cohort_qc_PCA

} # close server

shinyApp(ui, server)
