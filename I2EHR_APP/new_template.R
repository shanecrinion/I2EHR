#### 1. SET-UP & INSTALLATIONS

### --- 1.1 Install the required packages

list.of.packages <- c("ggplot2", 
                      "ggridges", 
                      "lattice",
                      "viridis",
                      "shiny",
                      "shinydashboard",
                      "DiagrammeR",
                      "shinyWidgets")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### --- 1.2 Load libraries 

library(shiny) 
library(shinydashboard)
library(shinyWidgets)
library(ggridges)
library(ggplot2)
library(lattice)
library(viridis)
library(DiagrammeR)
library(GEOquery)

### --- 1.3 File imports

# import clinical data stored in csv files
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

# load data from GEO for the series  
gse25462 <- getGEO("GSE25462", 
                   GSEMatrix = TRUE)


###### ------------------------------------------------------

### 2. UI

### --- 2.1 Structure layout
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "I2EHR"),
  dashboardSidebar(
    
    ### --- 2.2 Sidebar items 
    sidebarMenu(
      menuItem("Overview", 
               tabName = "overview", 
               icon = icon("id-card")),
      menuItem("Patient Data",
               tabName="PatientNu",
               menuSubItem("Clinical data", "patient-clinical"), 
               menuSubItem("Genomic data", "patient-genomic"),
               icon=icon("id-card")),
      menuItem("Cohort", icon = icon("poll"), 
               tabName = "cohort",
               menuSubItem("Clinical data", "cohort-clinical"), 
               menuSubItem("Genomic data", "cohort-genomic")))),
  
  ### --- 2.3 Body items 
  dashboardBody(
    tags$head(
      
      # 2.3.1 Font set-up
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),

      tabItems(
      # 2.3.2 Introductory information and aims
      tabItem(tabName = "overview", 
              box(title = "Welcome to the Interactive Integrated Electronic Health Record (I2EHR)", 
                  width=8,
                  tabsetPanel(
                    
                    # Project aims
                    tabPanel(title = "Project Aims", 
                             img(src="flowchart.png", width=400, height=400),
                             h5("This project involves the development of a Shiny application 
                                to analyse and interact with clinical data. Synthea will be used to model a disease cohort and
                                perform predictive analytics. Gene expression data will then be downloaded to
                                provide proof-of-concept for clinical and molecular analytics. The objective of the
                                project is to develop an interactive genomic health record that can be used to
                                obtain statistical data at a patient and cohort level.")),
                    
                    # Synthea
                    tabPanel(title = "Synthea", 
                             img(src="architecture.png", 
                                 width=600, height=300),
                             h5("Synthea (https://synthetichealth.github.io/synthea) is an open-source package
                                containing synthetic EHRs encoded in FHIR standard. Synthea models the
                                lifespan of patients based on the top 10 chronic conditions and reasons for medical
                                care encounters in the US. The objective of Synthea is to address the legal and 
                                ethical limitations that has caused lagging in health record technology [40]. The
                                framework for Synthea is based on the Publicly Available Data Approach to the
                                Realistic Synthetic Data (PADARSER). The model uses publicly
                                available health statistics as inputs to generate data from clinical workflow and
                                disease progression. Finally, the model includes a temporal model to provide a
                                complete profile for the patient beyond the disease of interest. The longitu
                                dinal model is ideal for modelling disease progression and performing population
                                analysis.")),
                    
                    # GEO
                    tabPanel(title= "GEO", 
                             img(src="diabetestype2.png", width=600, height=300),
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
                                restrictions.")))),
              
              # Contact details
              box(title="Contact Details", width = 4,
                  h4("Shane Crinion"),
                  h4("shanecrinion@gmail.com"), 
                  h4("+ 353 858018212"),
                  img(src="nui-galway.jpg", width=120, height=40))),
      
      ### --- 2.3.3 Patient clinical data
      
      tabItem(tabName="patient-clinical",

              # Controls and data ources
              box(title = "Patient Query", 
                  collapsible = TRUE,
                  tabsetPanel(
                    tabPanel("Search options", 
                             h5("Enter patient ID below to query current records in each dataset"),
                             searchInput(value = "1425bcf6-2853-4c3a-b4c5-64fbe03d43d2",
                                         inputId = "search", 
                                         label = "Patient search",
                                         placeholder = "Enter Patient ID number",
                                         btnSearch = icon("search"),
                                         btnReset = icon("remove"),
                                         width = "450px")), 
                    tabPanel("Data sources", 
                             tags$ul(
                               tags$li("Clinical guidelines"), 
                               tags$li("Caremaps from clinician input and CPGs"), 
                               tags$li("Publicly available documentation")
                             ),
                             h5("Sources collected on the internet for demographic information
                                include the US Census Bureau demographics, Centers for Disease 
                                Control and Prevention prevalence and incidence rates, 
                                and National Institutes of Health reports. ")))),
              
              # Patient clinical datatables
              box(title = "Patient Data", 
                  collapsible = TRUE,
                  width = 12,
                  tabsetPanel(
                    tabPanel("Allergies",  
                             dataTableOutput("patient_allergies_dt")), 
                    tabPanel("Careplans",  
                             dataTableOutput("patient_careplans_dt"),
                             plotOutput("patient_careplans_plot")),
                    tabPanel("Conditions",  
                             dataTableOutput("patient_conditions_dt")),
                    tabPanel("Encounters",  
                             dataTableOutput("patient_encounters_dt")), 
                    tabPanel("Imaging Studies", 
                             dataTableOutput("patient_imaging_studies_dt")),
                    tabPanel("Immunizations", 
                             dataTableOutput("patient_immunizations_dt")),  
                    tabPanel("Medications",  
                             dataTableOutput("patient_medications_dt")), 
                    tabPanel("Observations",  
                             dataTableOutput("patient_observations_dt")),
                    tabPanel("Organizations", 
                             dataTableOutput("patient_organizations_dt")),
                    tabPanel("Patients",  
                             dataTableOutput("patient_patients_dt")), 
                    tabPanel("Procedures",  
                             dataTableOutput("patient_procedures_dt")),
                    tabPanel("Providers",  
                             dataTableOutput("patient_providers_dt"))))),
      
      ### --- 2.3.4 Patient genomic data
      
      tabItem(tabName = "patient-genomic", 
              box(title = "Add checkboxes for data"),
              box(title = "Add tables for summary GEO data which
                  could be a selection of the data for each series")),
      
      # 2.3.4 Patient genomic data
      
      tabItem(tabName = "patient-genomic",
              
              # Description: movtivation for integration
              box(title="Motivation for integration",
                  h5("Gene expression profiles available from GEO 
                      are used to analyse the gene expression associated with 
                      clinical observations. Identification of genotypic patterns 
                      can then be mapped be to recordings from clinical 
                      encounters and create links 
                      between treatment and improvemnts or disprovements")),
              
              # Description: data from GEO description              
              box(title = h5("The data used in this model, GSE25462, consists of samples of
                  individuals of 3 subgroups: diabetes patients, normoglycemic but insulin 
                  resistant patients with parental family history (FH+) and family history 
                  negative control individuals (FH-). The expression of serum response 
                  factor (SRF) and cofactor (MKL1) have increased expression in 
                  T2D and FH+ groups. The medication most commmonly used to treat insulin 
                  resistance is metformin; the key pathophysiological result of T2D. 
                  This study identifies an increase in the expression of actin cytoskeleton 
                  mediating genes such as SRF and MKL1 and indicate that these genes may mediate 
                  alterations in glucose reuptake that consequently create insulin")),
              
              
      