
#import csv files
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))


#merging the data
library(plyr)
clinical_data <- ldply(.data = list.files(pattern="*.csv"),
                       .fun = read.csv,
                       header=TRUE)


##merge the data
observations_merge <- merge(x = patients.csv, 
                            y = observations.csv, 
                            by.x = "Id", 
                            by.y= "PATIENT") 

### install the required packages

list.of.packages <- c("ggplot2",
                      "affyPLM",
                      "hgu133plus2.db",
                      "ggridges",
                      "lattice",
                      "viridis",
                      "shiny",
                      "shinydashboard",
                      "DiagrammeR",
                      "plotly",
                      "rgl",
                      "shinyWidgets")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



### Worksheet 1 ####

library(shiny) 
library(shinydashboard)
library(shinyWidgets)
library(ggridges)
library(ggplot2)
library(lattice)
library(viridis)
library(DiagrammeR)
library(GEOquery)
library(plotly)
library(affyPLM)

### Microarray Libraries

#General Bioconductor packages
library(Biobase)
library(oligoClasses)
#Annotation and data import packages
library(GEOquery)
library(pd.hugene.1.0.st.v1)
library(pd.hugene.2.0.st)
library(hugene10sttranscriptcluster.db)
library(hugene20sttranscriptcluster.db)
library(gridExtra)
#Quality control and pre-processing packages
library(oligo)
library(arrayQualityMetrics)
#Analysis and statistics packages
library(limma)
library(topGO)
library(ReactomePA)
library(clusterProfiler)
#Plotting and color options packages
library(gplots)
library(ggplot2)
library(geneplotter)
library(RColorBrewer)
library(pheatmap)

#Formatting/documentation packages
#library(rmarkdown)
#library(BiocStyle)
library(dplyr)
library(tidyr)

#Helpers:
library(stringr)
library(matrixStats)
library(genefilter)
library(openxlsx)
library(data.table)
#library(devtools)



### load GEO data 

gse25462 <- getGEO("GSE25462", GSEMatrix = TRUE)
GSE115313 <- getGEO("GSE115313", GSEMatrix = TRUE)
CC  <- GSE115313[[1]]

# add additional categories

# make the empty column 
gse25462[[1]]$insulin_category <- 0
# assign each column to its appropriate bin 
gse25462[[1]]$insulin_category[(as.numeric(gse25462[[1]]$`fasting insulin (iv0inavg):ch1`)) >= 8] <- "diabetic"
gse25462[[1]]$insulin_category[((as.numeric(gse25462[[1]]$`fasting insulin (iv0inavg):ch1`)) < 8 
                                & (as.numeric(gse25462[[1]]$`fasting insulin (iv0inavg):ch1`)) > 3)] <- "optimal"
gse25462[[1]]$insulin_category[(as.numeric(gse25462[[1]]$`fasting insulin (iv0inavg):ch1`)) < 3] <- "low"

#### categorise data
# convert to information to character format
gse25462[[1]]$disease_cat <- 0
gse25462[[1]]$disease_cat[gse25462[[1]]$characteristics_ch1.3=="family history: Family history negative"] <- "FH-"
gse25462[[1]]$disease_cat[gse25462[[1]]$characteristics_ch1.3== "family history: DM"] <- "T2D"
gse25462[[1]]$disease_cat[gse25462[[1]]$characteristics_ch1.3== "family history: Family history positive - 2 parents"] <- "FH+"
gse25462[[1]]$disease_cat[gse25462[[1]]$characteristics_ch1.3=="family history: Family history positive - 1 parent"] <- "FH+"



IR_norm <- normalize(gse25462[[1]], transfn=c("log"))

# convert to information to character format
IR_norm$characteristics_ch1.3 <- as.character(IR_norm$characteristics_ch1.3)
# limit to three values for clustering
IR_norm$characteristics_ch1.3[IR_norm$characteristics_ch1.3 == "family history: Family history negative"] <- "FH-"
IR_norm$characteristics_ch1.3[IR_norm$characteristics_ch1.3 == "family history: DM"] <- "T2D"
IR_norm$characteristics_ch1.3[IR_norm$characteristics_ch1.3 == "family history: Family history positive - 2 parents"] <- "FH+"
IR_norm$characteristics_ch1.3[IR_norm$characteristics_ch1.3 == "family history: Family history positive - 1 parent"] <- "FH+"


# convert back to avoid creating future problems
IR_norm$characteristics_ch1.3 <- as.factor(IR_norm$characteristics_ch1.3)


IR_exprs <- Biobase::exprs(IR_norm)

IR_raw <- log2(Biobase::exprs(gse25462[[1]]))

anno_IR <- AnnotationDbi::select(hugene20sttranscriptcluster.db,
                                  keys = (featureNames(IR_norm)),
                                  columns = c("SYMBOL", "GENENAME"),
                                  keytype = "PROBEID")


anno_IR <- subset(anno_IR, !is.na("SYMBOL"))

anno_grouped <- group_by(anno_IR, PROBEID)

anno_summarized <- 
  dplyr::summarize(anno_grouped, 
                   no_of_matches = n_distinct(SYMBOL))

anno_filtered <- filter(anno_summarized, no_of_matches > 1)

probe_stats <- anno_filtered 

ids_to_exlude <- (featureNames(IR_norm) %in% probe_stats$PROBEID)





###  UI

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "I2EHR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", 
               tabName = "overview", 
               icon = icon("info")),
      menuItem("Patient Data",
               icon=icon("user"),
               tabName="PatientTab",
               menuSubItem("Clinical data", "patient-clinical"),
               menuSubItem("Observations", "patient-observations"), 
               menuSubItem("Genomic data", "patient-genomic")),
#      menuItem("Patient", 
#               tabName = "patient", 
#               icon = icon("id-card")),
      menuItem("Cohort Data", 
               tabName = "CohortTab",
               icon = icon("users"), 
               menuSubItem("Clinical data", "cohort-clinical"), 
               menuSubItem("Genomic data", "cohort-genomic"),
               menuSubItem("GSE115313: Colon cancer", "colon-genomic"))
               #    menuSubItem("Disease query", "cohort-query")#
               )),

#      menuItem("Cohort Data",
#               tabName="CohortNu",
#               icon=icon("poll")))),
#  sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
#                     label = "Type patient name...")),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem(tabName = "overview", 
              box(title = "Welcome to the Interactive Integrated Electronic Health Record (I2EHR)", 
                  width=8, 
                  status = "success", 
                  solidHeader = TRUE,
                  tabsetPanel(
                    
                    
                    tabPanel(title = "Project Aims", 
                             img(src="flowchart.png", width=400, height=400),
                             h5("This project involves the development of a Shiny application 
to analyse and interact with clinical data. Synthea will be used to model a disease cohort and
perform predictive analytics. Gene expression data will then be downloaded to
provide proof-of-concept for clinical and molecular analytics. The objective of the
project is to develop an interactive genomic health record that can be used to
obtain statistical data at a patient and cohort level.")),
                    
                    
                    tabPanel(title = "Synthea", 
                             img(src="architecture.png", 
                                 width=600, height=300),
                             h5("Synthea (https://synthetichealth.github.io/synthea) is an open-source package
containing synthetic EHRs encoded in FHIR standard. Synthea models the
lifespan of patients based on the top 10 chronic conditions and reasons for medical
care encounters in the US. The objective of Synthea is to address the legal and ethical limitations that 
has caused lagging in health record technology [40]. The
framework for Synthea is based on the Publicly Available Data Approach to the
Realistic Synthetic Data (PADARSER). The model uses publicly
available health statistics as inputs to generate data from clinical workflow and
disease progression. Finally, the model includes a temporal model to provide a
complete profile for the patient beyond the disease of interest. The longitu
dinal model is ideal for modelling disease progression and performing population
analysis.")),
                    
                    
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
restrictions."),
                             br(),
                             em("Plan with GEO data: There are 10 diabetes patients, 15 family history negative (control) and 15 family history positive (being treated). 
                                Drug of choice is metformin. Assign the diabetes patients as nontreated patients. 
                                Assign the FH+ as those being treated to show a decrease in effect. 
                                Assign the FH- as those with no symptoms of diabetes as and no treatment as a control.
                                Compare gene expression for the diabetes patients to treated family history people to compare drug and 
                                claim that the difference is drug to application of the drug
                                Compare the diabetes to family history negative and claim the difference is due to disease.
                                Compare the FH+ to FH- and claim the difference is due to treated individual regressing to the expression of a normal individual")))),
              
              
              box(title="Contact Details", 
                  background = "black",
                  width = 4,
                  h4("Shane Crinion"),
                  h4("shanecrinion@gmail.com"), 
                  h4("+ 353 858018212"),
                  img(src="nui-galway.jpg", width=120, height=40))),
#                  fluidRow(column(width = 5, h5("shanecrinion@gmail.com")),
#                           column(width = 2, align = "center",
#                                  img(src="nui-galway.jpg", 
#                                      width=120, height=40)))

#  -------------------------- PATIENT DATA UNUSED?     

#  -------------------------- PATIENT DATA CLINICAL TAB   


tabItem(tabName="patient-clinical",
        box(title = "Patient Query", 
            status="primary",
            solidHeader = TRUE,
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
                       h5("Sources collected on the internet for demographic information include the
                  US Census Bureau demographics, Centers for Disease Control and Prevention prevalence 
                  and incidence rates, and National Institutes of Health reports. "))
            )),
        box(title= "Patient Plots",
            collapsible = TRUE, 
            width = 12,
            plotlyOutput("PatientBMI")),
        
        box(title = "Patient Data", 
            collapsible = TRUE,
            width = 12,
            tabsetPanel(
              # "Allergies",  
              # dataTableOutput("patient_allergies_dt")), 
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



#  -------------------------- PATIENT DATA OBSERVATIONS TAB


      tabItem(tabName = "patient-observations",
              box(title="Patient info", 
                  solidHeader = TRUE,
                  textInput(inputId = "observations_patient",
                            label = "Select a patient",
                            placeholder = "Enter patient ID:",
                            value="1425bcf6-2853-4c3a-b4c5-64fbe03d43d2"),
                  fluidRow(column(DT::dataTableOutput(outputId="info_patient", width = "100%"), 
                                  width = 12)), status = "success", width = 12),
              
              selectInput(inputId= "observation_selection",
                          label = "Select an observation for comparison",
                          choices = c(sort(as.character(unique(observations.csv$DESCRIPTION)))),
                          selected = "Body Mass Index"),
              plotlyOutput(outputId = "observations_plot")),

#  -------------------------- PATIENT DATA GENOMIC TAB
      



      tabItem(tabName = "patient-genomic",
              box(title="Genomic expression profiles",
                  h5("Gene expression profiles available from GEO are used to analyse 
the gene expression associated with 
                 clinical observations. Identification of genotypic patterns 
can then be mapped be to recordings from clinical encounters and create links 
                 between treatment and improvemnts or disprovements")),
              box(title = h5("The data used in this model, GSE25462, consists of samples of
                  individuals of 3 subgroups: diabetes patients, normoglycemic but insulin resistant patients with parental family history (FH+)
                  and family history negative control individuals (FH-).
                  The expression of serum response factor (SRF) and cofactor (MKL1) have increased expression in 
                  T2D and FH+ groups. The medication most commmonly used to treat insulin resistance is metformin; the key pathophysiological result of T2D. 
                  This study identifies an increase in the expression of actin cytoskeleton mediating genes such as SRF and MKL1 and indicate that these genes may mediate alterations in glucose reuptake
                  that consequently create insul"))),

#  -------------------------- PATIENT DATA GENOMIC TAB


      tabItem(tabName = "cohort-clinical",
              box(title="Controls", collapsible = TRUE,
                  # Input: Selector for choosing dataset ----
                  selectInput(inputId = "patient_dataset_2",
                              label = "Choose a dataset:",
                              choices = c(
                                # "allergies", # no allergy data in this dataset
                                          "careplans",
                                          "conditions",
                                          "encounters",
                                          "immunization",
                                          "medications", 
                                          "observations",
                                          "patients",
                                          "procedures"),
                              selected = "observations"),
                  
                  uiOutput("secondSelection"),
                  
#                  checkboxGroupInput(inputId = "headers", 
#                                     "Included data", 
#                                     choices = names(input$patient_dataset_2)),
                  sliderInput("slider_2", 
                              "Number of Observations", 
                              min = 1, value=5, 
                              max = 300)),
          box(title = "Data Sources", 
              collapsible = TRUE,
              tags$ul(
                tags$li("US Census Bureau demographics"), 
                tags$li("Centers for Disease Control and Prevention prevalence"), 
                tags$li("National Institutes of Health reports."))),
          box(title="Graphs", 
              collapsible = TRUE,
              width = 12,
              tabsetPanel(
                tabPanel("Ethnicity", plotOutput("plot1")),
                tabPanel("Hg Measurements", plotOutput("plot2")),
                tabPanel("Disease Prevalence", plotOutput("plot3")),
                tabPanel("BMI", plotOutput("plot4")))),
          box(title="Data Table", 
              width = 12, 
              dataTableOutput("genTable"))),

      tabItem(tabName = "cohort-genomic", 
                  box(title="Microarray analysis results", 
                      width = 12,
                      tabsetPanel(
                        tabPanel("Report",
                                 includeHTML("index.html")),
                        tabPanel("RLE",
                                 plotOutput("RLE")),
                        tabPanel("PCA", 
                                 h5("Raw data PCA"),
                                 plotOutput("PCA_IR"),
                                 br(),
                                 h5("Varience explained by PC"),
                                 plotOutput("PCA_2D_normalised"),
                                 br(),
                                 h5("PCA following calibration"),
                                 img(src="calibrated_PCA.png"),
                                 plotOutput("PCA_Calibrated")),
                        tabPanel("Intensity Filtering",
                                 plotOutput("Intensity_Filtering")),
                        tabPanel("Heatmap_Samples",
                                 tags$iframe(style="height:600px; width:100%", 
                                             src="heatmap.pdf")),
                        tabPanel("Array Annotation",
                                 h5("Filtered probes due to ambiguous mapping:"),
                                 dataTableOutput("array_annotation"),
                                 h5("Exclusion frequency"),
                                 tableOutput("excluded_probes")),
                        tabPanel("Valid gene list",
                                 dataTableOutput("valid_genes")),
                        tabPanel("GEOdata", 
                                 dataTableOutput("gse25462_table")),
                        tabPanel("Multidimensional Scaling",
                                   img(src="microarray_MDS.png")),
                        tabPanel("Microarray Expression Density", 
                                 img(src="microarray_expression_density.png")),
                        tabPanel("Data Distribution", 
                                 img(src="microarray_boxplot_raw.png"),
                                 plotOutput("Log2_Microarray_Exp"),
                                 img(src="microarray_boxplot_normalised.png")),
                        tabPanel("Heatmap", 
                                 img(src="microarray_heatmap.png")),
                        tabPanel("H1Ac levels", 
                                 plotOutput("PCA_h1Ac"))))),
            tabItem(tabName= "colon-genomic",
                    box(title="Differential Expression",
                        width=12, status="success",
                        plotOutput("GSE115313_Differential_Expression")),
                    box(title = "Intensity Filtering",
                        width = 8,
                        status="success",
                        collapsible=TRUE,
                        solidHeader=TRUE,
                        plotOutput("GSE115313_IF")),
                    box(title = "Number of samples",
                        width = 4,
                        status = "success",
                        collapsible=TRUE,
                        solidHeader=TRUE,
                        tableOutput("GSE115313_Sample_Numbers")),
                    box(title = "PCA",
                        width = 7,
                        status="success",
                        collapsible = TRUE,
                        solidHeader=TRUE,
                        plotOutput("GSE115313_PCA")),
                    box(title = "Log2 Deviation",
                        status="success",
                        width = 5,
                        collapsible = TRUE,
                        solidHeader=TRUE,
                        plotOutput("GSE115313_Log2Deviation")),
                    box(title = "Heatmap",
                        status="success",
                        collapsible=TRUE,
                        width = 9,
                        solidHeader=TRUE,
                        plotOutput("GSE115313_Heatmap")),
                    box(title = "Annotation",
                        collapsible=TRUE,
                        collapsed =TRUE,
                        dataTableOutput("GSE115313_Annotation")),
                    
                    box(title = "GSE115313: patients with colon cancer +/- T2DM.",
                        width = 12,
                        status="success",
                        collapsible=TRUE, 
                        collapsed=TRUE,
                        h5("This is a transcriptomics analysis contributing to a bigger project 
                              that tries to shed light on the role of type 2 diabetes mellitus (T2DM) as a risk factor for colon cancer (CC). Here we present a gene expression screening of paired tumor and normal colon mucosa samples in a cohort of 42 CC patients, 
                            23 of them with T2DM. Using gene set enrichment, 
                           we identified an unexpected overlap of pathways over-represented in diabetics compared to non-diabetics, 
                           both in tumor and normal mucosa, including diabetes-related metabolic and signaling processes. An integration with other -omic studies suggests that in diabetics, the local 
                           micro-environment in normal colon mucosa may be a factor driving field cancerization which may promote carcinogenesis. Several of these pathways converged on the tumor initiation axis TEAD/YAP-TAZ. Cell culture studies 
                           confirmed that high glucose concentrations upregulate this pathway in non-tumor colon cells. In conclusion, diabetes is associated to deregulation of cancer-related processes in normal colon mucosa adjacent to 
                            tissue which has undergone a malignant transformation. 
                           These data support the existence of the field of cancerization paradigm in 
                           diabetes and set a new framework to study link between diabetes and cancer."))
                    ))))


server <- function(input, output, session) { 

#### PATIENT DATA ####
  

  output$patient_dataset_selection <- renderUI({
    selectInput("X Value", 
                "Date:", 
                choices = names(input$patient_dataset_1))
  })
  
  #### DATA TABLES

    
#  output$patient_allergies_dt <- renderDataTable({allergies.csv})
  output$patient_careplans_dt <- renderDataTable({subset(careplans.csv, PATIENT == input$search)})
  output$patient_conditions_dt <- renderDataTable({subset(conditions.csv, PATIENT == input$search)})
  output$patient_encounters_dt <- renderDataTable({subset(encounters.csv, PATIENT == input$search)})
  output$patient_imaging_studies_dt <- renderDataTable({subset(imaging_studies.csv, PATIENT == input$search)})
  output$patient_immunizations_dt <- renderDataTable({subset(immunizations.csv, PATIENT == input$search)})
  output$patient_medications_dt <- renderDataTable({subset(medications.csv, PATIENT == input$search)})
  output$patient_observations_dt <- renderDataTable({subset(observations.csv, PATIENT == input$search)})
  output$patient_organizations_dt <- renderDataTable({subset(organizations.csv, Id == input$search)})
  output$patient_patients_dt <- renderDataTable({subset(patients.csv, Id == input$search)})
  output$patient_procedures_dt <- renderDataTable({subset(procedures.csv, PATIENT == input$search)})
  output$patient_providers_dt <- renderDataTable({subset(providers.csv, Id == input$search)})
  
  #output$pa <- renderTable({
  #  patient_dataset_1 <- datasetInput()
  #  head(x=patient_dataset_1, n = input$slider_1)
  #}) 
  
  output$PatientBMI <- renderPlotly({
    #replace x with input$dropdown
    #bmi_all <- observations.csv[observations.csv$DESCRIPTION == "Body Mass Index",]
    #replace x with input$dropdown
    
    bmi_all <- observations.csv[observations.csv$DESCRIPTION == "Body Mass Index",]
    bmi_all$DATE <- as.character(bmi_all$DATE)
    bmi_all$VALUE <- as.numeric(as.character(bmi_all$VALUE))
    
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )        
    x <- list(
      title = "Date of observation",
      titlefont = f
    )
    y <- list(
      title = "BMI measurement (kg/m2)",
      titlefont = f
    )
    
    bmi_all[bmi_all$PATIENT == "1425bcf6-2853-4c3a-b4c5-64fbe03d43d2",]  %>% 
      plot_ly(
        x = ~DATE, y =~VALUE, 
        colors = "green", 
        type = "scatter")
  })
  

  
#### COHORT DATA #####
  
  observe({
    x <- input$datasetInput
    if (is.null(x))
      x <- character(0)
    updateCheckboxGroupInput(session, 
                             inputId = "inCheckboxGroup2",
                             label = paste("headings", length(x)),
                             choices = names(x),
                             selected=names(x))
  })
  
  

  ###SELECTION OF THE 
##  datasetInput <- reactive({
##    switch(input$patient_dataset_1,
##           "all data" = clinical_data,
##           "allergies" = allergies.csv,
##           "careplans" = careplans.csv,
##           "conditions" = conditions.csv,
#           "encounters" = encounters.csv,
#           "immunization" = immunizations.csv,
#           "medications" = medications.csv,
#           "observations" = observations.csv,
#           "patients" = patients.csv,
#           "procedures" = procedures.csv)
    
#  })
  

    # Can use character(0) to remove all choices
    #    if (is.null(x))
    #      x <- character(0)
    
    # Can also set the label and select items
    #updateCheckboxGroupInput(session, "inCheckboxGroup2",
    #                        label = paste("Checkboxgroup label", length(x)),
    #    #                        choices = names(x),
    #                   selected = x
    #    )
    #})

  
  output$plot1 <- renderPlot({
    library(ggplot2)
    
    ggplot(as.data.frame(patients.csv$ETHNICITY),
           aes(x=patients.csv$ETHNICITY, 
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
                                            colour = "white")) 
    
  })
  
  
  output$plot2 <- renderPlot({
    
    # Hemoglobin measurements and plots 
    hemoglobin_measurements <- 	
      subset(observations_merge,
             subset = (observations_merge$DESCRIPTION == "Hemoglobin A1c/Hemoglobin.total in Blood"))
    
    hemoglobin_measurements$BIRTHDATE <- 
      substring(hemoglobin_measurements$BIRTHDATE, 1, 4) 
    
    hemoglobin_measurements$DECADE <- 
      10*as.integer(as.numeric(as.character(hemoglobin_measurements$BIRTHDATE)) / 10)
    
    hemoglobin_measurements$VALUEBIN <- 
      1*as.integer(as.numeric(as.character(hemoglobin_measurements$VALUE)) / 1)
    
    # Plot hemoglobin measurements
    ggplot(hemoglobin_measurements) + 
      geom_density_ridges(aes(x = VALUEBIN, 
                              y = DECADE, 
                              group = interaction(GENDER, DECADE),
                              fill = GENDER), 
                          alpha = 0.6, 
                          scale = 0.8) +
      geom_vline(xintercept = 5.7, 
                 color = "lightskyblue1", size=0.5) +
      geom_vline(xintercept = 6.2, 
                 color = "tomato", size=0.5) +
      scale_fill_manual(values=c("#0571b0", "#ca0020")) +
      theme_classic() +
      theme(
        axis.text.x= element_text(angle = 30),
        panel.grid.major.y =element_line(colour = "gray95"),
        panel.grid.major.x =element_line(colour = "gray95"),
        panel.grid.minor.x =element_line(colour = "gray95"))
    
  })
  
output$plot3 <- renderPlot({
  
  disorders_vector <- as.vector(count(conditions.csv$DESCRIPTION))
  #disorders_vector$freqs <- as.numeric(disorders_vector$freqs)
  
  par(mar=c(2, 28, 5, 5))
  
  xlim <- c(0, 1.1*max(disorders_vector$freq))
  
  xx <- barplot(disorders_vector$freq,
                xaxt = 'n',
                horiz = TRUE,
                col=viridis(27),
                width = 12,
                xlim = xlim,
                main = "Frequency of each metabolics disorder",
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
    
  datasetInput <- reactive({
    switch(input$patient_dataset_2,
           "all data" = clinical_data,
           "allergies" = allergies.csv,
           "careplans" = careplans.csv,
           "conditions" = conditions.csv,
           "encounters" = encounters.csv,
           "immunization" = immunizations.csv,
           "medications" = medications.csv,
           "observations" = observations.csv,
           "patients" = patients.csv,
           "procedures" = procedures.csv)
  })
  
  
  output$secondSelection <- renderUI({
    selectInput("Options", "X choice:", 
                choices = colnames(input$patient_dataset_2))
  })
  
  output$plot4 <- renderPlot({
    
  library(ggplot2)
  library(ggridges)
  library(lattice)
  
  
  ##merge the data
  observations_merge <- merge(x = patients.csv, 
                              y = observations.csv, 
                              by.x = "Id", 
                              by.y= "PATIENT") 
  
  ## could alternatively do the below command using to obsevation code (probably better)
  bmi_measurements <- 	
    subset(observations_merge,
           subset = (observations_merge$DESCRIPTION == "Body Mass Index"))
  
  bmi_measurements$BIRTHDATE <- 
    substring(bmi_measurements$BIRTHDATE, 1, 4) 
  
  bmi_measurements$DEATHDATE <- substring(bmi_measurements$DEATHDATE, 1, 4)
  #  bmi_measurements$DECADE <- 
  #    10*as.integer(as.numeric(bmi_measurements$BIRTHDATE/10))
  
  bmi_measurements$DECADE <- 
    10*as.integer(as.numeric(as.character(bmi_measurements$BIRTHDATE)) / 10)
  
  bmi_measurements$VALUEBIN <- 
    1*as.integer(as.numeric(as.character(bmi_measurements$VALUE)) / 1)
  
  #plot the two
  ggplot(bmi_measurements) + 
    geom_density_ridges(aes(x = VALUEBIN, 
                            y = DECADE, 
                            group = interaction(GENDER, DECADE),
                            fill = GENDER), 
                        alpha = 0.6, 
                        scale = 0.8) +
    
#    geom_vline(xintercept = 5.7, 
#               color = "lightskyblue1", size=0.5) +
    
#    geom_vline(xintercept = 6.2, 
#               color = "tomato", size=0.5) +
    scale_fill_manual(values=c("#0571b0", "#ca0020")) +
    theme_classic() +
    theme(
      axis.text.x= element_text(angle = 30),
      panel.grid.major.y =element_line(colour = "gray95"),
      panel.grid.major.x =element_line(colour = "gray95"),
      panel.grid.minor.x =element_line(colour = "gray95"))
  })
  

  output$genTable <- renderDataTable({
    patient_dataset_2 <- datasetInput()
    head(x=patient_dataset_2, n = input$slider_2)
  }) 
  
 #### GEO DATA
  
  output$Log2_Microarray_Exp <- renderPlot({
    oligo::boxplot(exp_raw, target = "core", las=2,
                   main = "Boxplot of log2-intensitites for the raw data")
    par(cex.lab=0.5)
  })

  
  output$RLE <- renderPlot({
    
    row_medians_assayData <- 
      Biobase::rowMedians(as.matrix(
        log2(Biobase::exprs(gse25462[[1]]))))
    
    RLE_data <- sweep(log2(Biobase::exprs(gse25462[[1]])), 1, 
                      row_medians_assayData)


    # class for the fill
    RLE_class <- data.frame(patient_array = rownames(pData(gse25462[[1]])), 
                            disease_cat=gse25462[[1]]$disease_cat)
    
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
    
  })
  
  output$PCA_2D_normalised <-  renderPlot({

    ### get the prinicipal component values
    
    PCA <- prcomp(t(exp_gse), scale = FALSE)
   
    percentVar <- round(100*PCA$sdev^2/sum(PCA$sdev^2),1)
    
    barplot(percentVar, 
            main = "Variation Explained per PC", 
            col=rainbow(50), xlab="Principal Component", 
            ylab="%", names=c(1:50), 
            ylim=c(0,30), 
            las=2)
    
  })
  
  
  output$PCA_Calibrated <- renderPlot({

    ### get the prinicipal component values
    
    PCA <- prcomp(t(exp_gse), scale = FALSE)
    
    # get the percentage of variance
    # indicates that 28.4 pct of variance is from the first, maybe use only 2
    percentVar <- round(100*PCA$sdev^2/sum(PCA$sdev^2),1)
    
    sd_ratio <- sqrt(percentVar[2] / percentVar[1])

    dataGG <- data.frame(PC1 = PCA$x[,1], PC2 = PCA$x[,2],
                         Disease_Category = 
                           Biobase::pData(IR_norm)$characteristics_ch1.3,
                         Insulin_Resistance = 
                           Biobase::pData(IR_norm)$insulin_category)
    
    ggplot(dataGG, aes(PC1, PC2)) +
      geom_point(aes(shape = Disease_Category, 
                     colour = Insulin_Resistance)) +
      ggtitle("PCA plot of the calibrated, summarized data") +
      xlab(paste0("PC1, VarExp: ", percentVar[1], "%")) +
      ylab(paste0("PC2, VarExp: ", percentVar[2], "%")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_fixed(ratio = sd_ratio)
    
  })
    
  output$Intensity_Filtering <- renderPlot({
    
    gse_medians <- rowMedians(Biobase::exprs(IR_norm))
    man_threshold <- 60
    hist_res <- hist(gse_medians, 
                     breaks=10000,
                     xlim=c(0,10000),
                     ylim=c(0,0.002),
                     col = "cornsilk1", 
                     freq = FALSE, 
                     main = "Histogram of the median intensities", 
                     border = "antiquewhite4",
                     xlab = "Median intensities")
    hist_res
    abline(v = man_threshold, col = "coral4", lwd = 2)
    
  })
  
  ###### need to fix this issue 
  
  output$Heatmap_Samples <- renderPlotly({

    ### expression oof the normalised dat
    
    dists <- as.matrix(dist(t(exp_gse), method = "manhattan"))
    
    rownames(dists) <- row.names(pData(IR_norm))
    
    hmcol <- rev(colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(255))
    
    colnames(dists) <- NULL
    diag(dists) <- NA
    
    pheatmap(dists, col = (hmcol), 
             legend = TRUE, 
             treeheight_row = 0,
             legend_breaks = c(min(dists, na.rm = TRUE), 
                               max(dists, na.rm = TRUE)), 
             legend_labels = (c("small distance", "large distance")),
             main = "Clustering heatmap for the calibrated samples")
    
  })

  
  output$array_annotation <- renderDataTable({

    head(anno_summarized)
    anno_filtered
    
  })
    
  output$excluded_probes <- renderTable({


    table(ids_to_exlude)
  })
  
  
  output$valid_genes <- renderDataTable({
  

    gse_final <- subset(IR_norm, !ids_to_exlude)
    
    fData(gse_final)$PROBEID <- rownames(fData(gse_final))
    fData(gse_final) <- left_join(fData(gse_final), anno_IR)
    rownames(fData(gse_final)) <- fData(gse_final)$PROBEID 
    fData(gse_final)
    
  })
  
  output$gene_features <- renderDataTable({

    ids_to_exlude <- (featureNames(IR_norm) %in% probe_stats$PROBEID)
    gse_final <- subset(IR_norm, !ids_to_exlude)
    
    fData(gse_final)
  })
  
  output$gse25462_table <- renderDataTable({
    pData(phenoData(gse25462[[1]]))
  })
  
  output$PCA_h1Ac <- renderPlot({
    # make the empty column 
    gse25462[[1]]$diabetes_status <- 0
    # assign each column to its appropriate bin 
    gse25462[[1]]$diabetes_status[gse25462[[1]]$`hemoglobin a1c:ch1`>=6.5] <- "diabetic levels"
    gse25462[[1]]$diabetes_status[(gse25462[[1]]$`hemoglobin a1c:ch1`< 6.5 
                                   & gse25462[[1]]$`hemoglobin a1c:ch1`> 6)] <- "pre-diabetic levels"
    gse25462[[1]]$diabetes_status[gse25462[[1]]$`hemoglobin a1c:ch1`< 6] <- "normal levels"
    #log 2
    IR_raw <- log2(Biobase::exprs(gse25462[[1]]))
    #pca
    PCA_raw <- prcomp(t(exp_raw), scale. = FALSE)
    
    percentVar <- round(100*PCA_raw$sdev^2/sum(PCA_raw$sdev^2),1)
    
    sd_ratio <- sqrt(percentVar[2] / percentVar[1])
    
    dataGG <- data.frame(PC1 = PCA_raw$x[,1], PC2 = PCA_raw$x[,2],
                         Disease = pData(gse25462[[1]])$characteristics_ch1.3,# disease state
                         Phenotype = pData(gse25462[[1]])$diabetes_status, #fasting glucose levels
                         Individual = pData(gse25462[[1]])$title)
    
    ggplot(dataGG, aes(PC1, PC2)) +
      geom_point(
        aes(shape = Disease, 
            colour = Phenotype)) +
      
      ggtitle("PCA plot of the log-transformed raw expression data") +
      
      xlab(paste0("PC1, VarExp: ", percentVar[1], "%")) +
      
      ylab(paste0("PC2, VarExp: ", percentVar[2], "%")) +
      
      theme(plot.title = element_text(hjust = 0.5))+
      
      coord_fixed(ratio = sd_ratio) 
  }) 
  
  
  output$GSE115313_Sample_Numbers <- renderTable({
    
    raw_data <- GSE115313[[1]]
    minguez_eset_norm <- raw_data
    no_of_samples <- 
      table(paste0(pData(minguez_eset_norm)$'diabetes_status:ch1', "_", 
                   pData(minguez_eset_norm)$'tissue_type:ch1'))
    no_of_samples 
    
  })
  
 
  
  output$GSE115313_pData <- renderDataTable({
    pData(phenoData(GSE115313[[1]]))})
  
  #--- plot of insulin resistance levels
  output$PCA_IR <- renderPlot({

    #log 2
    exp_raw <- log2(Biobase::exprs(gse25462[[1]]))
    #pca
    PCA_raw <- prcomp(t(exp_raw), scale. = FALSE)
    percentVar <- round(100*PCA_raw$sdev^2/sum(PCA_raw$sdev^2),1)
    sd_ratio <- sqrt(percentVar[2] / percentVar[1])
    dataGG <- data.frame(PC1 = PCA_raw$x[,1], PC2 = PCA_raw$x[,2],
                         Disease = pData(gse25462[[1]])$characteristics_ch1.3,# disease state
                         Phenotype = pData(gse25462[[1]])$insulin_category, #fasting glucose levels
                         Individual = pData(gse25462[[1]])$title)
    ggplot(dataGG, aes(PC1, PC2)) +
      geom_point(
        aes(shape = Disease, 
            colour = Phenotype)) +
      ggtitle("PCA plot of the log-transformed raw expression data") +
      xlab(paste0("PC1, VarExp: ", percentVar[1], "%")) +
      ylab(paste0("PC2, VarExp: ", percentVar[2], "%")) +
      theme(plot.title = element_text(hjust = 0.5))+
      coord_fixed(ratio = sd_ratio) 
    #  scale_shape_manual(values = c(4,15)) + 
    # scale_color_manual(values = c("darkorange2", "dodgerblue4", "gold", "deepskyblue1"))
    
  })
  
  output$GSE115313_Differential_Expression <- renderPlot({
    
    raw_data <- GSE115313[[1]]
    
    minguez_eset <- raw_data
    minguez_eset_norm <- raw_data
    
    
    man_threshold <- 2.5
    
    no_of_samples <- 
      table(paste0(pData(minguez_eset_norm)$'diabetes_status:ch1', "_", 
                   pData(minguez_eset_norm)$'tissue_type:ch1'))
    no_of_samples 
    
    samples_cutoff <- min(no_of_samples)
    
    idx_man_threshold <- apply(Biobase::exprs(minguez_eset_norm), 1,
                               function(x){
                                 sum(x > man_threshold) >= samples_cutoff})
    
    minguez_manfiltered <- subset(minguez_eset_norm, idx_man_threshold)
    
    
    anno_minguez <- AnnotationDbi::select(hugene20sttranscriptcluster.db,
                                          keys = (featureNames(minguez_manfiltered)),
                                          columns = c("SYMBOL", "GENENAME"),
                                          keytype = "PROBEID")
    
    anno_minguez <- subset(anno_minguez, !is.na(SYMBOL))
    
    anno_grouped <- group_by(anno_minguez, PROBEID)
    anno_summarized <- 
      dplyr::summarize(anno_grouped, no_of_matches = n_distinct(SYMBOL))
    
    anno_filtered <- filter(anno_summarized, no_of_matches > 1)
    
    probe_stats <- anno_filtered 
    
    ids_to_exlude <- (featureNames(minguez_manfiltered) %in% probe_stats$PROBEID)
    
    minguez_final <- subset(minguez_manfiltered, !ids_to_exlude)
    
    individual <- 
      minguez_final$geo_accession
    
    tissue <- str_replace_all(Biobase::pData(minguez_final)$'tissue_type:ch1',
                              " ", "_")
    
    tissue <- ifelse(tissue == "Colon_cancer_Tumor",
                     "CC", "nC")
    
    disease <- str_replace_all(Biobase::pData(minguez_final)$'diabetes_status:ch1',
                               " ", "_")
    
    disease <- ifelse(disease == "diabetic_patient",
                      "T2D", "nD")
    
    tissue_T2D <- tissue[disease == "T2D"]
    TCFL2_expr <- Biobase::exprs(minguez_final)["16709333", disease == "T2D"]
    TCFL2_data <- as.data.frame(TCFL2_expr)
    
    colnames(TCFL2_data)[1] <- "org_value"
    TCFL2_data <- mutate(TCFL2_data, 
                         individual = i_T2D, 
                         tissue_T2D)
    
    
    TCFL2_data$tissue_T2D <- factor(TCFL2_data$tissue_T2D, 
                                    levels = c("CC", "nC"))
    
    TCFL2_EC <-
      
      ggplot(data = TCFL2_data, aes(x = tissue_T2D, y = org_value, 
                                    group = tissue_T2D, fill= tissue_T2D)) +
      geom_violin() +
      ggtitle("TCFL2 gene expression dispersal")
    
    grid.arrange(TCFL2_plot, TCFL2_EC, nrow = 1)
    
    
  })
  
  output$GSE115313_IF <-  renderPlot({
    raw_data <- GSE115313[[1]]
    minguez_eset_norm <- raw_data

    man_threshold <- 2.5
    
    minguez_medians <- rowMedians(Biobase::exprs(minguez_eset_norm))
    
    man_threshold <- 2.5
    
    hist(minguez_medians, 100, col = "cornsilk", freq = FALSE, 
                     main = "Histogram of the median intensities",
                     border = "antiquewhite4",
                     xlab = "Median intensities")
    
    abline(v = man_threshold, col = "coral4", lwd = 2)
    
    
  })
  output$GSE115313_PCA <- renderPlot({
    ## Principal Component Analysis 
    # 1. Generate values 
    raw_data <- GSE115313[[1]]
    pData(raw_data) <- pData(raw_data)[,c("geo_accession", 
                                          "characteristics_ch1.1", 
                                          "description", 
                                          "diabetes_status:ch1", 
                                          "tissue_type:ch1")]
    exp_raw <- exprs(raw_data)
    PCA_raw <- prcomp(t(exp_raw), scale. = FALSE)
    # 2. Get the percentage variance- understand how many components we should use
    percentVar <- round(100*PCA_raw$sdev^2/sum(PCA_raw$sdev^2),1)
    sd_ratio <- sqrt(percentVar[2] / percentVar[1])
    dataGG <- data.frame(PC1 = PCA_raw$x[,1], PC2 = PCA_raw$x[,2],
                         Disease = pData(raw_data)$'diabetes_status:ch1',
                         Phenotype = pData(raw_data)$'tissue_type:ch1',
                         Individual = pData(raw_data)$geo_accession)
    
    ggplot(dataGG, aes(PC1, PC2)) +
      geom_point(aes(shape = Disease, colour = Phenotype)) +
      ggtitle("PCA plot of the log-transformed raw expression data") +
      xlab(paste0("PC1, VarExp: ", percentVar[1], "%")) +
      ylab(paste0("PC2, VarExp: ", percentVar[2], "%")) +
      theme(plot.title = element_text(hjust = 0.5))+
      coord_fixed(ratio = sd_ratio) +
      scale_shape_manual(values = c(4,15)) + 
      scale_color_manual(values = c("darkorange2", "dodgerblue4"))
  })
  
  output$GSE115313_Log2 <- renderPlot({
    raw_data <- GSE115313[[1]]
    pData(raw_data) <- pData(raw_data)[,c("geo_accession", 
                                          "characteristics_ch1.1", 
                                          "description", 
                                          "diabetes_status:ch1", 
                                          "tissue_type:ch1")]
    
    oligo::boxplot(raw_data, target = "core", 
                   main = "Boxplot of log2-intensitites for the raw data")
    

  })
  
  output$GSE115313_Log2Deviation <- renderPlot({
    minguez_eset <- raw_data
    minguez_eset_norm <- raw_data
    
    row_medians_assayData <- 
      Biobase::rowMedians(as.matrix(Biobase::exprs(minguez_eset)))
    
    RLE_data <- sweep(Biobase::exprs(minguez_eset), 1, row_medians_assayData)
    
    RLE_data <- as.data.frame(RLE_data)
    RLE_data_gathered <- 
      tidyr::gather(RLE_data, patient_array, log2_expression_deviation)
    
    
    ggplot2::ggplot(RLE_data_gathered, aes(patient_array,
                                           log2_expression_deviation)) + 
      geom_boxplot(outlier.shape = NA) + 
      ylim(c(-2, 2)) + 
      theme(axis.text.x = element_text(colour = "aquamarine4", 
                                       angle = 60, size = 6.5, hjust = 1 ,
                                       face = "bold"))
    
  })
  

  
  output$GSE115313_Heatmap <- renderPlot({
  ### heatmap
  raw_data <- GSE115313[[1]]
  pData(raw_data) <- pData(raw_data)[,c("geo_accession", 
                                        "characteristics_ch1.1", 
                                        "description", 
                                        "diabetes_status:ch1", 
                                        "tissue_type:ch1")]
  
    
  exp_raw <- exprs(raw_data)  
    
  dists <- as.matrix(dist(t(exp_raw), method = "manhattan"))
  
  minguez_eset <- raw_data
  minguez_eset_norm <- raw_data
  
  rownames(dists) <- row.names(pData(minguez_eset_norm))
  hmcol <- rev(colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(255))
  colnames(dists) <- NULL
  diag(dists) <- NA
  
  ann_colors <- list(
    Phenotype = c(CC = "chartreuse4", nC = "burlywood3"),
    Disease = c(diabetic = "blue4", non_diabetic = "cadetblue2")
  )
  
  
  pheatmap(dists, col = (hmcol), 
           annotation_row = annotation_for_heatmap,
           annotation_colors = ann_colors,
           legend = TRUE, 
           treeheight_row = 0,
           legend_breaks = c(min(dists, na.rm = TRUE), 
                             max(dists, na.rm = TRUE)), 
           legend_labels = (c("small distance", "large distance")),
           main = "Clustering heatmap for the calibrated samples")})
  
  
  output$info_patient <- DT::renderDataTable ({
    
    patient <- input$observations_patient
    mydf <- patients.csv[patients.csv$Id == patient,]
    
    mydf[, colSums(mydf != "") != 0]

  })

  
  output$observations_plot <- renderPlotly({
    
    ## create merger to grab patient names
    patients.csv$PATIENT <- patients.csv$Id 
    observations_names <- merge(observations.csv,patients.csv, by  = "PATIENT") 
    
    patient <- input$observations_patient
    patient_data <- observations_names[observations_names$PATIENT == patient,]
    observation <- input$observation_selection
    patient_observation_data <- patient_data[patient_data$DESCRIPTION == observation,]
    
    
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
  })


  
  
  
#  genTable <- reactive({
#    validate(
#      need(!is.null(output$genTable[1,1]),
#         "No results for this dataset, choose another!"))
#  })
  
}


shinyApp(ui, server)