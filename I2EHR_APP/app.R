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

list.of.packages <- c("ggplot2", "ggridges", "lattice","viridis","shiny","shinydashboard","DiagrammeR", "plotly","shinyWidgets")
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


### load GEO data 

gse25462 <- getGEO("GSE25462", GSEMatrix = TRUE)


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
               menuSubItem("Genomic data", "patient-genomic")),
#      menuItem("Patient", 
#               tabName = "patient", 
#               icon = icon("id-card")),
      menuItem("Cohort Data", 
               tabName = "CohortTab",
               icon = icon("users"), 
               menuSubItem("Clinical data", "cohort-clinical"), 
               menuSubItem("Genomic data", "cohort-genomic")))),

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
care encounters in the US. The objective of Synthea is to address the legal and ethical limitations that has caused lagging in health record technology [40]. The
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
              
              
              box(title="Contact Details", width = 4,
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
                  US Census Bureau demographics, Centers for Disease Control and Prevention prevalence and incidence rates, 
                     and National Institutes of Health reports. "))
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

#  -------------------------- PATIENT DATA CLINICAL TAB

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
                                          "procedures")),
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

      tabItem(tabName ="cohort-genomic",
              box(title="Analysis results", 
                  width = 12,
                  tabsetPanel(
                    tabPanel("GEOdata", 
                             dataTableOutput("gse25462_table")),
                    tabPanel("Multidimensional Scaling",
                             box(
                             img(src="microarray_MDS.png"))),
                    tabPanel("Microarray Expression Density", 
                             img(src="microarray_expression_density.png")),
                    tabPanel("Data Distribution", 
                             img(src="microarray_boxplot_raw.png"),
                             plotOutput("Log2_Microarray_Exp"),
                             img(src="microarray_boxplot_normalised.png")),
                    tabPanel("Heatmap", 
                             img(src="microarray_heatmap.png")),
                    tabPanel("H1Ac levels", 
                             plotOutput("PCA_h1Ac")),
                    tabPanel("Insulin_resistance", 
                             plotOutput("PCA_IR"))
                  ))))))



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
    exp_raw <- log2(Biobase::exprs(gse25462[[1]]))
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
  
  
  #--- plot of insulin resistance levels
  output$PCA_IR <- renderPlot({
    # make the empty column 
    gse25462[[1]]$insulin_category <- 0
    # assign each column to its appropriate bin 
    gse25462[[1]]$insulin_category[(as.numeric(gse25462[[1]]$`fasting insulin (iv0inavg):ch1`)) >= 8] <- "diabetic"
    gse25462[[1]]$insulin_category[((as.numeric(gse25462[[1]]$`fasting insulin (iv0inavg):ch1`)) < 8 
                                    & (as.numeric(gse25462[[1]]$`fasting insulin (iv0inavg):ch1`)) > 3)] <- "optimal"
    gse25462[[1]]$insulin_category[(as.numeric(gse25462[[1]]$`fasting insulin (iv0inavg):ch1`)) < 3] <- "low"
    
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
  
  
#  genTable <- reactive({
#    validate(
#      need(!is.null(output$genTable[1,1]),
#         "No results for this dataset, choose another!"))
#  })
  
}


shinyApp(ui, server)