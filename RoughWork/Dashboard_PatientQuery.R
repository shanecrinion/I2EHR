#locate csv files
#setwd("/media/shane/Data/github/I2EHR/patient_files/csv/")

#import csv files
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))


#merging the data
library(plyr)
clinical_data <- ldply(.data = list.files(pattern="*.csv"),
                       .fun = read.csv,
                       header=TRUE)



#patient query
library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Patient Query"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "patientID",
                label = "Patient ID:",
                value = "0749b81c-9447-4b14-8e3c-a3486baae919"),
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset_selection",
                  label = "Choose a dataset:",
                  choices = c("all data",
                              "allergies",
                              "careplans",
                              "conditions",
                              "encounters",
                              "immunization",
                              "medications", 
                              "observations",
                              "patients",
                              "procedures"))),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Formatted text for caption ----
        h3(textOutput("patientID", container = span)),  
        
        
        # Output: Verbatim text for data summary ----
        verbatimTextOutput("patient_summary"),
        
        
        # Output: HTML table with requested number of observations ----
        tableOutput("view_entries")
        
        
      )
    )
  )
  
  
  # Define server logic to summarize and view selected dataset ----
  server <- function(input, output) {
    
    
    # Return the requested dataset ----
    # By declaring datasetInput as a reactive expression we ensure
    # that:
    #
    # 1. It is only called when the inputs it depends on changes
    # 2. The computation and result are shared by all the callers,
    #    i.e. it only executes a single time
    
    datasetInput <- reactive({
      switch(input$dataset_selection,
             "all data" = clinical_data,
             "allergies" = allergies.csv,
             "careplans" = careplans.csv,
             "conditions" = conditions.csv,
             "encounters" = encounters.csv,
             "immunization" = immunizations.csv,
             "medications" = medications.csv,
             "observations" = observations.csv,
             "patients" = patients.csv,
             "procedures" = procedures.csv
      )
    })
    
    output$patient_summary <- renderPrint({
      dataset_selection<-datasetInput()
      selection <- subset(dataset_selection, dataset_selection$PATIENT==input$patientID)
      print("The entries for patient ")
      nrow(selection)
    })
    
    
    # Show the first "n" observations ----
    output$view_entries <- renderTable({
      dataset_selection<-datasetInput()
      subset(dataset_selection, dataset_selection$PATIENT==input$patientID)
    })
    
  }
    
    
    # Create Shiny app ----
    shinyApp(ui = ui, server = server)