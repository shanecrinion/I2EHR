#import the synthea information that i want to make a datatable from
dir()
setwd('/home/shane/Documents/RShiny/csv/')
patients <- read.csv('patients.csv')
allergies <- read.csv('allergies.csv')
allergies <- read.csv('allergies.csv')
careplans <- read.csv('careplans.csv')
claims <- read.csv('claims.csv')
conditions <- read.csv('conditions.csv')
encounters <- read.csv('encounters.csv')
immunization <- read.csv('immunizations.csv')
medications <- read.csv('medications.csv')
observations <- read.csv('observations.csv')
procedures <- read.csv('procedures.csv')

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
                value = "00341a88-1cc1-4b39-b0f9-05b0531991a0"),
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset_selection",
                  label = "Choose a dataset:",
                  choices = c("all data","allergies","careplans","claims","conditions","encounters","immunization","medications", "observations","patients","procedures"))),
      
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
             "all data" = merged_patient_data,
             "allergies" = allergies,
             "careplans" = careplans,
             "claims" = claims,
             "conditions" = conditions,
             "encounters" = encounters,
             "immunization" = immunization,
             "medications" = medications,
             "observations" = observations,
             "patients" = patients,
             "procedures" = procedures
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