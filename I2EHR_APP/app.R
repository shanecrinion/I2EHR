library(shiny)

# Define UI for random distribution app ----
ui <- fluidPage(
  # App title ----
  titlePanel("Patient Data"),
  
  # Add Tab 1
    tabsetPanel(
      tabPanel("Patient Query", fluid = TRUE, 
                  sidebarLayout(
                    sidebarPanel(
                        # Input: Text for providing a caption ----
                        # Note: Changes made to the caption in the textInput control
                        # are updated in the output area immediately as you type
                        textInput(inputId = "patientID",
                                  label = "Patient ID:",
                                  value = "0749b81c-9447-4b14-8e3c-a3486baae919"),
                        
                        # br() element to introduce extra vertical spacing ----
                        br(),
                        
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
                      mainPanel(
                        # Output: Formatted text for caption ----
                        h3(textOutput("patientID", container = span)),  
                        # Output: Verbatim text for data summary ----
                        verbatimTextOutput("patient_summary"),
                        # Output: HTML table with requested number of observations ----
                        tableOutput("view_entries")
                      )
                    )
                  ),
      
      #Add Tab 2
      
      tabPanel("Sex Query", fluid = TRUE,
               ###SIDE
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId = "dataset_sex",
                               label = "Choose a dataset:",
                               choices = c("all data",
                                           "patients")),
                   radioButtons("genderpick", 
                                "Select the sex:",
                                c("Male"="M", 
                                  "Female"="F")),
                   sliderInput("tableno", 
                               "Table size:", 
                               max=100, min=0, value=20)),
                 ###MAIN
                 mainPanel(
                   tableOutput("genTable"))  
                  ))))

    

# Define server logic for random distribution app ----
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
  
  output$view_entries <- renderTable({
    dataset_selection<-datasetInput()
    subset(dataset_selection, 
           dataset_selection$PATIENT==input$patientID)
  })
  
  
  datasetInput <- reactive({
    switch(input$dataset_sex,
           "all data" = clinical_data,
           "patients" = patients.csv)                  
  })
  
  
  output$genTable <- renderTable({
    dataset_choice <- datasetInput()
    choice <- subset(dataset_choice, 
                     dataset_choice$GENDER==input$genderpick)
    head(x=choice, n = input$tableno)
  }) 
  
}

shinyApp(ui, server)

    