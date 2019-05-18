library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Patient Data"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(title = "Welcome"),
      box(title = "Controls",
          sliderInput("slider", 
                    "Number of Observations", 
                    1, 100, 150),
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
      box("Table", width = 12,
          tableOutput("genTable"))
    )
  )
)

server <- function(input, output) { 
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
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
           "procedures" = procedures.csv)
  })
  
  output$genTable <- renderTable({
    dataset_choice <- datasetInput()
    head(x=dataset_choice, n = input$slider)
  }) 
}

shinyApp(ui , server)