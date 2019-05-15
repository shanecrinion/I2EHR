library(shiny)

ui <- fluidPage(
  
  selectInput(inputId = "dataset_sex",
              label = "Choose a dataset:",
              choices = c("all data","patients")),
  
  radioButtons("genderpick", "Select the sex:",
               c("Male"="M", "Female"="F")),
  
  sliderInput("tableno", "Table size:", max=100, min=0, value=20),
  
  tableOutput("genTable"))
  
server <- function(input, output) {

  datasetInput <- reactive({
    switch(input$dataset_sex,
           "all data" = merged_patient_data,
           "patients" = patients)                  
  })
  

  output$genTable <- renderTable({
    dataset_choice <- datasetInput()
    choice <- subset(dataset_choice, dataset_choice$GENDER==input$genderpick)
    head(x=choice, n = input$tableno)
  })    
}

shinyApp(ui, server)