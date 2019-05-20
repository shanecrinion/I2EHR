### Worksheet 1 ####

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Patient Data"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Overview", 
               tabName = "overview", 
               icon = icon("id-card")),
      menuItem("Patient", 
               tabName = "patient", 
               icon = icon("id-card")),
      menuItem("Cohort", icon = icon("poll"), 
               tabName = "cohort",
               badgeLabel = "new", 
               badgeColor = "green")),
    
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Type patient name...")),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview", 
              box(title = "Welcome to the Interactive Integrated Electronic Health Record (I2EHR)", 
                  tabsetPanel(
                    tabPanel(title = "Study", h3(textOutput("Reason for Study"))),
                    tabPanel(title = "Synthea", textOutput("Information")),
                    tabPanel(title= "GEO", textOutput("Integration of GEO data"))),
                  width=21, footer="contact: shanecrinion@gmail.com"),
              box(title="Motivation", collapsible = TRUE),
              box(title="Contact Details", img(src="nui-galway.jpg", width=150, height=50))),
      
      tabItem(tabName = "patient",
              box(title="Controls",
                  # Input: Selector for choosing dataset ----
                  selectInput(inputId = "patient_dataset_1",
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
                                          "procedures")),
                  sliderInput("slider_1", 
                              "Number of Observations", 
                              1, 100, 150))),
      
      tabItem(tabName = "cohort",
              
              box(title="Controls",
                  # Input: Selector for choosing dataset ----
                  selectInput(inputId = "patient_dataset_2",
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
                                          "procedures")),
#                  checkboxGroupInput(inputId = "headers", 
#                                     "Included data", 
#                                     choices = names(input$patient_dataset_2)),
                  sliderInput("slider_2", 
                              "Number of Observations", 
                              1, 100, 150)),
              
              box(title="Available Data", width = 12,
                tabsetPanel(
                  tabPanel(
                    "DataTable", tableOutput("genTable")),
                  tabPanel(
                    "Graphs", plotOutput("plot1"))
                  ))))))

server <- function(input, output) { 
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider_2)]
    hist(data)
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
  

  output$genTable <- renderTable({
    patient_dataset_2 <- datasetInput()
    head(x=patient_dataset_2, n = input$slider_2)
  }) 
  
#  genTable <- reactive({
#    validate(
#      need(!is.null(output$genTable[1,1]),
#         "No results for this dataset, choose another!"))
#  })
  
}


shinyApp(ui, server)