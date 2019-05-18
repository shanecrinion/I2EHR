### Worksheet 1 ####

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Patient Data"),
  dashboardSidebar(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Type patient name...")),
  dashboardBody(
    fluidRow(
      box(title = "Welcome to the Interactive Integrated Electronic Health Record (I2EHR)", 
          width=4,
          footer="for more information please e-mail shanecrinion@gmail.com"),
      box(title="General Statistics",
          collapsible = TRUE),
      box(plotOutput("Patient Stats")))))


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
    head(conditions.csv[PATIENT(input$searchButton)], n = input$slider)
  })
  
}



shinyApp(ui , server)

### Worksheet 2 ###

## ui.R ##


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Patient", tabName = "dashboard", icon = icon("id-card")),
    menuItem("Cohort", icon = icon("poll"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)


# Put them together into a dashboardPage
ui <- 
  dashboardPage(
    skin="green",
  dashboardHeader(title = "I2EHR"),
  sidebar,
  body
)


server <- function(input, output){
  
}

shinyApp(ui, server)

### Worksheet 3 ###

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Patient Data"),
  dashboardSidebar(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Type patient name...")
  ),
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
    head(x=(conditions.csv[PATIENT(input$searchButton)], n = input$slider)
  }) 
}

shinyApp(ui , server)


