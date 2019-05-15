#import rshiny package 
library(shiny)
library(ggplot2)

ui <- fluidPage(
  title = "Data Tables of Synthea data",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "merged_patient_data"', label="all data",
        helpText("Select 2 or more columns."),
        checkboxGroupInput("show_vars0", "Columns in patients to show:",
                           names(merged_patient_data), selected = names(merged_patient_data))) #replace last bracket with ,
#     conditionalPanel('input.dataset === "merged_patient_data"', label="sex data",
#helpText('Search condition to identify effect of sex and age.'),
     #   checkboxGroupInput(inputId = "sex",label = "Select sex",choices = c("Female","Male"),inline = TRUE )
     # conditionalPanel(
      #'input.dataset === "merged_patient_data"', label="condition data",
      #helpText('Search condition to identify effect of sex and age.'),
      #checkboxGroupInput('show_vars1', 'Sex:', selected=names(merged_patient_data)))
      ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("merged_patient_data", DT::dataTableOutput("mytable0"))
    ))))

server <- function(input, output) {
  
  # choose columns to display
  merged_patient_data0 = merged_patient_data[sample(nrow(merged_patient_data), 1000), ]
  output$mytable0 <- DT::renderDataTable({
    DT::datatable(merged_patient_data[, input$show_vars0, drop = TRUE])
  })
}

shinyApp(ui, server)
