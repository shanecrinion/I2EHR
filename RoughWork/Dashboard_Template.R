library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Patient Data"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(dataTableOutput(outputId = "plot1")),
      
      box(title = "Controls",
          sliderInput("slider", 
                    "number of observations", 
                    1, 100, 150),
          fileInput("file", "CSV file"))
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
}

shinyApp(ui , server)