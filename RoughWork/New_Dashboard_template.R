library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Patient Data"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1", height = "250")),
      box(
        title = "Controls",
        sliderInput("slider", "number of observations", 1, 100, 150)
      )
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
  
shinyApp(ui, server)