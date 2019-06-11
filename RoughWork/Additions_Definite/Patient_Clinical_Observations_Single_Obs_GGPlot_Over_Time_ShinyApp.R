## -- create a new variable that matches the patient ID to their name 

patients.csv$PATIENT <- patients.csv$Id 
observations_names <- merge(observations.csv,patients.csv, by  = "PATIENT") 


ui <- fluidPage(
  textInput(inputId = "observations_patient",
            label = "Select a patient",
            placeholder = "Enter patient ID:",
            value="1425bcf6-2853-4c3a-b4c5-64fbe03d43d2"),
  
  selectInput(inputId= "observation_selection",
              label = "Select an observation for comparison",
              choices = c(sort(as.character(unique(observations.csv$DESCRIPTION)))),
              selected = "Body Mass Index"),
  plotlyOutput(outputId = "observations_plot"))

server <- function(input, output) { 

  output$observations_plot <- renderPlotly({
    
    ## create merger to grab patient names
    patients.csv$PATIENT <- patients.csv$Id 
    observations_names <- merge(observations.csv,
                                patients.csv, by  = "PATIENT") 
    
    patient <- input$observations_patient
    patient_data <- observations_names[observations_names$PATIENT == patient,]
    observation <- input$observation_selection
    patient_observation_data <- patient_data[patient_data$DESCRIPTION == observation,]
    
    
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Date of Observation",
      titlefont = f
    )
    
    y <- list(
      title = paste(patient_observation_data$DESCRIPTION[1], "(",
                    patient_observation_data$UNITS[1], ")"),
      titlefont = f
    )
    
    patient_observation_data  %>% 
      plot_ly(
        x = ~DATE, y =~as.numeric(as.character(VALUE)), 
        type = "scatter")  %>% 
      layout(xaxis = x, yaxis = y, 
             title= paste(patient_observation_data$DESCRIPTION[1], "for",
                          patient_observation_data$FIRST[[1]],
                          patient_observation_data$LAST[[1]]))
  })
}

shinyApp(ui = ui, server = server)

