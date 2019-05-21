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
                              1, 100, 150)),
              box(title="Data Sources")),
      
      tabItem(tabName = "cohort",
              
              box(title="Controls", collapsible = TRUE,
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
                  tabPanel("Graphs",
                    tabsetPanel(
                      tabPanel("Ethnicity", plotOutput("plot1")),
                      tabPanel("Hg Measurements", plotOutput("plot2"))))))
                  ))))

server <- function(input, output) { 
  
  datasetInput <- reactive({
    switch(input$patient_dataset_1,
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
    patient_dataset_1 <- datasetInput()
    head(x=patient_dataset_1, n = input$slider_1)
  }) 
  
  
  output$plot1 <- renderPlot({
#    data <- histdata[seq_len(input$slider_2)]
#    hist(data)
    library(ggplot2)
    
    ggplot(as.data.frame(patients.csv$ETHNICITY),
           aes(x=patients.csv$ETHNICITY, 
               color=patients.csv$GENDER)) +
      geom_histogram(fill = "white", 
                     alpha = 0.3, 
                     position = "identity", 
                     stat = "count") +
      #  geom_vline(data=as.data.frame(mu), 
      # aes(xintercept=mu, colour=patients.csv$GENDER), 
      #             linetype="dashed", alpha=0.5) +
      scale_colour_manual(values = c("#0571b0", 
                                     "#ca0020")) +
      theme_classic() +
      theme(legend.position = "top", 
            axis.text.x = element_text(face = "bold", 
                                       size = 8, 
                                       angle = 90)) 
    
      })
  
  
  output$plot2 <- renderPlot({
    
    library(ggplot2)
    library(ggridges)
    library(lattice)
    
    
    ##merge the data
    observations_merge <- merge(x = patients.csv, 
                                y = observations.csv, 
                                by.x = "Id", 
                                by.y= "PATIENT") 
    
    
    
    ## could alternatively do the below command using to obsevation code (probably better)
    hemoglobin_measurements <- 	
      subset(observations_merge,
             subset = (observations_merge$DESCRIPTION == "Hemoglobin A1c/Hemoglobin.total in Blood"))
    
    hemoglobin_measurements$BIRTHDATE <- 
      substring(hemoglobin_measurements$BIRTHDATE, 1, 4) 
    
    hemoglobin_measurements$DECADE <- 10*(as.integer(as.numeric(hemoglobin_measurements$BIRTHDATE/10)))
    
    hemoglobin_measurements$DECADE <- 
      10*as.integer(as.numeric(as.character(hemoglobin_measurements$BIRTHDATE)) / 10)
    
    hemoglobin_measurements$VALUEBIN <- 
      1*as.integer(as.numeric(as.character(hemoglobin_measurements$VALUE)) / 1)
    
    #plot the two
    ggplot(hemoglobin_measurements) + 
      geom_density_ridges(aes(x = VALUEBIN, 
                              y = DECADE, 
                              group = interaction(GENDER, DECADE),
                              fill = GENDER), 
                          alpha = 0.6, 
                          scale = 0.8) +
      
      geom_vline(xintercept = 5.7, 
                 color = "lightskyblue1", size=0.5) +
      
      geom_vline(xintercept = 6.2, 
                 color = "tomato", size=0.5) +
      
      scale_fill_manual(values=c("#0571b0", "#ca0020")) +
      
      theme_classic() +
      theme(
        axis.text.x= element_text(angle = 30),
        panel.grid.major.y =element_line(colour = "gray95"),
        panel.grid.major.x =element_line(colour = "gray95"),
        panel.grid.minor.x =element_line(colour = "gray95"))
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