#install DT package which renders interactive datatables
install.packages("DT")
library(DT)

#import the synthea information that i want to make a datatable from
dir()
setwd('/home/shane/Documents/RShiny/csv/')
patients <- read.csv('patients.csv')
allergies <- read.csv('allergies.csv')
allergies <- read.csv('allergies.csv')
careplans <- read.csv('careplans.csv')
claims <- read.csv('claims.csv')
conditions <- read.csv('conditions.csv')
encounters <- read.csv('encounters.csv')
immunization <- read.csv('immunizations.csv')
medications <- read.csv('medications.csv')
observations <- read.csv('observations.csv')
procedures <- read.csv('procedures.csv')

#import rshiny package 
library(shiny)
library(ggplot2)

ui <- fluidPage(
  title = "Data Tables of Synthea data",
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel(
        'input.dataset === "merged_patient_data"',
        checkboxGroupInput("show_vars0", "Columns in patients to show:",
                           names(merged_patient_data), selected = names(merged_patient_data))
      ),
      conditionalPanel(
        'input.dataset === "patients"',
        checkboxGroupInput("show_vars1", "Columns in patients to show:",
                           names(patients), selected = names(patients))
      ),
      conditionalPanel(
        'input.dataset === "allergies"',
        helpText("Click the column header to sort a column.")
      ),
      conditionalPanel(
        'input.dataset === "careplans"',
        helpText("Display 5 records by default.")
      ),
      
      conditionalPanel(
        'input.dataset === "claims"',
        checkboxGroupInput("show_vars4", "Columns in claims to show:",
                           names(claims), selected = names(claims))
      ),
      conditionalPanel(
        'input.dataset === "conditions"',
        checkboxGroupInput("show_vars5", "Columns in conditions to show:",
                           names(conditions), selected = names(conditions))
      ),
      conditionalPanel(
        'input.dataset === "encounters"',
        checkboxGroupInput("show_vars6", "Columns in encounters to show:",
                          names(encounters), selected = names(encounters))
      ),
      conditionalPanel(
          'input.dataset === "immmunization"',
           checkboxGroupInput("show_vars7", "Columns in immunization to show:",
                             names(immunization), selected = names(immunization))
      ),
      conditionalPanel(
          'input.dataset === "medications"',
           checkboxGroupInput("show_vars8", "Columns in medications to show:",
                              names(medications), selected = names(medications))
      ),
      conditionalPanel(
          'input.dataset === "observations"',
          checkboxGroupInput("show_vars9", "Columns in observations to show:",
                              names(observations), selected = names(observations))
      ),
      conditionalPanel(
        'input.dataset === "procedures"',
        checkboxGroupInput("show_vars10", "Columns in procedures to show:",
                           names(procedures), selected = names(procedures))
      )),
     mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("merged_patient_data", DT::dataTableOutput("mytable0")),
        tabPanel("patients", DT::dataTableOutput("mytable1")),
        tabPanel("allergies", DT::dataTableOutput("mytable2")),
        tabPanel("careplans", DT::dataTableOutput("mytable3")),
        tabPanel("claims", DT::dataTableOutput("mytable4")),
        tabPanel("conditions", DT::dataTableOutput("mytable5")),
        tabPanel("encounters ", DT::dataTableOutput("mytable6")),
        tabPanel("immunization", DT::dataTableOutput("mytable7")),
        tabPanel("medications", DT::dataTableOutput("mytable8")),
        tabPanel("observations", DT::dataTableOutput("mytable9")),
        tabPanel("procedures", DT::dataTableOutput("mytable10"))
      )
    )
  )
)


server <- function(input, output) {
  
  # choose columns to display
  merged_patient_data0 = merged_patient_data[sample(nrow(merged_patient_data), 1000), ]
  output$mytable0 <- DT::renderDataTable({
    DT::datatable(merged_patient_data[, input$show_vars0, drop = TRUE])
  })
  
  
  # choose columns to display
  patients2 = patients[sample(nrow(patients), 1000), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(patients2[, input$show_vars1, drop = TRUE])
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(allergies, options = list(orderClasses = TRUE))
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(careplans, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  # choose columns to display
  claims2 = claims[sample(nrow(claims), 1000), ]
  output$mytable4 <- DT::renderDataTable({
    DT::datatable(claims2[, input$show_vars4, drop = FALSE])
  })
  
  # choose columns to display
  conditions2 = conditions[sample(nrow(conditions), 1000), ]
  output$mytable5 <- DT::renderDataTable({
    DT::datatable(conditions2[, input$show_vars5, drop = FALSE])
  })
  
  # choose columns to display
  encounters2 = encounters[sample(nrow(encounters), 1000), ]
  output$mytable6 <- DT::renderDataTable({
    DT::datatable(encounters2[, input$show_vars6, drop = FALSE])
  })
  
  # choose columns to display
  immunization2 = immunization[sample(nrow(immunization), 1000), ]
  output$mytable7 <- DT::renderDataTable({
    DT::datatable(immunization2[, input$show_vars7, drop = FALSE])
  })
  
  # choose columns to display
  medications2 = medications[sample(nrow(medications), 1000), ]
  output$mytable8 <- DT::renderDataTable({
    DT::datatable(medications2[, input$show_vars8, drop = FALSE])
  })
  
  # choose columns to display
  observations2 = observations[sample(nrow(observations), 1000), ]
  output$mytable9 <- DT::renderDataTable({
    DT::datatable(observations2[, input$show_vars9, drop = FALSE])
  })
   
  # choose columns to display
  procedures2 = procedures[sample(nrow(procedures), 1000), ]
  output$mytable10 <- DT::renderDataTable({
    DT::datatable(procedures2[, input$show_vars10, drop = FALSE])
  })

}


shinyApp(ui, server)
