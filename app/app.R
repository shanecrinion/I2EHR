library(shiny)
library(skimr)
library(bs4Dash)
library(summarytools)
library(DT)

clinical_explorer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("n_patients")),
      valueBoxOutput(ns("mean_age")),
      valueBoxOutput(ns("sex_ratio"))
    ),
    fluidRow(
      box(width = 6, title = "Age Distribution", plotlyOutput(ns("age_hist"))),
      box(width = 6, title = "Diagnosis by Sex", plotlyOutput(ns("diagnosis_sex_plot")))
    ),
    fluidRow(
      box(width = 12, title = "Clinical Data Table", DTOutput(ns("clinical_table")))
    )
  )
}

clinical_explorer_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    clinical_df <- clinical_data

    # Basic summary stats
    output$n_patients <- renderValueBox({
      valueBox(
        value = nrow(clinical_df),
        subtitle = "Patients",
        icon = icon("users"),
        color = "lightblue"
      )
    })

    output$mean_age <- renderValueBox({
      valueBox(
        value = round(mean(clinical_df$age, na.rm = TRUE), 1),
        subtitle = "Mean Age",
        icon = icon("user-clock"),
        color = "teal"
      )
    })

    output$sex_ratio <- renderValueBox({
      ratio <- table(clinical_df$sex)
      text <- paste0(ratio["F"], " F / ", ratio["M"], " M")
      valueBox(
        value = text,
        subtitle = "Sex Ratio",
        icon = icon("venus-mars"),
        color = "purple"
      )
    })

    # Age histogram
    output$age_hist <- renderPlotly({
      p <- ggplot(clinical_df, aes(x = age)) +
        geom_histogram(fill = "#2E86C1", color = "white", bins = 20) +
        labs(x = "Age", y = "Count") +
        theme_minimal()
      ggplotly(p)
    })

    # Diagnosis by sex bar chart
    output$diagnosis_sex_plot <- renderPlotly({
      p <- ggplot(clinical_df, aes(x = diagnosis, fill = sex)) +
        geom_bar(position = "dodge") +
        labs(x = "Diagnosis", y = "Count") +
        theme_minimal()
      ggplotly(p)
    })

    # Interactive data table
    output$clinical_table <- renderDT({
      datatable(
        clinical_df,
        filter = "top",
        options = list(pageLength = 10)
      )
    })
  })
}

load_dataset <- function(dataset_path) {
  if (!file.exists(dataset_path)) stop("Dataset not found: ", dataset_path)
  dataset <- readRDS(dataset_path)
}

#load_dataset("data/GSE46097.clinical.rds")

ui <- bs4DashPage(
  title = "Genomic Explorer",
#  navbar = bs4DashNavbar(),
  header = dashboardHeader('I2EHR: Interactive, Integrated Electronic Health Records'),
  sidebar = bs4DashSidebar(
    bs4SidebarMenu(

      ## 1. Cohort Overview
      bs4SidebarMenuItem("Cohort Overview",
                         tabName = "overview"),
      bs4SidebarMenuItem("Clinical Data", tabName = "clinical"),
      bs4SidebarMenuItem("Genomic Explorer", tabName = "genomic"),
      bs4SidebarMenuItem("Differential Expression", tabName = "de"),
      bs4SidebarMenuItem("Pathway Analysis", tabName = "pathway")
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      # 1. Cohort Overview
      bs4TabItem("overview",
                 h2("Select a Dataset:"),
                 br(),
                 hr(),
                 selectInput(
                   inputId = "dataset_choice",
                   label = "Choose a dataset:",
                   choices = c("CVD Risk Reduction Study"= 'GSE46097.clinical')),
                 htmlOutput("dfsummary_output"),
                 DTOutput("data_preview"),
                 p("Welcome to the malaria facility visualisation app! To use this app, manipulate the widgets on the side to change the epidemic curve according to your preferences! To download a high quality image of the plot you've created, you can also download it with the download button. To see the raw data, use the raw data tab for an interactive form of the table. The data dictionary is as follows:"),
                 tags$ul(
                   tags$li(tags$b("location_name"), " - the facility that the data were collected at"),
                   tags$li(tags$b("data_date"), " - the date the data were collected at"),
                   tags$li(tags$b("submitted_daate"), " - the date the data were submitted at"),
                   tags$li(tags$b("Province"), " - the province the data were collected at (all 'North' for this dataset)"),
                   tags$li(tags$b("District"), " - the district the data were collected at"),
                   tags$li(tags$b("age_group"), " - the age group the data were collected for (0-5, 5-14, 15+, and all ages)"),
                   tags$li(tags$b("cases_reported"), " - the number of cases reported for the facility/age group on the given date")
                 ),
                 ),
      bs4TabItem("clinical", h2("Explore clinical variables")),
      bs4TabItem("genomic", h2("Gene expression / variant explorer")),
      bs4TabItem("de", h2("Differential expression results")),
      bs4TabItem("pathway", h2("Pathway enrichment visualization"))
    )
  )
)

server <- function(input, output, session) {

  dataset <- reactive({
    req(input$dataset_choice)

    # construct file path dynamically
    file_path <- paste0("data/", input$dataset_choice, ".rds")

    validate(
      need(file.exists(file_path), "Dataset not found. Please check the file name or path.")
    )

    readRDS(file_path)
  })


  # Interactive table preview
  output$data_preview <- renderDT({
    datatable(dataset(), options = list(pageLength = 10, scrollX = TRUE))
  })

  # dfSummary output
  output$dfsummary_output <- renderUI({
    summarytools::view(summarytools::dfSummary(dataset()), method = "render")
  })
}


shinyApp(ui, server)
