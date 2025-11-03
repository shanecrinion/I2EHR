library(shiny)
library(bs4Dash)
library(DT)
#library(shinycssloaders)
library(summarytools)
library(bslib)
library(later)
library(stringr)

ui <- bs4DashPage(
  title = "I2EHR",
  
  header = bs4DashNavbar(
    title = "I2EHR: Interactive, Integrated Electronic Health Records"
  ),
  
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "",
    elevation = 3,
    bs4SidebarMenu(
      bs4SidebarMenuItem("Overview", tabName = "overview", icon = icon("table")),
      bs4SidebarMenuItem("Explore Data", tabName = "explore", icon = icon("chart-bar")),
      bs4SidebarMenuItem("Genomic Analysis", tabName = "genomic", icon = icon("dna"))
    )
  ),
  
  body = bs4DashBody(
    bs4TabItems(
      
      # ---------------- Overview Tab ----------------
      bs4TabItem(
        tabName = "overview",
        fluidRow(
          column(
            width = 12,
            h2("Select a Dataset:"),
            selectInput(
              inputId = "dataset_choice",
              label = "Choose a dataset:",
              choices = c("CVD Risk Reduction Study" = "GSE46097.clinical")
            ),
            br()
          )
        ),
        
        fluidRow(
          # Info box takes 4/12 of the row
          column(
            width = 4,
            bs4InfoBox(
              title = "Study",
              value = "CVD Risk Reduction",
              subtitle = "Clinical & Transcriptomic Cohort",
              icon = icon("book-medical"),
              color = "info",
              width = 12  # full width of the column
            )
          ),
          
          # Tabs take 8/12 of the row
          column(
            width = 8,
            tabsetPanel(
              id = "study_tabs",
              
              tabPanel(
                "About This Study",
                bs4Card(
                  title = "About This Study",
                  width = NULL,   # NULL makes the card fill its container
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  status = "info",
                  p("The dataset originates from a study examining the molecular and physiological effects of lifestyle interventions on cardiovascular risk."),
                  p("Participants were assessed at three timepoints (baseline, month 3, year 1) for a range of clinical parameters."),
                  footer = 'doi.org/10.1161/CIRCGENETICS.113.000121'
                )
              ),
              
              tabPanel(
                "Treatment Info",
                bs4Card(
                  title = "Lifestyle Intervention Details",
                  width = NULL,  # fill the column
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  status = "info",
                  p("Participants were enrolled on an ongoing basis in a lifestyle intervention that consisted of four components:"),
                  tags$ol(
                    tags$li("A very low fat vegetarian diet (<10% of calories from fat)"),
                    tags$li("180 minutes/week of moderate aerobic exercise"),
                    tags$li("One hour of stress management each day"),
                    tags$li("Weekly group support sessions")
                  ),
                  p("Clinical staff met with patients twice each week during the first 12 weeks to orient participants to the program and maximize adherence."),
                  p("The remainder of the program was primarily self-directed but included ongoing weekly stress management and group support sessions.")
                )
              )
            )
          )
        ),
        
        
        fluidRow(
          bs4ValueBoxOutput("total_patients", width = 3),
          bs4ValueBoxOutput("cases", width = 3),
          bs4ValueBoxOutput("controls", width = 3),
          bs4ValueBoxOutput("n_vars", width = 3)
        ),
        br(),
        hr(),
        
        fluidRow(
          bs4Card(
            title = "Data Preview",
            width = NULL,
            DTOutput("data_preview")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Variable Summary",
            width = NULL,
            uiOutput("dfsummary_output")
          )
        )
      ),
      
      # ---------------- Explore Data Tab ----------------
      bs4TabItem(
        tabName = "explore",
        h2("Explore Clinical Variables"),
        fluidRow(
          column(
            width = 4,
            selectInput("x_var", "Select X variable:", choices = NULL),
            selectInput("y_var", "Select Y variable:", choices = NULL),
            selectInput("plot_type", "Plot type:", choices = c("Boxplot", "Histogram", "Scatter")),
            actionButton("generate_plot", "Enter", icon = icon("play"), class = "btn-primary"),
            br(), br(),
            downloadButton("download_plot", "Download Plot", class = "btn-success")
          ),
          column(
            width = 8,
            bs4Card(
              title = "Visualization",
              width = 12,
              plotOutput("explore_plot", height = "400px")
            )
          )
        ),
        br(),
        fluidRow(
          bs4Card(
            title = "Statistical Analysis Tools",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            collapsible = TRUE,
            selectInput(
              "analysis_type", "Select Analysis:",
              choices = c(
                "Chi-squared test" = "chisq",
                "Fisher’s exact test" = "fisher",
                "t-test" = "ttest",
                "Wilcoxon test" = "wilcox",
                "ANOVA" = "anova",
                "Correlation" = "cor",
                "Linear Regression" = "lm"
              )
            ),
            uiOutput("analysis_vars"),
            br(),
            actionButton("run_analysis", "Run Analysis", icon = icon("flask"), class = "btn-success"),
            br(), br(),
            verbatimTextOutput("analysis_result")
          )
        )
      ),
      
      # ---------------- Genomic Analysis Tab ----------------
      bs4TabItem(
        tabName = "genomic",
        h2("Genomic Analysis"),
        actionButton("load_genomic", "Load Genomic Data", icon = icon("play"), class = "btn-primary"),
        br(),
        br(),
        textOutput('load_genomic_message'),
        hr(),
        br(),
        fluidRow(
        bs4ValueBoxOutput("total_probes", width=3),
        bs4ValueBoxOutput("avg_exprs", width = 3),
        bs4ValueBoxOutput("counts_5", width = 3),
        bs4ValueBoxOutput("missing_gene_name", width = 3)
      ),
      br(),
      hr(),
        tabBox(
          id = "tabcard",
          #title = "Perform Genomic Analysis",
          selected = "Data & Metadata",
          status = "primary",
          width=NULL,
          collapsible = F,
          solidHeader = FALSE,
          type = "pills",
          tabPanel(
            title = "Data & Metadata",
            br(),
            hr(),
            bs4Card(
                width = NULL, collapsible = F, 
                headerBorder = F,
                title = 'Genomic Data Preview',
                DTOutput("genomic_preview")),
            bs4Card(
              title='Metadata' ,
              width=NULL, 
              DTOutput('processing_info')
            ),
            bs4Card(
              title = "Variable Summary",
              width = NULL,
              uiOutput("genomic_summary_output")
            )),
          tabPanel(
            title = "QC and Normalisation",
            selectInput(inputId = 'norm', choices = c('none', 'vst')),
            plotOutput('norm_pca'),
            plotOutput('norm_sampleDistance'),
            plotOutput('norm_meanVar'),
            tableOutput('normal_table')) ,
      
          
          
          tabPanel(
            title = "Filtering & Annotation",
            "Content 3"
          ),
          tabPanel(
            title='Differential Expression'
          ),
          tabPanel('Longitudinal Models'),
          tabPanel('Gene Explorer'),
          tabPanel('Enrichment & Pathways'),
          tabPanel('Co-expression (WGCNA)'),
          tabPanel('Clinical Integration & Correlation'),
          tabPanel('Export')
        ))
      
    )
  ),
  
  footer = bs4DashFooter(
    left = "© 2025 I2EHR Dashboard",
    right = "Built with bs4Dash"
  )
)



# 2. Explore Data
  # ----- 1.1 Explore Data

#




# ---------------- SERVER ----------------
server <- function(input, output, session) {
  
  # Load dataset reactively
  dataset <- reactive({
    req(input$dataset_choice)
    
    file_path <- paste0("data/", input$dataset_choice, ".rds")
    
    validate(
      need(file.exists(file_path), "Dataset not found. Please check the file name or path.")
    )
    
    readRDS(file_path)
  })
  
# ---- 1. Overview Tab ----
  # ---- 1.1 Study Info
  # ---- 1.2 SUMMARY KPI BOXES ----
  output$total_patients <- renderbs4ValueBox({
    df <- dataset()
    bs4ValueBox(
      value = nrow(df),
      subtitle = "Total Patients",
      icon = icon('hospital-user'),
      color = 'lightblue'
    )
  })
  
  output$controls <- renderbs4ValueBox({
    df <- dataset()
    control_count <- sum(df$group == "Matched Control Group", na.rm = TRUE) / 3 
    bs4ValueBox(
      value = control_count,
      subtitle = "Controls",
      icon = icon("shield-alt"),
      color = "success"
    )
  })
  
  output$cases <- renderbs4ValueBox({
    df <- dataset()
    case_count <- sum(df$group == 'Matched Ornish Participant', na.rm=T) / 3
    bs4ValueBox(
      value = case_count,
      subtitle = "Cases",
      icon = icon("virus"),
      color = "danger"
    )
  })
  
  output$n_vars <- renderbs4ValueBox({
    df <- dataset()
    bs4ValueBox(
      value = ncol(df),
      subtitle = "Variables",
      icon = icon("th"), 
      color = "info"
    )
  })
  
  # ---- 1.3 Data Preview -----
  output$data_preview <- renderDT({
    datatable(dataset(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- 1.4 Variable Summary ----
  output$dfsummary_output <- renderUI({
    df <- dataset()
    
    # Clean column names to avoid summarytools HTML issues
    names(df) <- make.names(names(df), unique = TRUE)
    
    tmpfile <- tempfile(fileext = ".html")
    
    print(summarytools::dfSummary(
      df,
      plain.ascii = FALSE,
      style = "grid",
      valid.col = TRUE,
      graph.col = TRUE,
      na.col = TRUE), 
      method='render', headings=F, bootstap.css=F)

  })
  
# ---- 2. Explore Data ----
  observe({
    df <- dataset()
    vars <- names(df)
    
    # Set default selections safely if available
    default_x <- if ("group" %in% vars) "group" else vars[1]
    default_y <- if ("year_weight_loss" %in% vars) "year_weight_loss" else vars[2]
    
    updateSelectInput(session, "x_var",
                      choices = vars,
                      selected = default_x)
    
    updateSelectInput(session, "y_var",
                      choices = vars,
                      selected = default_y)
  })
  
  # Reactive trigger: only runs when Enter is pressed
  plot_trigger <- eventReactive(input$generate_plot, {
    req(input$x_var)
    df <- dataset()
    df
  })
  
  # Render plot only after Enter is clicked
  output$explore_plot <- renderPlot({
    req(plot_trigger())  # ensures no plot until Enter pressed
    df <- plot_trigger()
    
    # Safely handle missing y_var (for histograms)
    x <- df[[input$x_var]]
    y <- if (!is.null(input$y_var) && input$y_var != "") df[[input$y_var]] else NULL
    
    library(ggplot2)
    
    p <- switch(
      input$plot_type,
      
      "Boxplot" = ggplot(df, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
        geom_boxplot(fill = "#69b3a2", alpha = 0.7) +
        labs(x = input$x_var, y = input$y_var, title = "Boxplot") +
        theme_minimal(),
      
      "Histogram" = ggplot(df, aes(x = .data[[input$x_var]])) +
        geom_histogram(bins = 30, fill = "#1f77b4", color = "white", alpha = 0.8) +
        labs(x = input$x_var, title = "Histogram") +
        theme_minimal(),
      
      "Scatter" = ggplot(df, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
        geom_point(alpha = 0.7, color = "#ff7f0e") +
        labs(x = input$x_var, y = input$y_var, title = "Scatter Plot") +
        theme_minimal()
    )
    
    p
  })
  
  # ---- Download handler ----
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("explore_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(plot_trigger())
      df <- plot_trigger()
      library(ggplot2)
      
      p <- switch(
        input$plot_type,
        "Boxplot" = ggplot(df, aes(x = .data[[input$y_var]], y = .data[[input$x_var]])) +
          geom_boxplot(fill = "#69b3a2", alpha = 0.7) + theme_minimal(),
        "Histogram" = ggplot(df, aes(x = .data[[input$x_var]])) +
          geom_histogram(bins = 30, fill = "#1f77b4", color = "white", alpha = 0.8) + theme_minimal(),
        "Scatter" = ggplot(df, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
          geom_point(alpha = 0.7, color = "#ff7f0e") + theme_minimal()
      )
      
      ggsave(file, p, width = 7, height = 5, dpi = 300)
    }
  )
  
  # Statistical Analysis
  
  # Dynamic variable selection depending on analysis type
  output$analysis_vars <- renderUI({
    df <- dataset()
    req(df)
    
    num_vars <- names(df)[sapply(df, is.numeric)]
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    switch(input$analysis_type,
           
           "chisq" = tagList(
             selectInput("var1", "Variable 1 (categorical):", choices = cat_vars),
             selectInput("var2", "Variable 2 (categorical):", choices = cat_vars)
           ),
           
           "fisher" = tagList(
             selectInput("var1", "Variable 1 (categorical):", choices = cat_vars),
             selectInput("var2", "Variable 2 (categorical):", choices = cat_vars)
           ),
           
           "ttest" = tagList(
             selectInput("num_var", "Numeric variable:", choices = num_vars),
             selectInput("group_var", "Grouping variable (2 levels):", choices = cat_vars)
           ),
           
           "wilcox" = tagList(
             selectInput("num_var", "Numeric variable:", choices = num_vars),
             selectInput("group_var", "Grouping variable:", choices = cat_vars)
           ),
           
           "anova" = tagList(
             selectInput("num_var", "Numeric variable:", choices = num_vars),
             selectInput("group_var", "Grouping variable (3+ levels):", choices = cat_vars)
           ),
           
           "cor" = tagList(
             selectInput("var1", "Variable 1 (numeric):", choices = num_vars),
             selectInput("var2", "Variable 2 (numeric):", choices = num_vars)
           ),
           
           "lm" = tagList(
             selectInput("outcome_var", "Outcome (numeric):", choices = num_vars),
             selectInput("predictor_vars", "Predictors:", choices = num_vars, multiple = TRUE)
           )
    )
  })
  
  # Run analysis
  observeEvent(input$run_analysis, {
    req(input$analysis_type)
    df <- dataset()
    
    result <- tryCatch({
      switch(input$analysis_type,
             
             "chisq" = chisq.test(table(df[[input$var1]], df[[input$var2]])),
             "fisher" = fisher.test(table(df[[input$var1]], df[[input$var2]])),
             "ttest" = t.test(df[[input$num_var]] ~ df[[input$group_var]]),
             "wilcox" = wilcox.test(df[[input$num_var]] ~ df[[input$group_var]]),
             "anova" = summary(aov(df[[input$num_var]] ~ df[[input$group_var]])),
             "cor" = cor.test(df[[input$var1]], df[[input$var2]], method = "pearson"),
             "lm" = summary(lm(as.formula(
               paste(input$outcome_var, "~", paste(input$predictor_vars, collapse = "+"))
             ), data = df))
      )
    }, error = function(e) e)
    
    output$analysis_result <- renderPrint({ result })
  })
  
  
  # ---- Genomic Analysis ----
  # reactive holder for the loaded data
  dataset_genomic <- reactiveVal(NULL)
  
  # status message
  loading_message <- reactiveVal("")
  output$load_genomic_message <- renderText(loading_message())
  
  observeEvent(input$load_genomic, {
    # 1) show message immediately
    
    loading_message("Data loading...")
    
    # 2) defer heavy lifting so the UI can update
    later::later(function() {
      
      dataset_genomic <- reactive({
        
        # do heavy work here
        req(input$dataset_choice)
        genomic_file_path <- paste0("data/", str_replace(input$dataset_choice, "clinical", "expression"), ".rds")
        
        if (!file.exists(genomic_file_path)) {
          loading_message("Dataset not found. Please check the file name or path.")
          return()
        }
        
        # read (this can be slow)
        readRDS(genomic_file_path)
        }) 
      
      
      # render dependent outputs
      output$total_probes <- renderbs4ValueBox({
        req(dataset_genomic())
        bs4ValueBox(
          value = nrow(dataset_genomic()),
          subtitle = "Probes",
          icon = icon("dna"),
          color = "lightblue"
        )
      })
    
      
      output$avg_exprs <- renderbs4ValueBox({
        req(dataset_genomic())
        counts_avg <- rowMeans(dataset_genomic()[,c(2:379)], na.rm = TRUE)
        bs4ValueBox(
          value = max(counts_avg),
          subtitle = "Avg Expression",
          icon = icon("dna"),
          color = "success"
        )
     } )
      
      output$counts_5 <- renderbs4ValueBox({
        req(dataset_genomic())
        counts_5 <- sum(rowMeans(dataset_genomic()[,c(2:379)], na.rm = TRUE) <5)
        bs4ValueBox(
          value = counts_5,
          subtitle = "Genes < 5 counts",
          icon = icon("dna"),
          color = "danger"
        )
      } )
      
      output$missing_gene_name <- renderbs4ValueBox({
        req(dataset_genomic())
        bs4ValueBox(
          value = nrow(dataset_genomic()[is.na(dataset_genomic()$gene_symbol),]),
          subtitle = "Missing Gene Symbol",
          icon = icon("dna"),
          color = "info"
        )
      } )
    
      
      output$genomic_preview <- renderDT({
        req(dataset_genomic())
        datatable(dataset_genomic(), options = list(pageLength = 10, scrollX = TRUE))
      })
      
      output$genomic_summary_output <- renderUI({
        df <- dataset_genomic()
        req(df)
        names(df) <- make.names(names(df), unique = TRUE)
        print(summarytools::dfSummary(
          df, plain.ascii = FALSE, style = "grid",
          valid.col = TRUE, graph.col = TRUE, na.col = TRUE
        ), method = "render", headings = FALSE, bootstrap.css = FALSE)
      })
      
      loading_message("Data loaded successfully.")
    }, delay = 0.01) # small delay is enough to allow UI to update
  })

  
  output$processing_info <- renderDT({
    # Wait until user clicks "QC and Normalisation" tab
    message('Loading..')
    req(input$tabcard == "QC and Normalisation")
    
    # Once selected, start rendering
    Sys.sleep(1)  # simulate time-consuming QC step
    
    datatable(processing_info)
  })
  
  # QC and normalisation
  
  
  

}

# Run the app
shinyApp(ui, server)
