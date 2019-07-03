
ui <- 
  sidebarPanel(selectInput(inputId= "gene_expression_analysis",
            label = "Analysis results for the microarray data",
            choices = c("Quality Control" = "qual", 
                        "Normalisation" = "PatientBMI",
                        "Differential expression analysis",
                        "Interpretation"),
            selected = "Quality Control"),
conditionalPanel('input.gene_expression_analysis=="qual"', 
                 plotOutput("quality")),
conditionalPanel('input.gene_expression_analysis=="PatientBMI"', 
                 plotlyOutput("PatientBMI_plot")))

server <- function(input, output) {
  
  output$quality <- renderPlot({
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
                                       angle = 90),
            panel.background = element_rect(fill = "white", colour = "white",
                                            size = 2, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "#d3d3d3"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "white")) })
  
  output$PatientBMI_plot <- renderPlotly({
    #replace x with input$dropdown
    #bmi_all <- observations.csv[observations.csv$DESCRIPTION == "Body Mass Index",]
    #replace x with input$dropdown
    
    bmi_all <- observations.csv[observations.csv$DESCRIPTION == "Body Mass Index",]
    bmi_all$DATE <- as.character(bmi_all$DATE)
    bmi_all$VALUE <- as.numeric(as.character(bmi_all$VALUE))
    
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )        
    x <- list(
      title = "Date of observation",
      titlefont = f
    )
    y <- list(
      title = "BMI measurement (kg/m2)",
      titlefont = f
    )
    
    bmi_all[bmi_all$PATIENT == "1425bcf6-2853-4c3a-b4c5-64fbe03d43d2",]  %>% 
      plot_ly(
        x = ~DATE, y =~VALUE, 
        colors = "green", 
        type = "scatter")})}

shinyApp(ui,server)