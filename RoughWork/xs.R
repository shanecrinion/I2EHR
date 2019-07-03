server <- function(input, output, session) {

    shiny::observeEvent(input$patient_genomic_step,{
      
    if (input$patient_genomic_step == "patient_qc"){
      shiny::renderPlot({
        
      })} #close renderPlot
    if (input$patient_genomic_step == "patient_qc"){
      shiny::renderPlot({
      })  #close renderPlot
    }})}


