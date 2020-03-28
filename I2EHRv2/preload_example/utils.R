# This is an example of the app using pre-processed data
# Multiclust requires matrices of expression and clinical data
# These were generated from GEO datasets using multiClust
# Import the clinical and genomic data

setwd('/Users/shanecrinion/Documents/business/projects/I2EHR/I2EHRv2/preload_example/')
exprs <- 
  multiClust::input_file('GSE46097.expression.txt')

radioButtons('probes_num_select',
             choices = c('Fixed', 'Percent', 'Poly'),
             label = shiny::tags$h4("Approach for number of probes/genes:"),
             inline = TRUE)

probe_num_method <- function(exprs_path, exprs_data, probe_num.selection, probe_val=NULL){
  ## fill in the appropriate variables depending on the probe selection type
  message('WARNING: Adaptive is computationally expensive and may cause your system to crash')
  if(probe_num.selection=='Percent'){ 
    probes_num <- number_probes(
      input=exprs_path, 
      data.exp=exprs_data, 
      Fixed=NULL, 
      Percent=probe_val, 
      Poly=NULL, 
      Adaptive=NULL)}
  
  else if(probe_num.selection=='Fixed'){
    probes_num <- number_probes(
      input=exprs_path, 
      data.exp=exprs_data, 
      Fixed=probe_val, 
      Percent=NULL, 
      Poly=NULL, 
      Adaptive=NULL)} 
  
  else if(probe_num.selection=='Poly'){
    probes_num <- number_probes(
      input=exprs_path, 
      data.exp=exprs_data, 
      Fixed=probe_val, 
      Percent=NULL, 
      Poly=TRUE, 
      Adaptive=NULL)}
  return(probes_num)}

probe_num_method(input$probe_num_select)