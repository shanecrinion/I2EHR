# This is an example of the app using pre-processed data
# Multiclust requires matrices of expression and clinical data
# These were generated from GEO datasets using multiClust
# Import the clinical and genomic data


# selection 1 - number of probes
probe_num_method <- function(exprs_path, 
                             exprs_data, 
                             probe_num.selection, 
                             probe_val=NULL){
  ## each probe method requires filling all variables 
  ## this function fills each unique entry
  if(probe_num.selection=='Percent'){ 
    probes_num <- multiClust::number_probes(
      input=exprs_path, 
      data.exp=exprs_data, 
      Fixed=NULL, 
      Percent=probe_val, 
      Poly=NULL, 
      Adaptive=NULL)}
  
  else if(probe_num.selection=='Fixed'){
    probes_num <- multiClust::number_probes(
      input=exprs_path, 
      data.exp=exprs_data, 
      Fixed=probe_val, 
      Percent=NULL, 
      Poly=NULL, 
      Adaptive=NULL)} 
  
  else if(probe_num.selection=='Poly'){
    probes_num <- multiClust::number_probes(
      input=exprs_path, 
      data.exp=exprs_data, 
      Fixed=NULL, 
      Percent=NULL, 
      Poly=TRUE, 
      Adaptive=NULL)}
  return(probes_num)}


cluster_assign_str <- function(data_name,
                               cluster_type,
                               probes_method_select,
                               probes_num_select,
                               linkage_hclust=NULL,
                               dist_hclust=NULL){
  if(cluster_type=='Kmeans'){
    cluster_assign_csv <- 
      paste0('/', data_name,
             ' Kmeans ',
             probes_method_select, ' ',
             probes_num_select,
             '_Probe_Num Fixed_Clust_Num Samples.Clusters.csv')}
  else if(cluster_type=='HClust'){
    cluster_assign_csv <- 
      paste0('/',data_name,
             ' HClust ',
             dist_hclust, ' ',
             linkage_hclust, ' ',
             probes_method_select, ' ',
             probes_num_select,
             '_Probe_Num Fixed_Clust_Num Samples.Clusters.csv')}
  cluster_assign_csv}


data <- read.csv('/Users/shanecrinion/Documents/business/projects/I2EHR/I2EHRv2/preload_example/test Kmeans CV_Rank Fixed_Probe_Num Fixed_Clust_Num Samples.Clusters.csv')
plyr::count(data$x)
plyr::count(data$x)$freq / sum(plyr::count(data$x)$freq)

cluster_counts <- plyr::count(data$x)
cluster_summary <-
  data.frame(
  cluster= cluster_counts$x,
  freq=cluster_counts$freq,
  freq_prop=(cluster_counts$freq / sum(cluster_counts$freq))
)
# 
# fig <- plotly::plot_ly(
#   x = as.character(cluster_summary$cluster),
#   y = cluster_summary$freq,
#   name = "test",
#   type = "bar"
# )
# 
# fig
# 
# library(plotly)
# x <- as.character(cluster_summary$cluster)
# y <- cluster_summary$freq
# text <- paste0(round(cluster_summary$freq_prop,2),'%')
# data <- data.frame(x, y, text)
# 
# fig <- plotly::plot_ly(data, x = ~x, y = ~y, type = 'bar', text = text,
#                marker = list(color = 'rgb(158,202,225)',
#                              line = list(color = 'rgb(8,48,107)',
#                                          width = 1.5)))
# fig <- fig %>% plotly::layout(title = "Test",
#                       xaxis = list(title = "Cluster"),
#                       yaxis = list(title = "No. of samples"))
# 
# fig
# 
# 
# fig <- plot_ly(cluster_summary, labels = ~cluster, values = ~freq_prop, type = 'pie')
# fig <- fig %>% layout(title = 'test',
#                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
# 
# fig
# 
# 

x <- as.character(cluster_summary$cluster)
y <- cluster_summary$freq
text <- paste0(round(cluster_summary$freq_prop,2),'%')
data <- data.frame(x, y, text)

fig <- plotly::plot_ly(data, x = ~x, y = ~y, type = 'bar', text = text,
                       marker = list(color = 'rgb(158,202,225)',
                                     line = list(color = 'rgb(8,48,107)',
                                                 width = 1.5)))
fig <- fig %>% plotly::layout(title = "Test",
                              xaxis = list(title = "Cluster"),
                              yaxis = list(title = "No. of samples"))

fig




# pie_chart <- function(data, 
#                       labels, 
#                       values, title){
# fig <- plot_ly(data, labels = ~labels, values = ~values, type = 'pie')
# fig <- fig %>% layout(title = title,
#                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
# 
# fig}
# 
# 
# pie_chart(data=cluster_summary, 
#             labels=~cluster, 
#             values =~freq_prop,
#             title='x')


#       