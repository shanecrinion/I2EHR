# PATIENT INTEGRATED PLOT

library(GOexpress)
sub.genomic_data <- subEset(
  eSet=Ellsworth_final,
  subset=list(
    title=c("baseline","year1")))


library(lattice)
ggplot(exprs(genomic_data[[1]])) + 
  geom_density(aes(x = colnames(exprs(genomic_data[[1]]), fill = gender), alpha = 0.3 ) +
  scale_fill_manual(values= c("#ca0020","#0571b0")) +
  theme_bw() +
  facet_wrap(~site)

library(ggplot2)
  ggplot(genomic_data[[1]], aes(x=exprs(genomic_data[[1]]), y= ..density.., color=gender)) + 
    geom_histogram(binwidth=2,fill="white", alpha=0.3, position="identity") +
    geom_density() +
    scale_colour_manual(values= c("#ca0020","#0571b0")) +
    theme_bw()

  
  row_medians_assayData <- 
    Biobase::rowMedians(as.matrix(Biobase::exprs(genomic_data[[1]])))
  sweep(Biobase::exprs(genomic_data[[1]]), 1, row_medians_assayData)
  
  
  
