library(ggplot2)

ggplot(exprs(genomic_data[[1]]), aes(x= , y= ..density.., color=gender)) + 
  geom_histogram(binwidth=2,fill="white", alpha=0.3, position="identity") +
  geom_density() +
  scale_colour_manual(values= c("#ca0020","#0571b0")) +
  
  theme_bw()

ggplot(exprs(genomic_data[[1]]))


## set up the subset
# Subset it to only samples of "CN" and "MB" treatments, and also only "2H",
# "6H", and "24H" time-points
library(GOexpress)
sub.genomic_data <- subEset(
  eSet=Ellsworth_final,
  subset=list(
    =c("Matched Ornish Participant")))


row_medians_assayData <- 
  Biobase::rowMedians(as.matrix(Biobase::exprs(genomic_data[[1]])))

RLE_data <- sweep(Biobase::exprs(genomic_data[[1]]), 1, row_medians_assayData)

RLE_data <- as.data.frame(RLE_data)
RLE_data_gathered <- 
  tidyr::gather(RLE_data, patient_array, log2_expression_deviation)

ggplot2::ggplot(RLE_data_gathered, aes(patient_array,
                                       log2_expression_deviation)) + 
  geom_boxplot(outlier.shape = NA) + 
  ylim(c(-2, 2)) + 
  theme(axis.text.x = element_text(colour = "aquamarine4", 
                                   angle = 60, size = 6.5, hjust = 1 ,
                                   face = "bold"))
