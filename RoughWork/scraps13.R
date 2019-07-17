######## PATIENT 
subset.patient <- subEset(
  eSet=genomic_data[[1]],
  subset=list(
    PATIENT=c(input$search)))
###### END PATIENT

#genomic_data_selection <- genomic_data[[1]][,genomic_data[[1]]$PATIENT == input$search]

## DATA
row_medians_assayData_patient <- 
  Biobase::rowMedians(as.matrix(exprs(subset.patient)))

RLE_data_patient <- sweep(log2(Biobase::exprs(subset.patient)), 1, 
                          row_medians_assayData_patient)
# class for the fill
RLE_class_patient <- data.frame(patient_array = rownames(pData(subset.patient)), 
                                disease_cat = Biobase::pData(subset.patient)$`diabetes:ch1`)

RLE_data_patient <- as.data.frame(RLE_data_patient)
RLE_data_gathered_patient <- 
  tidyr::gather(RLE_data_patient, 
                patient_array, 
                log2_expression_deviation)

RLE_data_gathered_diagnosis_patient <- 
  merge(RLE_data_gathered_patient, 
        RLE_class_patient, 
        by="patient_array")

###### CONTROL file set up

diabetes_state <- subset.patient$`diabetes:ch1`[1]
gender_state <- subset.patient$`gender:ch1`[1]
cad_state <- subset.patient$`cad:ch1`[1]
age_range <- c((as.numeric(subset.patient$`age:ch1`) - 5 ),
               (as.numeric(subset.patient$`age:ch1`) + 5 ))
age_range <- age_range[c(1,5)]


subset.control <- genomic_data[[1]][,genomic_data[[1]]$`diabetes:ch1` == diabetes_state]
subset.control <- subset.control[,subset.control$`gender:ch1` == gender_state]
subset.control <- subset.control[,subset.control$`cad:ch1` == cad_state]
subset.control <- subset.control[,subset.control$`age:ch1` > age_range[1] 
                                 & subset.control$`age:ch1` < age_range[2]]
subset.control <- subset.control[,subset.control$`cad:ch1` == cad_state]
subset.control <- subset.control[,subset.control$`group:ch1` != "Matched Ornish Participant"]
subset.control <- subset.control[1]



merge(x = pData(genomic_data[[1]]), y= observations.csv, by.x="PATIENT", by.y="PATIENT")

###### END CONTROL

## DATA
row_medians_assayData_control <- 
  Biobase::rowMedians(as.matrix(exprs(subset.control)))

RLE_data_control <- sweep(log2(Biobase::exprs(subset.control)), 1, 
                          row_medians_assayData_control)
# class for the fill
RLE_class_control <- data.frame(patient_array = rownames(pData(subset.control)), 
                                disease_cat = Biobase::pData(subset.patient)$`diabetes:ch1`)
RLE_data_control <- as.data.frame(RLE_data_control)
RLE_data_gathered_control <- 
  tidyr::gather(RLE_data_control, 
                patient_array, 
                log2_expression_deviation)

RLE_data_gathered_diagnosis_control <- 
  merge(RLE_data_gathered_control, 
        RLE_class_control, 
        by="patient_array")


plot1 <- ggplot2::ggplot(RLE_data_gathered_diagnosis_patient, 
                         aes(patient_array,
                             log2_expression_deviation, 
                             fill=disease_cat)) + 
  geom_boxplot(outlier.shape = NA) + 
  
  ylim(c(-2, 2)) + 
  theme(axis.text.x = element_text(colour = "aquamarine4", 
                                   angle = 60, size = 6.5, hjust = 1 ,
                                   face = "bold"))


plot2 <- ggplot2::ggplot(RLE_data_gathered_diagnosis_control, 
                         aes(patient_array,
                             log2_expression_deviation, 
                             fill=disease_cat)) + 
  
  geom_boxplot(outlier.shape = NA) + 
  ylim(c(-2, 2)) + 
  theme(axis.text.x = element_text(colour = "aquamarine4", 
                                   angle = 60, size = 6.5, hjust = 1 ,
                                   face = "bold"))


grid.arrange(plot1, plot2, ncol=2)





