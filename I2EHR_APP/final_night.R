

###### SET UP FOR PCA

sub.genomic_data.control <- subEset(
  eSet=genomic_data[[1]],
  subset=list(
    `group:ch1` =c("Matched Ornish Participant")))
control_patient <- sub.genomic_data.control$PATIENT 

sub.genomic_data.case <- subEset(
  eSet=genomic_data[[1]],
  subset=list(
    `group:ch1` =c("Matched Control Group")))

case_patient <- sub.genomic_data.case$PATIENT

'%!in%' <- function(x,y)!('%in%'(x,y))

observations.csv$`group:ch1` <- 0
observations.csv[observations.csv$PATIENT %in% case_patient,]$`group:ch1` <- "Matched Control Group"
observations.csv[observations.csv$PATIENT %in% control_patient,]$`group:ch1` <- "Matched Ornish Participant" 

observations.csv$PCA_sim_BMI <- 0
observations.csv[observations.csv$DESCRIPTION == "Body Mass Index" 
                 & observations.csv$`group:ch1` == "Matched Control Group",]$PCA_sim_BMI <- rnorm(1941, mean=23)

observations.csv[observations.csv$DESCRIPTION == "Body Mass Index" 
                 & observations.csv$`group:ch1` == "Matched Ornish Participant",]$PCA_sim_BMI <- rnorm(1788, mean = 29)

obs_bmi_overweight <- subset(x=observations.csv, DESCRIPTION == "Body Mass Index" & PCA_sim_BMI >= 25)
obs_bmi_healthy <- subset(x=observations.csv, DESCRIPTION == "Body Mass Index" & PCA_sim_BMI < 25)


genomic_data[[1]]$`ch1:highBMI_stat` <- NA
genomic_data[[1]]$`ch1:highBMI_stat`[genomic_data[[1]]$PATIENT %in% obs_bmi_overweight$PATIENT] <- "Overweight"
genomic_data[[1]]$`ch1:highBMI_stat`[genomic_data[[1]]$PATIENT %in% obs_bmi_healthy$PATIENT] <- "Healthy"


observations.csv$PCA_sim_SBP <- 0
observations.csv[observations.csv$DESCRIPTION == "Systolic Blood Pressure" 
                 & observations.csv$`group:ch1` == "Matched Control Group",]$PCA_sim_SBP <- rnorm(2504, mean=117)

observations.csv[observations.csv$DESCRIPTION == "Systolic Blood Pressure" 
                 & observations.csv$`group:ch1` == "Matched Ornish Participant",]$PCA_sim_SBP <- rnorm(2355, mean = 123)


obs_sbp_high_bp <- subset(x=observations.csv, DESCRIPTION == "Systolic Blood Pressure" & PCA_sim_SBP >= 120)
obs_sbp_healthy <- subset(x=observations.csv, DESCRIPTION == "Systolic Blood Pressure" & PCA_sim_SBP < 120)


genomic_data[[1]]$`ch1:SBP` <- NA
genomic_data[[1]]$`ch1:SBP`[genomic_data[[1]]$PATIENT %in% obs_sbp_high_bp$PATIENT] <- "High Systolic"
genomic_data[[1]]$`ch1:SBP`[genomic_data[[1]]$PATIENT %in% obs_sbp_healthy$PATIENT] <- "Healthy"


observations.csv$PCA_sim_DBP <- 0
observations.csv[observations.csv$DESCRIPTION == "Diastolic Blood Pressure" 
                 & observations.csv$`group:ch1` == "Matched Control Group",]$PCA_sim_DBP <- rnorm(2504, mean=73)

observations.csv[observations.csv$DESCRIPTION == "Diastolic Blood Pressure" 
                 & observations.csv$`group:ch1` == "Matched Ornish Participant",]$PCA_sim_DBP <- rnorm(2355, mean = 84)


obs_dbp_high_bp <- subset(x=observations.csv, DESCRIPTION == "Diastolic Blood Pressure" & PCA_sim_DBP < 80)
obs_dbp_healthy <- subset(x=observations.csv, DESCRIPTION == "Diastolic Blood Pressure" & PCA_sim_DBP >= 80)


genomic_data[[1]]$`ch1:DBP` <- NA
genomic_data[[1]]$`ch1:DBP`[genomic_data[[1]]$PATIENT %in% obs_dbp_high_bp$PATIENT] <- "High Diastolic"
genomic_data[[1]]$`ch1:DBP`[genomic_data[[1]]$PATIENT %in% obs_dbp_healthy$PATIENT] <- "Healthy"

######### plot difference




BMI.genomic_data.control <- subEset(
  eSet=genomic_data[[1]],
  subset=list(
    `ch1:highBMI_stat` =c("Overweight")))


BMI.genomic_data.case <- subEset(
  eSet=genomic_data[[1]],
  subset=list(
    `ch1:highBMI_stat` =c("Healthy")))




exprs(BMI.genomic_data.control)
exprs(BMI.genomic_data.case) 


exprs_class <- factor(c("case", "control")) 
case <- rep(exprs_class[1], length(exprs(BMI.genomic_data.case)))
control <- rep(exprs_class[2], length(exprs(BMI.genomic_data.control)))

case_exprs <- data.frame(exprs(BMI.genomic_data.case),case)

library(ggplot2)


ggplot(heights, aes(x=height, y= ..density.., color=gender)) + 
  geom_histogram(binwidth=2,fill="white", alpha=0.3, position="identity") +
  geom_density() +
  scale_colour_manual(values= c("#ca0020","#0571b0")) +
  
  theme_bw()
