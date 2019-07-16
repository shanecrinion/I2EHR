

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


observations.csv$`group:ch1` <- 0
observations.csv[observations.csv$PATIENT %in% case_patient,]$`group:ch1` <- "Matched Control Group"
observations.csv[observations.csv$PATIENT %in% control_patient,]$`group:ch1` <- "Matched Ornish Participant" 


observations.csv$PCA_sim_BMI <- 0
observations.csv[observations.csv$DESCRIPTION == "Body Mass Index" 
                 & observations.csv$`group:ch1` == "Matched Control Group",]$PCA_sim_BMI <- rnorm(1941, mean=20)

observations.csv[observations.csv$DESCRIPTION == "Body Mass Index" 
                 & observations.csv$`group:ch1` == "Matched Ornish Participant",]$PCA_sim_BMI <- rnorm(1788, mean = 30)


observations.csv$PCA_sim_SBP <- 0
observations.csv[observations.csv$DESCRIPTION == "Systolic Blood Pressure" 
                 & observations.csv$`group:ch1` == "Matched Control Group",]$PCA_sim_SBP <- rnorm(2504, mean=110)

observations.csv[observations.csv$DESCRIPTION == "Systolic Blood Pressure" 
                 & observations.csv$`group:ch1` == "Matched Ornish Participant",]$PCA_sim_SBP <- rnorm(2355, mean = 140)


observations.csv$PCA_sim_DBP <- 0
observations.csv[observations.csv$DESCRIPTION == "Diastolic Blood Pressure" 
                 & observations.csv$`group:ch1` == "Matched Control Group",]$PCA_sim_DBP <- rnorm(2504, mean=75)

observations.csv[observations.csv$DESCRIPTION == "Diastolic Blood Pressure" 
                 & observations.csv$`group:ch1` == "Matched Ornish Participant",]$PCA_sim_DBP <- rnorm(2355, mean = 90)




median(as.numeric(as.character(observations.csv[observations.csv$DESCRIPTION == "Body Mass Index" 
                                                & observations.csv$`group:ch1` == "Matched Ornish Participant",]$VALUE)))

median(as.numeric(as.character(observations.csv[observations.csv$DESCRIPTION == "Body Mass Index" 
                                                & observations.csv$`group:ch1` == "Matched Ornish Participant",]$VALUE)))
