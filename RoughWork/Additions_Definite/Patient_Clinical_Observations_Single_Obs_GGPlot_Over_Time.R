## -- UNUSED ---


patient_data <- 
  observations.csv[observations.csv$PATIENT == "1425bcf6-2853-4c3a-b4c5-64fbe03d43d2",]

observation <- 
  patient_data[patient_data$DESCRIPTION == as.character("Body Mass Index"),]

ggplot(data=subset(observation, !is.na(VALUE)), 
       aes(x=as.character(DATE), y=VALUE)) 

+ geom_point()

## plot variables
ggplot(data = subset(patient_observation_data, !is.na(VALUE)),
       aes(x=as.character(DATE), y=VALUE)) + geom_point()

## --- UNUSED ABOVE ---


## -- create a new variable that matches the patient ID to their name 

patients.csv$PATIENT <- patients.csv$Id 
observations_names <- merge(observations.csv,patients.csv, by  = "PATIENT") 

## assign variables
patient <- "1425bcf6-2853-4c3a-b4c5-64fbe03d43d2"
observation_selection <-  as.character("Body Mass Index")
patient_data <- observations_names[observations_names$PATIENT == patient,]
patient_observation_data <- patient_data[patient_data$DESCRIPTION == observation_selection,]


f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "Date of Observation",
  titlefont = f
)

y <- list(
  title = paste(patient_observation_data$DESCRIPTION[1], "(",
                patient_observation_data$UNITS[1], ")"),
  titlefont = f
)

patient_observation_data  %>% 
  plot_ly(
    x = ~DATE, y =~as.numeric(as.character(VALUE)), 
    type = "scatter")  %>% 
  layout(xaxis = x, yaxis = y, 
         title= paste(patient_observation_data$DESCRIPTION[1], "for",
                      patient_observation_data$FIRST[[1]],
                      patient_observation_data$LAST[[1]]))
