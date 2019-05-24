

library(ggplot2)
library(ggridges)
library(lattice)


##merge the data
observations_merge <- merge(x = patients.csv, 
                            y = observations.csv, 
                            by.x = "Id", 
                            by.y= "PATIENT") 



## could alternatively do the below command using to obsevation code (probably better)
  bmi_measurements <- 	
    subset(observations_merge,
           subset = (observations_merge$DESCRIPTION == "Body Mass Index"))
  
  bmi_measurements$BIRTHDATE <- 
    substring(bmi_measurements$BIRTHDATE, 1, 4) 
  
#  bmi_measurements$DECADE <- 
  #    10*as.integer(as.numeric(bmi_measurements$BIRTHDATE/10))
  
  bmi_measurements$DECADE <- 
    10*as.integer(as.numeric(as.character(bmi_measurements$BIRTHDATE)) / 10)
  
  bmi_measurements$VALUEBIN <- 
    1*as.integer(as.numeric(as.character(bmi_measurements$VALUE)) / 1)
  
  #plot the two
  ggplot(bmi_measurements) + 
    geom_density_ridges(aes(x = VALUEBIN, 
                            y = DECADE, 
                            group = interaction(GENDER, DECADE),
                            fill = GENDER), 
                        alpha = 0.6, 
                        scale = 0.8) +
    
    geom_vline(xintercept = 5.7, 
               color = "lightskyblue1", size=0.5) +
    
    geom_vline(xintercept = 6.2, 
               color = "tomato", size=0.5) +
    
    scale_fill_manual(values=c("#0571b0", "#ca0020")) +
    
    theme_classic() +
    theme(
      axis.text.x= element_text(angle = 30),
      panel.grid.major.y =element_line(colour = "gray95"),
      panel.grid.major.x =element_line(colour = "gray95"),
      panel.grid.minor.x =element_line(colour = "gray95"))

