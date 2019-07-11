#import all csv files containing clinical data from Synthea
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

# order to obtains with most detailed info
observations.csv <-with(observations.csv , observations.csv[order(PATIENT),])

# list 64 patients with most obs (match no of GEO samples)
patients_most_obs <- unique(observations.csv$PATIENT)[0:126]

# format patients.csv from Id to PATIENT to match other files
names(patients.csv)[names(patients.csv) == "Id"] <- "PATIENT"

# limit the info in each other csv file to these patients
allergies.csv <- allergies.csv[allergies.csv$PATIENT %in% patients_most_obs,]
careplans.csv <- careplans.csv[careplans.csv$PATIENT %in% patients_most_obs,]
conditions.csv <- conditions.csv[conditions.csv$PATIENT %in% patients_most_obs,]
encounters.csv <- encounters.csv[encounters.csv$PATIENT %in% patients_most_obs,]
immunizations.csv <- immunizations.csv[immunizations.csv$PATIENT %in% patients_most_obs,]
observations.csv <- observations.csv[observations.csv$PATIENT %in% patients_most_obs,]
organizations.csv <- organizations.csv[organizations.csv$PATIENT %in% patients_most_obs,]
patients.csv <- patients.csv[patients.csv$PATIENT %in% patients_most_obs,]
procedures.csv <- procedures.csv[procedures.csv$PATIENT %in% patients_most_obs,]
providers.csv <- providers.csv[providers.csv$PATIENT %in% patients_most_obs,]



write.csv(providers.csv, file = "providers.csv")

