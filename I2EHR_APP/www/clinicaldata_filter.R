# order to obtains with most detailed info
observations.csv <-with(observations.csv , observations.csv[order(PATIENT),])

# list 64 patients with most obs (match no of GEO samples)
patients_most_obs <- unique(observations.csv$PATIENT)

observations.csv <- subset(observations.csv, observations.csv$PATIENT %in% patients.csv$PATIENT)

# format patients.csv from Id to PATIENT to match other files
names(patients.csv)[names(patients.csv) == "Id"] <- "PATIENT"

# limit the info in each other csv file to these patients
allergies.csv <- subset(allergies.csv, allergies.csv$PATIENT %in% patients.csv$PATIENT)
careplans.csv <- subset(careplans.csv, careplans.csv$PATIENT %in% patients.csv$PATIENT)
conditions.csv <- subset(conditions.csv, conditions.csv$PATIENT %in% patients.csv$PATIENT)
encounters.csv <- subset(encounters.csv, encounters.csv$PATIENT %in% patients.csv$PATIENT)
immunizations.csv <- subset(immunizations.csv, immunizations.csv$PATIENT %in% patients.csv$PATIENT)
observations.csv <- subset(observations.csv, observations.csv$PATIENT %in% patients.csv$PATIENT)
organizations.csv <- subset(organizations.csv, organizations.csv$PATIENT %in% patients.csv$PATIENT)
procedures.csv <- subset(procedures.csv, procedures.csv$PATIENT %in% patients.csv$PATIENT)
providers.csv <- subset(providers.csv, providers.csv$PATIENT %in% patients.csv$PATIENT)


write.csv(allergies.csv, file = "allergies.csv")
write.csv(careplans.csv, file = "careplans.csv")
write.csv(conditions.csv, file = "conditions.csv")
write.csv(encounters.csv, file = "encounters.csv")
write.csv(immunizations.csv, file = "immunizations.csv")
write.csv(observations.csv, file = "observations.csv")
write.csv(organizations.csv, file = "organizations.csv")
write.csv(patients.csv, file = "patients.csv")
write.csv(procedures.csv, file = "procedures.csv")
write.csv(providers.csv, file = "providers.csv")

