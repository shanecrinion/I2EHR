# create a list of all the conditions
attach(conditions) 
top_conditions <- conditions[order(DESCRIPTION, decreasing = TRUE),]
head(top_conditions)
conditions_list <- unique(top_conditions[,6]), decreasing = FALSE)
print(conditions_list)
