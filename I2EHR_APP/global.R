#import csv files
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))


#merging the data
library(plyr)
clinical_data <- ldply(.data = list.files(pattern="*.csv"),
                       .fun = read.csv,
                       header=TRUE)

