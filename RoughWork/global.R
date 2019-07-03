# load the essential libraries
library(plyr)
library(GEOquery)
library(hugene20sttranscriptcluster.db)

# 1. import clinical data files
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))


#  2. GEO data import and set up
# ----- INSULIN RESISTANCE DATASET 
IR_raw <- getGEO("GSE25462", GSEMatrix = TRUE) # import raw
IR_norm <- normalize(IR_raw[[1]], transfn=c("log")) # normalize 


# categorize - based on insulin resistance measurements
IR_norm[[1]]$insulin_category <- 0
IR_norm[[1]]$insulin_category[
  (as.numeric(IR_norm[[1]]$`fasting insulin (iv0inavg):ch1`)) >= 8] <- "diabetic"

IR_norm[[1]]$insulin_category[
  ((as.numeric(IR_norm[[1]]$`fasting insulin (iv0inavg):ch1`)) < 8 
   & (as.numeric(IR_norm[[1]]$`fasting insulin (iv0inavg):ch1`)) > 3)] <- "optimal"

IR_norm[[1]]$insulin_category[(
  as.numeric(IR_norm[[1]]$`fasting insulin (iv0inavg):ch1`)) < 3] <- "low"

IR_norm$characteristics_ch1.3 <- 
  as.character(IR_norm$characteristics_ch1.3)


# categorize - based on diagnosis
IR_norm$characteristics_ch1.3[
  IR_norm$characteristics_ch1.3 == 
    "family history: Family history negative"] <- "FH-"

IR_norm$characteristics_ch1.3[
  IR_norm$characteristics_ch1.3 ==
    "family history: DM"] <- "T2D"

IR_norm$characteristics_ch1.3[
  IR_norm$characteristics_ch1.3 == 
    "family history: Family history positive - 2 parents"] <- "FH+"

IR_norm$characteristics_ch1.3[
  IR_norm$characteristics_ch1.3 == 
    "family history: Family history positive - 1 parent"] <- "FH+"

IR_norm$characteristics_ch1.3 <- as.factor(
  IR_norm$characteristics_ch1.3)


# annotation 

anno_IR <- AnnotationDbi::select(hugene20sttranscriptcluster.db,
                                 keys = (featureNames(IR_norm)),
                                 columns = c("SYMBOL", "GENENAME"),
                                 keytype = "PROBEID")

anno_IR <- subset(anno_IR, !is.na("SYMBOL"))

anno_grouped_IR <- group_by(anno_IR, PROBEID)

anno_summarized_IR <- 
  dplyr::summarize(anno_grouped_IR, 
                   no_of_matches = n_distinct(SYMBOL))

anno_filtered_IR <- filter(anno_summarized_IR, 
                           no_of_matches > 1)

probe_stats_IR <- anno_filtered_IR 

ids_to_exlude_IR <- (featureNames(IR_norm) %in% probe_stats_IR$PROBEID)

# expression 

exp_IR <- exprs(IR_raw)

# PCA

PCA_IR <- prcomp(t(exp_IR), scale = FALSE)
percentVar_IR <- round(100*PCA_IR$sdev^2/sum(PCA_IR$sdev^2),1)

