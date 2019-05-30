if (!require("BiocManager"))
  install.packages("BiocManager")
BiocManager::install("maEndToEnd", version = "devel")
#Quality control and pre-processing packages
library(oligo)
library(arrayQualityMetrics)