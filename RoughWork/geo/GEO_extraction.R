library(GEOquery)
## get the GEO accession for type 2 diabetes patients
# this contains the R structure for GDS3884
gds <- getGEO("GDS3884")

# Look at Column descriptions:
Columns(gds)
Columns(gds)[,1:3]

### Conversion of the data 
eset <- GDS2eSet(gds,do.log2 = TRUE)
eset
pData(eset)[,1:3]

#get the platform from the GDS metadata
Meta(gds)$platform

