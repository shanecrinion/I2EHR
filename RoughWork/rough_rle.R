#Some test data
dat <- data.frame(x=runif(10),y=runif(10),
                  grp = rep(LETTERS[1:5],each = 2),stringsAsFactors = TRUE)

#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(3,"Set1")
names(myColors) <- levels(gse25462[[1]]$disease_cat)
colScale <- scale_colour_manual(name = "disease_cat",values = myColors)


#One plot with all the data
p <- ggplot(dat,aes(x,y,colour = disease_cat)) + geom_point()
p1 <- p + colScale

#A second plot with only four of the levels
p2 <- p %+% droplevels(subset(dat[4:10,])) + colScale

library(RColorBrewer)

gse25462[[1]]$disease_cat <- 0

gse25462[[1]]$disease_cat[gse25462[[1]]$characteristics_ch1.3=="family history: Family history negative"] <- "FH-"
gse25462[[1]]$disease_cat[gse25462[[1]]$characteristics_ch1.3== "family history: DM"] <- "T2D"
gse25462[[1]]$disease_cat[gse25462[[1]]$characteristics_ch1.3== "family history: Family history positive - 2 parents"] <- "FH+"
gse25462[[1]]$disease_cat[gse25462[[1]]$characteristics_ch1.3=="family history: Family history positive - 1 parent"] <- "FH+"



# convert to information to character format
gse25462[[1]]$disease_cat <- 0
gse25462[[1]]$disease_cat[gse25462[[1]]$characteristics_ch1.3=="family history: Family history negative"] <- "FH-"
gse25462[[1]]$disease_cat[gse25462[[1]]$characteristics_ch1.3== "family history: DM"] <- "T2D"
gse25462[[1]]$disease_cat[gse25462[[1]]$characteristics_ch1.3== "family history: Family history positive - 2 parents"] <- "FH+"
gse25462[[1]]$disease_cat[gse25462[[1]]$characteristics_ch1.3=="family history: Family history positive - 1 parent"] <- "FH+"



# class for the fill
RLE_class <- data.frame(patient_array = rownames(pData(gse25462[[1]])), disease_cat=gse25462[[1]]$disease_cat) 
RLE_data <- as.data.frame(RLE_data)


RLE_data_gathered <- 
  tidyr::gather(RLE_data, patient_array, log2_expression_deviation)

RLE_data_gathered$class <- 0
RLE_data_gathered$class[RLE_data_gathered$patient_array == gse25462[[1]]$geo_accession] <- gse25462[[1]]$disease_cat

for(i in RLE_data_gathered){
  if(i$patient_array == gse25462[[1]]$geo_accession){
    i$class = gse25462[[1]]disease_cat}
  }

