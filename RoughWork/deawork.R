### expression oof the normalised data

gse_norm <- normalize(gse25462[[1]], transfn=c("log"))
exp_gse <- Biobase::exprs(gse_norm)
### get the prinicipal component values

PCA <- prcomp(t(exp_gse), scale = FALSE)

# get the percentage of variance
# indicates that 28.4 pct of variance is from the first, maybe use only 2
percentVar <- round(100*PCA$sdev^2/sum(PCA$sdev^2),1)

sd_ratio <- sqrt(percentVar[2] / percentVar[1])


# convert to information to character format
gse_norm$characteristics_ch1.3 <- as.character(gse_norm$characteristics_ch1.3)
# limit to three values for clustering
gse_norm$characteristics_ch1.3[gse_norm$characteristics_ch1.3 == "family history: Family history negative"] <- "FH-"
gse_norm$characteristics_ch1.3[gse_norm$characteristics_ch1.3 == "family history: DM"] <- "T2D"
gse_norm$characteristics_ch1.3[gse_norm$characteristics_ch1.3 == "family history: Family history positive - 2 parents"] <- "FH+"
gse_norm$characteristics_ch1.3[gse_norm$characteristics_ch1.3 == "family history: Family history positive - 1 parent"] <- "FH+"
# convert back to avoid creating future problems
gse_norm$characteristics_ch1.3 <- as.factor(gse_norm$characteristics_ch1.3)


dataGG <- data.frame(PC1 = PCA$x[,1], PC2 = PCA$x[,2],
                     Disease_Category = 
                       Biobase::pData(gse_norm)$characteristics_ch1.3,
                     Insulin_Resistance = 
                       Biobase::pData(gse_norm)$insulin_category)

ggplot(dataGG, aes(PC1, PC2)) +
  geom_point(aes(shape = Disease_Category, colour = Insulin_Resistance)) +
  ggtitle("PCA plot of the calibrated, summarized data") +
  xlab(paste0("PC1, VarExp: ", percentVar[1], "%")) +
  ylab(paste0("PC2, VarExp: ", percentVar[2], "%")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = sd_ratio)



