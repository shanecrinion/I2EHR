
### expression oof the normalised data
exp_gse <- Biobase::exprs(gse_norm)

dists <- as.matrix(dist(t(exp_gse), method = "manhattan"))

rownames(dists) <- row.names(pData(gse_norm))

hmcol <- rev(colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(255))

colnames(dists) <- NULL
diag(dists) <- NA

pheatmap(dists, col = (hmcol), 
         legend = TRUE, 
         treeheight_row = 0,
         legend_breaks = c(min(dists, na.rm = TRUE), 
                           max(dists, na.rm = TRUE)), 
         legend_labels = (c("small distance", "large distance")),
         main = "Clustering heatmap for the calibrated samples")




gse_medians <- rowMedians(Biobase::exprs(gse_norm))
man_threshold <- 60
hist_res <- hist(gse_medians, 
                 breaks=10000,
                 xlim=c(0,10000),
                 ylim=c(0,0.002),
                 col = "cornsilk1", 
                 freq = FALSE, 
                 main = "Histogram of the median intensities", 
                 border = "antiquewhite4",
                 xlab = "Median intensities")

abline(v = man_threshold, col = "coral4", lwd = 1)


no_of_samples <- 
  table(count(gse_norm$characteristics_ch1.3))
no_of_samples 

samples_cutoff <- min(no_of_samples)


idx_man_threshold <- apply(Biobase::exprs(gse_norm), 1,
                           function(x){
                             sum(x > man_threshold) >= samples_cutoff})
table(idx_man_threshold)



anno_gse <- AnnotationDbi::select(hgu133plus2.db,
                                       keys = (featureNames(gse_norm)),
                                       columns = c("SYMBOL", "GENENAME"),
                                       keytype = "PROBEID")

anno_gse <- subset(anno_gse, !is.na("SYMBOL"))


anno_grouped <- group_by(anno_gse, PROBEID)
anno_summarized <- 
  dplyr::summarize(anno_grouped, no_of_matches = n_distinct(SYMBOL))

head(anno_summarized)


anno_filtered <- filter(anno_summarized, no_of_matches > 1)

head(anno_filtered)

probe_stats <- anno_filtered 

nrow(probe_stats)

head(probe_stats)



anno_gse <- AnnotationDbi::select(hgu133plus2.db,
                                  keys = (featureNames(gse_norm)),
                                  columns = c("SYMBOL", "GENENAME"),
                                  keytype = "PROBEID")


output$excluded_probes <- renderTable({
gse_norm <- normalize(gse25462[[1]], transfn=c("log"))
anno_gse <- subset(anno_gse, !is.na("SYMBOL"))
anno_grouped <- group_by(anno_gse, PROBEID)
anno_summarized <- 
  dplyr::summarize(anno_grouped, no_of_matches = n_distinct(SYMBOL))
anno_filtered <- filter(anno_summarized, no_of_matches > 1)
probe_stats <- anno_filtered 
ids_to_exlude <- (featureNames(gse_norm) %in% probe_stats$PROBEID)
table(ids_to_exlude)
})


gse_final <- subset(gse_norm, !ids_to_exlude)
validObject(gse_final)

gse_norm <- normalize(gse25462[[1]], transfn=c("log"))
anno_gse <- subset(anno_gse, !is.na("SYMBOL"))
anno_grouped <- group_by(anno_gse, PROBEID)
anno_summarized <- 
  dplyr::summarize(anno_grouped, no_of_matches = n_distinct(SYMBOL))
anno_filtered <- filter(anno_summarized, no_of_matches > 1)
probe_stats <- anno_filtered 
ids_to_exlude <- (featureNames(gse_norm) %in% probe_stats$PROBEID)
gse_final <- subset(gse_norm, !ids_to_exlude)

fData(gse_final)
