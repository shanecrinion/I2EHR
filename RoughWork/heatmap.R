dists <- as.matrix(dist(t(exp_raw), method = "manhattan"))
rownames(dists) <- row.names(pData(minguez_eset_norm))
hmcol <- rev(colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(255))
colnames(dists) <- NULL
diag(dists) <- NA

ann_colors <- list(
  Phenotype = c(diabetic = "chartreuse4", non-diabetic = "burlywood3"),
  Disease = c(CC = "blue4", HY = "cadetblue2")
)


ann_colors <- list(
  Phenotype = c(non_infl. = "chartreuse4", infl. = "burlywood3"),
  Disease = c(CD = "blue4", UC = "cadetblue2")
)


print(
  
ann_colors <- list(
    Phenotype = c(diabetic = "chartreuse4", non-diabetic = "burlywood3"),
    Disease = c(CC = "blue4", HY = "cadetblue2")
)
  

  pheatmap(dists, col = (hmcol), 
           annotation_row = annotation_for_heatmap,
           annotation_colors = ann_colors,
           legend = TRUE, 
           treeheight_row = 0,
           legend_breaks = c(min(dists, na.rm = TRUE), 
                             max(dists, na.rm = TRUE)), 
           legend_labels = (c("small distance", "large distance")),
           main = "Clustering heatmap for the calibrated samples")
)