library(viridis)
disorders_vector <- as.vector(count(conditions.csv$DESCRIPTION))
#disorders_vector$freqs <- as.numeric(disorders_vector$freqs)

par(mar=c(2, 28, 5, 5))

xlim <- c(0, 1.1*max(disorders_vector$freq))

xx <- barplot(disorders_vector$freq,
              xaxt = 'n',
              horiz = TRUE,
              col=viridis(27),
              width = 12,
              xlim = xlim,
              main = "Frequency of each metabolics disorder",
              xlab = "Frequency")

## Add text at top of bars
text(y = xx, x = disorders_vector$freq,
     label = disorders_vector$freq,
     pos = 4,
     cex = 0.5,
     col = magma(1))
## Add x-axis labels
axis(side = 2, at=xx,
     labels=disorders_vector$x,
     tick=FALSE,
     las=2,
     line=-0.5,
     cex.axis=0.5)
