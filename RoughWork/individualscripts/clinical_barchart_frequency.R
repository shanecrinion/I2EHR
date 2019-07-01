library(dplyr)
disorders_tbl <- 
  conditions.csv %>% group_by(DESCRIPTION) %>% summarize(count=n())

disorders_tbl$count <- as.numeric(disorders_tbl$count)

par(mar=c(2, 28, 5, 5))

xlim <- c(0, 1.1*max(disorders_tbl$count))

xx <- barplot(disorders_tbl$count,
              xaxt = 'n',
              horiz = TRUE,
              col=heat.colors(27),
              width = 12,
              xlim = xlim,
              main = "Frequency of each metabolics disorder",
              xlab = "Frequency")

## Add text at top of bars
text(y = xx, x = disorders_tbl$count,
     label = disorders_tbl$count,
     pos = 4,
     cex = 0.5,
     col = magma(1))

## Add x-axis labels
axis(side = 2, at=xx,
     labels=disorders_tbl$DESCRIPTION,
     tick=FALSE,
     las=2,
     line=-0.5,
     cex.axis=0.5)
