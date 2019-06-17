PCA <- prcomp(t(exp_gse), scale = FALSE)
scores = as.data.frame(PCA$x) 
library(rgl)

exp_gse$diagnosis <-  

plot(scores[,1:2], col=,cex=1, pch=4,
     xlim = c(-225029.9,1186845), ylim=c(-467769.4,306593.5), main="PCA-2D")
text(scores[,1], scores[,2], 
     labels=c(rownames(scores)), cex= 0.3, pos=4)

plot3d(scores[,1:3], col=c(1:4), size=10, type='p', 
       xlim = c(-225029.9,1186845), 
       ylim=c(-467769.4,306593.5), 
       zlim=c(-288746.3,388504.9))
text3d(scores[,1]+2, scores[,2]+10, scores[,3]+2,
       texts=c(rownames(scores)), cex= 0.7, pos=3)

#output image
rgl.snapshot("3DPCA-Merge.png", "png") 
#ouput pdf:
rgl.postscript("3DPCA-Merge.pdf", "pdf")

dir.create("animation_merge")

for (i in 1:360) {
  view3d(userMatrix=rotationMatrix(2*pi * i/360, 0, 1, 0))
  rgl.snapshot(filename=paste("animation_merge/frame-",
                              sprintf("%03d", i), ".png", sep=""))}