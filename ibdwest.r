## IBD calculation
## load data
library(data.table)
## load genetic data
gendist = fread("WESTERN19_distances.csv", data.table = F)
## convert to matrix
gendistm = as.matrix(gendist[2:nrow(gendist), 2:ncol(gendist)])
colnames(gendistm) = gendist[1, 2:ncol(gendist)]
rownames(gendistm) = gendist[2:nrow(gendist), 1]
## make diagonal zero
diag(gendistm)=0
gendistm = as.numeric(gendistm)
dim(gendistm) = dim(gendist) - 1
## load latitude/long file
geolat = fread("WEST_latlong_corrected.csv", data.table = FALSE)
## calculate geographical distance
library(geosphere)
distgeo = distm(geolat[, c("long", "lat")]) / 1000

## mantel test
library(ade4)
mantel.rtest(as.dist(gendistm), as.dist(distgeo))

ibd = mantel.randtest(as.dist(gendistm), as.dist(distgeo))

## pdf("ibd.pdf")
## plot(ibd)
## dev.off()


## pdf("ibd_cor.pdf")
## plot(gendistm, distgeo)
## dev.off()

## library(MASS)
## pdf('ibd_mat.pdf')
## myPal <- colorRampPalette(c("white","blue","gold", "orange", "red"))
## mycolors = myPal(300)
## mycolors = paste(mycolors, "70", sep="")
## dens = kde2d(as.dist(gendistm), as.dist(distgeo), n = 300)
## plot(gendistm, distgeo)
## plot(lm(gendistm ~ distgeo))
## image(dens, col = mycolors, add = TRUE)

## dev.off()

## gendistm[:]

toplot =
     data.frame(
         x = c(gendistm), 
         y = c(distgeo)
     )

## densdf = data.frame(
##     x = c(dens$x),
##     y = c(dens$y),
##     z = c(dens$z))

## ggplot(data = toplot, aes(x = x, y = y)) +
##     stat_density2d(geom = 'polygon', aes(fill = ..level.., alpha = ..level..), contour = F, colour = 'black') +
##     scale_fill_continuous(low = 'green', high = 'red') + 
##     geom_smooth(method = 'lm', colour = "red", linetype = 2) +
##     guides(alpha = 'none') +
##     geom_jitter(alpha = 0.1) + theme_light()
## #    scale_fill_distiller(palette = "Spectral") 


mlm =lm(data = toplot, y ~ x )
r2 = summary(mlm)$adj.r.squared

pdf('ibd_mat.pdf')
ggplot() + stat_density_2d(geom = 'raster', data = toplot, aes(fill = ..density.., x, y), contour = F) + 
    scale_fill_distiller(palette = "Spectral") +
    geom_jitter(data = toplot, aes(x = x, y = y), alpha = 0.05) + 
    geom_smooth(data = toplot, aes(x, y), method = 'lm', linetype = 2, colour = 'red') +
    xlim(0, 0.018) + ylim(0, 500) + theme_linedraw() +
    ggtitle(sprintf("Adjusted R2 = %0.2g. Mantel test pvalue = %0.2g", r2, ibd$pvalue))
dev.off()




