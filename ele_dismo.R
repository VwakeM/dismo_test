library(dismo)
library(maptools)
library(ggplot2)
library(tidyverse)

ele_max = gbif("elephas", "maximus*", geo=TRUE)
ele_loc = select(ele_max, lat, lon)
ele_valid <- ele_loc%>% na.omit() 

data(wrld_simpl)
plot(wrld_simpl, xlim=c(110,120), ylim=c(-10,80), axes=TRUE, col="light yellow")
box()

points(ele_valid$lon, ele_valid$lat, col='orange', pch=20, cex=0.75)
points(ele_valid$lon, ele_valid$lat, col='red', cex=0.75)

clim_dat = getData(name = "worldclim", download = TRUE, var = "bio", res = 10)
ele_ext = extent(0,180, 0, 180)
clim_crop = crop(clim_dat, ele_ext)
plot(clim_crop$bio1)

presvals = raster::extract(clim_crop, ele_valid)
presvals <- vals%>% na.omit()

plot(clim_dat, 1)
points(ele_valid$lon, ele_valid$lat, col='orange', pch=20, cex=0.75)
points(ele_valid$lon, ele_valid$lat, col='red', cex=0.75)

set.seed(0)
backgr <- randomPoints(clim_crop, 500)
absvals <- raster::extract(clim_crop, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))

sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata[,'biome'] = as.factor(sdmdata[,'biome'])
head(sdmdata)

pairs(sdmdata[,2:5], cex=0.1)

saveRDS(sdmdata, "sdm.Rds")
saveRDS(presvals, "pvals.Rds")

pred_nf <- dropLayer(clim_crop, 'biome')

set.seed(0)
group <- kfold(ele_valid, 5)
pres_train <- ele_valid[group != 1, ]
pres_test <- ele_valid[group == 1, ]

set.seed(10)
backg <- randomPoints(pred_nf, n=1000, extf = 1.25)
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

r <- raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(ext, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-',  cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')