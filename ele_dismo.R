install.packages(c('raster', 'rgdal', 'dismo', 'rJava'))

library(dismo)
library(maptools)
library(ggplot2)
library(tidyverse)

#file = paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")
#bradypus <- read.table(file,  header=TRUE,  sep=",")
#head(bradypus)

ele_max = gbif("elephas", "maximus*", geo=TRUE)
ele_loc = select(ele_max, lat, lon)

#ras = raster(ele_max)

data(wrld_simpl)
plot(wrld_simpl, xlim=c(110,120), ylim=c(-10,80), axes=TRUE, col="light yellow")

box()
# add the points
points(ele_max$lon, ele_max$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility 
points(ele_max$lon, ele_max$lat, col='red', cex=0.75)

#duplicated(ele_max$lon)

# Use land use, elevation, rainfall and create a raster stack
clim_dat = getData(name = "worldclim", download = TRUE, var = "bio", res = 10)
ele_ext = extent(-180,100,-90,90)
clim_crop = crop(clim_dat, ele_ext)
plot(clim_crop$bio1)

vals = raster::extract(clim_dat, ele_valid)

ele_valid <- ele_loc%>% na.omit() 

sum(is.na(ele_loc$lon))

class(clim_dat)
plot(clim_crop, 3)

