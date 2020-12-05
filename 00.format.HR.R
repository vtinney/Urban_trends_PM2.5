library(raster)
library(rgdal)
library(ncdf4)
setwd('E:/GBD_2020/Final/Hammer/')

files <- list.files()

for (k in 1:length(files)){
  print(files[k])
  p <- raster(paste(files[k]))
  writeRaster(p, filename=paste(files[k]),format="GTiff",overwrite=TRUE)
}