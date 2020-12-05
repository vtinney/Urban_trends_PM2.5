library(raster)
library(rgdal)

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/urban_extents/')
list.files()

high <- raster("rgp.0.01.03.HDC.tif")
low <- raster("rgp.0.1x0.1.03.HDC.tif")

high[high == -32768] <- NA
low[low == -32768] <- NA

high[high != 30] <- NA
low[low !=30 ] <- NA

writeRaster(low, filename="rgp.0.1x0.1.03.HDC_3.tif", format='GTiff', overwrite=TRUE)
writeRaster(high, filename="rgp.0.01.03.HDC.tif", format='GTiff',overwrite=TRUE)

full <- raster('cities_13k_clip2.tif')

full.high <- mask(full, high)
full.low <- mask(full, low)
