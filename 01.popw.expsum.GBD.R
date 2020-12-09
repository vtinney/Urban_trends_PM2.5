# Created: 12-6-2020
# This is a code to create a population weighted PM2.5 based on HDC city boundaries.
# Sensitivity analysis using coarse resolution GBD 0.1x0.1 concentrations and population
# GPW v4 used as a ratio

#==================================================================
#Set working directory and load files
library(raster)
library(rgdal)
library(dplyr)
library(dplyr)
library(plyr)

conc.year <- c("2000","2005","2010","2011","2012","2013","2014","2015","2016","2017","2018")

#==========================================================================================================================
# City boundaries - HDC GHSSMOD - 0.1
#==========================================================================================================================

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/urban_extents/')

shp <- raster("cities_13k_clip2_10km.tif")

for(j in 1:length(conc.year)){

  r <- raster(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/GBD/pm/mean_raster_',conc.year[j],'.tif',sep=''))
  pop <- raster(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/GBD/pop/POP_',conc.year[j],'.tif',sep=''))
  
  r <- crop(r, shp)
  pop <- crop(pop, shp)
  shp <- resample(shp, r)
  
  popxconc <- r*pop

  zone.in <- shp
  raster.in <- popxconc

  # Use the function to sum pop*concentration to HDC
  shp_df <- zonal(raster.in, zone.in, fun="sum", na.rm=TRUE)
  df1 <- as.data.frame(shp_df) # dataframe formatting
  colnames(df1)[ncol(df1)] <- 'popxconc'
  colnames(df1)[1] <- 'id'
  print('zone1')

  # Use the function sum to population to HDC
  raster.in <- pop
  shp_df1 <- zonal(raster.in, zone.in, fun="sum",na.rm=TRUE)
  df2 <- as.data.frame(shp_df1) # dataframe formatting
  colnames(df2)[ncol(df2)] <- 'pop.sum'
  colnames(df2)[1] <- 'id'
  print('zone2')

  df <- merge(df1, df2, by='id') # merge pop*conc sum and pop sum

  # Population weighting
  df$popw <- df$popxconc/df$pop.sum
  df$year <- paste(conc.year[j],sep='')
  df$res <- '10km'

  print('merge')
  # Write out CSV file
  fout = paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/exp_sum/gbd/hdc_GBD_',conc.year[j],'.csv',sep='')
  write.csv(df, file=fout)
  print(fout)

}


#=======================================================================================
setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/urban_extents/')

conc.year <- c("2000","2010","2015","2019")

shp <- raster("cities_13k_clip2.tif") #0.01
ex <- extent(shp)

#=======================================================================================

for(j in 1:length(conc.year)){

  r <- raster(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/GBD/pm/rpg2.pm_',conc.year[j],'.tif',sep=''))
  pop <- raster(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/GBD/pop/gpw_',conc.year[j],'.tif',sep=''))
  popxconc <- r*pop

  zone.in <- shp
  raster.in <- popxconc

  # Use the function to sum pop*concentration to HDC
  shp_df <- zonal(raster.in, zone.in, fun="sum", na.rm=TRUE)
  df1 <- as.data.frame(shp_df) # dataframe formatting
  colnames(df1)[ncol(df1)] <- 'popxconc'
  colnames(df1)[1] <- 'id'
  print('zone1')

  # Use the function sum to population to HDC
  raster.in <- pop
  shp_df1 <- zonal(raster.in, zone.in, fun="sum",na.rm=TRUE)
  df2 <- as.data.frame(shp_df1) # dataframe formatting
  colnames(df2)[ncol(df2)] <- 'pop.sum'
  colnames(df2)[1] <- 'id'
  print('zone2')

  df <- merge(df1, df2, by='id') # merge pop*conc sum and pop sum


  # Population weighting
  df$popw <- df$popxconc/df$pop.sum
  df$year <- paste(conc.year[j],sep='')
  df$res <- '1km'

  print('merge')
  # Write out CSV file
  fout = paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/exp_sum/gbd/hdc_popw_rpg_',conc.year[j],'.csv',sep='')
  write.csv(df, file=fout)
  print(fout)

}

# Combine all results for all years into one dataframe
setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/exp_sum/gbd/')

filenames <- list.files(path='/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/exp_sum/gbd/',pattern="*.csv", full.names=TRUE)
dataset <- do.call("rbind",lapply(filenames,FUN=function(files){read.csv(files, header=TRUE, sep=",")}))
write.csv(dataset, "combined.hdc.GBD.popw.csv")

