# ==========================================================================================================================================
# Created: 2020-12-5
# Combined all mortality endpoints (minus age specific CVD & Stroke)
# This calculates burden at the country level using all concentrations
# ==========================================================================================================================================

library(raster)
library(rgdal)
library(plyr)
library(dplyr)
library(reshape2)
library(safejoin)
library(tidyverse)

#Specify age group years for rates
years <- c(2000,2005,2010:2018)

cats <- c('lri_rr','resp_copd_rr','t2_dm_rr','neo_lung_rr')

conc <- read.csv(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/combined.hdc.GBD.popw.1km.csv',sep=''))
print('conc')

hap <- read.csv(paste('/GWSPH/groups/anenberggrp/GBD_2019_June_2020/exposure_summaries/hap_pm2.5_exposure_nat&subnat.csv',sep=''))
hap.prop <- read.csv(paste('/GWSPH/groups/anenberggrp/GBD_2019_June_2020/exposure_summaries/hap_prop_exposure_nat&subnat.csv',sep=''))

rates <- read.csv(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/allages.ihme.final.csv',sep=''))

ihme2 <- rates[,c(3,4,5,9,11,13,14,15)]

df2 <- subset(ihme2, cause_name %in% c('Diabetes mellitus type 2','Lower respiratory infections',
                                    'Chronic obstructive pulmonary disease', 'Tracheal, bronchus, and lung cancer'))

df3 <- subset(df2, age_name %in% 'All Ages')
df4 <- subset(df3, measure_name %in% 'Deaths')

causes <- c('Lower respiratory infections','Chronic obstructive pulmonary disease', 'Diabetes mellitus type 2',
            'Tracheal, bronchus, and lung cancer')

# ==========================================================================================================================================
# Start Loop
# ==========================================================================================================================================

  for (j in 1:length(cats)){
  
    for (k in 1:length(years)){
    years.x <- years[k]
        
      mrbrt <- read.csv(paste('/GWSPH/groups/anenberggrp/GBD_2019_June_2020/MRBRT/',cats[j],'.csv',sep=''))
      mrbrt <- mrbrt[,c(2,3)]
      colnames(mrbrt) <- c('rr','exposure_spline')
      print('mrbrt')
        
      # Read in the rates for year------------------------------------------------------------------------------------------------------------
      
      df5 <- subset(df4, year %in% years.x)
      df6 <- subset(df5, cause_name %in% causes[j])
      df7 <- df6 %>% distinct()
      
      df8 <- df7 %>% spread(metric_name, val)
      print('rates')
      
      hap1 <- subset(hap, year_id %in% years.x)
      hap1 <- subset(hap1, grouping %in% 'indoor')
      
      colnames(hap1)[5] <- 'mean.hap'
      
      hap.prop1 <- subset(hap.prop, year_id %in% years.x)
      
      #---------------------------------------------------------------------------------------------------------------------------------------
      # Step 2
      # Create an upper bound for the concentration. This way you can look up if a concentration (numeric) is within 
      # a concentration range (presented as an integer in the RR look-up tables)
      
      for (t in 1:nrow(mrbrt)){
        
        mrbrt$upper2[t] <- mrbrt$exposure_spline[t+1]
      }
      
      # Set the maximum value in the lookup table to something high
      mrbrt[is.na(mrbrt)] <- 2500000
      #---------------------------------------------------------------------------------------------------------------------------------------
      # Step 3 - assign RR estiamtes to concentrations

      conc1 <- conc
      conc1 <- subset(conc1, year %in% years.x)
      conc1 <- conc1[,c(2:15)]
      colnames(conc1)[10] <- 'parent_id'
      colnames(conc1)[6] <- 'location_id'
      colnames(hap1)[1] <- 'parent_id'
      colnames(hap.prop1)[1] <- 'parent_id'
      
      concs <- merge(conc1, hap1, by='parent_id',all=TRUE)
      
      concs2 <- merge(concs, hap.prop1, by='parent_id',all=TRUE)
      print('concs')
      #colnames(concs2)[18] <- 'mean.hap'
      
      # Sum concentrations OAP and HAP
      concs2$sum.pm <- concs2$popw + concs2$mean.hap     
      concs2$popw[concs2$popw < 0.01] <- NA
      concs2 <- concs2[complete.cases(concs2$popw), ]
      concs2 <- concs2[complete.cases(concs2$sum.pm), ]
      
      conc.ids <- as.vector(unique(concs2$location_id))
      
      # Set concentrations all equal to 
      concs2$WHO <- 10
      
      # Assign exposures based on OAP and OAP+HAP (pm.sum)
      for (x in 1:nrow(concs2)){
        if(concs2[x,4]>0){
          concs2[x,33] <- mrbrt[,1][concs2[x,4] > mrbrt[,2] & concs2[x,4] < mrbrt[,3]]
          concs2[x,34] <- mrbrt[,1][concs2[x,31] > mrbrt[,2] & concs2[x,31] < mrbrt[,3]]
        }else{
          concs2[x,33] <- 0
          concs2[x,34] <- 0
        }
      } 
      
      for (x in 1:nrow(concs2)){
        if(concs2[x,4] == 0){
          concs2[x,35] <- 0
        }
        if(concs2[x,4] < 10 & concs2[x,4] > 0){
          concs2[x,35] <- mrbrt[,1][concs2[x,4] > mrbrt[,2] & concs2[x,4] < mrbrt[,3]]
        }else{
          concs2[x,35] <- mrbrt[,1][mrbrt[,2] == 10.00]
        }
      } 
      
      colnames(concs2)[33] <- 'rr.oap'
      colnames(concs2)[34] <- 'rr.hap'
      colnames(concs2)[35] <- 'rr.who'
      
      colnames(df8)[2] <- 'parent_id'
      df9 <- merge(concs2, df8, by='parent_id',all=TRUE)
      # RR incorporating HAP
    
      
      # RR incorporating HAP
      df9$pm.rr.hap <- (df9$rr.oap*(1-df9$mean))+(df9$rr.hap*df9$mean)
      df9$paf.pm.hap <- (df9$pm.rr.hap-1)/df9$pm.rr.hap
      
      # RR based on only OAP
      df9$rr.nohap <- df9$rr.oap
      df9$paf.nohap <- (df9$rr.nohap-1)/df9$rr.nohap
      
      # RR based only on WHO
      df9$rr.who <- df9$rr.who
      df9$paf.who <- (df9$rr.who-1)/df9$rr.who
      
      # Estimate PAF - both using HAP and not using HAP
      df9$ac.hap <- df9$paf.pm.hap*df9$pop.sum*df9$Rate*10^-5 # using HAP
      df9$ac.nohap <- df9$paf.nohap*df9$pop.sum*df9$Rate*10^-5 # not using HAP
      df9$ac.who <- df9$paf.who*df9$pop.sum*df9$Rate*10^-5 # not using HAP
      
      df9<-df9[complete.cases(df9$popw), ]

    # Write out new raster
    fout_1 <- paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/gbd/',cats[j],years[k],'.csv',sep='')
    
    write.csv(df9, fout_1)
    
    print(fout_1)
    #---------------------------------------------------------------------------------------------------------------------------------------
  }
}
# ==========================================================================================================================================
# End
# ==========================================================================================================================================

# Combine all results for all years into one dataframe
setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/gbd/')

filenames <- list.files(path='/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/gbd/',pattern="*.csv", full.names=TRUE)
dataset <- do.call("rbind",lapply(filenames,FUN=function(files){read.csv(files, header=TRUE, sep=",")}))
write.csv(dataset, "combined.city.paf.gbd.csv")
