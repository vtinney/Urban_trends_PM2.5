# ==========================================================================================================================================
# Created: 2021-06-13
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
years <- c(2000:2019)
#years <- c(2019)

cats <- c('lri_rr','resp_copd_rr','t2_dm_rr','neo_lung_rr')

conc <- read.csv(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/combined.hdc.exp.sum.lu.csv',sep=''))
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
        
      mrbrt <- read.csv(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/mrbrt/',cats[j],'2.csv',sep=''))
      mrbrt <- mrbrt[,c(2:5)]
      colnames(mrbrt) <- c('rr','upper','lower', 'exposure_spline')
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
      colnames(conc1)[9] <- 'parent_id'
      colnames(conc1)[5] <- 'location_id'
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
      
      # Assign exposures based on pm
      for (x in 1:nrow(concs2)){
         if(concs2[x,4]>0){
         concs2[x,33] <- mrbrt[,1][concs2[x,4] > mrbrt[,4] & concs2[x,4] < mrbrt[,5]]
         concs2[x,34] <- mrbrt[,2][concs2[x,4] > mrbrt[,4] & concs2[x,4] < mrbrt[,5]]
         concs2[x,35] <- mrbrt[,3][concs2[x,4] > mrbrt[,4] & concs2[x,4] < mrbrt[,5]]
         }else{
           concs2[x,33] <- 0
           concs2[x,34] <- 0
           concs2[x,35] <- 0
         }
       
      
        if(concs2[x,4] < 10 & concs2[x,4] > 0){
          concs2[x,36] <- mrbrt[,1][concs2[x,4] > mrbrt[,4] & concs2[x,4] < mrbrt[,5]]
          concs2[x,37] <- mrbrt[,2][concs2[x,4] > mrbrt[,4] & concs2[x,4] < mrbrt[,5]]
          concs2[x,38] <- mrbrt[,3][concs2[x,4] > mrbrt[,4] & concs2[x,4] < mrbrt[,5]]
        }else{
          concs2[x,36] <- mrbrt[,1][mrbrt[,4] == 10.00]
          concs2[x,37] <- mrbrt[,2][mrbrt[,4] == 10.00]
          concs2[x,38] <- mrbrt[,3][mrbrt[,4] == 10.00]
        }
      } 
      
      colnames(concs2)[33] <- 'rr.point'
      colnames(concs2)[34] <- 'rr.upper'
      colnames(concs2)[35] <- 'rr.lower'
      
      colnames(concs2)[36] <- 'rr.who.point'
      colnames(concs2)[37] <- 'rr.who.upper'
      colnames(concs2)[38] <- 'rr.who.lower'
      
      colnames(df8)[2] <- 'parent_id'
      df9 <- merge(concs2, df8, by='parent_id',all=TRUE)

      # RR based on only OAP
      df9$paf.point <- (df9$rr.point-1)/df9$rr.point
      df9$paf.upper <- (df9$rr.upper-1)/df9$rr.upper
      df9$paf.lower <- (df9$rr.lower-1)/df9$rr.lower
      
      # RR based only on WHO
      df9$paf.who.point <- (df9$rr.who.point-1)/df9$rr.who.point
      df9$paf.who.upper <- (df9$rr.who.upper-1)/df9$rr.who.upper
      df9$paf.who.lower <- (df9$rr.who.lower-1)/df9$rr.who.lower
      
      # Estimate PAF - both using HAP and not using HAP
      df9$ac.point <- df9$paf.point*df9$pop.sum*df9$Rate*10^-5 
      df9$ac.upper <- df9$paf.upper*df9$pop.sum*df9$Rate*10^-5 
      df9$ac.lower <- df9$paf.lower*df9$pop.sum*df9$Rate*10^-5 
      
      df9$ac.who.point <- df9$paf.who.point*df9$pop.sum*df9$Rate*10^-5 
      df9$ac.who.upper <- df9$paf.who.upper*df9$pop.sum*df9$Rate*10^-5 
      df9$ac.who.lower <- df9$paf.who.lower*df9$pop.sum*df9$Rate*10^-5 
      
      df9<-df9[complete.cases(df9$popw), ]

    # Write out new raster
    fout_1 <- paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/',cats[j],years[k],'.csv',sep='')
    
    write.csv(df9, fout_1)
    
    print(fout_1)
    #---------------------------------------------------------------------------------------------------------------------------------------
  }
}


# ==========================================================================================================================================
# End
# ==========================================================================================================================================

# Combine all results for all years into one dataframe
setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/')

filenames <- list.files(path='/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/',pattern="*.csv", full.names=TRUE)
dataset <- do.call("rbind",lapply(filenames,FUN=function(files){read.csv(files, header=TRUE, sep=",")}))
write.csv(dataset, "combined.city.paf.csv")
