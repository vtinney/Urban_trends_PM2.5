# ==========================================================================================================================================
# Created: 2020-08-09
# Updated: 2020-10-25
# Combined all mortality endpoints
# Part 1 - GBD HIA
# This calculates burden at the city level using all concentrations
# ==========================================================================================================================================

start_time <- Sys.time()

library(raster)
library(rgdal)
library(plyr)
library(dplyr)
library(reshape2)
library(safejoin)
library(tidyverse)

#Specify age groups for population
age.groups <- c(25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)
ages <- c("25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64",
          "65 to 69","70 to 74","75 to 79","80 to 84","85 to 89","90 to 94","90 to 94")

#Specify age group years for rates
years <- c(2000,2005,2010:2018)

conc <- read.csv(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/combined.hdc.GBD.popw.1km.csv',sep=''))
print('conc')

rates <- read.csv(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/allages.ihme.final.csv',sep=''))


hap <- read.csv(paste('/GWSPH/groups/anenberggrp/GBD_2019_June_2020/exposure_summaries/hap_pm2.5_exposure_nat&subnat.csv',sep=''))
hap.prop <- read.csv(paste('/GWSPH/groups/anenberggrp/GBD_2019_June_2020/exposure_summaries/hap_prop_exposure_nat&subnat.csv',sep=''))

ages1 <- read.csv(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/age.fractions.csv',sep=''))

ages2 <- ages1 %>% gather(year,pop,"pop.2000":"pop.2018")

ages2$year[ages2$year == 'pop.2000'] <- 2000
ages2$year[ages2$year == 'pop.2001'] <- 2001
ages2$year[ages2$year == 'pop.2002'] <- 2002
ages2$year[ages2$year == 'pop.2003'] <- 2003
ages2$year[ages2$year == 'pop.2004'] <- 2004
ages2$year[ages2$year == 'pop.2005'] <- 2005
ages2$year[ages2$year == 'pop.2006'] <- 2006
ages2$year[ages2$year == 'pop.2007'] <- 2007
ages2$year[ages2$year == 'pop.2008'] <- 2008
ages2$year[ages2$year == 'pop.2009'] <- 2009
ages2$year[ages2$year == 'pop.2010'] <- 2010
ages2$year[ages2$year == 'pop.2011'] <- 2011
ages2$year[ages2$year == 'pop.2012'] <- 2012
ages2$year[ages2$year == 'pop.2013'] <- 2013
ages2$year[ages2$year == 'pop.2014'] <- 2014
ages2$year[ages2$year == 'pop.2015'] <- 2015
ages2$year[ages2$year == 'pop.2016'] <- 2016
ages2$year[ages2$year == 'pop.2017'] <- 2017
ages2$year[ages2$year == 'pop.2018'] <- 2018

ages2 <- ages2[,c(2,3,5,6)]

ihme2 <- rates[,c(3,4,5,9,11,13,14,15)]


cats <- c('cvd_stroke_','cvd_stroke_','cvd_ihd_')

df2 <- subset(ihme2, cause_name %in% c('Ischemic heart disease','Ischemic stroke','Intracerebral hemorrhage'))
df3 <- subset(df2, measure_name %in% 'Deaths')

ihme3 <- merge(df3, ages2, by=c('location_id','year','age_name'),all.x=TRUE)

causes <- c('Ischemic stroke','Intracerebral hemorrhage','Ischemic heart disease')

# ==========================================================================================================================================
# Start Loop
# ==========================================================================================================================================
for (j in 1:length(cats)){
  
  for (k in 1:length(years)){
    
    
    for (i in 1:length(ages)){
      years.x <- years[k]
      # Read in the relative -----------------------------------------------------------------------------------------------------------------
      mrbrt <- read.csv(paste('/GWSPH/groups/anenberggrp/GBD_2019_June_2020/MRBRT/',cats[j],age.groups[i],'_rr.csv',sep=''))
      mrbrt <- mrbrt[,c(2,3)]
      colnames(mrbrt) <- c('rr','exposure_spline')
      print('mrbrt')

      # Subset the rates for year------------------------------------------------------------------------------------------------------------

      df5 <- subset(ihme3, year %in% years.x)
      df6 <- subset(df5, cause_name %in% causes[j])
      df7 <- df6 %>% distinct()
      df8 <- subset(df7, age_name %in% ages[i])
      
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
        if(concs2[x,3] < 10 & concs2[x,3] > 0){
          concs2[x,35] <- mrbrt[,1][concs2[x,4] > mrbrt[,2] & concs2[x,4] < mrbrt[,3]]
        }else{
          concs2[x,35] <- mrbrt[,1][mrbrt[,2] == 10.00]
        }
      } 
      
      colnames(concs2)[33] <- 'rr.oap'
      colnames(concs2)[34] <- 'rr.hap'
      colnames(concs2)[35] <- 'rr.who'
      
      colnames(df8)[1] <- 'parent_id'
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
      
      df9$pop.frac <- df9$pop/df9$population
      
      # Estimate PAF - both using HAP and not using HAP
      df9$ac.hap <- df9$paf.pm.hap*(df9$pop.sum*df9$pop.frac)*df9$val*10^-5 # using HAP
      df9$ac.nohap <- df9$paf.nohap*(df9$pop.sum*df9$pop.frac)*df9$val*10^-5 # not using HAP
      df9$ac.who <- df9$paf.who*(df9$pop.sum*df9$pop.frac)*df9$val*10^-5 # not using HAP
      
      df9<-df9[complete.cases(df9$popw), ]
      
      fout_2 <- paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/gbd/cvd/',cats[j],years[k],age.groups[i],'.csv',sep='')
      
      write.csv(df9, fout_2)
      
    }    
  }
}

# ==========================================================================================================================================
# End
# ==========================================================================================================================================
# Combine all results for all years into one dataframe
setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/gbd/cvd/')

filenames <- list.files(path='/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/gbd/cvd/',pattern="*.csv", full.names=TRUE)
dataset <- do.call("rbind",lapply(filenames,FUN=function(files){read.csv(files, header=TRUE, sep=",")}))
write.csv(dataset, "combined.city.paf.cvd.stroke.gbd.csv")


