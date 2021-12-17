# ==========================================================================================================================================
# Created: 2020-08-09
# Updated: 2020-10-25
# Combined all mortality endpoints
# Part 1 - GBD HIA
# This calculates burden at the city level using all concentrations
# ==========================================================================================================================================

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
years <- c(2000:2019)
#years <- c(2019)

conc <- read.csv(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/combined.hdc.exp.sum.lu.csv',sep=''))
print('conc')

rates <- read.csv(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/allages.ihme.final.csv',sep=''))


hap <- read.csv(paste('/GWSPH/groups/anenberggrp/GBD_2019_June_2020/exposure_summaries/hap_pm2.5_exposure_nat&subnat.csv',sep=''))
hap.prop <- read.csv(paste('/GWSPH/groups/anenberggrp/GBD_2019_June_2020/exposure_summaries/hap_prop_exposure_nat&subnat.csv',sep=''))

ages1 <- read.csv(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/age.fractions.csv',sep=''))

ages2 <- ages1 %>% gather(year,pop,"pop.2000":"pop.2019")

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
ages2$year[ages2$year == 'pop.2019'] <- 2019

ages2 <- ages2[,c(2,3,6,7)]

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
      mrbrt <- read.csv(paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/mrbrt/',cats[j],age.groups[i],'_rr2.csv',sep=''))
      mrbrt <- mrbrt[,c(2:5)]
      colnames(mrbrt) <- c('rr','upper','lower', 'exposure_spline')
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
          
          concs2[x,36] <- mrbrt[,1][concs2[x,31] > mrbrt[,4] & concs2[x,31] < mrbrt[,5]]
          concs2[x,37] <- mrbrt[,2][concs2[x,31] > mrbrt[,4] & concs2[x,31] < mrbrt[,5]]
          concs2[x,38] <- mrbrt[,3][concs2[x,31] > mrbrt[,4] & concs2[x,31] < mrbrt[,5]]
          
        }else{
          concs2[x,33] <- 0
          concs2[x,34] <- 0
          concs2[x,35] <- 0
          concs2[x,36] <- 0
          concs2[x,37] <- 0
          concs2[x,38] <- 0
        }
        
        
        if(concs2[x,4] < 10 & concs2[x,4] > 0){
          concs2[x,39] <- mrbrt[,1][concs2[x,4] > mrbrt[,4] & concs2[x,4] < mrbrt[,5]]
          concs2[x,40] <- mrbrt[,2][concs2[x,4] > mrbrt[,4] & concs2[x,4] < mrbrt[,5]]
          concs2[x,41] <- mrbrt[,3][concs2[x,4] > mrbrt[,4] & concs2[x,4] < mrbrt[,5]]
          
          concs2[x,42] <- mrbrt[,1][concs2[x,31] > mrbrt[,4] & concs2[x,31] < mrbrt[,5]]
          concs2[x,43] <- mrbrt[,2][concs2[x,31] > mrbrt[,4] & concs2[x,31] < mrbrt[,5]]
          concs2[x,44] <- mrbrt[,3][concs2[x,31] > mrbrt[,4] & concs2[x,31] < mrbrt[,5]]
          
        }else{
          concs2[x,39] <- mrbrt[,1][mrbrt[,4] == 10.00]
          concs2[x,40] <- mrbrt[,2][mrbrt[,4] == 10.00]
          concs2[x,41] <- mrbrt[,3][mrbrt[,4] == 10.00]
          
          concs2[x,42] <- mrbrt[,1][mrbrt[,4] == 10.00]
          concs2[x,43] <- mrbrt[,2][mrbrt[,4] == 10.00]
          concs2[x,44] <- mrbrt[,3][mrbrt[,4] == 10.00]
        }
      } 
      
      colnames(concs2)[33] <- 'rr_oap_point'
      colnames(concs2)[34] <- 'rr_oap_upper'
      colnames(concs2)[35] <- 'rr_oap_lower'
      
      colnames(concs2)[36] <- 'rr_hap_point'
      colnames(concs2)[37] <- 'rr_hap_upper'
      colnames(concs2)[38] <- 'rr_hap_lower'
      
      colnames(concs2)[39] <- 'rr_oap_who_point'
      colnames(concs2)[40] <- 'rr_oap_who_upper'
      colnames(concs2)[41] <- 'rr_oap_who_lower'
      
      colnames(concs2)[42] <- 'rr_hap_who_point'
      colnames(concs2)[43] <- 'rr_hap_who_upper'
      colnames(concs2)[44] <- 'rr_hap_who_lower'
      
      colnames(df8)[2] <- 'parent_id'
      df9 <- merge(concs2, df8, by='parent_id',all=TRUE)
      
      df9 <- df9[complete.cases(df9$popw), ]
      df9 <- df9[complete.cases(df9$sum.pm), ]
      
      # Point
      df9$rr_pm_point <- df9$rr_hap_point*df9$mean+df9$rr_oap_point*(1-df9$mean) # mean == hap.prop
      df9$rr_hap_point <- (df9$rr_hap_point-1) - (df9$rr_oap_point-1) + 1
      
      df9$pop_average_pm <- df9$popw+df9$mean*df9$mean.hap              #population average exposure (denominator of proportion for splitting pafs)
      df9$hap_paf_ratio <- (df9$mean)*(df9$mean.hap)/df9$pop_average_pm #proportion of paf attributable to hap
      df9$ambient_paf_ratio <- df9$popw/df9$pop_average_pm              #proportion of paf attributable to ambient
      
      df9$paf_pm_point <- (df9$rr_pm_point-1)/df9$rr_pm_point           #calculate PM paf based on weighted PM RR
      df9$paf_hap_point <- df9$paf_pm_point*df9$hap_paf_ratio           #proprotionally split PM paf to Hap and ambient
      df9$paf_oap_point <- df9$paf_pm_point*df9$ambient_paf_ratio 
      
      # Lower
      df9$rr_pm_lower <- df9$rr_hap_lower*df9$mean+df9$rr_oap_lower*(1-df9$mean) # mean == hap.prop
      df9$rr_hap_lower <- (df9$rr_hap_lower-1) - (df9$rr_oap_lower-1) + 1
      
      df9$paf_pm_lower <- (df9$rr_pm_lower-1)/df9$rr_pm_lower           #calculate PM paf based on weighted PM RR
      df9$paf_hap_lower <- df9$paf_pm_lower*df9$hap_paf_ratio           #proprotionally split PM paf to Hap and ambient
      df9$paf_oap_lower <- df9$paf_pm_lower*df9$ambient_paf_ratio 
      
      # upper
      df9$rr_pm_upper <- df9$rr_hap_upper*df9$mean+df9$rr_oap_upper*(1-df9$mean) # mean == hap.prop
      df9$rr_hap_upper <- (df9$rr_hap_upper-1) - (df9$rr_oap_upper-1) + 1           #proportion of paf attributable to ambient
      
      df9$paf_pm_upper <- (df9$rr_pm_upper-1)/df9$rr_pm_upper          #calculate PM paf based on weighted PM RR
      df9$paf_hap_upper <- df9$paf_pm_upper*df9$hap_paf_ratio           #proprotionally split PM paf to Hap and ambient
      df9$paf_oap_upper <- df9$paf_pm_upper*df9$ambient_paf_ratio 
      
      # Point - WHO
      df9$rr_pm_who_point <- df9$rr_hap_who_point*df9$mean+df9$rr_oap_who_point*(1-df9$mean) # mean == hap.prop
      df9$rr_hap_who_point <- (df9$rr_hap_who_point-1) - (df9$rr_oap_who_point-1) + 1
      
      df9$pop_average_pm_who <- df9$WHO*df9$mean.hap           #population average exposure (denominator of proportion for splitting pafs)
      df9$hap_paf_ratio_who <- (df9$WHO)*(df9$mean.hap)/df9$pop_average_pm #proportion of paf attributable to hap
      df9$ambient_paf_ratio_who <- df9$WHO/df9$pop_average_pm              #proportion of paf attributable to ambient
      
      df9$paf_pm_who_point <- (df9$rr_pm_who_point-1)/df9$rr_pm_who_point           #calculate PM paf based on weighted PM RR
      df9$paf_hap_who_point <- df9$paf_pm_who_point*df9$hap_paf_ratio_who          #proprotionally split PM paf to Hap and ambient
      df9$paf_oap_who_point <- df9$paf_pm_who_point*df9$ambient_paf_ratio_who 
      
      # Lower
      df9$rr_pm_who_lower <- df9$rr_hap_who_lower*df9$mean+df9$rr_oap_who_lower*(1-df9$mean) # mean == hap.prop
      df9$rr_hap_who_lower <- (df9$rr_hap_who_lower-1) - (df9$rr_oap_who_lower-1) + 1
      
      df9$paf_pm_who_lower <- (df9$rr_pm_who_lower-1)/df9$rr_pm_who_lower          #calculate PM paf based on weighted PM RR
      df9$paf_hap_who_lower <- df9$paf_pm_who_lower*df9$hap_paf_ratio_who          #proprotionally split PM paf to Hap and ambient
      df9$paf_oap_who_lower <- df9$paf_pm_who_lower*df9$ambient_paf_ratio_who 
      
      # upper
      df9$rr_pm_who_upper <- df9$rr_hap_who_upper*df9$mean+df9$rr_oap_who_upper*(1-df9$mean) # mean == hap.prop
      df9$rr_hap_who_upper <- (df9$rr_hap_who_upper-1) - (df9$rr_oap_who_upper-1) + 1
      
      df9$paf_pm_who_upper <- (df9$rr_pm_who_upper-1)/df9$rr_pm_who_upper           #calculate PM paf based on weighted PM RR
      df9$paf_hap_who_upper <- df9$paf_pm_who_upper*df9$hap_paf_ratio_who          #proprotionally split PM paf to Hap and ambient
      df9$paf_oap_who_upper <- df9$paf_pm_who_upper*df9$ambient_paf_ratio_who 
      
      df9$pop.frac <- df9$pop/df9$population

      # Estimate PAF - both using HAP and not using HAP
      df9$ac.point <- df9$paf_oap_point*(df9$pop.sum*df9$pop.frac)*df9$val*10^-5 
      df9$ac.upper <- df9$paf_oap_upper*(df9$pop.sum*df9$pop.frac)*df9$val*10^-5 
      df9$ac.lower <- df9$paf_oap_lower*(df9$pop.sum*df9$pop.frac)*df9$val*10^-5 
      
      df9$ac.who.point <- df9$paf_oap_who_point*(df9$pop.sum*df9$pop.frac)*df9$val*10^-5 
      df9$ac.who.upper <- df9$paf_oap_who_upper*(df9$pop.sum*df9$pop.frac)*df9$val*10^-5 
      df9$ac.who.lower <- df9$paf_oap_who_lower*(df9$pop.sum*df9$pop.frac)*df9$val*10^-5 
      
      fout_2 <- paste('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/cvd/',causes[j],years[k],age.groups[i],'.csv',sep='')
      print(fout_2)
      write.csv(df9, fout_2)
      
    }    
  }
}

# ==========================================================================================================================================
# End
# ==========================================================================================================================================
# Combine all results for all years into one dataframe
setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/cvd/')

filenames <- list.files(path='/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/cvd/',pattern="*.csv", full.names=TRUE)
dataset <- do.call("rbind",lapply(filenames,FUN=function(files){read.csv(files, header=TRUE, sep=",")}))
write.csv(dataset, "combined.city.paf.cvd.stroke.csv")


