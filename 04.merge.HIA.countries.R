library(tidyverse)

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/')
lu <- read.csv('gbd2019_location_hierarchies.csv')
lu <- lu[,c(1,2)]

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/country_level/')

all <- read.csv('combined.country.paf.csv')
all <- all[,c(3,47,45,46,37,6,5)]
colnames(all) <- c("location_id","ac.who","ac.hap","ac.nohap","cause","year","location_name")

cvd <- read.csv('combined.country.paf.cvd.csv')
cvd <- cvd[,c(3:8)]
colnames(cvd)[1] <- 'location_id'
cvd.df <- merge(cvd, lu, by='location_id')

df <- rbind(all, cvd.df)

all.deaths <- df %>% 
  group_by(year) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm = T),
            ac.who = sum(ac.who, na.rm = T),
            ac.hap = sum(ac.hap, na.rm=T))
all.deaths <- as.data.frame(all.deaths)


all.lc <- df %>% 
  group_by(year,cause) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm = T),
            ac.who = sum(ac.who, na.rm = T),
            ac.hap = sum(ac.hap, na.rm=T))
all.lc <- as.data.frame(all.lc)

all.lc.2 <- subset(all.lc, cause %in% 'Tracheal, bronchus, and lung cancer')
all.copd <- subset(all.lc, cause %in% 'Chronic obstructive pulmonary disease')
all.t2 <- subset(all.lc, cause %in% 'Diabetes mellitus type 2')
all.lri <- subset(all.lc, cause %in% 'Lower respiratory infections')

all.cvd <- subset(all.lc, cause %in% 'cvd_ihd_')
all.stroke <- subset(all.lc, cause %in% 'cvd_stroke_')