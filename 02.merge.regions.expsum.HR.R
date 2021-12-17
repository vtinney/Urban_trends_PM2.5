library(tidyverse)
setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/')

lu_hdc <- read.csv('hdc_gbd_lookup.csv')
lu_hdc <- lu_hdc[,c(1,3,4,5,7,9:15)]
colnames(lu_hdc)[2] <- 'id'

hdc <- read.csv('combined.hdc.expsum.csv')

hdc <- hdc[,c(3,5:7)]
df <- merge(hdc, lu_hdc, by='id')

write.csv(df, 'combined.hdc.exp.sum.lu.csv')

