library(tidyverse)

setwd('E:/GBD_2020/Final/results/')
hr.popw <- read.csv('avg.pm.region.long.csv')
hr.popw <- hr.popw[,c(2,3,21,22)]
colnames(hr.popw) <- c('WHORegion','hr.popw.2000','hr.popw.2018','hr.popw.pc')

hr.rate <- read.csv('avg.acrate.region.long.csv')
hr.rate <- hr.rate[,c(2,3,21,22)]
colnames(hr.rate) <- c('WHORegion','hr.rate.2000','hr.rate.2018','hr.rate.pc')

setwd('E:/GBD_2020/Final/results/gbd/')
gbd.popw <- read.csv('avg.pm.region.long.gbd.csv')
gbd.popw <- gbd.popw[,c(2,3,21,22)]
colnames(gbd.popw) <- c('WHORegion','gbd.popw.2000','gbd.popw.2018','gbd.popw.pc')

gbd.rate <- read.csv('avg.acrate.region.long.gbd.csv')
gbd.rate <- gbd.rate[,c(2,3,21,22)]
colnames(gbd.rate) <- c('WHORegion','gbd.rate.2000','gbd.rate.2018','gbd.rate.pc')

df <- merge(hr.popw, hr.rate, by='WHORegion')
df2 <- merge(df, gbd.popw, by='WHORegion')
df3 <- merge(df2, gbd.rate, by='WHORegion')

write.csv(df3, 'compare.hr.gbd.csv')

