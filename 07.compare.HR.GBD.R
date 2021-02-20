library(tidyverse)

setwd('F:/GBD_2020/Final/results/')
hr.popw <- read.csv('avg.pm.region.long.csv')
hr.popw <- hr.popw[,c(2,3,21,22)]
colnames(hr.popw) <- c('WHORegion','hr.popw.2000','hr.popw.2018','hr.popw.pc')

hr.rate <- read.csv('avg.acrate.region.long.csv')
hr.rate <- hr.rate[,c(2,3,21,22)]
colnames(hr.rate) <- c('WHORegion','hr.rate.2000','hr.rate.2018','hr.rate.pc')

setwd('F:/GBD_2020/Final/results/gbd/')
gbd.popw <- read.csv('avg.pm.region.long.gbd.csv')
gbd.popw <- gbd.popw[,c(2,3,13,14)]
colnames(gbd.popw) <- c('WHORegion','gbd.popw.2000','gbd.popw.2018','gbd.popw.pc')

gbd.rate <- read.csv('avg.acrate.region.long.gbd.csv')
gbd.rate <- gbd.rate[,c(2,3,13,14)]
colnames(gbd.rate) <- c('WHORegion','gbd.rate.2000','gbd.rate.2018','gbd.rate.pc')

df <- merge(hr.popw, hr.rate, by='WHORegion')
df2 <- merge(df, gbd.popw, by='WHORegion')
df3 <- merge(df2, gbd.rate, by='WHORegion')

write.csv(df3, 'compare.hr.gbd.csv')

#============================================================================

setwd('F:/GBD_2020/Final/results/')
hr <- read.csv('allcauses.city.results.csv')
names(hr) <- c("X","year","id","pop.sum.hr","popw.hr",          
"city","cluster","location_id","GBDRegion","GBDSuperRegion", 
"SDGRegion","WHORegion","WHOIncomeRegion","parent_id","ac.nohap.hr",       
"ac.who.hr")

setwd('F:/GBD_2020/Final/results/gbd/')
gbd <- read.csv('allcauses.city.results.GBD.csv')

df <- merge(hr, gbd, by=c('year','id','city','cluster','location_id','GBDRegion','GBDSuperRegion',
                          'SDGRegion','WHORegion','WHOIncomeRegion'))

write.csv(df,'compare.GBD.HR.city.csv')

