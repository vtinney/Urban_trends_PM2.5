library(tidyverse)

# setwd('D:/GBD_2020/Final/results/')
# hr.popw <- read.csv('avg.pm.region.long.csv')
# 
# hr.popw <- hr.popw[,c(2,22:24)]
# colnames(hr.popw) <- c('WHORegion','hr.popw.2000','hr.popw.2018','hr.popw.pc')
# 
# hr.rate <- read.csv('avg.acrate.region.long.csv')
# hr.rate <- hr.rate[,c(2,22:24)]
# colnames(hr.rate) <- c('WHORegion','hr.rate.2000','hr.rate.2018','hr.rate.pc')
# 
# setwd('F:/GBD_2020/Final/results/gbd/')
# gbd.popw <- read.csv('avg.pm.region.long.gbd.csv')
# gbd.popw <- gbd.popw[,c(2,3,10:13)]
# gbd.popw$mean.2018 <- (gbd.popw$X2016+gbd.popw$X2017+gbd.popw$X2018)/3
# gbd.popw$pc <- round((gbd.popw$mean.2018-gbd.popw$X2000)/gbd.popw$X2000*100,0)
# gbd.popw <- gbd.popw[,c(1,2,7,8)]
# colnames(gbd.popw) <- c('WHORegion','gbd.popw.2000','gbd.popw.2018','gbd.popw.pc')
# 
# gbd.rate <- read.csv('avg.acrate.region.long.gbd.csv')
# gbd.rate <- gbd.rate[,c(2,3,10:13)]
# gbd.rate$mean.2018 <- (gbd.rate$X2016+gbd.rate$X2017+gbd.rate$X2018)/3
# gbd.rate$pc <- round((gbd.rate$mean.2018-gbd.rate$X2000)/gbd.rate$X2000*100,0)
# gbd.rate <- gbd.rate[,c(1,2,7,8)]
# colnames(gbd.rate) <- c('WHORegion','gbd.rate.2000','gbd.rate.2018','gbd.rate.pc')
# 
# df <- merge(hr.popw, hr.rate, by='WHORegion')
# df2 <- merge(gbd.rate, gbd.popw, by='WHORegion')
# df3 <- merge(df2, df, by='WHORegion')
# 
# write.csv(df3, 'compare.hr.gbd.csv')
# 
# #============================================================================
# 
# setwd('F:/GBD_2020/Final/results/')
# hr.popw <- read.csv('avg.pm.region.long.csv')
# #2000,2010,2015,2018
# hr.popw <- hr.popw[,c(2,3,13,18,21)]
# colnames(hr.popw) <- c("WHORegion","hr.2000","hr.2010","hr.2015","hr.2018")
# 
# hr.rate <- read.csv('avg.acrate.region.long.csv')
# hr.rate <- hr.rate[,c(2,3,13,18,21)]
# colnames(hr.rate) <- c("WHORegion","hr.rate.2000","hr.rate.2010","hr.rate.2015","hr.rate.2018")
# 
# setwd('F:/GBD_2020/Final/results/gbd/')
# gbd.popw <- read.csv('avg.pm.region.long.gbd.csv')
# gbd.popw <- gbd.popw[,c(2,3,5,10,13)]
# colnames(gbd.popw) <- c("WHORegion","gbd.2000","gbd.2010","gbd.2015","gbd.2018")
# 
# gbd.rate <- read.csv('avg.acrate.region.long.gbd.csv')
# gbd.rate <- gbd.rate[,c(2,3,5,10,13)]
# colnames(gbd.rate) <- c("WHORegion","gbd.rate.2000","gbd.rate.2010","gbd.rate.2015","gbd.rate.2018")
# 
# df <- merge(hr.popw, hr.rate, by='WHORegion')
# df2 <- merge(gbd.rate, gbd.popw, by='WHORegion')
# df3 <- merge(df2, df, by='WHORegion')



#==========================================================================================
setwd('D:/GBD_2020/Final/results/')

all <- read.csv('allcauses.city.results.csv')

setwd('D:/GBD_2020/Final/results/gbd/')

gbd <- read.csv('allcauses.city.results.GBD.csv')

x <- unique(gbd$year)
all <- subset(all, year %in% x)

all <- all[,c(2,3,4,5,6,13,18)]
gbd <- gbd[,c(2,3,4,5,6,12,13)]

all$anal <- 'all'
gbd$anal <- 'gbd'

comp <- merge(all,gbd,by=c('year','id','WHORegion'))

comp$popxconc.hr <- comp$pop.sum.x*comp$popw.x
comp$popxconc.gbd <- comp$pop.sum.y*comp$popw.y

comp2 <- comp %>%
  dplyr::group_by(WHORegion,year) %>%
  dplyr::summarize(pop.sum.hr=sum(pop.sum.x ,na.rm=T),
                   popxconc.hr=sum(popxconc.hr,na.rm=T),
                   ac.nohap.hr=sum(ac.point,na.rm=T),
                   pop.sum.gbd=sum(pop.sum.y,na.rm=T),
                   popxconc.gbd=sum(popxconc.gbd,na.rm=T),
                   ac.nohap.gbd=sum( ac.nohap ,na.rm=T))

comp2 <- as.data.frame(comp2)

comp2$rate.hr <- (comp2$ac.nohap.hr*100000)/comp2$pop.sum.hr
comp2$rate.gbd <- (comp2$ac.nohap.gbd*100000)/comp2$pop.sum.gbd

comp2$popw.hr <- comp2$popxconc.hr/comp2$pop.sum.hr
comp2$popw.gbd <- comp2$popxconc.gbd/comp2$pop.sum.gbd

write.csv(comp2, 'compare.hr.gbd.region.csv')
