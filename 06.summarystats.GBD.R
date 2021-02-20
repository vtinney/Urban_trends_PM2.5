library(tidyverse)

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/gbd/df/')

all <- read.csv('allcauses.city.results.GBD.csv')

all$ac.nohap <- as.numeric(all$ac.nohap)

all$acrate.nohap <- (all$ac.nohap*100000)/all$pop.sum
all$acrate.who <- (all$ac.who*100000)/all$pop.sum

colnames(all)[4] <- 'popw'
colnames(all)[5] <- 'pop.sum'
# Global mean and SD popw PM
avg.pm <- all %>% 
  group_by(year) %>%
  summarize(popw = mean(popw, na.rm = T),
            sd = sd(popw, na.rm=T),
            min = min(popw, na.rm=T),
            max = max(popw, na.rm=T))
avg.pm <- as.data.frame(avg.pm)
write.csv(avg.pm, 'avg.pm.gbd.csv')
# Avg PM by region and year
avg.pm.region <- all %>% 
  group_by(WHORegion,year) %>%
  summarize(popw = mean(popw, na.rm = T),
            sd = sd(popw, na.rm=T),
            min = min(popw, na.rm=T),
            max = max(popw, na.rm=T))
avg.pm.region <- as.data.frame(avg.pm.region)
write.csv(avg.pm.region, 'avg.pm.region.gbd.csv')

# Mean popw per year and region plus percent change
avg.pm.region <- all %>% 
  group_by(WHORegion,year) %>%
  summarize(popw = mean(popw, na.rm = T))

avg.pm.region.spread <- as.data.frame(avg.pm.region)
avg.pm.region.long <- avg.pm.region.spread %>% spread(year, popw)
avg.pm.region.long$pc <- (avg.pm.region.long$`2018`-avg.pm.region.long$`2000`)/avg.pm.region.long$`2000`*100
write.csv(avg.pm.region.long, 'avg.pm.region.long.gbd.csv')

# Unique number of cities meeting the WHO recommendation
all.who <- subset(all, popw < 10)
x <- unique(all.who$city)

# #=====================================================================================
# Mean attributable mortality rate by year and region
avg.acrate.region <- all %>% 
  group_by(WHORegion,year) %>%
  summarize(acrate = mean(acrate.nohap, na.rm = T))


# Percent change in attributable mortality rate per region
avg.acrate.region.spread <- as.data.frame(avg.acrate.region)
avg.acrate.region.long <- avg.acrate.region.spread %>% spread(year, acrate)
avg.acrate.region.long$pc <- (avg.acrate.region.long$`2018`-avg.acrate.region.long$`2000`)/avg.acrate.region.long$`2000`*100

write.csv(avg.acrate.region.long, 'avg.acrate.region.long.gbd.csv')

all.spread <- all[,c(2,3,4,6,12)]
all.spread <- all.spread %>% spread(year, popw)
all.spread$pc <- (all.spread$`2018`-all.spread$`2000`)/all.spread$`2000`*100

all.dec <- subset(all.spread, pc < 0)
all.inc <- subset(all.spread, pc >=0)
  
# Total global cases by year
all.total <- all %>%
  group_by(year) %>%
  summarize(cases=sum(ac.nohap,na.rm=T),
            cases.who=sum(ac.who,na.rm=T))
all.total <- as.data.frame(all.total)
write.csv(all.total, 'all.total.gbd.csv')

# Regional attributbale mortality rate means
all.rate <- all[,c(2,3,6,12,16)]
all.rate$acrate.nohap <- as.numeric(all.rate$acrate.nohap)

all.rate <- all.rate %>%
  group_by(year,WHORegion) %>%
  summarize(rate = mean(acrate.nohap,na.rm=T))

all.rate <- as.data.frame(all.rate)
write.csv(all.rate, 'all.rate.csv')

# Global attributable mortality rate mean
all.rate.gbl <- all %>%
  group_by(year) %>%
  summarize(rate2 = mean(acrate.nohap,na.rm=T))
all.rate.gbl <- as.data.frame(all.rate.gbl)
write.csv(all.rate.gbl, 'all.rate.gbl.csv')
#==============================================================
# Concentration variation among all cities
mm.rate <- all %>%
  group_by(year) %>%
  summarize(min = min(acrate.nohap, na.rm=T),
            max = max(acrate.nohap, na.rm=T))
mm.rate <- as.data.frame(mm.rate)
write.csv(mm.rate, 'mm.rate.csv')