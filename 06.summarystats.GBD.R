library(tidyverse)

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/gbd/df/')

all <- read.csv('allcauses.city.results.GBD.csv')

colnames(all)[4] <- 'popw'
colnames(all)[5] <- 'pop.sum'
# Global mean and SD popw PM

all$product <- all$popw*all$pop.sum

all.popw <- all %>%
  group_by(year) %>%
  summarize(product = sum(product,na.rm=T),
            pop = sum(pop.sum, na.rm=T))

all.popw <- as.data.frame(all.popw)
all.popw$popw <- all.popw$product/all.popw$pop

write.csv(all.popw, 'avg.pm.gbd.csv')

# Avg PM by region and year
avg.pm <- all %>% 
  group_by(year,WHORegion) %>%
  summarize(sum.region = sum(product, na.rm = T),
            sum.pop =sum(pop.sum, na.rm=T))
avg.pm <- as.data.frame(avg.pm)

avg.pm$popw <- avg.pm$sum.region/avg.pm$sum.pop

avg.pm.region.spread <- as.data.frame(avg.pm)
avg.pm.region.spread <- avg.pm.region.spread[,c(1,2,5)]
avg.pm.region.long <- avg.pm.region.spread %>% spread(year, popw)

avg.pm.region.long$pc <- round((avg.pm.region.long$`2018`-avg.pm.region.long$`2000`)/avg.pm.region.long$`2000`*100,0)

write.csv(avg.pm.region.long, 'avg.pm.region.long.gbd.csv')


# #=====================================================================================
# Mean attributable mortality rate by year and region
avg.acrate.region <- all %>% 
  group_by(WHORegion,year) %>%
  summarize(ac = sum(ac.nohap, na.rm = T),
            pop=sum(pop.sum, na.rm=T))
avg.acrate.region <- as.data.frame(avg.acrate.region)
avg.acrate.region$acrate <- (avg.acrate.region$ac*100000)/avg.acrate.region$pop

avg.acrate.region <- avg.acrate.region[,c(1,2,5)]
# Percent change in attributable mortality rate per region
avg.acrate.region.spread <- as.data.frame(avg.acrate.region)
avg.acrate.region.long <- avg.acrate.region.spread %>% spread(year, acrate)

avg.acrate.region.long$pc <- round((avg.acrate.region.long$`2018`-avg.acrate.region.long$`2000`)/avg.acrate.region.long$`2000`*100,0)

write.csv(avg.acrate.region.long, 'avg.acrate.region.long.gbd.csv')
