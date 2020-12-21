library(tidyverse)

setwd('E:/GBD_2020/Final/results/')

popw <- read.csv('popw.long.city.csv')
rate <- read.csv('rate.long.city.csv')
rates <- read.csv('rates.country.long.city.csv')
pop <- rates

popw <- popw[,c(2:5,23,24)]
names(popw) <- c('id','city','WHORegion','popw.2000','popw.2018','popw.pc')

rate <- rate[,c(2:5,23,24)]
names(rate) <- c('id','city','WHORegion','rate.2000','rate.2018','rate.pc')

rates <- rates[,c(3,5,8,14,19)]
rates <- rates %>% spread(year,avgrate)
rates <- rates[,c(1:4,22)]
rates$pc <- round((rates$`2018`-rates$`2000`)/rates$`2000`*100,0)
names(rates) <- c('id','city','WHORegion','rates.2000','rates.2018','rates.pc')

df <- merge(popw,rate,by=c('id','city','WHORegion'))
df2 <- merge(df, rates, by=c('id','city'))

df2 <- df2[,c(1,2,4:13)]
df2 <- df2[,c(1,2,9,3:8,10:12)]
colnames(df2)[3] <- 'WHORegion'

paf <-read.csv('allcauses.city.results.paf.csv')
paf <- paf[,c(2,3,17)]
paf <- paf %>% spread(year,paf.pm.hap)
paf$pc <- round((paf$`2018`-paf$`2000`)/paf$`2000`*100,0)
paf <- paf[,c(1,2,20,21)]
names(paf) <- c('id','paf.2000','paf.2018','paf.pc')

pop <- rates[,c(3,5,6)]
pop <- pop %>% spread(year,pop.sum)
pop <- pop[,c(1:2,20)]
pop$pc <- round((pop$`2018`-pop$`2000`)/pop$`2000`*100,0)
names(pop) <- c('id','pop.2000','pop.2018','pop.pc')

ac <- rates[,c(3,5,16)]
ac <- ac %>% spread(year,ac.nohap)
ac <- ac[,c(1,2,20)]
ac$pc <- round((ac$`2018`-ac$`2000`)/ac$`2000`*100,0)
names(ac) <- c('id','ac.2000','ac.2018','ac.pc')

df3 <- merge(df2, paf, by='id')
df3$rates.2000 <- df3$rates.2000*10^-5
df3$rates.2018 <- df3$rates.2018*10^-5

df4 <- merge(df3,pop,by='id')
df5 <- merge(df4,ac,by='id')
write.csv(df5,'merged.paf.csv')

m <- read.csv('merged.paf.subet.csv')
m2 <- m[,c(1:5,8,9,22,23,24,42)]
m3 <- m2[complete.cases(m2), ]
write.csv(m3,'merged.paf.subset.short.csv')

m4 <- m3 %>% 
  group_by(WHORegion) %>%
  summarize(popw.2000 = mean(popw.2000, na.rm = T),
            popw.2018 = mean(popw.2018, na.rm = T),
            rates.2000 = mean(rates.2000, na.rm = T),
            rates.2018 = mean(rates.2018, na.rm = T),
            acrate.2000 = mean(acrate.2000, na.rm = T),
            acrate.2018 = mean(acrate.2018, na.rm = T),
            popw.pc = mean(popw.pc, na.rm = T),
            PERCENT.DECREASE = mean(PERCENT.DECREASE, na.rm = T))
write.csv(m4, 'merged.paf.subset.short.REGION.csv')
