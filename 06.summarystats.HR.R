library(tidyverse)

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/')

popw <- read.csv('combined.hdc.exp.sum.lu.csv')
#==============================================================================
# Total population in cities by year
#==============================================================================
all.popw <- popw %>%
  group_by(year) %>%
  summarize(pop.sum = sum(pop.sum,na.rm=T))

all.popw <- as.data.frame(all.popw)


setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/df/')
write.csv(all.popw, 'all.popw.csv')

all <- read.csv('allcauses.city.results.csv')

all$acrate.nohap <- (all$ac.nohap*100000)/all$pop.sum
all$acrate.who <- (all$ac.who*100000)/all$pop.sum

#==============================================================================
# Global mean and SD popw PM
#==============================================================================
avg.pm <- all %>% 
  group_by(year) %>%
  summarize(popw = mean(popw, na.rm = T),
            sd = sd(popw, na.rm=T),
            min = min(popw, na.rm=T),
            max = max(popw, na.rm=T))
avg.pm <- as.data.frame(avg.pm)
write.csv(avg.pm, 'avg.pm.csv')
#==============================================================================
# Avg PM by region and year
#==============================================================================
avg.pm.region <- all %>% 
  group_by(WHORegion,year) %>%
  summarize(popw = mean(popw, na.rm = T),
            sd = sd(popw, na.rm=T),
            min = min(popw, na.rm=T),
            max = max(popw, na.rm=T))
avg.pm.region <- as.data.frame(avg.pm.region)
write.csv(avg.pm.region, 'avg.pm.region.csv')

#==============================================================================
# Mean popw per year and region plus percent change
#==============================================================================
avg.pm.region <- all %>% 
  group_by(WHORegion,year) %>%
  summarize(popw = mean(popw, na.rm = T))

avg.pm.region.spread <- as.data.frame(avg.pm.region)
avg.pm.region.long <- avg.pm.region.spread %>% spread(year, popw)
avg.pm.region.long$pc <- (avg.pm.region.long$`2018`-avg.pm.region.long$`2000`)/avg.pm.region.long$`2000`*100
write.csv(avg.pm.region.long, 'avg.pm.region.long.csv')

#==============================================================================
# Unique number of cities meeting the WHO recommendation
#==============================================================================
all.who <- subset(df, popw < 10)
x <- unique(all.who$city)

# #=====================================================================================
# Mean attributable mortality rate by year and region
#==============================================================================
avg.acrate.region <- all %>% 
  group_by(WHORegion,year) %>%
  summarize(acrate = mean(acrate.nohap, na.rm = T))

#==============================================================================
# Percent change in attributable mortality rate per region
#==============================================================================
avg.acrate.region.spread <- as.data.frame(avg.acrate.region)
avg.acrate.region.long <- avg.acrate.region.spread %>% spread(year, acrate)
avg.acrate.region.long$pc <- (avg.acrate.region.long$`2018`-avg.acrate.region.long$`2000`)/avg.acrate.region.long$`2000`*100

write.csv(avg.acrate.region.long, 'avg.acrate.region.long.csv')

#==============================================================================
# all changes mortality (inc/dec)
#==============================================================================
all.spread <- df[,c(2,3,15)]
all.spread <- all.spread %>% spread(year, ac.nohap)
all.spread$pc <- (all.spread$`2018`-all.spread$`2000`)/all.spread$`2000`*100

all.dec <- subset(all.spread, pc < 0)
all.inc <- subset(all.spread, pc >=0)
  
#==============================================================================
# Total global cases by year
#==============================================================================
all.total <- all %>%
  group_by(year) %>%
  summarize(cases=sum(ac.nohap,na.rm=T),
            cases.who=sum(ac.who,na.rm=T))
all.total <- as.data.frame(all.total)
write.csv(all.total, 'all.total.csv')

#==============================================================================
# Regional attributbale mortality rate means
#==============================================================================
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
#==============================================================================
# Concentration variation among all 
#==============================================================================
mm.rate <- all %>%
  group_by(year) %>%
  summarize(min = min(acrate.nohap, na.rm=T),
            max = max(acrate.nohap, na.rm=T))
mm.rate <- as.data.frame(mm.rate)
write.csv(mm.rate, 'mm.rate.csv')

#==============================================================================
#Long percent change each city for PM and rates
#==============================================================================
setwd('F:/GBD_2020/Final/results/')
df <- read.csv('allcauses.city.results.csv')

df$acrate.nohap <- (df$ac.nohap*100000)/df$pop.sum
df$acrate.who <- (df$ac.who*100000)/df$pop.sum

dfc <- df[,c(2,3,6,12,17)]
dfc <- dfc %>% spread(year,acrate.nohap)
dfc$pc <- round((dfc$`2018`-dfc$`2000`)/dfc$`2000`*100,0)
write.csv(dfc, 'all.city.acrate.long.csv')
#==============================================================================
# All cases per region
#==============================================================================
case <- df %>%
  group_by(WHORegion,year) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm=T))
case <- as.data.frame(case)
case <- case %>% spread(year,ac.nohap)
case$pc <- round((case$`2018`-case$`2000`)/case$`2000`*100,0)

write.csv(case, 'cases.pc.region.csv')
#==============================================================================
# TPM all cities
#==============================================================================
df2 <- df[,c(2,3,5,6,12)]
df2long <- df2 %>% spread(year, popw)
df2long$pc <- round((df2long$`2018`-df2long$`2000`)/df2long$`2000`*100,0)
write.csv(df2long,'popw.long.city.csv')

#==============================================================================
# Rate
#==============================================================================
df3 <- df[,c(2,17,3,6,12)]
df3long <- df3 %>% spread(year, acrate.nohap)
df3long$pc <- round((df3long$`2018`-df3long$`2000`)/df3long$`2000`*100,0)
write.csv(df3long,'rate.long.city.csv')

dec <- subset(df3long, pc < 0)
inc <- subset(df3long, pc > 0)

df$who[df$popw <= 10] <- 'TRUE'
df$who[df$popw > 10] <- 'FALSE'

who <- subset(df, who %in% 'TRUE')
x <- unique(who$city)
length(x)

who.2000 <- subset(who, year %in% 2000)
who.2018 <- subset(who, year %in% 2018)

w0 <- unique(who.2000$city)
w18 <- unique(who.2018$city)

not.who <- subset(df, who %in% 'FALSE')
not.who.sum <- not.who %>%
  group_by(year) %>%
  summarize(pop = sum(pop.sum,na.rm=T))
not.who.sum <- as.data.frame(not.who.sum)
write.csv(not.who.sum, 'not.who.pop.sum.csv')

all.pop <- df %>%
  group_by(year) %>%
  summarize(pop = sum(pop.sum,na.rm=T))
all.pop <- as.data.frame(all.pop)


#==============================================================================
# Top 250 cities
#==============================================================================

df4 <- df
ls <- subset(df4, year %in% 2018)
ls <- ls[order(-ls$pop.sum),]
ls <- ls[c(1:250),]
ls <- unique(ls$id)
df4 <- subset(df4, id %in% ls)

top250.sum <- df4 %>%
  group_by(year) %>%
  summarize(ac.nohap = sum(ac.nohap,na.rm=T))

top250.sum <- as.data.frame(top250.sum)
top250.sum <- top250.sum %>% spread(year,ac.nohap)
top250.sum$pc <- round((top250.sum$`2018`-top250.sum$`2000`)/top250.sum$`2000`*100.0)
write.csv(top250.sum, 'top250.sum.csv')


top250.sum.region <- df4 %>%
  group_by(year,WHORegion) %>%
  summarize(ac.nohap = sum(ac.nohap,na.rm=T))
top250.sum.region <- as.data.frame(top250.sum)

top250.sum.region <- as.data.frame(top250.sum.region)
top250.sum.region <- top250.sum.region %>% spread(year,ac.nohap)
top250.sum.region$pc <- round((top250.sum.region$`2018`-top250.sum.region$`2000`)/top250.sum.region$`2000`*100.0)
write.csv(top250.sum.region, 'top250.sum.region.csv')


top250.sum.pop <- df4 %>%
  group_by(year) %>%
  summarize(pop.sum = sum(pop.sum,na.rm=T))
top250.sum.pop <- as.data.frame(top250.sum.pop)
write.csv(top250.sum.pop, 'top250.sum.pop.csv')

top250.sum.region <- as.data.frame(top250.sum.region)
top250.sum.region <- top250.sum.region %>% spread(year,ac.nohap)
top250.sum.region$pc <- round((top250.sum.region$`2018`-top250.sum.region$`2000`)/top250.sum.region$`2000`*100.0)
write.csv(top250.sum.region, 'top250.sum.region.csv')

top250.long <- df4[,c(2,4,7,13,19)]
top250.long <- top250.long %>% spread(year,rate)
top250.long$pc <- round((top250.long$`2018`-top250.long$`2000`)/top250.long$`2000`*100.0)
write.csv(top250.long, 'top250.long.rate.csv')

top250.long.cases <- df4[,c(2,4,7,13,15)]
top250.long.cases <- top250.long.cases %>% spread(year,ac.nohap)
top250.long.cases$pc <- round((top250.long.cases$`2018`-top250.long.cases$`2000`)/top250.long.cases$`2000`*100.0)
write.csv(top250.long.cases, 'top250.long.cases.csv')

top250.long.pm <- df4[,c(2,4,7,13,15)]
top250.long.pm <- top250.long.pm %>% spread(year,ac.nohap)
top250.long.pm$pc <- round((top250.long.pm$`2018`-top250.long.pm$`2000`)/top250.long.pm$`2000`*100.0)
write.csv(top250.long.cases, 'top250.long.cases.csv')

#==============================================================================
# Rates percent change
#==============================================================================


setwd('E:/GBD_2020/Final/rates/')
all <- read.csv('allages.ihme.final.csv')
allages <- subset(all, age_name %in% 'All Ages')
allcvd <- subset(all, cause_name %in% c("Ischemic heart disease","Ischemic stroke",
                                        "Intracerebral hemorrhage"))

rates.all <- all %>% 
  group_by(location_id,year,location_name) %>%
  summarize(avgrate = mean(val, na.rm = T))

rates <- as.data.frame(rates.all)
colnames(rates)[1] <- 'parent_id'


setwd('E:/GBD_2020/Final/results/')
df <- read.csv('allcauses.city.results.csv')
head(df)

df2 <- merge(df, rates, by=c('parent_id','year'))

df <- df2
rm(df2)

df$"WHORegion" <- as.character(df$"WHORegion")
df$"WHORegion"[df$"WHORegion" == 'WPRO'] <- 'Western Pacific'
df$"WHORegion"[df$"WHORegion" == 'SEARO'] <- 'South-East Asia'
df$"WHORegion"[df$"WHORegion" == 'EURO'] <- 'Europe'
df$"WHORegion"[df$"WHORegion" == 'AMRO'] <- 'Americas'
df$"WHORegion"[df$"WHORegion" == 'AFRO'] <- 'Africa'
df$"WHORegion"[df$"WHORegion" == 'EMRO'] <- 'Eastern Mediterranean'

df2 <- df[,c(2,13,17,18)]

df3 <- df2 %>% distinct(location_name, year,avgrate,WHORegion) %>% group_by(year) 
df3 <- as.data.frame(df3)

df4 <- df3 %>% spread(year,avgrate)
df4$pc <- round((df4$`2018`-df4$`2000`)/df4$`2000`*100,0)
write.csv(df4,'rates.country.pc.csv')

df5 <- df3 %>% 
  group_by(year,WHORegion) %>%
  summarize(avgrate = mean(avgrate, na.rm = T))
df5 <- as.data.frame(df5)
df5 <- df5 %>% spread(year, avgrate)
df5$pc <- round((df5$`2018`-df5$`2000`)/df5$`2000`*100,0)
write.csv(df5,'rates.whoregion.pc.csv')
