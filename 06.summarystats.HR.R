library(tidyverse)

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/')

popw <- read.csv('combined.hdc.exp.sum.lu.csv')

all <- read.csv('allcauses.city.results.csv')

#==============================================================================
# Total population in cities by year and global pop weighted
#==============================================================================

popw$product <- popw$pop.sum*popw$popw

all.popw <- popw %>%
   group_by(year) %>%
   summarize(product = sum(product,na.rm=T),
             pop = sum(pop.sum, na.rm=T))

all.popw <- as.data.frame(all.popw)
all.popw$popw <- all.popw$product/all.popw$pop


setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/df/')
write.csv(all.popw, 'all.popw.csv')

#==============================================================================
# Avg PM by WHO region and year
#==============================================================================
all$product <- all$pop.sum*all$popw

avg.pm <- all %>% 
  group_by(year,WHORegion) %>%
  summarize(sum.region = sum(product, na.rm = T),
            sum.pop =sum(pop.sum, na.rm=T))
avg.pm <- as.data.frame(avg.pm)

avg.pm$popw <- avg.pm$sum.region/avg.pm$sum.pop

write.csv(avg.pm, 'avg.pm.WHO.region.csv')

avg.pm.region <- avg.pm[,c(1,2,5)]

avg.pm.region.spread <- as.data.frame(avg.pm.region)
avg.pm.region.long <- avg.pm.region.spread %>% spread(year, popw)
avg.pm.region.long$mean.2000 <- (avg.pm.region.long$`2000`+avg.pm.region.long$`2001`+avg.pm.region.long$`2002`)/3
avg.pm.region.long$mean.2018 <- (avg.pm.region.long$`2018`+avg.pm.region.long$`2017`+avg.pm.region.long$`2016`)/3
avg.pm.region.long$pc <- (avg.pm.region.long$mean.2018-avg.pm.region.long$mean.2000)/avg.pm.region.long$mean.2000*100
write.csv(avg.pm.region.long, 'avg.pm.region.long.csv')

#==============================================================================
# Avg PM by GBD region and year
#==============================================================================

avg.pm.gbd <- all %>% 
  group_by(year,GBDRegion) %>%
  summarize(sum.region = sum(product, na.rm = T),
            sum.pop =sum(pop.sum, na.rm=T))
avg.pm.gbd <- as.data.frame(avg.pm.gbd)

avg.pm.gbd$popw <- avg.pm.gbd$sum.region/avg.pm.gbd$sum.pop


write.csv(avg.pm.gbd, 'avg.pm.GBD.region.csv')

avg.pm.gbd.long <- avg.pm.gbd[,c(1,2,5)]

avg.pm.region.spread <- as.data.frame(avg.pm.gbd.long)
avg.pm.region.long <- avg.pm.region.spread %>% spread(year, popw)
avg.pm.region.long$mean.2000 <- (avg.pm.region.long$`2000`+avg.pm.region.long$`2001`+avg.pm.region.long$`2002`)/3
avg.pm.region.long$mean.2018 <- (avg.pm.region.long$`2018`+avg.pm.region.long$`2017`+avg.pm.region.long$`2016`)/3
avg.pm.region.long$pc <- (avg.pm.region.long$mean.2018-avg.pm.region.long$mean.2000)/avg.pm.region.long$mean.2000*100
write.csv(avg.pm.region.long, 'avg.pm.gbd.long.csv')

#==============================================================================
# Unique number of cities meeting the WHO recommendation
#==============================================================================
all.who <- subset(df, popw < 10)
x <- unique(all.who$city)

# #============================================================================
# Mean attributable mortality rate by year and region
#==============================================================================
avg.acrate.region <- all %>% 
  group_by(WHORegion,year) %>%
  summarize(ac = sum(ac.nohap, na.rm = T),
            pop=sum(pop.sum, na.rm=T))
avg.acrate.region <- as.data.frame(avg.acrate.region)
avg.acrate.region$acrate <- (avg.acrate.region$ac*100000)/avg.acrate.region$pop

#==============================================================================
# Percent change in attributable mortality rate per region
#==============================================================================
avg.acrate.region <- avg.acrate.region[,c(1,2,5)]
avg.acrate.region.spread <- as.data.frame(avg.acrate.region)
avg.acrate.region.long <- avg.acrate.region.spread %>% spread(year, acrate)

avg.acrate.region.long$mean.2000 <- (avg.acrate.region.long$`2000`+avg.acrate.region.long$`2001`+avg.acrate.region.long$`2002`)/3
avg.acrate.region.long$mean.2018 <- (avg.acrate.region.long$`2018`+avg.acrate.region.long$`2017`+avg.acrate.region.long$`2016`)/3
avg.acrate.region.long$pc <- (avg.acrate.region.long$mean.2018-avg.acrate.region.long$mean.2000)/avg.acrate.region.long$mean.2000*100
write.csv(avg.acrate.region.long, 'avg.acrate.region.long.csv')
  
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
# Global attributable mortality rate mean
#==============================================================================

all.rate.gbl <- all %>%
  group_by(year) %>%
  summarize(ac = sum(ac.nohap,na.rm=T),
            pop=sum(pop.sum, na.rm=T))

all.rate.gbl$acrate <- (all.rate.gbl$ac*100000)/all.rate.gbl$pop
all.rate.gbl <- as.data.frame(all.rate.gbl)
write.csv(all.rate.gbl, 'all.rate.gbl.csv')

#==============================================================================
# Rate variation among all 
#==============================================================================
all$acrate.nohap <- (all$ac.nohap*100000)/acrate.nohap$pop.sum

mm.rate <- all %>%
  group_by(year) %>%
  summarize(min = min(acrate.nohap, na.rm=T),
            max = max(acrate.nohap, na.rm=T))
mm.rate <- as.data.frame(mm.rate)
write.csv(mm.rate, 'mm.rate.csv')

#==============================================================================
#Long percent change each city for rates
#==============================================================================
df <- all
df$acrate.nohap <- (df$ac.nohap*100000)/df$pop.sum
df$acrate.who <- (df$ac.who*100000)/df$pop.sum

dfc <- df[,c(2,3,6,12,17)]
dfc <- dfc %>% spread(year,acrate.nohap)

dfc$mean.2000 <- (dfc$`2000`+dfc$`2001`+dfc$`2002`)/3
dfc$mean.2018 <- (dfc$`2018`+dfc$`2017`+dfc$`2016`)/3
dfc$pc <- (dfc$mean.2018-dfc$mean.2000)/dfc$mean.2000*100

write.csv(dfc, 'all.city.acrate.long.csv')

#==============================================================================
# all changes mortality (inc/dec)
#==============================================================================
all.spread <- df[,c(2,3,15)]
all.spread <- all.spread %>% spread(year, ac.nohap)
all.spread$mean.2000 <- (all.spread$`2000`+all.spread$`2001`+all.spread$`2002`)/3
all.spread$mean.2018 <- (all.spread$`2018`+all.spread$`2017`+all.spread$`2016`)/3
all.spread$pc <- (all.spread$mean.2018-all.spread$mean.2000)/all.spread$mean.2000*100

all.dec <- subset(all.spread, pc < 0)
all.inc <- subset(all.spread, pc >=0)

#==============================================================================
# All cases per region
#==============================================================================
case <- df %>%
  group_by(WHORegion,year) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm=T),
            ac.who = sum(ac.who, na.rm=T))
case <- as.data.frame(case)

cases <- df %>%
  group_by(year) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm=T),
            ac.who = sum(ac.who, na.rm=T))

case <- case %>% spread(year,ac.nohap)

case$mean.2000 <- (case$`2000`+case$`2001`+case$`2002`)/3
case$mean.2018 <- (case$`2018`+case$`2017`+case$`2016`)/3

case$pc <- round((case$mean.2018-case$mean.2000)/case$mean.2000*100,0)

write.csv(case, 'cases.pc.region.csv')

#==============================================================================
# TPM all cities
#==============================================================================
df2 <- df[,c(2,3,5,6,12)]
df2long <- df2 %>% spread(year, popw)

df2long$mean.2000 <- (df2long$`2000`+df2long$`2001`+df2long$`2002`)/3
df2long$mean.2018 <- (df2long$`2018`+df2long$`2017`+df2long$`2016`)/3
df2long$pc <- round((df2long$mean.2018-df2long$mean.2000)/df2long$mean.2000*100,0)

write.csv(df2long,'popw.long.city.csv')

#==============================================================================
# Rate
#==============================================================================
df3 <- df[,c(2,17,3,6,12)]
df3long <- df3 %>% spread(year, acrate.nohap)

df3long$mean.2000 <- (df3long$`2000`+df3long$`2001`+df3long$`2002`)/3
df3long$mean.2018 <- (df3long$`2018`+df3long$`2017`+df3long$`2016`)/3
df3long$pc <- round((df3long$mean.2018-df3long$mean.2000)/df3long$mean.2000*100,0)

write.csv(df3long,'rate.long.city.csv')

#==============================================================================
# WHO
#==============================================================================

dec <- subset(df3long, pc < 0)
inc <- subset(df3long, pc > 0)


df$who[df$popw <= 10] <- 'TRUE'
df$who[df$popw > 10] <- 'FALSE'

who <- subset(df, who %in% 'TRUE')
x <- unique(who$city)
length(x)
who$count <- 1

who <- distinct(who,city, .keep_all= TRUE)

who.sum <- who %>%
  group_by(WHORegion) %>%
  summarize(sum = sum(count,na.rm=T))
who.sum <- as.data.frame(who.sum)
write.csv(who.sum, 'who.sum.csv')

who.2 <- who %>%
  group_by(year) %>%
  summarize(sum = sum(count,na.rm=T))
who.2 <- as.data.frame(who.2)

who.2000 <- subset(who, year %in% 2000)
who.2018 <- subset(who, year %in% 2018)

w0 <- unique(who.2000$city)
w18 <- unique(who.2018$city)

not.who <- subset(df, who %in% 'FALSE')
not.who.sum <- not.who %>%
  group_by(year,WHORegion) %>%
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

top250.sum$mean.2000 <- (top250.sum$`2000`+top250.sum$`2001`+top250.sum$`2002`)/3
top250.sum$mean.2018 <- (top250.sum$`2018`+top250.sum$`2017`+top250.sum$`2016`)/3
top250.sum$pc <- round((top250.sum$mean.2018-top250.sum$mean.2000)/top250.sum$mean.2000*100,0)

write.csv(top250.sum, 'top250.sum.csv')


top250.sum.region <- df4 %>%
  group_by(year,WHORegion) %>%
  summarize(ac.nohap = sum(ac.nohap,na.rm=T))
top250.sum.region <- as.data.frame(top250.sum.region)

top250.sum.region <- as.data.frame(top250.sum.region)
top250.sum.region <- top250.sum.region %>% spread(year,ac.nohap)

top250.sum.region$mean.2000 <- (top250.sum.region$`2000`+top250.sum.region$`2001`+top250.sum.region$`2002`)/3
top250.sum.region$mean.2018 <- (top250.sum.region$`2018`+top250.sum.region$`2017`+top250.sum.region$`2016`)/3
top250.sum.region$pc <- round((top250.sum.region$mean.2018-top250.sum.region$mean.2000)/top250.sum.region$mean.2000*100,0)

write.csv(top250.sum.region, 'top250.sum.region.csv')


top250.sum.pop <- df4 %>%
  group_by(year) %>%
  summarize(pop.sum = sum(pop.sum,na.rm=T))
top250.sum.pop <- as.data.frame(top250.sum.pop)
write.csv(top250.sum.pop, 'top250.sum.pop.csv')


top250.long <- df4[,c(2,6,12,17)]
top250.long <- top250.long %>% spread(year,acrate.nohap)
top250.long$mean.2000 <- (top250.long$`2000`+top250.long$`2001`+top250.long$`2002`)/3
top250.long$mean.2018 <- (top250.long$`2018`+top250.long$`2017`+top250.long$`2016`)/3
top250.long$pc <- round((top250.long$mean.2018-top250.long$mean.2000)/top250.long$mean.2000*100,0)
write.csv(top250.long, 'top250.long.rate.csv')

top250.long.cases <- df4[,c(2,6,12,15)]
top250.long.cases <- top250.long.cases %>% spread(year,ac.nohap)
top250.long.cases$mean.2000 <- (top250.long.cases$`2000`+top250.long.cases$`2001`+top250.long.cases$`2002`)/3
top250.long.cases$mean.2018 <- (top250.long.cases$`2018`+top250.long.cases$`2017`+top250.long.cases$`2016`)/3
top250.long.cases$pc <- round((top250.long.cases$mean.2018-top250.long.cases$mean.2000)/top250.long.cases$mean.2000*100,0)
write.csv(top250.long.cases, 'top250.long.cases.csv')

top250.long.pm <- df4[,c(2,5,6,12)]
top250.long.pm <- top250.long.pm %>% spread(year,popw)

top250.long.pm$mean.2000 <- (top250.long.pm$`2000`+top250.long.pm$`2001`+top250.long.pm$`2002`)/3
top250.long.pm$mean.2018 <- (top250.long.pm$`2018`+top250.long.pm$`2017`+top250.long.pm$`2016`)/3
top250.long.pm$pc <- round((top250.long.pm$mean.2018-top250.long.pm$mean.2000)/top250.long.pm$mean.2000*100,0)

write.csv(top250.long.cases, 'top250.long.cases.csv')

#==============================================================================
# Rates percent change
#==============================================================================


setwd('F:/GBD_2020/Final/rates/')
all <- read.csv('allages.ihme.final.csv')
allages <- subset(all, age_name %in% 'All Ages')
allcvd <- subset(all, cause_name %in% c("Ischemic heart disease","Ischemic stroke",
                                        "Intracerebral hemorrhage"))

rates.all <- all %>% 
  group_by(location_id,year,location_name) %>%
  summarize(avgrate = mean(val, na.rm = T))

rates <- as.data.frame(rates.all)
colnames(rates)[1] <- 'parent_id'


setwd('F:/GBD_2020/Final/results/')
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
