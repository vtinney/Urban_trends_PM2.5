library(tidyverse)

setwd('D:/GBD_2020/Final/results/')

all <- read.csv('allcauses.city.results.UI.csv')


# #============================================================================
# Mean attributable mortality rate by year and region
#==============================================================================
avg.acrate.region <- all %>% 
  group_by(WHORegion,year) %>%
  summarize(ac.point = sum(ac.point, na.rm = T),
            ac.lower=sum(ac.lower,na.rm=T),
            ac.upper=sum(ac.upper,na.rm=T),
            ac.who.point=sum(ac.who.point,na.rm=T),
            ac.who.lower=sum(ac.who.lower,na.rm=T),
            ac.who.upper=sum(ac.who.upper,na.rm=T),
            pop=sum(pop.sum, na.rm=T))
avg.acrate.region <- as.data.frame(avg.acrate.region)
avg.acrate.region$acrate.point <- (avg.acrate.region$ac.point*100000)/avg.acrate.region$pop
avg.acrate.region$acrate.lower <- (avg.acrate.region$ac.lower*100000)/avg.acrate.region$pop
avg.acrate.region$acrate.upper <- (avg.acrate.region$ac.upper*100000)/avg.acrate.region$pop
write.csv(avg.acrate.region, 'avg.acrate.region.UI.csv')

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
  summarize(ac.point = sum(ac.point, na.rm = T),
            ac.lower=sum(ac.lower,na.rm=T),
            ac.upper=sum(ac.upper,na.rm=T),
            ac.who.point=sum(ac.who.point,na.rm=T),
            ac.who.lower=sum(ac.who.lower,na.rm=T),
            ac.who.upper=sum(ac.who.upper,na.rm=T),
            pop=sum(pop.sum, na.rm=T))

all.rate.gbl$acrate.point <- (all.rate.gbl$ac.point*100000)/all.rate.gbl$pop
all.rate.gbl$acrate.lower <- (all.rate.gbl$ac.lower*100000)/all.rate.gbl$pop
all.rate.gbl$acrate.upper <- (all.rate.gbl$ac.upper*100000)/all.rate.gbl$pop
all.rate.gbl <- as.data.frame(all.rate.gbl)
write.csv(all.rate.gbl, 'all.rate.gbl.UI.csv')

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

df <- all

df4 <- df
ls <- subset(df4, year %in% 2018)
ls <- ls[order(-ls$pop.sum),]
ls <- ls[c(1:250),]
ls <- unique(ls$id)
df4 <- subset(df4, id %in% ls)

top250.sum <- df4 %>%
  group_by(year) %>%
  summarize(ac.point = sum(ac.point, na.rm = T),
            ac.lower=sum(ac.lower,na.rm=T),
            ac.upper=sum(ac.upper,na.rm=T),
            ac.who.point=sum(ac.who.point,na.rm=T),
            ac.who.lower=sum(ac.who.lower,na.rm=T),
            ac.who.upper=sum(ac.who.upper,na.rm=T),
            pop=sum(pop.sum, na.rm=T))

top250.sum <- as.data.frame(top250.sum)

write.csv(top250.sum, 'top250.sum.csv')

