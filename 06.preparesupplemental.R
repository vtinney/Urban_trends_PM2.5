library(tidyverse)
setwd('E:/GBD_2020/Final/results/')
df <- read.csv('allcauses.city.results.csv')
ls <- subset(df, year %in% 2018)
ls <- ls[order(-ls$pop.sum),]
ls2 <- ls[c(1:250),]
list <- unique(ls2$id)
df <- subset(df, id %in% list)            

df$"WHORegion" <- as.character(df$"WHORegion")
df$"WHORegion"[df$"WHORegion" == 'WPRO'] <- 'Western Pacific'
df$"WHORegion"[df$"WHORegion" == 'SEARO'] <- 'South-East Asia'
df$"WHORegion"[df$"WHORegion" == 'EURO'] <- 'Europe'
df$"WHORegion"[df$"WHORegion" == 'AMRO'] <- 'Americas'
df$"WHORegion"[df$"WHORegion" == 'AFRO'] <- 'Africa'
df$"WHORegion"[df$"WHORegion" == 'EMRO'] <- 'Eastern Mediterranean'

df$city[df$city == 'SÃ£o Paulo'] <- 'Sao Paulo'
df$city[df$city == 'Delhi [New Delhi]'] <- 'New Delhi'

library(tidyverse)

setwd('D:/GBD_2020/Final/results/july_2021/')

all <- read.csv("allcauses.city.results.csv")
sm <- read.csv("combined.hdc.expsum.sm.csv")
sm <- sm[,c(3,4,5)]

df <- all
df <- df[,c(2:23)]
df$"WHORegion" <- as.character(df$"WHORegion")
df$"WHORegion"[df$"WHORegion" == 'WPRO'] <- 'Western Pacific'
df$"WHORegion"[df$"WHORegion" == 'SEARO'] <- 'South-East Asia'
df$"WHORegion"[df$"WHORegion" == 'EURO'] <- 'Europe'
df$"WHORegion"[df$"WHORegion" == 'AMRO'] <- 'Americas'
df$"WHORegion"[df$"WHORegion" == 'AFRO'] <- 'Africa'
df$"WHORegion"[df$"WHORegion" == 'EMRO'] <- 'Eastern Mediterranean'

df2 <- merge(df, sm, by=c('id','year'))
write.csv(df2, 'updated_supplemental_july_2021_pm.csv')


write.csv(df,'Supplemental_excel.csv')
#====================================================================
setwd('D:/GBD_2020/Paper_draft/Supplemental and Figures/')

df <- read.csv("top 250 for supplemental table.csv")

dfpm <- subset(df, Year %in% c(2000,2001,2002,2016,2017,2018))
dfcases <- subset(df, Year %in% c(2000,2001,2002,2016,2017,2018))
dfpop <- subset(df, Year %in% c(2000,2001,2002,2016,2017,2018))

dfpop <- dfpop[,c(1:3,5:8)]
dfpm <- dfpm[,c(1,2,4:8)]
dfcases <- dfcases[,c(1,2,5:9)]

dfpm <- dfpm %>% spread(Year,Population.weighted.PM2.5..µg.m3.)
dfcases <- dfcases %>% spread(Year,PM2.5.attributable.cases)
dfpop <- dfpop %>% spread(Year,Population)

dfpm$avg2001 <- round((dfpm$`2000`+dfpm$`2001`+dfpm$`2002`)/3,2)
dfpm$avg2017 <- round((dfpm$`2016`+dfpm$`2017`+dfpm$`2017`)/3,2)

dfcases$avg2001 <- round((dfcases$`2000`+dfcases$`2001`+dfcases$`2002`)/3)
dfcases$avg2017 <- round((dfcases$`2016`+dfcases$`2017`+dfcases$`2017`)/3)

dfpop$avg2001 <- round((dfpop$`2000`+dfpop$`2001`+dfpop$`2002`)/3)
dfpop$avg2017 <- round((dfpop$`2016`+dfpop$`2017`+dfpop$`2017`)/3)

dfpm <- dfpm[,c(1:5,12,13)]
dfcases <- dfcases[,c(1:5,12,13)]
dfpop <- dfpop[,c(1:5,12,13)]

colnames(dfpm)[6] <- 'popw.2001'
colnames(dfpm)[7] <- 'popw.2017'

colnames(dfcases)[6] <- 'cases.2001'
colnames(dfcases)[7] <- 'cases.2017'

colnames(dfpop)[6] <- 'pop.2001'
colnames(dfpop)[7] <- 'pop.2017'

df2 <- cbind(dfpm,dfcases,dfpop)

write.csv(df2, 'supp.table.csv')
                     