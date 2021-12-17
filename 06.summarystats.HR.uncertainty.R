library(tidyverse)

setwd('D:/GBD_2020/Final/results/')

all <- read.csv('allcauses.city.results.csv')


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
write.csv(avg.acrate.region, 'avg.acrate.region.csv')

#==============================================================================
# Percent change in attributable mortality rate per region
#==============================================================================
avg.acrate.region <- avg.acrate.region[,c(1,2,10)]
avg.acrate.region.spread <- as.data.frame(avg.acrate.region)
avg.acrate.region.long <- avg.acrate.region.spread %>% spread(year, acrate.point)

avg.acrate.region.long$mean.2000 <- (avg.acrate.region.long$`2000`+avg.acrate.region.long$`2001`+avg.acrate.region.long$`2002`)/3
avg.acrate.region.long$mean.2019 <- (avg.acrate.region.long$`2019`+avg.acrate.region.long$`2018`+avg.acrate.region.long$`2017`)/3
avg.acrate.region.long$pc <- (avg.acrate.region.long$mean.2019-avg.acrate.region.long$mean.2000)/avg.acrate.region.long$mean.2000*100
write.csv(avg.acrate.region.long, 'avg.acrate.region.long.csv')
  
#==============================================================================
# Total global cases by year
#==============================================================================
all.total <- all %>%
  group_by(year) %>%
  summarize(ac.point = sum(ac.point, na.rm = T),
            ac.lower=sum(ac.lower,na.rm=T),
            ac.upper=sum(ac.upper,na.rm=T),
            ac.who.point=sum(ac.who.point,na.rm=T),
            ac.who.lower=sum(ac.who.lower,na.rm=T),
            ac.who.upper=sum(ac.who.upper,na.rm=T),
            pop=sum(pop.sum, na.rm=T))
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
write.csv(all.rate.gbl, 'all.rate.gbl.csv')


#==============================================================================
# All cases per region
#==============================================================================
case <- all %>%
  dplyr::group_by(WHORegion,year) %>%
  dplyr::summarize(ac.point = sum(ac.point, na.rm = T),
            ac.lower=sum(ac.lower,na.rm=T),
            ac.upper=sum(ac.upper,na.rm=T),
            ac.who.point=sum(ac.who.point,na.rm=T),
            ac.who.lower=sum(ac.who.lower,na.rm=T),
            ac.who.upper=sum(ac.who.upper,na.rm=T),
            pop=sum(pop.sum, na.rm=T))
case <- as.data.frame(case)

write.csv(case, 'cases.region.csv')




#=================================================================
# Top 250
#=================================================================
df <- all

df4 <- df
ls <- subset(df4, year %in% 2019)
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

#==============================================================================
# Total population in cities by year and global pop weighted
#==============================================================================

all$product <- all$pop.sum*all$popw

all.popw <- all %>%
  group_by(year) %>%
  summarize(product = sum(product,na.rm=T),
            pop = sum(pop.sum, na.rm=T))

all.popw <- as.data.frame(all.popw)
all.popw$popw <- all.popw$product/all.popw$pop


write.csv(all.popw, 'all.popw.csv')

test <- subset(all, year %in% 2019)

# Long hand


# Function base R package
wt <- test$pop.sum/sum(test$pop.sum)
x <- test$popw
xm <- weighted.mean(x,wt)
v <- sum(wt*(x-xm)^2)
sd <- sqrt(var)

# Function Hmisc package
xm <- wtd.mean(x,wt)
sd <- w.sd(x,wt)

# Year 2000
test <- subset(all, year %in% 2000)

# Long hand
wm <- sum(test$product)/sum(test$pop.sum)
num <- sum(test$pop.sum*((test$popw-wm)^2))
den <- sum(test$pop.sum)-(sum(test$pop.sum^2)/sum(test$pop.sum))
var <- num/den
sd <- sqrt(var)

#==============================================================================
# Avg PM by WHO region and year
#==============================================================================
all$product <- all$pop.sum*all$popw

avg.pm <- all %>% 
  dplyr::group_by(year,WHORegion) %>%
  dplyr::summarize(sum.region = sum(product, na.rm = T),
            sum.pop =sum(pop.sum, na.rm=T))
avg.pm <- as.data.frame(avg.pm)

avg.pm$popw <- avg.pm$sum.region/avg.pm$sum.pop

write.csv(avg.pm, 'avg.pm.region.csv')

avg.pm.region <- avg.pm[,c(1,2,5)]

avg.pm.region.spread <- as.data.frame(avg.pm.region)
avg.pm.region.long <- avg.pm.region.spread %>% spread(year, popw)
avg.pm.region.long$mean.2000 <- (avg.pm.region.long$`2000`+avg.pm.region.long$`2001`+avg.pm.region.long$`2002`)/3
avg.pm.region.long$mean.2019 <- (avg.pm.region.long$`2019`+avg.pm.region.long$`2018`+avg.pm.region.long$`2017`)/3
avg.pm.region.long$pc <- (avg.pm.region.long$mean.2019-avg.pm.region.long$mean.2000)/avg.pm.region.long$mean.2000*100
write.csv(avg.pm.region.long, 'avg.pm.region.long.csv')

#===========================
sd.region <- subset(all, year %in% c(2000,2019))
sd.region <- sd.region[,c(1,2,3,4,12)]
sdg <- sd.region %>% gather(anal,val,'pop.sum':'popw')

sds <- sdg %>% spread(year,val)
pm <- subset(sds, anal %in% 'popw')
pop <- subset(sds, anal %in% 'pop.sum')

sdf <- merge(pm,pop,by=c('id','WHORegion'))
colnames(sdf) <- c('id','WHORegion','popw','popw2000','popw2019','pop.sum',
                   'pop.sum2000','pop.sum2019')
sdf$diffpm <- sdf$popw2019-sdf$popw2000
sdf$diffpop <- sdf$pop.sum2019-sdf$pop.sum2000
sdf$product2000 <- sdf$pop.sum2000*sdf$popw2001
sdf$product2019 <- sdf$pop.sum2019*sdf$popw2019
regions <- as.vector(unique(sdf$WHORegion))

mat <- matrix(ncol=7,nrow=6)

for(k in 1:length(regions)){
  region_df <- subset(sdf, WHORegion %in% regions[k])
  
  wt2000 <- region_df$pop.sum2000/sum(region_df$pop.sum2000,na.rm=T)
  wt2019 <- region_df$pop.sum2019/sum(region_df$pop.sum2019,na.rm=T)
  x2000 <- region_df$popw2000
  x2019 <- region_df$popw2019
  xm2000 <- weighted.mean(x2000,wt2000,na.rm=T)
  xm2019 <- weighted.mean(x2019,wt2019,na.rm=T)
  v2000 <- sum(wt2000*(x2000-xm2000)^2,na.rm=T)
  v2019 <- sum(wt2019*(x2019-xm2019)^2,na.rm=T)
  sd2000 <- sqrt(v2000)
  sd2019 <- sqrt(v2019)
  
  wtdiff <- region_df$diffpop/sum(region_df$diffpop,na.rm=T)
  xdiff <- region_df$diffpm
  xmdiff <- weighted.mean(xdiff,wtdiff,na.rm=T)
  vdiff <- sum(wtdiff*(xdiff-xmdiff)^2,na.rm=T)
  sddiff <- sqrt(vdiff)
  
  mat[k,1] <- regions[k]
  mat[k,2] <- round(xm2000,1)
  mat[k,3] <- round(sd2000,1)
  mat[k,4] <- round(xm2019,1)
  mat[k,5] <- round(sd2019,1)
  mat[k,6] <- round(xmdiff,1)
  mat[k,7] <- round(sddiff,1)
  
}

mat <- as.data.frame(mat)
colnames(mat) <- c('WHORegion','xm2000','sd2000','xm2019','sd2019',
                   'xmdiff','sddiff')
write.csv(mat, 'region.pm.avg.sd.csv')
#==============================================================================
# Unique number of cities meeting the WHO recommendation
#==============================================================================
all.who <- subset(all, popw < 10)
x <- unique(all.who$city)
# 2054


all.who <- subset(all, popw > 10)
x <- unique(all.who$city)
all.who <- as.data.frame(all.who %>%
                           group_by(year) %>%
                           summarize(sum=sum(pop.sum,na.rm=T)))
write.csv(all.who,'all_pop_not_who.csv')
#==============================================================================
#Long percent change each city for rates
#==============================================================================
df <- all
df$acrate.point <- (df$ac.point*100000)/df$pop.sum 
df$acrate.who <- (df$ac.who.point*100000)/df$pop.sum

dfc <- df[,c(2,3,6,13,25)]
dfc <- dfc %>% spread(year,acrate.point)

dfc$mean.2000 <- (dfc$`2000`+dfc$`2001`+dfc$`2002`)/3
dfc$mean.2019 <- (dfc$`2019`+dfc$`2018`+dfc$`2017`)/3
dfc$pc <- (dfc$mean.2019-dfc$mean.2000)/dfc$mean.2000*100

write.csv(dfc, 'all.city.acrate.long.csv')

#==============================================================================
# TPM all cities
#==============================================================================
df2 <- all[,c(1,2,4,5,12)]
df2long <- df2 %>% spread(year, popw)

df2long$mean.2000 <- (df2long$`2000`+df2long$`2001`+df2long$`2002`)/3
df2long$mean.2019 <- (df2long$`2018`+df2long$`2017`+df2long$`2019`)/3
df2long$pc <- round((df2long$mean.2019-df2long$mean.2000)/df2long$mean.2000*100,0)

write.csv(df2long,'popw.long.city.csv')

#==============================================================================
# WHO*********************************************************************
#==============================================================================

dec <- subset(df2long, pc <= 0)
# > nrow(dec)
# [1] 5969
inc <- subset(df2long, pc > 0)
# > nrow(inc)
# [1] 7164

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


dec <- subset(dfc, pc <= 0)
nrow(dec)
# [1] 5740
inc <- subset(dfc, pc > 0)
nrow(inc)
# [1] 7339

#==============================================================================
# IQR popw
#==============================================================================

iqr19 <- subset(all, year %in% 2019)

quant <- quantile(iqr19$popw,na.rm=T)
quant

iqr00 <- subset(all, year %in% 2000)
quant00 <- quantile(iqr00$popw,na.rm=T)
quant00SA
quant00AF
