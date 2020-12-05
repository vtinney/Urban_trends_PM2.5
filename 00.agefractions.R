library(tidyverse)
setwd('E:/GBD_2020/Final/rates/')
list.files()
age <- read.csv("ages.csv")

age <- age[,c(2,3,4,8,10,12,13,14)]
age.df <- age %>% spread(year,val)
rate <- subset(age.df, metric_name %in% 'Rate')
n <- subset(age.df, metric_name %in% 'Number')
colnames(n) <-  c("measure_name","location_id","location_name","age_name","cause_name","metric_name",  
"n.2000","n.2001","n.2002","n.2003","n.2004","n.2005",         
"n.2006","n.2007","n.2008","n.2009","n.2010","n.2011",         
"n.2012","n.2013","n.2014","n.2015","n.2016","n.2017",        
"n.2018") 

df <- merge(rate, n, by=c('location_id','age_name'))

df$pop.2000 <- (df$n.2000*100000)/df$`2000`
df$pop.2001 <- (df$n.2001*100000)/df$`2001`
df$pop.2002 <- (df$n.2002*100000)/df$`2002`
df$pop.2003 <- (df$n.2003*100000)/df$`2003`
df$pop.2004 <- (df$n.2004*100000)/df$`2004`
df$pop.2005 <- (df$n.2005*100000)/df$`2005`
df$pop.2006 <- (df$n.2006*100000)/df$`2006`
df$pop.2007 <- (df$n.2007*100000)/df$`2007`
df$pop.2008 <- (df$n.2008*100000)/df$`2008`
df$pop.2009 <- (df$n.2009*100000)/df$`2009`
df$pop.2010 <- (df$n.2010*100000)/df$`2010`
df$pop.2011 <- (df$n.2011*100000)/df$`2011`
df$pop.2012 <- (df$n.2012*100000)/df$`2012`
df$pop.2013 <- (df$n.2013*100000)/df$`2013`
df$pop.2014 <- (df$n.2014*100000)/df$`2014`
df$pop.2015 <- (df$n.2015*100000)/df$`2015`
df$pop.2016 <- (df$n.2016*100000)/df$`2016`
df$pop.2017 <- (df$n.2017*100000)/df$`2017`
df$pop.2018 <- (df$n.2018*100000)/df$`2018`

names(df)
df <- df[,c(1,2,4,49:67)] 
colnames(df)[3] <- 'location_name'
write.csv(df, 'age.fractions.csv')

# Standardized all-ages rates
copd <- read.csv("copd.csv")
cvd <- read.csv("cvd.csv")
t2 <- read.csv("diabetes.csv")
lc <- read.csv("lc.csv")        
lri <- read.csv("lri.csv")
stroke <- read.csv("stroke.csv")

df <- rbind(copd,cvd,t2,lc,lri,stroke)
write.csv(df, 'all.ihme.final.csv')

# All ages rates
copd <- read.csv("copd.all.csv")
cvd <- read.csv("cvd.csv")
t2 <- read.csv("t2.all.csv")
lc <- read.csv("lc.all.csv")        
lri <- read.csv("lri.all.csv")
stroke <- read.csv("stroke.csv")

df <- rbind(copd,cvd,t2,lc,lri,stroke)
write.csv(df, 'allages.ihme.final.csv')
