library(tidyverse)

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/df/')
#=================================================================================================
df <- read.csv("combined.city.paf.cvd.stroke.csv")
df3 <- df[,c(4:16,42,45,49:51,55:60)]

# [1] "X.1"             "X"               "parent_id"       "id"
# [5] "pop.sum"         "popw"            "year.x"          "location_id"
# [9] "city"            "cluster"         "loc_name"        "GBDRegion"
# [13] "GBDSuperRegion"  "SDGRegion"       "WHORegion"       "WHOIncomeRegion"
# [17] "grouping"        "year_id.x"       "lower.x"         "mean.hap"
# [21] "median.x"        "upper.x"         "location_name.x" "level.x"
# [25] "level.y"         "location_name.y" "year_id.y"       "mean"
# [29] "median.y"        "lower.y"         "upper.y"         "population"
# [33] "sum.pm"          "WHO"             "rr.point"        "rr.upper"
# [37] "rr.lower"        "rr.who.point"    "rr.who.upper"    "rr.who.lower"
# [41] "year.y"          "age_name"        "measure_name"    "location_name"
# [45] "cause_name"      "metric_name"     "val"             "pop"
# [49] "paf.point"       "paf.upper"       "paf.lower"       "paf.who.point"
# [53] "paf.who.upper"   "paf.who.lower"   "ac.point"        "ac.upper"
# [57] "ac.lower"        "ac.who.point"    "ac.who.upper"    "ac.who.lower"

df2 <- read.csv('combined.city.paf.csv')

df2 <- df2[,c(4:8,9,10:16,43,44,47,48,49,53,54,55,56,57,58)]

# > names(df2)
# [1] "X.1"             "X"               "parent_id"       "id"
# [5] "pop.sum"         "popw"            "year.x"          "location_id"
# [9] "city"            "cluster"         "loc_name"        "GBDRegion"
# [13] "GBDSuperRegion"  "SDGRegion"       "WHORegion"       "WHOIncomeRegion"
# [17] "grouping"        "year_id.x"       "lower.x"         "mean.hap"
# [21] "median.x"        "upper.x"         "location_name.x" "level.x"
# [25] "level.y"         "location_name.y" "year_id.y"       "mean"
# [29] "median.y"        "lower.y"         "upper.y"         "population"
# [33] "sum.pm"          "WHO"             "rr.point"        "rr.upper"
# [37] "rr.lower"        "rr.who.point"    "rr.who.upper"    "rr.who.lower"
# [41] "measure_name"    "location_name"   "age_name"        "cause_name"
# [45] "year.y"          "Rate"            "paf.point"       "paf.upper"
# [49] "paf.lower"       "paf.who.point"   "paf.who.upper"   "paf.who.lower"
# [53] "ac.point"        "ac.upper"        "ac.lower"        "ac.who.point"
# [57] "ac.who.upper"    "ac.who.lower"



names(df3) <- c("id","pop.sum","popw","year",
                "location_id","city","cluster","location_name",
                "GBDRegion","GBDSuperRegion","SDGRegion","WHORegion",
                "WHOIncomeRegion","age_name","cause_name","paf.point",
                "paf.upper","paf.lower","ac.point","ac.upper",
                "ac.lower","ac.who.point","ac.who.upper","ac.who.lower")

names(df2) <- c("id","pop.sum","popw","year",
"location_id","city","cluster","location_name",
"GBDRegion","GBDSuperRegion","SDGRegion","WHORegion",
"WHOIncomeRegion","age_name","cause_name","paf.point",
"paf.upper","paf.lower","ac.point","ac.upper",
"ac.lower","ac.who.point","ac.who.upper","ac.who.lower")


all.cvd <- df3 %>% 
  group_by(year,id,pop.sum,popw,city,cluster,location_id,GBDRegion,location_name,        
           GBDSuperRegion,SDGRegion,WHORegion,WHOIncomeRegion) %>%
  summarize(mean.paf.point = mean(paf.point,na.rm=T),
            mean.paf.lower = mean(paf.lower,na.rm=T),
            mena.paf.upper = mean(paf.upper,na.rm=T),
            ac.point = sum(ac.point,na.rm=T),
            ac.lower = sum(ac.lower,na.rm=T),
            ac.upper = sum(ac.upper,na.rm=T),
            ac.who.point = sum(ac.who.point,na.rm=T),
            ac.who.lower = sum(ac.who.lower,na.rm=T),
            ac.who.upper = sum(ac.who.upper,na.rm=T))

all.cvd <- as.data.frame(all.cvd)


all.comb <- df2 %>% 
  group_by(year,id,pop.sum,popw,city,cluster,location_id,GBDRegion,location_name,        
           GBDSuperRegion,SDGRegion,WHORegion,WHOIncomeRegion) %>%
  summarize(mean.paf.point = mean(paf.point,na.rm=T),
            mean.paf.lower = mean(paf.lower,na.rm=T),
            mena.paf.upper = mean(paf.upper,na.rm=T),
            ac.point = sum(ac.point,na.rm=T),
            ac.lower = sum(ac.lower,na.rm=T),
            ac.upper = sum(ac.upper,na.rm=T),
            ac.who.point = sum(ac.who.point,na.rm=T),
            ac.who.lower = sum(ac.who.lower,na.rm=T),
            ac.who.upper = sum(ac.who.upper,na.rm=T))

all.comb <- as.data.frame(all.comb)

m <- rbind(all.cvd, all.comb)

all.nohap <- m %>% 
  group_by(year,id,pop.sum,popw,city,cluster,location_id,GBDRegion,location_name,        
           GBDSuperRegion,SDGRegion,WHORegion,WHOIncomeRegion) %>%
  summarize(mean.paf.point = mean(mean.paf.point,na.rm=T),
            mean.paf.lower = mean(mean.paf.lower,na.rm=T),
            mena.paf.upper = mean(mena.paf.upper,na.rm=T),
            ac.point = sum(ac.point,na.rm=T),
            ac.lower = sum(ac.lower,na.rm=T),
            ac.upper = sum(ac.upper,na.rm=T),
            ac.who.point = sum(ac.who.point,na.rm=T),
            ac.who.lower = sum(ac.who.lower,na.rm=T),
            ac.who.upper = sum(ac.who.upper,na.rm=T))

all.nohap <- as.data.frame(all.nohap)


write.csv(all.nohap, 'allcauses.city.results.csv')
