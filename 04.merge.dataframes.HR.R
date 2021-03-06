library(tidyverse)

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/df/')
#=================================================================================================
df <- read.csv("combined.city.paf.cvd.stroke.csv")
df3 <- df[,c(3:16,39,42,53,54)]


# [1] "X.1"             "X"               "parent_id"       "id"
# [5] "pop.sum"         "popw"            "year.x"          "location_id"
# [9] "city"            "cluster"         "loc_name"        "GBDRegion"
# [13] "GBDSuperRegion"  "SDGRegion"       "WHORegion"       "WHOIncomeRegion"
# [17] "grouping"        "year_id.x"       "lower.x"         "mean.hap"
# [21] "median.x"        "upper.x"         "location_name.x" "level.x"
# [25] "level.y"         "location_name.y" "year_id.y"       "mean"
# [29] "median.y"        "lower.y"         "upper.y"         "population"
# [33] "sum.pm"          "WHO"             "rr.oap"          "rr.hap"
# [37] "rr.who"          "year.y"          "age_name"        "measure_name"
# [41] "location_name"   "cause_name"      "metric_name"     "val"
# [45] "pop"             "pm.rr.hap"       "paf.pm.hap"      "rr.nohap"
# [49] "paf.nohap"       "paf.who"         "pop.frac"        "ac.hap"
# [53] "ac.nohap"        "ac.who"


df2 <- read.csv('combined.city.paf.csv')
df2 <- df2[,c(3:16,40,41,50,51)]

# > names(df2)
# [1] "X.1"             "X"               "parent_id"       "id"
# [5] "pop.sum"         "popw"            "year.x"          "location_id"
# [9] "city"            "cluster"         "loc_name"        "GBDRegion"
# [13] "GBDSuperRegion"  "SDGRegion"       "WHORegion"       "WHOIncomeRegion"
# [17] "grouping"        "year_id.x"       "lower.x"         "mean.hap"
# [21] "median.x"        "upper.x"         "location_name.x" "level.x"
# [25] "level.y"         "location_name.y" "year_id.y"       "mean"
# [29] "median.y"        "lower.y"         "upper.y"         "population"
# [33] "sum.pm"          "WHO"             "rr.oap"          "rr.hap"
# [37] "rr.who"          "measure_name"    "location_name"   "age_name"
# [41] "cause_name"      "year.y"          "Rate"            "pm.rr.hap"
# [45] "paf.pm.hap"      "rr.nohap"        "paf.nohap"       "paf.who"
# [49] "ac.hap"          "ac.nohap"        "ac.who"


names(df3) <- c("parent_id","id",
"pop.sum","popw","year","location_id","city","cluster",
"location_name","GBDRegion","GBDSuperRegion",
"SDGRegion","WHORegion","WHOIncomeRegion",
"age_name","cause_name","ac.nohap","ac.who")

names(df2) <- c("parent_id","id",
               "pop.sum","popw","year","location_id","city","cluster",
               "location_name","GBDRegion","GBDSuperRegion",
               "SDGRegion","WHORegion","WHOIncomeRegion",
               "age_name","cause_name","ac.nohap","ac.who")

all.cvd <- df3 %>% 
  group_by(year,id,pop.sum,popw,city,cluster,location_id,GBDRegion,         
           GBDSuperRegion,SDGRegion,WHORegion,WHOIncomeRegion,parent_id) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm = T),
            ac.who = sum(ac.who, na.rm = T))
all.cvd <- as.data.frame(all.cvd)


all.comb <- df2 %>% 
  group_by(year,id,pop.sum,popw,city,cluster,location_id,GBDRegion,         
           GBDSuperRegion,SDGRegion,WHORegion,WHOIncomeRegion,parent_id) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm = T),
            ac.who = sum(ac.who, na.rm = T))
all.comb <- as.data.frame(all.comb)

m <- rbind(all.cvd, all.comb)

all.nohap <- m %>% 
  group_by(year,id,pop.sum,popw,city,cluster,location_id,GBDRegion,         
           GBDSuperRegion,SDGRegion,WHORegion,WHOIncomeRegion,parent_id) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm = T),
            ac.who = sum(ac.who, na.rm = T))
all.nohap <- as.data.frame(all.nohap)


all.years <- all.nohap %>% 
  group_by(year) %>%
  summarize(ac = sum(ac.nohap, na.rm = T),
            pop = sum(pop.sum, na.rm = T))
all.years <- as.data.frame(all.years)

write.csv(all.nohap, 'allcauses.city.results.csv')


all.cvd <- df3 %>% 
  group_by(year,WHORegion) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm = T))
all.cvd <- as.data.frame(all.cvd)
write.csv(all.cvd, 'all.cvd.region.csv')


all.comb <- df2 %>% 
  group_by(year,WHORegion) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm = T))
all.comb <- as.data.frame(all.comb)
write.csv(all.comb, 'all.comb.region.csv')
