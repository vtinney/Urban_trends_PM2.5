library(tidyverse)

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/results/city_level/dem/df/')
#=================================================================================================
df <- read.csv("combined.city.paf.cvd.stroke.dem.csv")
df3 <- df[,c(4,15,41,55,57)]

# [1] "X.1"             "X"               "parent_id"       "id"
# [5] "pop.sum.x"       "popw"            "year"            "location_id"
# [9] "city"            "cluster"         "loc_name"        "GBDRegion"
# [13] "GBDSuperRegion"  "SDGRegion"       "WHORegion"       "WHOIncomeRegion"
# [17] "grouping"        "year_id"         "lower"           "mean.hap"
# [21] "median"          "upper"           "location_name"   "level"
# [25] "population.y"    "pop.sum.y"       "sum.pm"          "WHO"
# [29] "rr.oap"          "rr.hap"          "rr.who"          "year.x"
# [33] "age_name.x"      "measure_name.x"  "location_name.x" "cause_name.x"
# [37] "metric_name.x"   "val.x"           "pop.x"           "year.y"
# [41] "age_name.y"      "measure_name.y"  "location_name.y" "cause_name.y"
# [45] "metric_name.y"   "val.y"           "pop.y"           "pm.rr.hap"
# [49] "paf.pm.hap"      "rr.nohap"        "paf.nohap"       "paf.who"
# [53] "pop.frac"        "ac.hap"          "ac.nohap"        "ac.who"
# [57] "anal.names"


df2 <- read.csv('combined.city.paf.dem.csv')
df2 <- df2[,c(4,15,41,51,53)]

# > names(df2)
# [1] "X.1"             "X"               "parent_id"       "id"
# [5] "pop.sum.x"       "popw"            "year.x"          "location_id"
# [9] "city"            "cluster"         "loc_name"        "GBDRegion"
# [13] "GBDSuperRegion"  "SDGRegion"       "WHORegion"       "WHOIncomeRegion"
# [17] "grouping"        "year_id.x"       "lower.x"         "mean.hap"
# [21] "median.x"        "upper.x"         "location_name.x" "level.x"
# [25] "level.y"         "location_name.y" "year_id.y"       "mean"
# [29] "median.y"        "lower.y"         "upper.y"         "population"
# [33] "pop.sum.y"       "sum.pm"          "WHO"             "rr.oap"
# [37] "rr.hap"          "rr.who"          "measure_name"    "location_name"
# [41] "age_name"        "cause_name"      "year.y"          "Rate"
# [45] "pm.rr.hap"       "paf.pm.hap"      "rr.nohap"        "paf.nohap"
# [49] "paf.who"         "ac.hap"          "ac.nohap"        "ac.who"
# [53] "anal.names"

names(df3) <- c("id","WHORegion","age_name","ac.nohap","anal")
names(df2) <- c("id","WHORegion","age_name","ac.nohap","anal")

df3$WHORegion <- as.character(df3$WHORegion)
all.cvd <- df3 %>% 
  group_by(WHORegion,anal) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm = T))
all.cvd <- as.data.frame(all.cvd)
write.csv(all.cvd, 'allcauses.region.cvd.dem.csv')

all.comb <- df2 %>% 
  group_by(WHORegion,anal) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm = T))

all.comb <- as.data.frame(all.comb)
write.csv(all.comb, 'allcauses.region.comb.dem.csv')

m <- rbind(all.cvd, all.comb)
 
all.nohap <- m %>%
  group_by(WHORegion,anal) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm = T))
all.nohap <- as.data.frame(all.nohap)

all.region <- m %>%
  group_by(WHORegion,anal) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm = T))
all.region <- as.data.frame(all.region)
 
write.csv(all.region, 'allcauses.region.results.dem.csv')
write.csv(all.nohap, 'allcauses.city.results.dem.csv')
