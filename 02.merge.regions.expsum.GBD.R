library(tidyverse)
setwd('E:/GBD_2020/Final/exp_sum/')
list.files()

lu_hdc <- read.csv('hdc_gbd_lookup.csv')
lu_hdc <- lu_hdc[,c(1,3,4,5,7,9:15)]
colnames(lu_hdc)[2] <- 'id'

hdc <- read.csv("combined.hdc.GBD.popw.csv")
hdc <- hdc[,c(3,5:8)]
df <- merge(hdc, lu_hdc, by='id')
dfx <- df[,c(1,2,4:16)]

df2 <- subset(dfx, year %in% 2000)
df3 <- subset(dfx, year %in% 2010)
df4 <- subset(dfx, year %in% 2015)

df2 <- df2 %>% spread(res, pop.sum)
df3 <- df3 %>% spread(res, pop.sum)
df4 <- df4 %>% spread(res, pop.sum)

n <- rbind(df2, df3, df4)
n$ratio <- n$`1km`/n$`10km`

dfy <- subset(df, res %in% '1km')
dfy <- dfy[,c(1,3,4)]

dfm <- merge(dfy, n, by=c('year','id'),all=TRUE)
dfm$pop2 <- dfm$`10km`*dfm$ratio

#==================================================================

d <- subset(dfm, year %in% 2015)
sum(d$pop2, na.rm=TRUE)
#[1] 2008138175

#==================================================================


df.2000 <- subset(dfm, year %in% 2000) #****************************rbind
df.merge.2000 <- subset(df, year %in% c(2005))
dfx2000 <- merge(df.merge.2000, df.2000, by='id',all=TRUE)
dfx2000$pop2 <- dfx2000$pop.sum*dfx2000$ratio#**********************rbind

df.2010 <- subset(dfm, year %in% 2010)#**********************rbind
df.merge.2010 <- subset(df, year %in% c(2011:2014))
dfx2010 <- merge(df.merge.2010, df.2010, by='id',all=TRUE)
dfx2010$pop2 <- dfx2010$pop.sum*dfx2010$ratio#**********************rbind

df.2015 <- subset(dfm, year %in% 2015)#**********************rbind
df.merge.2015 <- subset(df, year %in% c(2016:2018))
dfx2015 <- merge(df.merge.2015, df.2015, by='id',all=TRUE)
dfx2015$pop2 <- dfx2015$pop.sum*dfx2015$ratio#**********************rbind

dft <- rbind(df.2000, df.2010, df.2015)
dfo <- rbind(dfx2000, dfx2010, dfx2015)

# > names(dft)
# [1] "year"            "id"              "popw"            "loc_id"          "city"           
# [6] "cluster"         "loc_name"        "parent_id_sub"   "GBDRegion"       "GBDSuperRegion" 
# [11] "SDGRegion"       "WHORegion"       "WHOIncomeRegion" "ihme_loc_id"     "10km"           
# [16] "1km"             "ratio"           "pop2" 

dft <- dft[,c(1:13,18)]
# > names(dfo)
# [1] "id"                "pop.sum"           "popw.x"            "year.x"            "res"              
# [6] "loc_id.x"          "city.x"            "cluster.x"         "loc_name.x"        "parent_id_sub.x"  
# [11] "GBDRegion.x"       "GBDSuperRegion.x"  "SDGRegion.x"       "WHORegion.x"       "WHOIncomeRegion.x"
# [16] "ihme_loc_id.x"     "year.y"            "popw.y"            "loc_id.y"          "city.y"           
# [21] "cluster.y"         "loc_name.y"        "parent_id_sub.y"   "GBDRegion.y"       "GBDSuperRegion.y" 
# [26] "SDGRegion.y"       "WHORegion.y"       "WHOIncomeRegion.y" "ihme_loc_id.y"     "10km"             
# [31] "1km"               "ratio"             "pop2"
dfo <- dfo[,c(1:4,6:15,33)]

# > names(dfo)
# [1] "id"                "popw.x"            "year.x"            "loc_id.x"          "city.x"           
# [6] "cluster.x"         "loc_name.x"        "parent_id_sub.x"   "GBDRegion.x"       "GBDSuperRegion.x" 
# [11] "SDGRegion.x"       "WHORegion.x"       "WHOIncomeRegion.x" "pop2"                   
# > names(dft)
# [1] "year"            "id"              "popw"            "loc_id"          "city"           
# [6] "cluster"         "loc_name"        "parent_id_sub"   "GBDRegion"       "GBDSuperRegion" 
# [11] "SDGRegion"       "WHORegion"       "WHOIncomeRegion" "pop2"  

dfo <- dfo[,c(1,3:15)]
dft <- dft[,c(2,3,1,4:14)]

names(dfo) <- names(dft)

comb <- rbind(dfo,dft)

write.csv(comb, 'combined.hdc.GBD.expsum.csv')
