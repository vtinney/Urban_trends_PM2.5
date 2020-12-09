library(tidyverse)

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/lookup/')
lu_hdc <- read.csv('hdc_gbd_lookup.csv')
lu_hdc <- lu_hdc[,c(1,3,4,5,7,9:15)]
colnames(lu_hdc)[2] <- 'id'

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/exp_sum/gbd/')
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

dfy <- subset(df, res %in% '10km')
dfy <- dfy[,c(1,3,4)]

dfm <- merge(dfy, n, by=c('year','id'),all=TRUE)
dfm$pop2 <- dfm$`10km`*dfm$ratio

write.csv(dfm, 'combined.hdc.GBD.expsum.csv')

d <- subset(dfm, year %in% 2015)
sum(d$pop2, na.rm=TRUE)
#[1] 2008138175

