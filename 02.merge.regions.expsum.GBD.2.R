library(tidyverse)
setwd('D:/GBD_2020/Final/exp_sum/')
list.files()

lu_hdc <- read.csv('hdc_gbd_lookup.csv')
lu_hdc <- lu_hdc[,c(1,3,4,5,7,9:15)]
colnames(lu_hdc)[2] <- 'id'

hdc <- read.csv("combined.hdc.GBD.popw.1km.csv")
hdc <- hdc[,c(3,5:8)]
df <- merge(hdc, lu_hdc, by='id')

write.csv(df, 'combined.hdc.GBD.popw.1km.csv')

#=========================================================================================

df2 <- read.csv('combined.hdc.GBD.popw.10km.csv')
df2 <- df2[,c(3,5:8)]
df2 <- merge(df2, lu_hdc, by='id')
write.csv(df2, 'combined.hdc.GBD.popw.10km.csv')
