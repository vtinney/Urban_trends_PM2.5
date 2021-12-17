# Created: August 7, 2021
# Compare sensitivity analysis with updated results
library(foreign)
library(tidyverse)
library(data.table)
library(writexl)
#================================================================================
library(dplyr)
library(ggplot2)
library(ggspatial)
library(tidyverse)
library(cowplot)
library(tidyverse)
library(ggtext)
library(ggpubr)
library(raster)
library(rgdal)
library(ggdist)
library(ggrepel)
library(ggspatial)
library(sf)
library(mapproj)
library(maps)
library(tinter)
library(rgeos)
library(maptools)
library(gpclib)
library(Cairo)
library(showtext)
library(patchwork)
font_add_google("Alegreya Sans", "Alegreya Sans SC Light")
options("device" = "RStudioGD")
options("device" = "windows")
showtext_auto()
library(ggExtra)
library(tidyverse)
library(colorspace)
library(ragg)
library(cowplot)
library(ggtext)
library(pdftools)
library(grid)
library(gridExtra)
library(rstatix)
library(foreign)
library(ggridges)
library(readxl)

#==================================================================================
# Plot race change
#==================================================================================
theme_plots <- function(...) {
  theme_bw()+
    theme(plot.title=element_text(size=16,family = "Alegreya Sans SC Light"),
          plot.subtitle=element_text(size=14,family = "Alegreya Sans SC Light"),
          strip.background=element_blank(),  
          panel.grid.major = element_blank(),
          plot.caption = element_text(size=9,family = "Alegreya Sans SC Light"),
          strip.text=element_text(size=14,family = "Alegreya Sans SC Light"),
          axis.title=element_text(size=14,family = "Alegreya Sans SC Light"),
          legend.text=element_text(size=11,family = "Alegreya Sans SC Light"),
          legend.title =element_text(size=11,family = "Alegreya Sans SC Light"),
          axis.ticks.length=unit(-0.25, "cm"), 
          axis.text.x = element_text(margin=unit(c(0.3,0.3,0,0.3), "cm"),hjust = 0, size=11,family = "Alegreya Sans SC Light"), 
          axis.text.y = element_text(margin=
                                       unit(c(0.3,0.3,0.3,0), "cm"),hjust = 0, size=11,family = "Alegreya Sans SC Light"),
          ...
    )
}




setwd('D:/GBD_2020/Paper_draft/June_2021/Submission/')

main <- read_xlsx('Supp_data_file_July_2021.xlsx')
main <- main[,c(1:7,13,16)]
main$anal <- 'main'
main <- main[,c(1:3,5:10)]

setwd('D:/GBD_2020/Final/results/july_2021/')

sa <- read.csv('updated_supplemental_july_2021_pm.csv')
sa <- sa[,c(1:6,13,16)]
sa$anal <- 'sa'

main$ID <- as.integer(main$ID)

dfs <- merge(sa,main,by=c('Year','ID'))

names(dfs) <- c("Year","ID","pop.main","popw.sa",                       
                "sm.sa","City.x",                           
                "WHORegion.x","ac.sa",
                "anal.x","City.y",                           
                "Population.y","popw.main",                       
                "sm.main","WHORegion.y",                      
                "ac.main","anal.y") 

dfs <- dfs[,c(1:8,12,13,15)]
colnames(dfs)[6] <- 'city'
colnames(dfs)[7] <- 'WHORegion'

df_16 <- subset(dfs, Year %in% 2019)

dot_main <- ggplot() +
  geom_point(data=df_16, 
             aes(x=popw.main, y=sm.main), color="#39b185",shape=16)+
  labs(title='A. Main analysis: 2019',
       x=bquote('Population-weighted '~PM[2.5]),
       y=bquote('Simple mean '~PM[2.5]))+
  geom_abline(intercept = 0, slope = 1,linetype=2)+
  theme_plots()

dot_sa <- ggplot() +
  geom_point(data=df_16, 
             aes(x=popw.sa, y=sm.sa), color="#39b185",shape=16)+
  labs(title='B. Sensitivity analysis: 2019',
       x=bquote('Population-weighted '~PM[2.5]),
       y=bquote('Simple mean '~PM[2.5]))+
  geom_abline(intercept = 0, slope = 1,linetype=2)+
  theme_plots()


plots <- plot_grid(dot_main,dot_sa,ncol=2)
ggsave('dot_comparison.pdf',width=9,height=5,device=cairo_pdf)

df_16$ac.main <- round(df_16$ac.main,0)

df_16$rate.main <- (df_16$ac.main*100000)/df_16$pop.main
df_16$rate.sa <- (df_16$ac.sa*100000)/df_16$pop.main

newdata <- df_16[order(-df_16$pop.main),]
newdata <- newdata[c(1:50),]
newdata$pc <- (newdata$rate.main-newdata$rate.sa)/newdata$rate.main*100
mean.50 <- mean(newdata$pc,na.rm=T)

# Main analysis 3% lower than sensitivity analysis

dot_ac_50 <- ggplot() +
  geom_point(data=newdata, 
             aes(x=rate.main, y=rate.sa), color="#cf597e",shape=16)+
  labs(title='A. Top 50 most populated urban areas',
       caption='Main analysis 2% on average lower than sensitivity analysis',
       x=bquote(~PM[2.5]~'attributable mortality - main analysis'),
       y=bquote(~PM[2.5]~'attributable mortality - sensitivity analysis'))+
  geom_abline(intercept = 0, slope = 1,linetype=2)+
  theme_plots()


df_16$pc <- (df_16$rate.main-df_16$rate.sa)/df_16$rate.main*100
df_16$pc[is.infinite(df_16$pc)] <- 0
df_16$pc[is.nan(df_16$pc)] <- 0
df_16$pc[is.na(df_16$pc)] <- 0
mean.all <- mean(df_16$pc,na.rm=T)

# Main analysis 63% lower than sensitivity analysis

dot_ac_all <- ggplot() +
  geom_point(data=df_16, 
             aes(x=rate.main, y=rate.sa), color="#cf597e",shape=16)+
  labs(title='B. All urban areas',
       caption='Main analysis 90% lower on average than sensitivity analysis',
       x=bquote(~PM[2.5]~'attributable mortality - main analysis'),
       y=bquote(~PM[2.5]~'attributable mortality - sensitivity analysis'))+
  geom_abline(intercept = 0, slope = 1,linetype=2)+
  theme_plots()

plots <- plot_grid(dot_ac_50,dot_ac_all,ncol=2)
ggsave('dot_comparison_rates.pdf',width=9,height=5,device=cairo_pdf)

dfs$popxconc.main <- dfs$pop.main*dfs$popw.main
dfs$popxconc.sa <- dfs$pop.main*dfs$popw.sa

avg <- dfs %>% 
  group_by(WHORegion,Year) %>%
  summarize(ac.main = sum(ac.main, na.rm = T),
            ac.sa = sum(ac.sa, na.rm = T),
            popxconc.main=sum(popxconc.main,na.rm=T),
            popxconc.sa=sum(popxconc.sa,na.rm=T),
            pop=sum(pop.main, na.rm=T))

avg$rate.main <- (avg$ac.main*100000)/avg$pop
avg$rate.sa <- (avg$ac.sa*100000)/avg$pop
avg$popw.main <- avg$popxconc.main/avg$pop
avg$popw.sa <- avg$popxconc.sa/avg$pop
avg$pc <- round((avg$rate.main-avg$rate.sa)/avg$rate.main*100,0)
avg$pc.pm <- round((avg$popw.main-avg$popw.sa)/avg$popw.main*100,0)
write.csv(avg, 'results_comparison_region.csv')

write.csv(df_16, 'comparison_urban_2019.csv')
