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
library(ggrepel)
library(ggspatial)
library(sf)
library(mapproj)
library(maps)
library(rgeos)
library(maptools)
library(gpclib)
library(Cairo)
library(showtext)
library(sysfonts)
font_add_google("Alegreya Sans", "Alegreya Sans SC Light")
options("device" = "RStudioGD")
options("device" = "windows")
showtext_auto()

#=================================================================================
# barchart
#=================================================================================
setwd('D:/GBD_2020/Final/results/')



pal <- c("Net change"="grey80",
         "Concentrations"="#E68310",
         "Baseline disease rates"="#3969AC",
         "Population ageing"="#E73F74",
         "Population growth"="#11A579")

df <- read.csv('allcauses.city.results.dem.csv')
df <- df[,c(2:4)]
df <- df %>% spread(anal,ac.nohap)

control <- read.csv('cases.region.csv')
control <- control[,c(2,3,4)]
control <- subset(control, year %in% c(2000,2019))
control <- control %>% spread(year,ac.point)
control$pc.cases <- (control$`2019`-control$`2000`)/control$`2000`

df2 <- merge(df,control,by='WHORegion')

# Equation S7
df2$ratio.age <- df2$`2000`/df2$age
df2$ratio.bdr <- df2$`2000`/df2$bdr
df2$ratio.conc <- df2$`2000`/df2$conc
df2$ratio.pop <- df2$`2000`/df2$pop

#Equation S8
df2$m <- df2$ratio.age*df2$ratio.bdr*df2$ratio.conc*df2$ratio.pop

#Equation S9
df2$log.age <- log(df2$ratio.age,df2$m)
df2$log.bdr <- log(df2$ratio.bdr,df2$m)
df2$log.conc <- log(df2$ratio.conc,df2$m)
df2$log.pop <- log(df2$ratio.pop,df2$m)

df2$sum <- df2$log.age+df2$log.bdr+df2$log.conc+df2$log.pop

#Equation S10
df2$pc.age <- df2$pc.cases*df2$log.age
df2$pc.bdr <- df2$pc.cases*df2$log.bdr
df2$pc.conc <- df2$pc.cases*df2$log.conc
df2$pc.pop <- df2$pc.cases*df2$log.pop

df2$test <- df2$pc.age+df2$pc.bdr+df2$pc.conc+df2$pc.pop

dfa2 <- df2[,c(1,8,19:22)]

# Version 3
#dfa3 <- read.csv('netchange_barchart_input.csv')
dfa3 <- dfa2 %>% gather(Parameter, Contribution,"pc.cases":"pc.pop")
dfa3$"Parameter"[dfa3$"Parameter" == 'pc.cases'] <- 'Net change'
dfa3$"Parameter"[dfa3$"Parameter" == 'pc.pop'] <- 'Population growth'
dfa3$"Parameter"[dfa3$"Parameter" == 'pc.age'] <- 'Population ageing'
dfa3$"Parameter"[dfa3$"Parameter" == 'pc.conc'] <- 'Concentrations'
dfa3$"Parameter"[dfa3$"Parameter" == 'pc.bdr'] <- 'Baseline disease rates'
dfa3$"Group"[dfa3$"Parameter" == 'Net change'] <- 'Net'
dfa3$"Group"[dfa3$"Parameter" == 'Population growth'] <- 'Parameters'
dfa3$"Group"[dfa3$"Parameter" == 'Concentrations'] <- 'Parameters'
dfa3$"Group"[dfa3$"Parameter" == 'Baseline disease rates'] <- 'Parameters'
dfa3$"Group"[dfa3$"Parameter" == 'Population ageing'] <- 'Parameters'


dfa3$"WHORegion" <- as.character(dfa3$"WHORegion")
dfa3$"WHORegion"[dfa3$"WHORegion" == 'WPRO'] <- 'Western\nPacific'
dfa3$"WHORegion"[dfa3$"WHORegion" == 'SEARO'] <- 'South-East\nAsia'
dfa3$"WHORegion"[dfa3$"WHORegion" == 'EURO'] <- 'Europe'
dfa3$"WHORegion"[dfa3$"WHORegion" == 'AMRO'] <- 'Americas'
dfa3$"WHORegion"[dfa3$"WHORegion" == 'AFRO'] <- 'Africa'
dfa3$"WHORegion"[dfa3$"WHORegion" == 'EMRO'] <- 'Eastern\nMediterranean'


#dfa3$Contribution <- dfa3$Contribution*100

dfa4 <- dfa3 %>%
  arrange(Contribution,WHORegion) %>%
  mutate(Parameter = factor(Parameter, levels=c("Net change", "Population growth", "Population ageing", 
                                      "Concentrations", "Baseline disease rates")),
        Group = factor(Group, levels=c("Net","Parameters")))
         
dfa4$Contribution <- dfa4$Contribution*100

bar <- ggplot() +
  geom_bar(data=dfa4, aes(x=Group,y=Contribution,fill=Parameter),
           position="stack", stat="identity", color="white",size=0.1)+#,width=0.5)+
  facet_wrap(.~WHORegion,strip.position = "bottom",nrow=1)+
  labs(x='WHO Region',
       y=bquote('Percent difference between 2000 and 2019 [(2019-2000)/2000]'))+
  scale_y_continuous(breaks=c(-100,-75,-50,-25,0,25,50,75,100))+
  geom_hline(yintercept=0, color = 'grey50', size=0.5,linetype='dotted')+
  scale_fill_manual(values = pal)+
  theme(plot.title=element_text(hjust = 0, size=16,family = "Alegreya Sans SC Light"),
        panel.background = element_rect(fill = NA),
        legend.position = 'right',
        legend.text=element_text(size=9,family = "Alegreya Sans SC Light"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(margin=unit(c(0.3,0.3,0.3,0), "cm"),hjust = 0, size=11,family = "Alegreya Sans SC Light"),
        strip.text.x=element_text(hjust=0.5,size=7,family = "Alegreya Sans SC Light"),
        legend.title = element_blank())

ggsave('barchart_pc.eps',width=6.5,height=5,device='eps')

#write.csv(df, 'parameter.contribution.csv')
