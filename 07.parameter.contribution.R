#================================================================================
# library(dplyr)
# library(ggplot2)
# library(ggspatial)
library(tidyverse)
# library(cowplot)
# library(tidyverse)
# library(ggtext)
# library(ggpubr)
# library(raster)
# library(rgdal)
# library(ggrepel)
# library(ggspatial)
# library(sf)
# library(mapproj)
# library(maps)
# library(rgeos)
# library(maptools)
# library(gpclib)
# library(Cairo)
# library(showtext)
# library(sysfonts)
# font_add_google("Alegreya Sans", "Alegreya Sans SC Light")
# options("device" = "RStudioGD")
# options("device" = "windows")
# showtext_auto()

#=================================================================================
# barchart
#=================================================================================
setwd('F:/GBD_2020/Final/results/')


df <- read.csv('allcauses.city.results.csv')
df <- df[,c(2,12,15)]

dfz <- read.csv('allcauses.region.comb.dem.csv')
dfz <- dfz[,c(2:4)]
dfz <- dfz %>% spread(anal,ac.nohap)
dfz$age <- 0
#
dfx <- read.csv('allcauses.region.cvd.dem.csv')
dfx <- dfx[,c(2:4)]
dfx <- dfx %>% spread(anal,ac.nohap)
#
dfy <- rbind(dfz,dfx)
#
dfy <- dfy %>%
  group_by(WHORegion) %>%
  summarize(age = sum(age, na.rm = T),
            bdr = sum(bdr, na.rm = T),
            conc = sum(conc, na.rm = T),
            pop = sum(pop, na.rm = T))
dfy <- as.data.frame(dfy)


df <- subset(df, year %in% c(2000,2018))
df2 <-
  df %>%
  group_by(WHORegion,year) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm = T))
df2 <- as.data.frame(df2)


df3 <- df2 %>% spread(year, ac.nohap)


dfa <- merge(df3,dfy,by='WHORegion')


dfa$"WHORegion" <- as.character(dfa$"WHORegion")
dfa$"WHORegion"[dfa$"WHORegion" == 'WPRO'] <- 'Western\nPacific'
dfa$"WHORegion"[dfa$"WHORegion" == 'SEARO'] <- 'South-East\nAsia'
dfa$"WHORegion"[dfa$"WHORegion" == 'EURO'] <- 'Europe'
dfa$"WHORegion"[dfa$"WHORegion" == 'AMRO'] <- 'Americas'
dfa$"WHORegion"[dfa$"WHORegion" == 'AFRO'] <- 'Africa'
dfa$"WHORegion"[dfa$"WHORegion" == 'EMRO'] <- 'Eastern\nMediterranean'

pal <- c("Net change"="grey80",
         "Concentrations"="#E68310",
         "Baseline disease rates"="#3969AC",
         "Population ageing"="#E73F74",
         "Population growth"="#11A579")

dfa$diff.bdr <- round(dfa$bdr/dfa$`2000`-1,4)
dfa$diff.conc <- round(dfa$conc/dfa$`2000`-1,4)
dfa$diff.pop <- round(dfa$pop/dfa$`2000`-1,4)
dfa$diff.age <- round(dfa$age/dfa$`2000`-1,4)
dfa$param <- (dfa$diff.bdr*dfa$diff.conc*dfa$diff.pop)-1
dfa$diff.net <- round((dfa$`2018`-dfa$`2000`)/dfa$`2000`,4)

dfa$ratio <- dfa$diff.bdr+dfa$diff.conc+dfa$diff.pop
dfa$ratio2 <- dfa$diff.net/dfa$ratio
dfa$diff.bdf2 <- dfa$diff.bdr*dfa$ratio2*100
dfa$diff.pop2 <- dfa$diff.pop*dfa$ratio2*100
dfa$diff.conc2 <- dfa$diff.conc*dfa$ratio2*100
dfa$diff.age2 <- dfa$diff.age*dfa$ratio2*100
dfa$diff.net <- dfa$diff.net*100

dfa2 <- dfa[,c(1,13,16:18)]
write.csv(dfa2, 'parameter.contribution.region.csv')

# Version 1
dfa3 <- dfa2 %>% gather(Parameter, Contribution,"diff.net":"diff.conc2")
dfa3$"Parameter"[dfa3$"Parameter" == 'diff.net'] <- 'Net change'
dfa3$"Parameter"[dfa3$"Parameter" == 'diff.pop2'] <- 'Population growth'
#dfa3$"Parameter"[dfa3$"Parameter" == 'diff.age2'] <- 'Population ageing'
dfa3$"Parameter"[dfa3$"Parameter" == 'diff.conc2'] <- 'Concentrations'
dfa3$"Parameter"[dfa3$"Parameter" == 'diff.bdf2'] <- 'Baseline disease rates'

# Version 2
dfa2 <- read.csv('netchange_barchart_input.csv')
dfa3 <- dfa2 %>% gather(Parameter, Contribution,"Population.Ageing":"Net.Change")
dfa3$"Parameter"[dfa3$"Parameter" == 'Net.Change'] <- 'Net change'
dfa3$"Parameter"[dfa3$"Parameter" == 'Population.Growth'] <- 'Population growth'
dfa3$"Parameter"[dfa3$"Parameter" == 'Population.Ageing'] <- 'Population ageing'
dfa3$"Parameter"[dfa3$"Parameter" == 'Concentrations'] <- 'Concentrations'
dfa3$"Parameter"[dfa3$"Parameter" == 'Baseline.disease.rates'] <- 'Baseline disease rates'
dfa3$"Group"[dfa3$"Parameter" == 'Net change'] <- 'Net'
dfa3$"Group"[dfa3$"Parameter" == 'Population growth'] <- 'Parameters'
dfa3$"Group"[dfa3$"Parameter" == 'Concentrations'] <- 'Parameters'
dfa3$"Group"[dfa3$"Parameter" == 'Baseline disease rates'] <- 'Parameters'
dfa3$"Group"[dfa3$"Parameter" == 'Population ageing'] <- 'Parameters'


dfa3$"WHORegion"[dfa3$"WHORegion" == 'Western Pacific'] <- 'Western\nPacific'
dfa3$"WHORegion"[dfa3$"WHORegion" == 'South-East Asia'] <- 'South-East\nAsia'
dfa3$"WHORegion"[dfa3$"WHORegion" == 'Eastern Mediterranean'] <- 'Eastern\nMediterranean'

dfa3$Contribution <- dfa3$Contribution*100

dfa4 <- dfa3 %>%
  arrange(Contribution,WHORegion) %>%
  mutate(Parameter = factor(Parameter, levels=c("Net change", "Population growth", "Population ageing", 
                                      "Concentrations", "Baseline disease rates")),
        Group = factor(Group, levels=c("Net","Parameters")))
         


bar <- ggplot() +
  geom_bar(data=dfa4, aes(x=Group,y=Contribution,fill=Parameter),
           position="stack", stat="identity", color="white",size=0.1,width=0.5)+
  facet_wrap(.~WHORegion,strip.position = "bottom",nrow=1)+
  labs(x='WHO Region',
       y=bquote(~PM[2.5]~'attributable mortality percent change (%)'))+
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

ggsave('barchart_pc.pdf',width=6.5,height=5,device=cairo_pdf)

#write.csv(df, 'parameter.contribution.csv')
