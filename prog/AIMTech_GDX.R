# library -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)
library(foreach)
library(imputeTS)
library(countrycode)
library(patchwork)
library(fuzzyjoin)
library(gdxrrw)
library(nlme)
library(makedummies)

# setting -----------------------------------------------------------------

root <- getwd()
ddir <- paste0(root,'/data/GDX/')
odir <- paste0(root,'/output/GDX/')

MyTheme <- theme_bw() +
  theme(
    panel.border=element_blank(),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=10, colour = "black", angle = 0,face="bold"),
    strip.text.y = element_text(size=10, colour = "black", angle = 270,face="bold"),
    axis.text.x=element_text(size = 10,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    axis.text.y=element_text(size = 10,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    axis.title.x = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    axis.ticks.length=unit(0.15,"cm")
  )


# Input data --------------------------------------------------------------

source(paste0(root,'/prog/NEDO_HVC.R'))
source(paste0(root,'/prog/MMSA_Methanol.R'))
source(paste0(root,'/prog/USGS_Nitrogen.R'))



# Production --------------------------------------------------------------

df <- bind_rows(df_NH3,df_MOH,df_HVC)

g <- df %>% 
  group_by(H,J) %>% 
  summarise(value=sum(value)) %>% 
  ggplot() +
  geom_area(aes(x=H,y=value,fill=J))
plot(g)


# Feed stock ---------------------------------------------------------------

R33 <- rgdx.param(paste0(ddir,'serv_global_SSP2.gdx'),'ind_t') %>% 
  distinct(R)

NH3_feedshare <- R33 %>% 
  mutate(N_NH3_NGS=case_when(R33=='CHN' ~ 0.14,
                             TRUE ~ 1,
  ),
  N_NH3_COL=case_when(R33=='CHN' ~ 0.86,
                      TRUE ~ 0))# MOH

MOH_feedshare <- R33_17 %>% 
  select(R33) %>% 
  mutate(N_MOH_NGS=case_when(R33=='CHN' ~ 0.2,
                             TRUE ~ 1),
         N_MOH_COL=case_when(R33=='CHN' ~ 0.8,
                             TRUE ~ 0))

HVC_feedshare <- R33_17 %>% 
  select(R33) %>% 
  mutate(N_HVC_OIL=case_when(R33=='JPN' ~ 0.96,
                             R33=='USA' ~ 0.38,
                             R33=='BRA' ~ 0.99,
                             R33=='CAN' ~ 0.11,
                             R33=='CHN' ~ 1,
                             R33=='IND' ~ 0.68,
                             R33=='KOR' ~ 1,
                             R33%in%c('XE15','XE10','XE3') ~ 0.86,
                             TRUE ~ 0.52),
         N_HVC_GAS=case_when(R33=='JPN' ~ 0.04,
                             R33=='USA' ~ 0.62,
                             R33=='BRA' ~ 0.01,
                             R33=='CAN' ~ 0.89,
                             R33=='CHN' ~ 0,
                             R33=='IND' ~ 0.32,
                             R33=='KOR' ~ 0,
                             R33%in%c('XE15','XE10','XE3') ~ 0.14,
                             TRUE ~ 0.48))


lstHVC <- wgdx.reshape(HVC_feedshare,symDim=2,symName="feedshare",tName="L")
lstNH3 <- wgdx.reshape(NH3_feedshare,symDim=2,symName="feedshare",tName="L")
lstMOH <- wgdx.reshape(MOH_feedshare,symDim=2,symName="feedshare",tName="L")

wgdx.lst(paste0(odir,"/HVC_feedshare.gdx"),lstHVC)
wgdx.lst(paste0(odir,"/NH3_feedshare.gdx"),lstNH3)
wgdx.lst(paste0(odir,"/MOH_feedshare.gdx"),lstMOH)
