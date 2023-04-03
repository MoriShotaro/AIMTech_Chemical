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
# ddir <- paste0(root,'/data/GEER/output/global2010/2303270142/gams_output/gdx_primary/')
ddir <- paste0(root,'/data/GEER/input/')
# ddir <- paste0(root,'/data/GEER/output/global2050CCU/2303282245/gams_output/gdx_primary/')
odir <- paste0(root,'/output/check/')

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


# Input -------------------------------------------------------------------

df_emission <- rgdx.param(paste0(ddir,'emission_scenario.gdx'),'emission') %>% 
  mutate(Y5=as.numeric(as.character(Y5))) %>% 
  filter(MQ=='WOR',Y5<=2050) %>% 
  filter(str_detect(.i2,'SSP2-CACN-CO2T-1[0-4]00|SSP2-INDC-CO2T-1[0-4]00|SSP2-CACN-CO2T-0[0-9]00|SSP2-INDC-CO2T-0[0-9]00'))

df_emission <- rgdx.param(paste0(ddir,'emission_scenario.gdx'),'emission') %>% 
  mutate(Y5=as.numeric(as.character(Y5))) %>% 
  filter(MQ=='WOR',Y5<=2050) %>% 
  filter(.i2%in%c('SR15_15C_high_OS','SR15_15C_low_OS','SR15_Below_15C','SR15_Lower_2C','SR15_Higher_2C'))

df_emission2 <- rgdx.param(paste0(ddir,'emission_scenario.gdx'),'emission') %>% 
  mutate(Y5=as.numeric(as.character(Y5))) %>% 
  filter(MQ=='WOR',Y5<=2050) %>% 
  filter(str_detect(.i2,'EN_'))

df_emission3 <- rgdx.param(paste0(ddir,'emission_scenario.gdx'),'emission') %>% 
  mutate(Y5=as.numeric(as.character(Y5))) %>% 
  filter(MQ=='WOR',Y5<=2050) %>% 
  filter(.i2 %in% c('EN_INDCi2030_500f', 'EN_NPi2020_600', 'EN_NPi2020_700',
                    'EN_INDCi2030_800', 'EN_NPi2020_800', 'EN_INDCi2030_900',
                    'EN_NPi2020_900', 'EN_INDCi2030_1000'))

df <- bind_rows(df_emission,df_emission2)

g <- df_emission %>% 
  ggplot() +
  geom_path(aes(x=Y5,y=emission,color=.i2))
plot(g)
