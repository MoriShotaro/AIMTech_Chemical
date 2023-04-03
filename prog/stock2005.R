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
ddir <- paste0(root,'/data/stock2005/')
odir <- paste0(root,'/output/stock2005/')

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

prod2005 <- rgdx.param(paste0(ddir,'dem_chem_extension.gdx'),'serv_t') %>% 
  rename(R=1,I=2,L=3,H=4) %>% 
  filter(H==2005) %>%
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>% 
  select(-I,-L,-H)
shr_HVC <- rgdx.param(paste0(ddir,'HVC_feedshare.gdx'),'feedshare') %>% mutate(J='HVC') %>% rename(R=1,K=2,shr=3)
shr_MOH <- rgdx.param(paste0(ddir,'MOH_feedshare.gdx'),'feedshare') %>% mutate(J='MOH') %>% rename(R=1,K=2,shr=3)
shr_NH3 <- rgdx.param(paste0(ddir,'NH3_feedshare.gdx'),'feedshare') %>% mutate(J='NH3') %>% rename(R=1,K=2,shr=3)
shr_NH3U <- rgdx.param(paste0(ddir,'NH3U_feedshare.gdx'),'feedshare') %>% mutate(J='NH3U') %>% rename(R=1,K=2,shr=3)

shr_chem <- bind_rows(shr_HVC,shr_MOH,shr_NH3,shr_NH3U)

stock2005 <- prod2005 %>% 
  left_join(shr_chem) %>% 
  mutate(stock=value*shr) %>% 
  mutate(F=case_when(str_detect(K,'.{6,}O')~'OIL',
                     str_detect(K,'.{6,}C')~'COL',
                     str_detect(K,'.{6,}G')&J=='HVC'~'GAS',
                     str_detect(K,'.{6,}G')&J!='HVC'~'NGS'),
         L=paste0('N_',J,'_',F,'_Lv1')) %>% mutate(I='NEN') %>% 
  select(I,R,L,stock) %>% rename(value=stock) %>% pivot_wider(names_from=L,values_from=value,values_fill=0)

ls_stock <- wgdx.reshape(stock2005,symDim=3,symName="stock",tName='L',setsToo=FALSE)
wgdx.lst(gdxName=paste0(odir,"/chem_stock.gdx"),ls_stock,squeeze=FALSE)
