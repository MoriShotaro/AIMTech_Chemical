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
# ddir <- paste0(root,'/data/GEER/output/global2050/2303310138/gams_output/gdx_primary/')
ddir <- paste0(root,'/data/GEER/output/global2050CCU/2304030222/gams_output/gdx_primary/')
odir <- paste0(root,'/output/GEER/plot/')

# R33_R5 <- read.csv(paste0(ddir,'define/R33_R5.csv'),header=FALSE) %>% 
#   rename(R=1,R5=2)


## Input data -------------------------------------------------------------
# Load GDX file -----------------------------------------------------------

cpol <- c('Baseline','NPi500','NPi700','NPi1000')
# cpol <- c('Baseline','NPi500','NPi600','NPi700','NPi800','NPi900','NPi1000')
techpol <- c('Default','limCCS','limBio')
# techpol <- c('Default','Biofueloff','limCCS')

list_Sc <- expand.grid(cpol=cpol,techpol=techpol) %>% 
  mutate(Sc=paste0(cpol,'_',techpol)) %>% 
  mutate(Sc=str_remove_all(Sc,'_Default')) 

cpol_techpol <- list_Sc %>%
  filter(!str_detect(Sc,'Baseline_')&Sc!='NPi500_limCCS') %>% 
  select(Sc)

Sc <- cpol_techpol$Sc

vx_l <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'vx_l') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i) %>% 
    filter(I=='NEN')
} %>% bind_rows()

ve_l <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'ve_l') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i) %>% 
    filter(I=='NEN')
} %>% bind_rows()

vq_l <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'vq_l') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i) %>% 
    filter(I=='NEN')
} %>% bind_rows()

e_t <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'e_t') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i) %>% 
    filter(I=='NEN')
} %>% bind_rows()

gas_t1 <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'gas_t1') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i) %>% 
    filter(I=='NEN')
} %>% bind_rows() %>% 
  distinct(K,gas_t1)


# Model -------------------------------------------------------------------

VX1 <- vx_l %>% 
  group_by(L,Sc,H) %>% 
  summarise(vx_l=sum(vx_l)) %>% 
  filter(!str_detect(L,'NEN|SC_L_')) %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>% 
  mutate(J2=case_when(str_detect(L,'HVC')~'HVC',
                      str_detect(L,'MOH')~'Methanol',
                      str_detect(L,'NH3')~'Ammonia')) %>%
  mutate(F=case_when(str_detect(L,'BMS')~'BMS',
                     str_detect(L,'NGS')~'NGS',
                     str_detect(L,'COL')~'COL',
                     str_detect(L,'ELE')~'ELE',
                     str_detect(L,'OIL')&J!='HVC'~'OIL',
                     str_detect(L,'OIL')&J=='HVC'~'OLN',
                     str_detect(L,'GAS')~'OLE')) %>% 
  mutate(F2=case_when(str_detect(L,'BMSX')~'Biomass w/ CCS',
                      str_detect(L,'BMS')~'Biomass w/o CCS',
                      str_detect(L,'NGSX')~'Natural gas w/ CCS',
                      str_detect(L,'NGS')~'Natural gas w/o CCS',
                      str_detect(L,'COLX')~'Coal w/ CCS',
                      str_detect(L,'COL')~'Coal w/o CCS',
                      str_detect(L,'ELE')~'Electrolysis',
                      str_detect(L,'OIL')&J!='HVC'~'Oil product',
                      str_detect(L,'OIL')&J=='HVC'~'Oil product (Naphtha)',
                      str_detect(L,'GAS')~'Oil product (Ethane)')) %>% 
  mutate(J=factor(J,levels=c('HVC','MOH','NH3U','NH3')),
         J2=factor(J2,levels=c('HVC','Methanol','Ammonia')),
         F2=factor(F,levels=c('Electrolysis',
                              'Biomass w/ CCS',
                              'Biomass w/o CCS',
                              'Natural gas w/ CCS',
                              'Natural gas w/o CCS',
                              'Oil product (Naphtha)',
                              'Oil product (Ethane)',
                              'Oil product',
                              'Coal w/ CCS',
                              'Coal w/o CCS'))) %>% 
  left_join(list_Sc)

VX5 <- vx_l %>% 
  group_by(L,Sc,H) %>% 
  summarise(vx_l=sum(vx_l)) %>% 
  group_by(L,Sc) %>% 
  complete(H=c(2005:2050)) %>% 
  ungroup() %>% 
  mutate(Y5=5*floor(H/5-401)+2005) %>%
  replace_na(list(vx_l=0)) %>% 
  group_by(L,Sc,Y5) %>% 
  summarise(vx_l=mean(vx_l)) %>% 
  filter(!str_detect(L,'NEN|SC_L_')) %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>% 
  mutate(J2=case_when(str_detect(L,'HVC')~'HVC',
                      str_detect(L,'MOH')~'Methanol',
                      str_detect(L,'NH3')~'Ammonia')) %>% 
  mutate(F=case_when(str_detect(L,'BMS')~'BMS',
                     str_detect(L,'NGS')~'NGS',
                     str_detect(L,'COL')~'COL',
                     str_detect(L,'ELE')~'ELE',
                     str_detect(L,'OIL')&J!='HVC'~'OIL',
                     str_detect(L,'OIL')&J=='HVC'~'OLN',
                     str_detect(L,'GAS')~'OLE')) %>% 
  mutate(F2=case_when(str_detect(L,'BMSX')~'Biomass w/ CCS',
                      str_detect(L,'BMS')~'Biomass w/o CCS',
                      str_detect(L,'NGSX')~'Natural gas w/ CCS',
                      str_detect(L,'NGS')~'Natural gas w/o CCS',
                      str_detect(L,'COLX')~'Coal w/ CCS',
                      str_detect(L,'COL')~'Coal w/o CCS',
                      str_detect(L,'ELE')~'Electrolysis',
                      str_detect(L,'OIL')&J!='HVC'~'Oil product',
                      str_detect(L,'OIL')&J=='HVC'~'Oil product (Naphtha)',
                      str_detect(L,'GAS')~'Oil product (Ethane)')) %>% 
  mutate(J=factor(J,levels=c('HVC','MOH','NH3U','NH3')),
         J2=factor(J2,levels=c('HVC','Methanol','Ammonia')),
         F2=factor(F,levels=c('Electrolysis',
                              'Biomass w/ CCS',
                              'Biomass w/o CCS',
                              'Natural gas w/ CCS',
                              'Natural gas w/o CCS',
                              'Oil product (Naphtha)',
                              'Oil product (Ethane)',
                              'Oil product',
                              'Coal w/ CCS',
                              'Coal w/o CCS'))) %>% 
  left_join(list_Sc)

VE1 <- vx_l %>%
  left_join(e_t) %>%
  filter(!str_detect(L,'NEN|SC_L_'),!str_detect(K,'CCO2')) %>% 
  mutate(ve_l=-vx_l*e_t) %>% 
  select(R,L,H,Sc,K,ve_l) %>% 
  group_by(L,H,Sc,K) %>% 
  summarise(ve_l=sum(ve_l)) %>% 
  mutate(K2=case_when(str_detect(K,'BMS')~'Liquid biomass',
                      str_detect(K,'CRN')~'Solid biomass',
                      str_detect(K,'ELY')~'Electricity',
                      str_detect(K,'NGS')~'Natural gas',
                      str_detect(K,'OIL')~'Oil product',
                      str_detect(K,'COL')~'Coal',
                      str_detect(K,'OLN')~'Oil product (Naphtha)',
                      str_detect(K,'OLE')~'Oil product (Ethane)')) %>%
  mutate(E=case_when(str_detect(K,'.{3}F')~'Feedstock',
                     str_detect(K,'.{3}S')~'Feedstock',
                     str_detect(K,'.{3}P')~'Process',
                     str_detect(K,'.{3}C')~'CCUS',
                     TRUE~'Energy')) %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>% 
  mutate(J2=case_when(str_detect(L,'HVC')~'HVC',
                      str_detect(L,'MOH')~'Methanol',
                      str_detect(L,'NH3')~'Ammonia')) %>% 
  mutate(J=factor(J,levels=c('HVC','MOH','NH3U','NH3')),
         J2=factor(J2,levels=c('HVC','Methanol','Ammonia')),
         E=factor(E,levels=c('Feedstock','CCUS','Process','Energy')),
         K2=factor(K2,levels=c('Electricity',
                               'Liquid biomass',
                               'Solid biomass',
                               'Natural gas',
                               'Oil product (Naphtha)',
                               'Oil product (Ethane)',
                               'Oil product',
                               'Coal'))) %>% 
  left_join(list_Sc)

VE5 <- VE1 %>% 
  group_by(L,Sc,K) %>% 
  complete(H=c(2005:2050)) %>% 
  ungroup() %>% 
  mutate(Y5=5*floor(H/5-401)+2005) %>%
  replace_na(list(ve_l=0)) %>% 
  group_by(L,K,Y5,Sc) %>%
  summarise(ve_l=mean(ve_l)) %>% 
  mutate(K2=case_when(str_detect(K,'BMS')~'Liquid biomass',
                      str_detect(K,'CRN')~'Solid biomass',
                      str_detect(K,'ELY')~'Electricity',
                      str_detect(K,'NGS')~'Natural gas',
                      str_detect(K,'OIL')~'Oil product',
                      str_detect(K,'COL')~'Coal',
                      str_detect(K,'OLN')~'Oil product (Naphtha)',
                      str_detect(K,'OLE')~'Oil product (Ethane)')) %>% 
  mutate(E=case_when(str_detect(K,'.{3}F')~'Feedstock',
                     str_detect(K,'.{3}S')~'Feedstock',
                     str_detect(K,'.{3}P')~'Process',
                     str_detect(K,'.{3}C')~'CCUS',
                     TRUE~'Energy')) %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>% 
  mutate(J2=case_when(str_detect(L,'HVC')~'HVC',
                      str_detect(L,'MOH')~'Methanol',
                      str_detect(L,'NH3')~'Ammonia')) %>% 
  mutate(J=factor(J,levels=c('HVC','MOH','NH3U','NH3')),
         J2=factor(J2,levels=c('HVC','Methanol','Ammonia')),
         E=factor(E,levels=c('Feedstock','CCUS','Process','Energy')),
         K2=factor(K2,levels=c('Electricity',
                               'Liquid biomass',
                               'Solid biomass',
                               'Natural gas',
                               'Oil product (Naphtha)',
                               'Oil product (Ethane)',
                               'Oil product',
                               'Coal'))) %>% 
  left_join(list_Sc)

VQ1 <- VE1 %>% 
  left_join(gas_t1) %>% 
  filter(gas_t1>0) %>% 
  mutate(vq_l=ve_l*gas_t1/10^3) %>% # kt -> Mt
  select(L,H,Sc,K,vq_l) %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>% 
  mutate(J2=case_when(str_detect(L,'HVC')~'HVC',
                      str_detect(L,'MOH')~'Methanol',
                      str_detect(L,'NH3')~'Ammonia')) %>% 
  mutate(E=case_when(str_detect(K,'.{3}F')~'Feedstock',
                     str_detect(K,'.{3}S')~'Feedstock',
                     str_detect(K,'.{3}P')~'Process',
                     str_detect(K,'.{3}C')~'CCUS',
                     TRUE~'Energy')) %>% 
  mutate(K2=case_when(str_detect(K,'BMS')~'Liquid biomass',
                      str_detect(K,'CRN')~'Solid biomass',
                      str_detect(K,'ELY')~'Electricity',
                      str_detect(K,'NGS')~'Natural gas',
                      str_detect(K,'OIL')~'Oil product',
                      str_detect(K,'COL')~'Coal',
                      str_detect(K,'OLN')~'Oil product (Naphtha)',
                      str_detect(K,'OLE')~'Oil product (Ethane)')) %>% 
  mutate(J=factor(J,levels=c('HVC','MOH','NH3U','NH3')),
         J2=factor(J2,levels=c('HVC','Methanol','Ammonia')),
         E=factor(E,levels=c('Feedstock','CCUS','Process','Energy')),
         K2=factor(K2,levels=c('Electricity',
                               'Liquid biomass',
                               'Solid biomass',
                               'Natural gas',
                               'Oil product (Naphtha)',
                               'Oil product (Ethane)',
                               'Oil product',
                               'Coal'))) %>% 
  left_join(list_Sc)

VQ5 <- VQ1 %>% 
  group_by(L,Sc,K) %>% 
  complete(H=c(2005:2050)) %>% 
  mutate(Y5=5*floor(H/5-401)+2005) %>%
  replace_na(list(vq_l=0)) %>% 
  group_by(L,K,Sc,Y5) %>% 
  summarise(vq_l=mean(vq_l)) %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>% 
  mutate(J2=case_when(str_detect(L,'HVC')~'HVC',
                      str_detect(L,'MOH')~'Methanol',
                      str_detect(L,'NH3')~'Ammonia')) %>% 
  mutate(E=case_when(str_detect(K,'.{3}F')~'Feedstock',
                     str_detect(K,'.{3}P')~'Process',
                     str_detect(K,'.{3}C')~'CCUS',
                     TRUE~'Energy')) %>% 
  mutate(K2=case_when(str_detect(K,'BMS')~'Liquid biomass',
                      str_detect(K,'CRN')~'Solid biomass',
                      str_detect(K,'ELY')~'Electricity',
                      str_detect(K,'NGS')~'Natural gas',
                      str_detect(K,'OIL')~'Oil product',
                      str_detect(K,'COL')~'Coal',
                      str_detect(K,'OLN')~'Oil product (Naphtha)',
                      str_detect(K,'OLE')~'Oil product (Ethane)')) %>% 
  mutate(J=factor(J,levels=c('HVC','MOH','NH3U','NH3')),
         J2=factor(J2,levels=c('HVC','Methanol','Ammonia')),
         E=factor(E,levels=c('Feedstock','CCUS','Process','Energy')),
         K2=factor(K2,levels=c('Electricity',
                               'Liquid biomass',
                               'Solid biomass',
                               'Natural gas',
                               'Oil product (Naphtha)',
                               'Oil product (Ethane)',
                               'Oil product',
                               'Coal'))) %>% 
  left_join(list_Sc)


# Plot theme --------------------------------------------------------------

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


## Main -------------------------------------------------------------------

# Historical---------------------------------------------------------------
# Emission 2005 - 2020-----------------------------------------------------

# IEA,2022 Direct CO2 emissions from primary chemical production in MtCO2/yr
emission_historical <- data.frame(H=c(2010,2015,2020),
                                  NH3=c(357.56,407.48,408.43),
                                  MOH=c(76.84,159.28,225.56),
                                  HVC=c(190.14,219.07,248.30)) %>% mutate(TOTAL=HVC+MOH+NH3)

# Total
g_emi_J <- VQ1 %>% 
  filter(H<=2020,Sc=='Baseline') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vq_l,fill=E),stat='identity') +
  geom_point(data=emission_historical,aes(x=H,y=TOTAL),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Emission from primary chemical production (MtCO2/yr)') +
  MyTheme
plot(g_emi_J)
ggsave(paste0(odir,'historical/emission_total.png'),g_emi_J)

# HVC 2005 - 2020
g_emi_HVC <- VQ1 %>% 
  filter(H<=2020,J2=='HVC',Sc=='Baseline') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vq_l,fill=E),stat='identity') +
  geom_point(data=emission_historical,aes(x=H,y=HVC),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Emission from HVC production (MtCO2/yr)') +
  MyTheme
plot(g_emi_HVC)
ggsave(paste0(odir,'historical/emission_HVC.png'),g_emi_HVC)

# Ammonia 2005 - 2020
g_emi_NH3 <- VQ1 %>% 
  filter(H<=2020,J2=='Ammonia',Sc=='Baseline') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vq_l,fill=E),stat='identity') +
  geom_point(data=emission_historical,aes(x=H,y=NH3),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Emission from ammonia production (MtCO2/yr)') +
  MyTheme
plot(g_emi_NH3)
ggsave(paste0(odir,'historical/emission_NH3.png'),g_emi_NH3)

# Methanol 2005 - 2020
g_emi_MOH <- VQ1 %>% 
  filter(H<=2020,J2=='Methanol',Sc=='Baseline') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vq_l,fill=E),stat='identity') +
  geom_point(data=emission_historical,aes(x=H,y=MOH),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Emission from methanol production (MtCO2/yr)') +
  MyTheme
plot(g_emi_MOH)
ggsave(paste0(odir,'historical/emission_MOH.png'),g_emi_MOH)


# Production 2005 - 2020---------------------------------------------------

# Load historical production data
load(paste0(root,'/data/GEER/historical/RData/USGS_NH3.RData'))
load(paste0(root,'/data/GEER/historical/RData/NEDO_HVC.RData'))
load(paste0(root,'/data/GEER/historical/RData/MMSA_MOH.RData'))

# HVC 2005 - 2020
g_pro_HVC_h <- VX1 %>%
  filter(H<=2020,J2=='HVC',Sc=='Baseline') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vx_l,fill=F2),stat='identity') +
  geom_path(data=NEDO_HVC,aes(x=Y,y=value/1000),color='indianred1',size=1) +
  geom_point(data=NEDO_HVC,aes(x=Y,y=value/1000),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='HVC production (Mt/yr)') +
  MyTheme
plot(g_pro_HVC_h)
ggsave(paste0(odir,'historical/production_HVC.png'),g_pro_HVC_h)

# NH3 2005 - 2020
g_pro_NH3_h <- VX1 %>%
  filter(H<=2020,J2=='Ammonia',Sc=='Baseline') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vx_l,fill=F2),stat='identity') +
  geom_path(data=USGS_NH3,aes(x=Year,y=value),color='indianred1',size=1) +
  geom_point(data=USGS_NH3,aes(x=Year,y=value),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Ammonia production (Mt/yr)') +
  MyTheme
plot(g_pro_NH3_h)
ggsave(paste0(odir,'historical/production_NH3.png'),g_pro_NH3_h)

# MOH 2005 - 2020
g_pro_MOH_h <- VX1 %>%
  filter(H<=2020,J2=='Methanol',Sc=='Baseline') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vx_l,fill=F2),stat='identity') +
  geom_path(data=MMSA_MOH,aes(x=Y,y=value/1000),color='indianred1',size=1) +
  geom_point(data=MMSA_MOH,aes(x=Y,y=value/1000),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Methanol production (Mt/yr)') +
  MyTheme
plot(g_pro_MOH_h)
ggsave(paste0(odir,'historical/production_MOH.png'),g_pro_MOH_h)


# Energy consumption 2005 - 2020 ------------------------------------------

# IEA, 2022 Process energy for primary chemical production
proene_historical <- data.frame(H=c(2010,2015,2020),
                                value=c(1.7+0.1+3.7+0.4+0.0,
                                        2.6+0.1+4.0+0.5+0.0,
                                        3.0+0.1+4.7+0.6+0.0))

# Total -energy
g_ene_K <- VE1 %>%
  filter(H<=2020,Sc=='Baseline') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=ve_l/1000,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Total energy consumption (EJ/yr)',
       title='Total energy consumption for primary chemical production') +
  MyTheme
plot(g_ene_K)
ggsave(paste0(odir,'historical/energy_total_K.png'),g_ene_K)

# Feedstock energy
g_ene_K <- VE1 %>%
  filter(H<=2020,Sc=='Baseline',E%in%c('Feedstock','Process')) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=ve_l/1000,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Feedstock energy consumption (EJ/yr)',
       title='Feedstock energy consumption for primary chemical production') +
  MyTheme
plot(g_ene_K)
ggsave(paste0(odir,'historical/energy_feedstock_K.png'),g_ene_K)

# Process energy
g_ene_p_K <- VE1 %>%
  filter(H<=2020,Sc=='Baseline',E=='Energy') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=ve_l/1000,fill=K2),stat='identity') +
  geom_point(data=proene_historical,aes(x=H,y=value),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Process energy consumption (EJ/yr)',
       title='Process energy consumption for primary chemical production') +
  MyTheme
plot(g_ene_p_K)
ggsave(paste0(odir,'energy/energy_process_K.png'),g_ene_p_K)


# Production 2020 - 2050 --------------------------------------------------

# HVC 2020 - 2050
g_pro_HVC <- VX5 %>%
  filter(Y5>=2020,J2=='HVC') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=F2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='HVC production (Mt/yr)',
       title='HVC production') +
  MyTheme
plot(g_pro_HVC)
ggsave(paste0(odir,'production/production_HVC.png'),g_pro_HVC)

# NH3 2020 - 2050
g_pro_NH3 <- VX5 %>%
  filter(Y5>=2020,J2=='Ammonia') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=F2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Ammonia production (Mt/yr)',
       title='Ammonia production') +
  MyTheme
plot(g_pro_NH3)
ggsave(paste0(odir,'production/production_NH3.png'),g_pro_NH3)

# MOH 2020 - 2050
g_pro_MOH <- VX5 %>%
  filter(Y5>=2020,J2=='Methanol') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=F2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Methanol production (Mt/yr)',
       title='Methanol production') +
  MyTheme
plot(g_pro_MOH)
ggsave(paste0(odir,'production/production_MOH.png'),g_pro_MOH)


# Emission 2020 - 2050 ----------------------------------------------------

# Total -flow
g_emi_E <- VQ5 %>% 
  filter(K!='CRN',E!='CCUS') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vq_l,fill=E),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Emission from primary chemical production (MtCO2/yr)',
       title='Emission from primary chemical production by source') +
  MyTheme
plot(g_emi_E)
ggsave(paste0(odir,'emission/emission_total_E.png'),g_emi_E)

# Total -product
g_emi_J <- VQ5 %>% 
  filter(E!='CCUS',K!='CRN') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vq_l,fill=J2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Emission from primary chemical production (MtCO2/yr)',
       title='Emission from primary chemical production by product') +
  MyTheme
plot(g_emi_J)
ggsave(paste0(odir,'emission/emission_total_J.png'),g_emi_J)

# HVC 2005 - 2050
g_emi_HVC <- VQ5 %>% 
  filter(J2=='HVC',E!='CCUS',K!='CRN') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vq_l,fill=E),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Emission from HVC production (MtCO2/yr)',
       title='Emission from HVC production by source') +
  MyTheme
plot(g_emi_HVC)
ggsave(paste0(odir,'emission/emission_HVC.png'),g_emi_HVC)

# Ammonia 2020 - 2050
g_emi_NH3 <- VQ5 %>% 
  filter(J2=='Ammonia',E!='CCUS',K!='CRN') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vq_l,fill=E),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Emission from ammonia production (MtCO2/yr)',
       title='Emission from Ammonia production by source') +
  MyTheme
plot(g_emi_NH3)
ggsave(paste0(odir,'emission/emission_NH3.png'),g_emi_NH3)

# Methanol 2020 - 2050
g_emi_MOH <- VQ5 %>% 
  filter(J2=='Methanol',E!='CCUS',K!='CRN') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vq_l,fill=E),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Emission from methanol production (MtCO2/yr)',
       title='Emission from Methanol production by source') +
  MyTheme
plot(g_emi_MOH)
ggsave(paste0(odir,'emission/emission_MOH.png'),g_emi_MOH)


# Energy consumption ------------------------------------------------------

# Total -flow
g_ene_E <- VE5 %>%
  ggplot() +
  geom_bar(aes(x=Y5,y=ve_l,fill=E),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Total energy consumption (PJ/yr)',
       title='Total energy consumption for primary chemical production') +
  MyTheme
plot(g_ene_E)
ggsave(paste0(odir,'energy/energy_total_E.png'),g_ene_E)

# Total -product
g_ene_J <- VE5 %>%
  ggplot() +
  geom_bar(aes(x=Y5,y=ve_l,fill=J2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Total energy consumption (PJ/yr)',
       title='Total energy consumption for primary chemical production by product') +
  MyTheme
plot(g_ene_J)
ggsave(paste0(odir,'energy/energy_total_J.png'),g_ene_J)

# Total -energy
g_ene_K <- VE5 %>%
  ggplot() +
  geom_bar(aes(x=Y5,y=ve_l,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Total energy consumption (PJ/yr)',
       title='Total energy consumption for primary chemical production by energy') +
  MyTheme
plot(g_ene_K)
ggsave(paste0(odir,'energy/energy_total_K.png'),g_ene_K)

# Feedstock -energy
g_ene_f_K <- VE5 %>%
  filter(E%in%c('Process','Feedstock')) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=ve_l,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Feedstock energy consumption (PJ/yr)',
       title='Feedstock energy consumption for primary chemical production by energy') +
  MyTheme
plot(g_ene_f_K)
ggsave(paste0(odir,'energy/energy_feedstock_K.png'),g_ene_f_K)

# Feedstock -product
g_ene_f_J <- VE5 %>%
  filter(E%in%c('Process','Feedstock')) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=ve_l,fill=J),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Feedstock energy consumption (PJ/yr)',
       title='Feedstock energy consumption for primary chemical production by product') +
  MyTheme
plot(g_ene_f_J)
ggsave(paste0(odir,'energy/energy_feedstock_J.png'),g_ene_f_J)

# HVC 2005 - 2050
g_ene_HVC <- VE5 %>%
  filter(J2=='HVC') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=ve_l,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Energy consumption (PJ/yr)',
       title='Total energy consumption for HVC production') +
  MyTheme
plot(g_ene_HVC)
ggsave(paste0(odir,'energy/energy_feedstock_HVC.png'),g_ene_HVC)

# HVC feedstock 2005 - 2050
g_ene_f_HVC <- VE5 %>%
  filter(J2=='HVC',E%in%c('Process','Feedstock')) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=ve_l,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Energy consumption (PJ/yr)',
       title='Feedstock energy consumption for HVC production') +
  MyTheme
plot(g_ene_f_HVC)
ggsave(paste0(odir,'energy/energy_feedstock_HVC.png'),g_ene_f_HVC)

# NH3 2005 - 2050
g_ene_NH3 <- VE5 %>%
  filter(J2=='Ammonia') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=ve_l,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Energy consumption (PJ/yr)',
       title='Total energy consumption for Ammonia production') +
  MyTheme
plot(g_ene_NH3)
ggsave(paste0(odir,'energy/energy_feedstock_NH3.png'),g_ene_NH3)

# NH3 feedstock 2005 - 2050
g_ene_f_NH3 <- VE5 %>%
  filter(J2=='Ammonia',E%in%c('Process','Feedstock')) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=ve_l,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Energy consumption (PJ/yr)',
       title='Feedstock energy consumption for Ammonia production') +
  MyTheme
plot(g_ene_f_NH3)
ggsave(paste0(odir,'energy/energy_feedstock_NH3.png'),g_ene_f_NH3)

# MOH 2005 - 2050
g_ene_MOH <- VE5 %>%
  filter(J2=='Methanol') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=ve_l,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Energy consumption (PJ/yr)',
       title='Total energy consumption for Methanol production') +
  MyTheme
plot(g_ene_MOH)
ggsave(paste0(odir,'energy/energy_feedstock_MOH.png'),g_ene_MOH)

# MOH feedstock 2005 - 2050
g_ene_f_MOH <- VE5 %>%
  filter(J2=='Methanol',E%in%c('Process','Feedstock')) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=ve_l,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y='Energy consumption (PJ/yr)',
       title='Feedstock energy consumption for Methanol production') +
  MyTheme
plot(g_ene_f_MOH)
ggsave(paste0(odir,'energy/energy_feedstock_MOH.png'),g_ene_f_MOH)
