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
ddir <- paste0(root,'/data/GEER/output/global2050CCU/2304042310/gams_output/gdx_primary/')
ddir2 <- paste0(root,'/data/GEER/output/global2050CCU/2304042310/main/')
odir <- paste0(root,'/output/GEER/plot/0405/')

# R33_R5 <- read.csv(paste0(ddir,'define/R33_R5.csv'),header=FALSE) %>% 
#   rename(R=1,R5=2)


## Input data -------------------------------------------------------------
# Load GDX file -----------------------------------------------------------

cpol <- c('Baseline','NPi500','NPi700','NPi1000')
# cpol <- c('Baseline','NPi500')
# cpol <- c('Baseline','NPi500','NPi600','NPi700','NPi800','NPi900','NPi1000')
techpol <- c('Default','limCCS','Biofueloff')
# techpol <- c('Default')

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
} %>% bind_rows()

ele_emf <- rgdx.param(paste0(ddir2,'merged_output.gdx'),'data_all') %>% 
  filter(Sv%in%c('Sec_Ene_Ele','Emi_CO2_Ene_Sup_Ele')) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Sy=as.numeric(Sy)) %>% 
  pivot_wider(names_from=Sv,values_from=data_all) %>% 
  mutate(gas_t1=Emi_CO2_Ene_Sup_Ele/Sec_Ene_Ele) %>% # Mt-CO2/EJ -> kt-CO2/PJ
  select(Sc,Sr,Sy,gas_t1) %>%
  mutate(K='ELYI',I='NEN',M='CO2e') %>% 
  rename(R=Sr,H=Sy) %>% select(R,I,K,M,H,gas_t1,Sc)

gas_t <- bind_rows(gas_t1,ele_emf) %>% 
  group_by(R,I,K,M,H,Sc) %>% 
  summarise(gas_t1=mean(gas_t1))

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
  mutate(F2=case_when(str_detect(L,'BMSX')~'Biomass w/ CCUS',
                      str_detect(L,'BMS')~'Biomass w/o CCUS',
                      str_detect(L,'NGSX')~'Natural gas w/ CCUS',
                      str_detect(L,'NGS')~'Natural gas w/o CCUS',
                      str_detect(L,'COLX')~'Coal w/ CCUS',
                      str_detect(L,'COL')~'Coal w/o CCUS',
                      str_detect(L,'ELE')~'Electrolysis',
                      str_detect(L,'OIL')&J!='HVC'~'Oil product',
                      str_detect(L,'OIL')&J=='HVC'~'Oil product (Naphtha)',
                      str_detect(L,'GAS')~'Oil product (Ethane)')) %>% 
  mutate(J=factor(J,levels=c('HVC','MOH','NH3U','NH3')),
         J2=factor(J2,levels=c('HVC','Methanol','Ammonia')),
         F2=factor(F2,levels=c('Electrolysis',
                               'Biomass w/ CCUS',
                               'Biomass w/o CCUS',
                               'Natural gas w/ CCUS',
                               'Natural gas w/o CCUS',
                               'Oil product (Naphtha)',
                               'Oil product (Ethane)',
                               'Oil product',
                               'Coal w/ CCUS',
                               'Coal w/o CCUS'))) %>% 
  left_join(list_Sc) %>% 
  mutate(techpol=as.character(techpol)) %>% 
  mutate(techpol=recode(techpol,'Biofueloff'='limBio')) %>% 
  mutate(cpol=str_remove(cpol,'NPi')) %>% 
  mutate(cpol=case_when(cpol!='Baseline'~paste0(cpol,'C'),
                        TRUE~cpol),
         techpol=case_when(techpol=='limCCS'~'NoCCS',
                        TRUE~techpol)) %>% 
  mutate(techpol=factor(techpol,levels=c('Default','NoCCS','limBio')))

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
  mutate(F2=case_when(str_detect(L,'BMSX')~'Biomass w/ CCUS',
                      str_detect(L,'BMS')~'Biomass w/o CCUS',
                      str_detect(L,'NGSX')~'Natural gas w/ CCUS',
                      str_detect(L,'NGS')~'Natural gas w/o CCUS',
                      str_detect(L,'COLX')~'Coal w/ CCUS',
                      str_detect(L,'COL')~'Coal w/o CCUS',
                      str_detect(L,'ELE')~'Electrolysis',
                      str_detect(L,'OIL')&J!='HVC'~'Oil product',
                      str_detect(L,'OIL')&J=='HVC'~'Oil product (Naphtha)',
                      str_detect(L,'GAS')~'Oil product (Ethane)')) %>% 
  mutate(J=factor(J,levels=c('HVC','MOH','NH3U','NH3')),
         J2=factor(J2,levels=c('HVC','Methanol','Ammonia')),
         F2=factor(F2,levels=c('Electrolysis',
                               'Biomass w/ CCUS',
                               'Biomass w/o CCUS',
                               'Natural gas w/ CCUS',
                               'Natural gas w/o CCUS',
                               'Oil product (Naphtha)',
                               'Oil product (Ethane)',
                               'Oil product',
                               'Coal w/ CCUS',
                               'Coal w/o CCUS'))) %>% 
  left_join(list_Sc) %>% 
  mutate(techpol=as.character(techpol)) %>% 
  mutate(techpol=recode(techpol,'Biofueloff'='limBio')) %>% 
  mutate(cpol=str_remove(cpol,'NPi')) %>% 
  mutate(cpol=case_when(cpol!='Baseline'~paste0(cpol,'C'),
                        TRUE~cpol),
         techpol=case_when(techpol=='limCCS'~'NoCCS',
                           TRUE~techpol)) %>% 
  mutate(techpol=factor(techpol,levels=c('Default','NoCCS','limBio')))


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
                     str_detect(K,'ELY')~'Electricity (indirect)',
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
         E=factor(E,levels=c('Feedstock','Electricity (indirect)','CCUS','Process','Energy')),
         K2=factor(K2,levels=c('Electricity',
                               'Liquid biomass',
                               'Solid biomass',
                               'Natural gas',
                               'Oil product (Naphtha)',
                               'Oil product (Ethane)',
                               'Oil product',
                               'Coal'))) %>% 
  left_join(list_Sc) %>% 
  mutate(techpol=as.character(techpol)) %>% 
  mutate(techpol=recode(techpol,'Biofueloff'='limBio')) %>% 
  mutate(cpol=str_remove(cpol,'NPi')) %>% 
  mutate(cpol=case_when(cpol!='Baseline'~paste0(cpol,'C'),
                        TRUE~cpol),
         techpol=case_when(techpol=='limCCS'~'NoCCS',
                           TRUE~techpol)) %>% 
  mutate(techpol=factor(techpol,levels=c('Default','NoCCS','limBio')))


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
                     str_detect(K,'ELY')~'Electricity (indirect)',
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
         E=factor(E,levels=c('Feedstock','Electricity (indirect)','CCUS','Process','Energy')),
         K2=factor(K2,levels=c('Electricity',
                               'Liquid biomass',
                               'Solid biomass',
                               'Natural gas',
                               'Oil product (Naphtha)',
                               'Oil product (Ethane)',
                               'Oil product',
                               'Coal'))) %>% 
  left_join(list_Sc) %>% 
  mutate(techpol=as.character(techpol)) %>% 
  mutate(techpol=recode(techpol,'Biofueloff'='limBio')) %>% 
  mutate(cpol=str_remove(cpol,'NPi')) %>% 
  mutate(cpol=case_when(cpol!='Baseline'~paste0(cpol,'C'),
                        TRUE~cpol),
         techpol=case_when(techpol=='limCCS'~'NoCCS',
                           TRUE~techpol)) %>% 
  mutate(techpol=factor(techpol,levels=c('Default','NoCCS','limBio')))


VQ1 <- vx_l %>%
  left_join(e_t) %>%
  filter(!str_detect(L,'NEN|SC_L_'),!str_detect(K,'CCO2')) %>% 
  mutate(ve_l=-vx_l*e_t) %>% 
  left_join(gas_t) %>% 
  filter(gas_t1>0) %>% 
  mutate(vq_l=ve_l*gas_t1/10^3) %>% # kt -> Mt
  group_by(L,H,Sc,K) %>% 
  summarise(vq_l=sum(vq_l)) %>% 
  select(L,H,Sc,K,vq_l) %>% 
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
                     str_detect(K,'ELY')~'Electricity (indirect)',
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
         E=factor(E,levels=c('Feedstock','Electricity (indirect)','CCUS','Process','Energy')),
         K2=factor(K2,levels=c('Electricity',
                               'Liquid biomass',
                               'Solid biomass',
                               'Natural gas',
                               'Oil product (Naphtha)',
                               'Oil product (Ethane)',
                               'Oil product',
                               'Coal'))) %>% 
  left_join(list_Sc) %>% 
  mutate(techpol=as.character(techpol)) %>% 
  mutate(techpol=recode(techpol,'Biofueloff'='limBio')) %>% 
  mutate(cpol=str_remove(cpol,'NPi')) %>% 
  mutate(cpol=case_when(cpol!='Baseline'~paste0(cpol,'C'),
                        TRUE~cpol),
         techpol=case_when(techpol=='limCCS'~'NoCCS',
                           TRUE~techpol)) %>% 
  mutate(techpol=factor(techpol,levels=c('Default','NoCCS','limBio')))


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
                     str_detect(K,'.{3}S')~'Feedstock',
                     str_detect(K,'.{3}P')~'Process',
                     str_detect(K,'.{3}C')~'CCUS',
                     str_detect(K,'ELY')~'Electricity (indirect)',
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
         E=factor(E,levels=c('Feedstock','Electricity (indirect)','CCUS','Process','Energy')),
         K2=factor(K2,levels=c('Electricity',
                               'Liquid biomass',
                               'Solid biomass',
                               'Natural gas',
                               'Oil product (Naphtha)',
                               'Oil product (Ethane)',
                               'Oil product',
                               'Coal'))) %>% 
  left_join(list_Sc) %>% 
  mutate(techpol=as.character(techpol)) %>% 
  mutate(techpol=recode(techpol,'Biofueloff'='limBio')) %>% 
  mutate(cpol=str_remove(cpol,'NPi')) %>% 
  mutate(cpol=case_when(cpol!='Baseline'~paste0(cpol,'C'),
                        TRUE~cpol),
         techpol=case_when(techpol=='limCCS'~'NoCCS',
                           TRUE~techpol)) %>% 
  mutate(techpol=factor(techpol,levels=c('Default','NoCCS','limBio')))



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
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    axis.ticks.length=unit(0.15,"cm")
  )


## Main -------------------------------------------------------------------

# Figure 1 Emission -------------------------------------------------------

Fig1 <- rgdx.param(paste0(ddir2,'merged_output.gdx'),'data_all') %>% 
  filter(Sv%in%c('Emi_CO2_Ene_and_Ind_Pro'),Sr=='World',Sc%in%c('Baseline','NPi1000','NPi700','NPi500')) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Sc=str_remove(Sc,'NPi')) %>% 
  mutate(Sc=case_when(Sc!='Baseline'~paste0(Sc,'C'),
                        TRUE~Sc)) %>% 
  mutate(Sc=factor(Sc,levels=c('Baseline','1000C','700C','500C'))) %>% 
  mutate(Sy=as.numeric(Sy))

g_Fig1 <- Fig1 %>% 
  ggplot() +
  geom_path(aes(x=Sy,y=data_all/1000,linetype=Sc)) +
  labs(y=expression(paste({CO[2]},' emissions (Gt',{CO[2]},'  ',{yr^-1},')'))) +
  scale_y_continuous(limits=c(0,NA)) +
  scale_linetype_manual(values=c('dotted','longdash','twodash','solid')) +
  MyTheme +
  theme(legend.position = c(0.85,0.6))
plot(g_Fig1)
ggsave(paste0(odir,'Fig1.png'),g_Fig1,width=4,height=3.5)


# Figure 2 Historical emission --------------------------------------------

# IEA, 2022 Energy-related CO2 emissions from chemical sector
emission_historical <- data.frame(H=c(2010,2015,2020),
                                  NH3=c(357.56,407.48,408.43),
                                  MOH=c(76.84,159.28,225.56),
                                  HVC=c(190.14,219.07,248.30)) %>% mutate(TOTAL=HVC+MOH+NH3)

# IEA, 2022 Process energy for primary chemical production
proene_historical <- data.frame(H=c(2010,2015,2020),
                                value=c(1.7+0.1+3.7+0.4+0.0,
                                        2.6+0.1+4.0+0.5+0.0,
                                        3.0+0.1+4.7+0.6+0.0))

# IEA, 2022 Energy-related CO2 emissions from chemical sector
emission_historical_chem_e <- read_csv(paste0(root,'/data/GEER/historical/Emission/IEA_GHG_Energy_2022.csv')) %>% 
  slice(10) %>% 
  pivot_longer(cols=-c(TIME),names_to='H',names_transform=as.numeric,values_to='value') %>% 
  select(H,value) %>% filter(H>=2005)

# EDGAR, 2022 Process CO2 emissions from chemical sector
emission_historical_chem_p <- rgdx.param(paste0(root,'/data/GEER/historical/Emission/edgar2.gdx'),'Pedgar60')

# Total
Fig2a <- VQ1 %>%
  filter(H<=2020,Sc=='Baseline',E%in%c('Energy','Process')) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vq_l/1000,fill=J2),stat='identity') +
  geom_point(data=emission_historical,aes(x=H,y=TOTAL/1000),color='indianred2',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_manual(values=c('lightsteelblue','lightsalmon','darkolivegreen2'),
                    guide=guide_legend(ncol=2)) +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y=expression(paste('Direct ',{CO[2]},' emissions (Gt-',{CO[2]},'  ',{yr^-1},')'))) +
  MyTheme +
  theme(legend.position = 'bottom',
        strip.text.x = element_blank(),
        strip.text.y = element_blank())
plot(Fig2a)
ggsave(paste0(odir,'Fig2a.png'),width=5,height=3.5,Fig2a)

Fig2b <- VE1 %>%
  filter(H<=2020,Sc=='Baseline',E%in%c('Energy','Electricity (indirect)')) %>% 
  mutate(K2=recode(K2,
                   'Oil product (Ethane)'='Oil',
                   'Oil product'='Oil',
                   'Natural gas'='Gas')) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=ve_l/1000,fill=K2),stat='identity') +
  geom_point(data=proene_historical,aes(x=H,y=value),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_manual(guide=guide_legend(ncol=2),
                    values=c('lightsteelblue',
                             'lightgoldenrod',
                             'sandybrown',
                             'grey70')) +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y=expression(paste('Process energy consumption (EJ ',{yr^-1},')'))) +
  MyTheme +
  theme(legend.position = 'bottom',
        strip.text.x = element_blank(),
        strip.text.y = element_blank())
plot(Fig2b)

Fig2 <- Fig2a + Fig2b + plot_annotation(tag_levels='a')
plot(Fig2)
ggsave(paste0(odir,'Fig2.png'),width=6,height=4.5,Fig2)


# Figure 3 Primary chemical production ------------------------------------

# HVC 2020 - 2050 -----
Fig3a.1 <- VX5 %>%
  filter(J2=='HVC',techpol=='Default',Sc!=c('Baseline'),Y5%in%c(2020,2030,2040,2050)) %>% 
  mutate(cpol=factor(cpol,levels=c('1000C','700C','500C'))) %>% 
  mutate(F2=as.character(F2)) %>% 
  mutate(F3=case_when(str_detect(F2,'Biomass')~F2,
                      str_detect(L,'_.{3}X')~'Fossil w/ CCUS',
                      F2=='Electrolysis'~F2,
                      TRUE~'Fossil w/o CCUS')) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=F3),stat='identity') +
  scale_fill_manual(guide=guide_legend(nrow=1),
                    values=c('darkolivegreen2',
                             'grey50',
                             'grey70')) +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y=expression(paste('HVC production (Mt ',{yr^-1},')')),
       subtitle='Default',
       title='a') +
  MyTheme +
  theme(legend.position = 'none',
        strip.text.y = element_blank(),
        axis.title.y=element_text(size = 11,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")))
plot(Fig3a.1)

Fig3a.2 <- VX5 %>%
  filter(Y5==2050,J2=='HVC',Sc!=c('Baseline')) %>% 
  mutate(cpol=factor(cpol,levels=c('1000C','700C','500C'))) %>% 
  mutate(F2=as.character(F2)) %>% 
  mutate(F3=case_when(str_detect(F2,'Biomass')~F2,
                      str_detect(L,'_.{3}X')~'Fossil w/ CCUS',
                      F2=='Electrolysis'~F2,
                      TRUE~'Fossil w/o CCUS')) %>% 
  group_by(Sc,F3,techpol,cpol) %>%
  summarise(vx_l=sum(vx_l)) %>% 
  group_by(Sc) %>%
  mutate(vx_ls=vx_l*100/sum(vx_l)) %>%
  ggplot() +
  geom_bar(aes(x=techpol,y=vx_ls,fill=F3),stat='identity') +
  scale_fill_manual(guide=guide_legend(nrow=1),
                    values=c('darkolivegreen2',
                             'grey50',
                             'grey70')) +
  scale_y_continuous(position='right') +
  facet_wrap(vars(cpol)) +
  labs(y=expression(paste('Share in HVC production (%)')),
       subtitle='2050') +
  MyTheme +
  theme(legend.position = 'none',
        axis.title.y=element_text(size = 11,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")))
plot(Fig3a.2)

Fig3a <- Fig3a.1 + Fig3a.2 + plot_layout(widths=c(4,3))
plot(Fig3a)

ggsave(paste0(odir,'Fig3a.png'),Fig3a,width=7,height=4)


# Methanol 2020 - 2050 -----
Fig3b.1 <- VX5 %>%
  filter(J2=='Methanol',techpol=='Default',Sc!=c('Baseline'),Y5%in%c(2020,2030,2040,2050)) %>% 
  mutate(cpol=factor(cpol,levels=c('1000C','700C','500C'))) %>% 
  mutate(F2=as.character(F2)) %>% 
  mutate(F3=case_when(str_detect(F2,'Biomass')~F2,
                      str_detect(L,'_.{3}X')~'Fossil w/ CCUS',
                      F2=='Electrolysis'~F2,
                      TRUE~'Fossil w/o CCUS')) %>% 
  mutate(F3=factor(F3,levels=c('Fossil w/ CCUS',
                               'Fossil w/o CCUS'))) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=F3),stat='identity') +
  scale_fill_manual(guide=guide_legend(nrow=1),
                    values=c('grey50',
                             'grey70')) +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y=expression(paste('Methanol production (Mt ',{yr^-1},')')),
       title='b',
       subtitle='Default') +
  MyTheme +
  theme(legend.position = 'none',
        strip.text.y = element_blank(),
        axis.title.y=element_text(size = 11,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")))
plot(Fig3b.1)

Fig3b.2 <- VX5 %>%
  filter(Y5==2050,J2=='Methanol',Sc!=c('Baseline')) %>% 
  mutate(cpol=factor(cpol,levels=c('1000C','700C','500C'))) %>% 
  mutate(F2=as.character(F2)) %>% 
  mutate(F3=case_when(str_detect(F2,'Biomass')~F2,
                      str_detect(L,'_.{3}X')~'Fossil w/ CCUS',
                      F2=='Electrolysis'~F2,
                      TRUE~'Fossil w/o CCUS')) %>% 
  mutate(F3=factor(F3,levels=c('Biomass w/ CCUS',
                               'Biomass w/o CCUS',
                               'Fossil w/ CCUS',
                               'Fossil w/o CCUS'))) %>% 
  group_by(Sc,F3,techpol,cpol) %>%
  summarise(vx_l=sum(vx_l)) %>% 
  group_by(Sc) %>%
  mutate(vx_ls=vx_l*100/sum(vx_l)) %>%
  ggplot() +
  geom_bar(aes(x=techpol,y=vx_ls,fill=F3),stat='identity') +
  scale_fill_manual(guide=guide_legend(nrow=1),
                    values=c('darkolivegreen4',
                             'darkolivegreen2',
                             'grey50',
                             'grey70')) +
  scale_y_continuous(position='right') +
  facet_wrap(vars(cpol)) +
  labs(y=expression(paste('Share in Methanol production (%)')),
       subtitle='2050') +
  MyTheme +
  theme(legend.position = 'none',
        axis.title.y=element_text(size = 11,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")))
plot(Fig3b.2)

Fig3b <- Fig3b.1 + Fig3b.2 + plot_layout(widths=c(4,3))
plot(Fig3b)


# Ammonia 2020 - 2050 -----
Fig3c.1 <- VX5 %>%
  filter(J2=='Ammonia',techpol=='Default',Sc!=c('Baseline'),Y5%in%c(2020,2030,2040,2050)) %>% 
  mutate(cpol=factor(cpol,levels=c('1000C','700C','500C'))) %>% 
  mutate(F2=as.character(F2)) %>% 
  mutate(F3=case_when(str_detect(F2,'Biomass')~F2,
                      str_detect(L,'_.{3}X')~'Fossil w/ CCUS',
                      F2=='Electrolysis'~F2,
                      TRUE~'Fossil w/o CCUS')) %>% 
  mutate(F3=factor(F3,levels=c('Electrolysis',
                               'Fossil w/ CCUS',
                               'Fossil w/o CCUS'))) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=F3),stat='identity') +
  scale_fill_manual(guide=guide_legend(nrow=1),
                    values=c('lightsteelblue',
                             'grey50',
                             'grey70')) +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y=expression(paste('Ammonia production (Mt ',{yr^-1},')')),
       subtitle='Default',
       title='c') +
  MyTheme +
  theme(legend.position = 'none',
        strip.text.y = element_blank(),
        axis.title.y=element_text(size = 11,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")))
plot(Fig3b.1)

Fig3c.2 <- VX5 %>%
  filter(Y5==2050,J2=='Ammonia',Sc!=c('Baseline')) %>% 
  mutate(cpol=factor(cpol,levels=c('1000C','700C','500C'))) %>% 
  mutate(F2=as.character(F2)) %>% 
  mutate(F3=case_when(str_detect(F2,'Biomass')~F2,
                      str_detect(L,'_.{3}X')~'Fossil w/ CCUS',
                      F2=='Electrolysis'~F2,
                      TRUE~'Fossil w/o CCUS')) %>% 
  mutate(F3=factor(F3,levels=c('Electrolysis',
                               'Biomass w/o CCUS',
                               'Fossil w/ CCUS',
                               'Fossil w/o CCUS'))) %>% 
  group_by(Sc,F3,techpol,cpol) %>%
  summarise(vx_l=sum(vx_l)) %>% 
  group_by(Sc) %>%
  mutate(vx_ls=vx_l*100/sum(vx_l)) %>%
  ggplot() +
  geom_bar(aes(x=techpol,y=vx_ls,fill=F3),stat='identity') +
  scale_fill_manual(guide=guide_legend(nrow=1),
                    values=c('lightsteelblue',
                             'darkolivegreen2',
                             'grey50',
                             'grey70')) +
  scale_y_continuous(position='right') +
  facet_wrap(vars(cpol)) +
  labs(y=expression(paste('Share in Ammonia production (%)')),
       subtitle='2050') +
  MyTheme +
  theme(legend.position = 'none',
        axis.title.y=element_text(size = 11,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")))
plot(Fig3c.2)

Fig3c <- Fig3c.1 + Fig3c.2 + plot_layout(widths=c(4,3))
plot(Fig3c)


# Total 2020 - 2050 -----
Fig3d.1 <- VX5 %>%
  filter(techpol=='Default',Sc!=c('Baseline'),Y5%in%c(2020,2030,2040,2050)) %>% 
  mutate(cpol=factor(cpol,levels=c('1000C','700C','500C'))) %>% 
  mutate(F2=as.character(F2)) %>% 
  mutate(F3=case_when(str_detect(F2,'Biomass')~F2,
                      str_detect(L,'_.{3}X')~'Fossil w/ CCUS',
                      F2=='Electrolysis'~F2,
                      TRUE~'Fossil w/o CCUS')) %>% 
  mutate(F3=factor(F3,levels=c('Electrolysis',
                               'Biomass w/o CCUS',
                               'Fossil w/ CCUS',
                               'Fossil w/o CCUS'))) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=F3),stat='identity') +
  scale_fill_manual(guide=guide_legend(nrow=1),
                    values=c('lightsteelblue',
                             'darkolivegreen2',
                             'grey50',
                             'grey70')) +
  facet_grid(cols=vars(cpol),rows=vars(techpol)) +
  labs(y=expression(paste('Primary chemical production (Mt ',{yr^-1},')')),
       subtitle='Default',
       title='d') +
  MyTheme +
  theme(legend.position = 'none',
        strip.text.y = element_blank(),
        axis.title.y=element_text(size = 11,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")))
plot(Fig3d.1)

Fig3d.2 <- VX5 %>%
  filter(Y5==2050,Sc!=c('Baseline')) %>% 
  mutate(cpol=factor(cpol,levels=c('1000C','700C','500C'))) %>% 
  mutate(F2=as.character(F2)) %>% 
  mutate(F3=case_when(str_detect(F2,'Biomass')~F2,
                      str_detect(L,'_.{3}X')~'Fossil w/ CCUS',
                      F2=='Electrolysis'~F2,
                      TRUE~'Fossil w/o CCUS')) %>% 
  mutate(F3=factor(F3,levels=c('Electrolysis',
                               'Biomass w/ CCUS',
                               'Biomass w/o CCUS',
                               'Fossil w/ CCUS',
                               'Fossil w/o CCUS'))) %>% 
  group_by(Sc,F3,techpol,cpol) %>%
  summarise(vx_l=sum(vx_l)) %>% 
  group_by(Sc) %>%
  mutate(vx_ls=vx_l*100/sum(vx_l)) %>%
  ggplot() +
  geom_bar(aes(x=techpol,y=vx_ls,fill=F3),stat='identity') +
  scale_fill_manual(guide=guide_legend(nrow=1),
                    values=c('lightsteelblue',
                             'darkolivegreen4',
                             'darkolivegreen2',
                             'grey50',
                             'grey70')) +
  scale_y_continuous(position='right') +
  facet_wrap(vars(cpol)) +
  labs(y=expression(paste('Share in primary chemical production (%)')),
       subtitle='2050') +
  MyTheme +
  theme(legend.position = 'none',
        axis.title.y=element_text(size = 11,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")))
plot(Fig3d.2)

Fig3d <- Fig3d.1 + Fig3d.2 + plot_layout(widths=c(4,3))
plot(Fig3d)


# Legend -----
Fig3.l <- VX5 %>%
  filter(Y5==2050,Sc!=c('Baseline')) %>%
  mutate(F2=as.character(F2)) %>% 
  mutate(F3=case_when(str_detect(F2,'Biomass')~F2,
                      str_detect(L,'_.{3}X')~'Fossil w/ CCUS',
                      F2=='Electrolysis'~F2,
                      TRUE~'Fossil w/o CCUS')) %>% 
  mutate(F3=factor(F3,levels=c('Electrolysis',
                               'Biomass w/ CCUS',
                               'Biomass w/o CCUS',
                               'Fossil w/ CCUS',
                               'Fossil w/o CCUS'))) %>% 
  ggplot() +
  geom_bar(aes(x=techpol,y=vx_l,fill=F3),stat='identity') +
  scale_fill_manual(guide=guide_legend(nrow=1),
                    values=c('lightsteelblue2',
                             'darkolivegreen4',
                             'darkolivegreen2',
                             'grey50',
                             'grey70')) +
  MyTheme +
  theme(legend.text = element_text(size = 14),
        legend.title = element_blank())
plot(Fig3.l)
Legend3 <- lemon::g_legend(Fig3.l)
plot(Legend3)
ggsave(paste0(odir,'Fig3L.png'),Legend3,width=12,height=0.5)

# Summarise -----
Fig3 <- Fig3a.1 + Fig3a.2 + Fig3b.1 + Fig3b.2 + Fig3c.1 + Fig3c.2 + Fig3d.1 + Fig3d.2 +
  plot_layout(ncol=4,widths=c(4,3,4,3))
plot(Fig3)
ggsave(paste0(odir,'Fig3.png'),Fig3,width=12,height=6.5)


# Electricity feedstock
Fig4.1 <- VX5 %>%
  filter(J2=='Ammonia',Sc!=c('Baseline'),Y5%in%c(2020,2030,2040,2050)) %>% 
  mutate(cpol=factor(cpol,levels=c('1000C','700C','500C'))) %>% 
  mutate(F2=as.character(F2)) %>% 
  mutate(F3=case_when(str_detect(F2,'Biomass')~F2,
                      str_detect(L,'_.{3}X')~'Fossil w/ CCUS',
                      F2=='Electrolysis'~F2,
                      TRUE~'Fossil w/o CCUS')) %>% 
  mutate(F3=factor(F3,levels=c('Electrolysis',
                               'Fossil w/ CCUS',
                               'Fossil w/o CCUS'))) %>%
  filter(F3=='Electrolysis') %>% 
  mutate(ve_l=18.6*vx_l) %>% 
  select(Sc,Y5,ve_l,cpol,techpol) %>%
  mutate(K3='Electricity')


# Total -energy
Fig4.2 <- VE5 %>%
  filter(Y5%in%c(2020,2030,2040,2050),E=='Feedstock') %>% 
  mutate(K2=as.character(K2)) %>% 
  mutate(K3=case_when(str_detect(K2,'biomass')~'Biomass',
                      TRUE~'Fossil')) %>% 
  bind_rows(Fig4.1) %>% 
  mutate(K3=factor(K3,levels=c('Electricity',
                               'Biomass',
                               'Fossil')))

Fig4.3 <- Fig4.2 %>%  
  filter(techpol=='Default',Sc!='Baseline') %>% 
  mutate(cpol=factor(cpol,levels=c('Baseline','1000C','700C','500C'))) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=ve_l/1000,fill=K3),stat='identity') +
  scale_fill_manual(values=c('lightsteelblue',
                             'darkolivegreen2',
                             'grey60')) +
  facet_wrap(vars(cpol),nrow=1) +
  labs(y=expression(paste('Feedstock energy (EJ ',{yr^-1},')')),
       subtitle='Default') +
  MyTheme +
  theme(legend.position = 'none')

Fig4.4 <- Fig4.2 %>%  
  filter(Y5==2050,Sc!='Baseline') %>% 
  mutate(cpol=factor(cpol,levels=c('Baseline','1000C','700C','500C'))) %>% 
  group_by(Sc,K3,techpol,cpol) %>%
  summarise(ve_l=sum(ve_l)) %>% 
  group_by(Sc) %>%
  mutate(ve_ls=ve_l*100/sum(ve_l)) %>%
  ggplot() +
  geom_bar(aes(x=techpol,y=ve_ls,fill=K3),stat='identity') +
  scale_fill_manual(guide=guide_legend(nrow=1),
                    values=c('lightsteelblue',
                             'darkolivegreen2',
                             'grey60')) +
  scale_y_continuous(position='right') +
  facet_wrap(vars(cpol),nrow=1) +
  labs(y=expression(paste('Share in feedstock energy (%)')),
       subtitle='2050') +
  MyTheme +
  theme(legend.position = 'none',
        axis.title.y=element_text(size = 11,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")))
plot(Fig4.4)

Fig4 <- Fig4.3 + Fig4.4 + plot_layout(widths=c(4,3))
plot(Fig4)
ggsave(paste0(odir,'Fig4.png'),Fig4,width=7,height=3.5)

Legend4 <- lemon::g_legend(Fig4.4+theme(legend.position = 'bottom'))
ggsave(paste0(odir,'Legend4.png'),Legend4,width=8,height=0.3)

# Figure 4 Emission --------------------------------------------------------

# Total
df5 <- VQ5 %>% 
  filter(K!='CRN',E!='CCUS') %>% 
  group_by(Sc,Y5,E) %>% 
  summarise(vq_l=sum(vq_l))
  

Fig5a <- df5 %>% 
  filter(E%in%c('Energy','Process')) %>% 
  group_by(Sc,Y5) %>% 
  summarise(vq_l=sum(vq_l)) %>% 
  left_join(list_Sc) %>% 
  mutate(techpol=recode(techpol,'Biofueloff'='limBio')) %>% 
  mutate(cpol=str_remove(cpol,'NPi')) %>% 
  mutate(cpol=case_when(cpol!='Baseline'~paste0(cpol,'C'),
                        TRUE~cpol)) %>% 
  mutate(cpol=factor(cpol,levels=c('Baseline','1000C','700C','500C')),
         techpol=factor(techpol,levels=c('Default','NoCCS','limBio'))) %>% 
  ggplot() +
  geom_line(aes(x=Y5,y=vq_l/1000,color=techpol,group=Sc,linetype=cpol),size=0.5) +
  geom_point(aes(x=Y5,y=vq_l/1000,color=techpol,shape=cpol),size=1.5,stroke=1,fill='white') +
  scale_color_manual(values=c('indianred2','palegreen3','deepskyblue3')) +
  scale_linetype_manual(values=c('dotted','solid','solid','solid')) +
  scale_shape_manual(values=c(23,22,21,24)) +
  scale_y_continuous(limits=c(0,NA)) +
  labs(y=expression(paste('Direct ',{CO[2]},' emissions (Gt',{CO[2]},'  ',{yr^-1},')')),
       subtitle='a') +
  MyTheme +
  theme(legend.position='none',
        legend.text = element_text(size = 10))
plot(Fig5a)
ggsave(paste0(odir,'Fig5a.png'),Fig5a,width=5,height=4.5)

Fig5b <- df5 %>% 
  filter(E%in%c('Feedstock','Electricity (indirect)')) %>% 
  group_by(Sc,Y5) %>% 
  summarise(vq_l=sum(vq_l)) %>% 
  left_join(list_Sc) %>% 
  mutate(techpol=recode(techpol,'Biofueloff'='limBio')) %>% 
  mutate(cpol=str_remove(cpol,'NPi')) %>% 
  mutate(cpol=case_when(cpol!='Baseline'~paste0(cpol,'C'),
                        TRUE~cpol)) %>% 
  mutate(cpol=factor(cpol,levels=c('Baseline','1000C','700C','500C')),
         techpol=factor(techpol,levels=c('Default','NoCCS','limBio'))) %>% 
  ggplot() +
  geom_line(aes(x=Y5,y=vq_l/1000,color=techpol,group=Sc,linetype=cpol),size=0.5) +
  geom_point(aes(x=Y5,y=vq_l/1000,color=techpol,shape=cpol),size=1.5,stroke=1,fill='white') +
  scale_color_manual(values=c('indianred2','palegreen3','deepskyblue3'),guide=guide_legend(nrow=2)) +
  scale_linetype_manual(values=c('dotted','solid','solid','solid'),guide=guide_legend(nrow=2)) +
  scale_shape_manual(values=c(23,22,21,24)) +
  scale_y_continuous(limits=c(0,NA)) +
  labs(y=expression(paste('Indirect ',{CO[2]},' emissions (Gt',{CO[2]},'  ',{yr^-1},')')),
       subtitle = 'b') +
  MyTheme +
  theme(legend.position='bottom',
        legend.text = element_text(size = 10))
plot(Fig5b)
ggsave(paste0(odir,'Fig5b.png'),Fig5b,width=5,height=4.5)

Fig5 <- Fig5a + Fig5b + plot_layout(ncol=1)
plot(Fig5)
ggsave(paste0(odir,'Fig5.png'),Fig5,width=4.5,height=8)

# Figure 5 Emission --------------------------------------------------------

# Production
df6.1 <- vx_l %>%
  filter(Sc=='Baseline',L%in%c('NENHVC','NENMOH','NENNH3','NENNH3U')) %>% 
  mutate(L=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'Methanol',
                     str_detect(L,'NH3')~'Ammonia')) %>% 
  group_by(L,Sc,H) %>% 
  summarise(vx_l=sum(vx_l)) %>% 
  group_by(L,Sc) %>% 
  complete(H=c(2005:2050)) %>% 
  ungroup() %>% 
  mutate(Y5=5*floor(H/5-401)+2005) %>%
  replace_na(list(vx_l=0)) %>% 
  group_by(L,Sc,Y5) %>% 
  summarise(vx_l=mean(vx_l)) %>% 
  rename(J2=L)

# Total -product
df6.2 <- VQ5 %>% 
  filter(K!='CRN',E!='CCUS') %>% 
  group_by(Sc,Y5,E,J2) %>% 
  summarise(vq_l=sum(vq_l))

Fig6 <- df6.2 %>%
  left_join(df6.1) %>% 
  mutate(vq_vx=vq_l/vx_l) %>% 
  left_join(list_Sc) %>% 
  mutate(techpol=recode(techpol,'Biofueloff'='limBio')) %>% 
  mutate(cpol=str_remove(cpol,'NPi')) %>% 
  mutate(cpol=case_when(cpol!='Baseline'~paste0(cpol,'C'),
                        TRUE~cpol)) %>% 
  mutate(cpol=factor(cpol,levels=c('Baseline','1000C','700C','500C')),
         techpol=factor(techpol,levels=c('Default','lim','limBio'))) %>% 
  filter(J2=='HVC') %>% 
  ggplot() +
  geom_area(aes(x=Y5,y=vq_l,fill=E),stat='identity') +
  facet_grid(cols=vars(cpol),rows=vars(techpol))
plot(Fig6)  


