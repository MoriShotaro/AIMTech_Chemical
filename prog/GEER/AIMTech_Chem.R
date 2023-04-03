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
# ddir <- paste0(root,'/data/GEER/output/global2050/2303310027/gams_output/gdx_primary/')
ddir <- paste0(root,'/data/GEER/output/global2050CCU/2304022201/gams_output/gdx_primary/')
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

# R33_R5 <- read.csv(paste0(ddir,'define/R33_R5.csv'),header=FALSE) %>% 
#   rename(R=1,R5=2)


# Input data --------------------------------------------------------------

# Sc <- c('Baseline','NPi700_limBioCCS')
# Sc <- c('Baseline','SR15_below')
Sc <- c('Baseline','NPi500','NPi700','NPi1000')
# Sc <- c('BaU')


vx_l <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'vx_l') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i) %>% 
    filter(I=='NEN')
} %>% bind_rows() # Mt-production

ve_l <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'ve_l') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i) %>% 
    filter(I=='NEN')
} %>% bind_rows() # PJ

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


# Reproduce model ---------------------------------------------------------

EQ_ENG <- vx_l %>% 
  left_join(e_t) %>% 
  mutate(ve_l=-vx_l*e_t)

hoge <- vx_l %>% 
  filter(str_detect(L,'N_.{3,4}_')) %>% 
  mutate(J=case_when(str_detect(L,'HVC_')~'HVC',
                     str_detect(L,'MOH_')~'MOH',
                     str_detect(L,'NH3U_')~'NH3U',
                     str_detect(L,'NH3_')~'NH3')) %>% 
  group_by(H,J,Sc) %>% 
  summarise(vx_l=sum(vx_l))

fuga <- vx_l %>% 
  left_join(e_t) %>% 
  mutate(ve_l1=-vx_l*e_t) %>% 
  group_by(H,Sc,K) %>% 
  summarise(ve_l1=sum(ve_l1))

piyo <- ve_l %>% 
  group_by(H,Sc,K) %>%
  summarise(ve_l2=sum(ve_l)) %>% 
  filter(!str_detect(K,'N_|CCO2'),K!='OLNS') %>% 
  mutate(K2=case_when(str_detect(K,'COL')~'COL',
                   str_detect(K,'NGS')~'NGS',
                   str_detect(K,'OIL')~'OIL',
                   str_detect(K,'OLE')~'OLE',
                   str_detect(K,'OLN')~'OLN',
                   str_detect(K,'ELY')~'ELY',
                   str_detect(K,'P2H')~'P2H',
                   str_detect(K,'BMS')~'BMS',
                   str_detect(K,'CRN')~'CRN',
                   ))
g <- piyo %>% 
  ggplot() +
  geom_bar(aes(x=H,y=ve_l2/1000,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc)) # PJ -> EJ
plot(g)

EQ_EMISS <- EQ_ENG %>% 
  left_join(gas_t1) %>%
  drop_na() %>% 
  mutate(vq_l=ve_l*gas_t1)

df_WORLD <- EQ_EMISS %>% 
  select(R,L,H,K,Sc,vx_l,gas_t1,vq_l) %>%
  group_by(L,H,K,Sc) %>% 
  summarise(vx_l=sum(vx_l),vq_l=sum(vq_l),gas_t1=mean(gas_t1)) %>% 
  mutate(J=case_when(str_detect(L,'HVC_')~'HVC',
                     str_detect(L,'MOH_')~'MOH',
                     str_detect(L,'NH3U_')~'NH3U',
                     str_detect(L,'NH3_')~'NH3')) %>% 
  mutate(E=case_when(str_detect(K,'.{3}F')~'Feedstock',
                        str_detect(K,'.{3}P')~'Process',
                        TRUE~'Energy'))

df_EmiJ <- df_WORLD %>%
  group_by(H,J,E,Sc) %>% 
  summarise(vq_l=sum(vq_l)/10^3) %>% 
  mutate(E=factor(E,levels=c('Feedstock','Process','Energy')))

df_checkEmi <- df_EmiJ %>% 
  filter(H%in%c(2010,2015,2020),E!='Feedstock') %>% 
  group_by(J,Sc,H) %>% 
  summarise(vq_l=sum(vq_l)) %>% 
  pivot_wider(names_from=J,values_from=vq_l) %>% 
  mutate(NH3_TOTAL=NH3+NH3U)

df_historical <- data.frame(H=c(2010,2015,2020),
                            value=c(357.56+76.84+190.14,407.48+159.28+219.07,408.43+225.56+248.30))

df_prod <- vx_l %>% 
  filter(L%in%c('NENNH3','NENNH3U','NENMOH','NENHVC')) %>% 
  group_by(L,H,Sc) %>% 
  summarise(vx_l=sum(vx_l)) %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>% 
  rename(prod=vx_l) %>% # Mt-product
  ungroup() %>% 
  select(H,Sc,J,prod)

g_emi_prod <- df_EmiJ %>% 
  filter(E!='Feedstock') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vq_l,fill=J),stat='identity') +
  geom_point(data=df_historical,aes(x=H,y=value),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_emi_prod)

g_emi_flow <- df_EmiJ %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vq_l,fill=E),stat='identity') +
  geom_point(data=df_historical,aes(x=H,y=value),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_emi_flow)

df_EneJ <- EQ_ENG %>% 
  filter(!str_detect(L,'NEN')) %>% 
  mutate(J=case_when(str_detect(L,'HVC_')~'HVC',
                     str_detect(L,'MOH_')~'MOH',
                     str_detect(L,'NH3U_')~'NH3U',
                     str_detect(L,'NH3_')~'NH3')) %>% 
  mutate(E=case_when(str_detect(K,'.{3}F')~'Feedstock',
                     str_detect(K,'.{3}P')~'Process',
                     TRUE~'Energy')) %>% 
  group_by(H,J,E,K,Sc) %>% 
  summarise(ve_l=sum(ve_l)) %>% 
  left_join(df_prod) %>% 
  mutate(value=ve_l/prod)

g_ene_flow <- df_EneJ %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=E),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_ene_flow)

vx_l2 <- vx_l %>%
  group_by(L,H,Sc) %>% 
  summarise(vx_l=sum(vx_l)) %>% 
  mutate(Y5=5*floor(H/5-401)+2005) %>%
  group_by(L,Y5,Sc) %>% 
  summarise(vx_l=mean(vx_l)) 

g_prodK_HVC <- vx_l2 %>% 
  filter(str_detect(L,'NENHVC.|NENMOH.|NENNH3.|NENNH3U.')&L!='NENNH3U') %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>%
  filter(J=='HVC') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=L),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_prodK_HVC)

g_prodK_HVC <- vx_l %>%
  filter(str_detect(L,'NENHVC.|NENMOH.|NENNH3.|NENNH3U.')&L!='NENNH3U') %>%
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>%
  filter(J=='HVC') %>%
  ggplot() +
  geom_bar(aes(x=H,y=vx_l,fill=L),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_prodK_HVC)

g_prodK_NH3 <- vx_l2 %>% 
  filter(str_detect(L,'NENHVC.|NENMOH.|NENNH3.|NENNH3U.')&L!='NENNH3U') %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>%
  filter(J=='NH3') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=L),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_prodK_NH3)

g_prodK_NH3 <- vx_l %>% 
  filter(str_detect(L,'NENHVC.|NENMOH.|NENNH3.|NENNH3U.')&L!='NENNH3U') %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>%
  filter(J=='NH3') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vx_l,fill=L),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_prodK_NH3)

g_prodK_NH3U <- vx_l2 %>% 
  filter(str_detect(L,'NENHVC.|NENMOH.|NENNH3.|NENNH3U.')&L!='NENNH3U') %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>%
  filter(J=='NH3U') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=L),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_prodK_NH3U)

g_prodK_MOH <- vx_l2 %>% 
  filter(str_detect(L,'NENHVC.|NENMOH.|NENNH3.|NENNH3U.')&L!='NENNH3U') %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>% 
  filter(J=='MOH') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=L),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_prodK_MOH)

g_prodK_HVC <- vx_l2 %>% 
  filter(!str_detect(L,'NEN')&L!='NENNH3U') %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>%
  filter(J=='HVC') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=L),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_prodK_HVC)


g_prodK_NH3 <- vx_l2 %>% 
  filter(!str_detect(L,'NEN')&L!='NENNH3U') %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>%
  filter(J=='NH3') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=L),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_prodK_NH3)

g_prodK_NH3U <- vx_l2 %>% 
  filter(!str_detect(L,'NEN')&L!='NENNH3U') %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>%
  filter(J=='NH3U') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=L),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_prodK_NH3U)

g_prodK_MOH <- vx_l2 %>% 
  filter(!str_detect(L,'NEN')&L!='NENNH3U') %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3')) %>% 
  filter(J=='MOH') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=vx_l,fill=L),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_prodK_MOH)

