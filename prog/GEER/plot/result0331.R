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
odir <- paste0(root,'/output/check/')

# R33_R5 <- read.csv(paste0(ddir,'define/R33_R5.csv'),header=FALSE) %>% 
#   rename(R=1,R5=2)


# Input data --------------------------------------------------------------

# Sc <- c('Baseline','NPi700_limBioCCS')
# Sc <- c('Baseline' )
# Sc <- c('Baseline','NPi500_limBioCCS','NPi700_limBioCCS','NPi1000_limBioCCS')
# Sc <- c('BaU')

cpol <- c('Baseline','NPi500','NPi700','NPi1000')
# cpol <- c('Baseline','NPi500','NPi600','NPi700','NPi800','NPi900','NPi1000')
techpol <- c('Default','limCCS')
# techpol <- c('Default','Biofueloff','limCCS')

list_Sc <- expand.grid(cpol=cpol,techpol=techpol) %>% 
  mutate(Sc=paste0(cpol,'_',techpol)) %>% 
  mutate(Sc=str_remove_all(Sc,'_Default')) 

cpol_techpol <- list_Sc %>%
  filter(!str_detect(Sc,'Baseline_')) %>% 
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


# Reproduce model ---------------------------------------------------------

EQ_ENG <- vx_l %>% 
  left_join(e_t) %>% 
  mutate(ve_l=-vx_l*e_t)

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

hoge <- df_WORLD %>% 
  ungroup() %>% 
  distinct(K)

df_EmiJ <- df_WORLD %>%
  filter(!str_detect(K,'CCO2')) %>% 
  group_by(H,J,K,E,Sc) %>% 
  summarise(vq_l=sum(vq_l)/10^3) %>% 
  mutate(E=factor(E,levels=c('Feedstock','Process','Energy'))) %>% 
  left_join(list_Sc)

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
  rename(prod=vx_l) %>%
  ungroup() %>% 
  select(H,Sc,J,prod)

g_emi_prod <- df_EmiJ %>% 
  filter(E!='Feedstock') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vq_l,fill=J),stat='identity') +
  geom_point(data=df_historical,aes(x=H,y=value),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol))
plot(g_emi_prod)

g_emi_ene <- df_EmiJ %>% 
  filter(E!='Feedstock',K!='CRN',K!='NGSC') %>%
  mutate(K2=case_when(str_detect(K,'COL')~'COL',
                   str_detect(K,'NGS')~'NGS',
                   TRUE~'OIL')) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vq_l,fill=K2),stat='identity') +
  geom_point(data=df_historical,aes(x=H,y=value),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol))
plot(g_emi_ene)

g_emi_flow <- df_EmiJ %>% 
  filter(E!='Feedstock') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vq_l,fill=E),stat='identity') +
  geom_point(data=df_historical,aes(x=H,y=value),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol))
plot(g_emi_flow)

df_EneJ <- EQ_ENG %>% 
  filter(!str_detect(L,'NEN')) %>% 
  mutate(J=case_when(str_detect(L,'HVC_')~'HVC',
                     str_detect(L,'MOH_')~'MOH',
                     str_detect(L,'NH3U_')~'NH3U',
                     str_detect(L,'NH3_')~'NH3')) %>% 
  mutate(E=case_when(str_detect(K,'.{3}F')~'Feedstock',
                     str_detect(K,'.{3}S')~'Feedstock',
                     str_detect(K,'.{3}E')~'Energy',
                     str_detect(K,'.{3}P')~'Process',
                     str_detect(K,'.{3}C')~'Energy',
                     TRUE~'Energy')) %>% 
  group_by(H,J,E,K,Sc) %>% 
  summarise(ve_l=sum(ve_l)) %>% 
  left_join(df_prod) %>% 
  mutate(value=ve_l/prod) %>% 
  mutate(H=5*floor(H/5-401)+2005) %>%
  group_by(H,J,E,K,Sc) %>% 
  summarise(vx_l=mean(vx_l),ve_l=mean(ve_l),value=mean(value)) %>% 
  left_join(list_Sc)

df_EneJ_K <- df_EneJ %>% 
   mutate(K2=case_when(str_detect(K,'BMS')~'Liquid biomass',
                       str_detect(K,'COL')~'Coal',
                       str_detect(K,'CRN')~'Solid biomass',
                       str_detect(K,'NGS')~'Natural gas',
                       str_detect(K,'OIL')~'Oil product',
                       str_detect(K,'OLE')~'Oil product',
                       str_detect(K,'OLN')~'Oil product',
                       str_detect(K,'P2H')~'Hydrogen',
                       str_detect(K,'ELYI')~'Electricity'
                       ))

g_ene_flow <- df_EneJ %>% 
  filter(!str_detect(K,'CCO2'),H<=2020) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=E),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol))
plot(g_ene_flow)

g_ene_flowJ <- df_EneJ %>% 
  filter(!str_detect(K,'CCO2'),H<=2020) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=E),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(J))
plot(g_ene_flowJ)

g_ene_prod <- df_EneJ %>% 
  filter(!str_detect(K,'CCO2'),H<=2020) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=J),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol))
plot(g_ene_prod)

g_ene_ene <- df_EneJ_K %>% 
  filter(!str_detect(K,'CCO2')) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol))
plot(g_ene_ene)

g_ene_eneJ <- df_EneJ_K %>% 
  filter(!str_detect(K,'CCO2'),H<=2020) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(J))
plot(g_ene_eneJ)

g_ene_ene_feed <- df_EneJ_K %>% 
  filter(!str_detect(K,'CCO2')) %>% filter(E=='Feedstock') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(techpol))
plot(g_ene_ene_feed)

g_ene_ene_feedJ <- df_EneJ_K %>% 
  filter(!str_detect(K,'CCO2'),H<=2020) %>% filter(E=='Feedstock') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=K2),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_grid(cols=vars(cpol),rows=vars(J))
plot(g_ene_ene_feedJ)

vx_l2 <- vx_l %>%
  group_by(L,H,Sc) %>% 
  summarise(vx_l=sum(vx_l)) %>%
  group_by(L,Sc) %>% 
  complete(H=c(2005:2050)) %>% 
  ungroup() %>% 
  mutate(Y5=5*floor(H/5-401)+2005) %>%
  replace_na(list(vx_l=0)) %>% 
  group_by(L,Y5,Sc) %>% 
  summarise(vx_l=mean(vx_l)) %>% 
  left_join(list_Sc)


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
  facet_grid(cols=vars(cpol),rows=vars(techpol))
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
  facet_grid(cols=vars(cpol),rows=vars(techpol))
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
  facet_grid(cols=vars(cpol),rows=vars(techpol))
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
  facet_grid(cols=vars(cpol),rows=vars(techpol))
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


# Plot --------------------------------------------------------------------

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

MyTheme2 <- theme_bw() +
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
    axis.title.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    axis.ticks.length=unit(0.15,"cm")
  )

# Primary Chemical Production by Feedstock --------------------------------

for (i in c('Baseline','NPi500','NPi700','NPi1000')){
  g_prodK_HVC <- vx_l2 %>% 
    filter(!str_detect(L,'NEN')&L!='NENNH3U') %>% 
    mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                       str_detect(L,'MOH')~'MOH',
                       str_detect(L,'NH3U')~'NH3U',
                       str_detect(L,'NH3')~'NH3')) %>%
    filter(J=='HVC',cpol==i,Y5%in%c(2030,2050)) %>% 
    mutate(L2=case_when(str_detect(L,'BMS')~'Biomass',
                        TRUE~'Fossil')) %>% 
    ggplot() +
    geom_bar(aes(x=techpol,y=vx_l,fill=L2),stat='identity') +
    labs(title=paste0(i)) +
    scale_fill_brewer(palette='Set3') +
    facet_wrap(vars(Y5),nrow=1) +
    MyTheme2 + theme(legend.position='none')
  assign(paste0('g_prodK_HVC_',i),g_prodK_HVC)
}
g_prodK_HVC <- g_prodK_HVC_Baseline + g_prodK_HVC_NPi1000 + g_prodK_HVC_NPi700 + g_prodK_HVC_NPi500 + plot_layout(nrow=1)
plot(g_prodK_HVC)

for (i in c('NPi500','NPi700','NPi1000')){
  g_prodK_MOH <- vx_l2 %>% 
    filter(!str_detect(L,'NEN')&L!='NENNH3U') %>% 
    mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                       str_detect(L,'MOH')~'MOH',
                       str_detect(L,'NH3U')~'NH3U',
                       str_detect(L,'NH3')~'NH3')) %>%
    filter(J=='MOH',cpol==i,Y5%in%c(2030,2050)) %>% 
    mutate(L2=case_when(str_detect(L,'BMSX')~'Biomass w/ CCUS',
                        str_detect(L,'BMS')~'Biomass',
                        str_detect(L,'.{3}X')~'Fossil w/ CCUS',
                        TRUE~'Fossil')) %>% 
    ggplot() +
    geom_bar(aes(x=techpol,y=vx_l,fill=L2),stat='identity') +
    scale_fill_brewer(palette='Set3') +
    facet_wrap(vars(Y5),nrow=1) +
    MyTheme2 + theme(legend.position='none')
  assign(paste0('g_prodK_MOH_',i),g_prodK_MOH)
}
g_prodK_MOH <-  g_prodK_MOH_NPi1000 + g_prodK_MOH_NPi700 + g_prodK_MOH_NPi500 + plot_layout(nrow=1)
plot(g_prodK_MOH)


for (i in c('NPi500','NPi700','NPi1000')){
  g_prodK_NH3 <- vx_l2 %>% 
    filter(!str_detect(L,'NEN')&L!='NENNH3U') %>% 
    mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                       str_detect(L,'MOH')~'MOH',
                       str_detect(L,'NH3U')~'NH3U',
                       str_detect(L,'NH3')~'NH3')) %>%
    filter(J%in%c('NH3','NH3U'),cpol==i,Y5%in%c(2030,2050)) %>% 
    mutate(L2=case_when(str_detect(L,'BMSX')~'Biomass w/ CCUS',
                        str_detect(L,'BMS')~'Biomass',
                        str_detect(L,'ELE')~'Electrolysis',
                        str_detect(L,'.{3}X')~'Fossil w/ CCUS',
                        TRUE~'Fossil')) %>% 
    ggplot() +
    geom_bar(aes(x=techpol,y=vx_l,fill=L2),stat='identity') +
    scale_fill_brewer(palette='Set3') +
    facet_wrap(vars(Y5),nrow=1) +
    MyTheme2 + theme(legend.position='bottom') +
    labs(title=i)
  assign(paste0('g_prodK_NH3_',i),g_prodK_NH3)
}
g_prodK_NH3 <-  g_prodK_NH3_NPi1000 + g_prodK_NH3_NPi700 + g_prodK_NH3_NPi500 + plot_layout(nrow=1)
plot(g_prodK_NH3)

