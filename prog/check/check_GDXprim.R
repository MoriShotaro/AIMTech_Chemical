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
ddir <- paste0(root,'/data/GEER/output/global2050/2303291705/gams_output/gdx_primary/')
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

# R33_R5 <- read.csv(paste0(ddir,'define/R33_R5.csv'),header=FALSE) %>% 
#   rename(R=1,R5=2)


# Input data --------------------------------------------------------------

Sc <- c('Baseline')
# Sc <- c('Baseline','SR15_lowOS_DAC','SR15_lowOS_limBio','SR15_lowOS_DAClimBio')
# Sc <- c('BaU')

vx_l <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'vx_l') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i)
} %>% bind_rows()

ve_l <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'ve_l') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i)
} %>% bind_rows()

# e_t <- foreach(i=Sc,.combine='rbind') %do% {
#   tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'e_t') %>% 
#     mutate(across(where(is.factor),~as.character(.))) %>% 
#     mutate(H=as.numeric(H)) %>% 
#     mutate(Sc=i)
# } %>% bind_rows()


check <- vx_l %>%
  filter(I=='NEN',!str_detect(L,'NEN')) %>% 
  group_by(L,H,Sc) %>% 
  summarise(value=sum(vx_l)) %>% 
  ungroup() %>% 
  group_by(L,H,Sc) %>% 
  summarise(value=mean(value)) %>% 
  mutate(J=case_when(str_detect(L,'HVC_')~'HVC',
                     str_detect(L,'MOH_')~'MOH',
                     str_detect(L,'NH3U_')~'NH3U',
                     str_detect(L,'NH3_')~'NH3')) %>% 
  group_by(H,J,Sc) %>% 
  summarise(value=sum(value))

# Production --------------------------------------------------------------

# Y5 plot
df_vx1 <- vx_l %>%
  filter(I=='NEN') %>% 
  group_by(L,H,Sc) %>% 
  summarise(value=sum(vx_l)) %>% 
  ungroup() %>% 
  mutate(Y5=5*floor(H/5-401)+2005) %>%
  group_by(L,Y5,Sc) %>% 
  summarise(value=mean(value)) %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3'))

# by J
g_vx_HVC <- df_vx1 %>% 
  filter(J=='HVC'&!str_detect(L,'NEN')) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_HVC)

g_vx_NH3 <- df_vx1 %>% 
  filter(J=='NH3'&!str_detect(L,'NEN')) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_NH3)

g_vx_NH3U <- df_vx1 %>% 
  filter(J=='NH3U'&!str_detect(L,'NEN')) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_NH3U)

g_vx_MOH <- df_vx1 %>% 
  filter(J=='MOH'&!str_detect(L,'NEN')) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_MOH)

g_vx <- g_vx_HVC + g_vx_NH3 + g_vx_NH3U +g_vx_MOH
plot(g_vx)

# by L
g_vx_HVC <- df_vx1 %>% 
  filter(J=='HVC'&str_detect(L,'NEN')&L!='NENHVC') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_HVC)

g_vx_NH3 <- df_vx1 %>% 
  filter(J=='NH3'&str_detect(L,'NEN')&L!='NENNH3') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_NH3)

g_vx_NH3U <- df_vx1 %>% 
  filter(J=='NH3U'&str_detect(L,'NEN')&L!='NENNH3U') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_NH3U)

g_vx_NH3_TOTAL <- df_vx1 %>% 
  filter(J%in%c('NH3','NH3U')&str_detect(L,'NEN')&L!='NENNH3U'&L!='NENNH3') %>% 
  mutate(K=case_when(str_detect(L,'NH3C|NH3UC')~'Coal',
                     str_detect(L,'NH3O|NH3UO')~'Oil',
                     str_detect(L,'NH3G|NH3UG')~'Gas',
                     str_detect(L,'NH3B|NH3UB')~'Biomass',
                     str_detect(L,'NH3E')~'Electrolysis')) %>%
  mutate(K=factor(K,levels=c('Electrolysis','Biomass','Gas','Oil','Coal'))) %>% 
  group_by(Y5,Sc,K) %>% 
  summarise(value=sum(value)) %>% 
  ungroup() %>% 
  complete(Y5,Sc,K) %>% 
  replace_na(list(value=0)) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value,fill=K),stat='identity',position='fill') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_NH3_TOTAL)

g_vx_MOH <- df_vx1 %>% 
  filter(J=='MOH'&str_detect(L,'NEN')&L!='NENMOH') %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_MOH)

g_vx <- g_vx_HVC + g_vx_NH3 + g_vx_NH3U + g_vx_MOH
plot(g_vx)

# Y1 plot
df_vx2 <- vx_l %>%
  filter(I=='NEN') %>% 
  group_by(L,H,Sc) %>% 
  summarise(value=sum(vx_l)) %>% 
  ungroup() %>% 
  group_by(L,H,Sc) %>% 
  summarise(value=mean(value)) %>% 
  mutate(J=case_when(str_detect(L,'HVC')~'HVC',
                     str_detect(L,'MOH')~'MOH',
                     str_detect(L,'NH3U')~'NH3U',
                     str_detect(L,'NH3')~'NH3'))

g_vx_HVC <- df_vx2 %>% 
  filter(J=='HVC'&L!='NENHVC',!str_detect(L,'NEN')) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_HVC)
g_vx_NH3 <- df_vx2 %>% 
  filter(J=='NH3'&L!='NENNH3',!str_detect(L,'NEN')) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_NH3)
g_vx_NH3U <- df_vx2 %>% 
  filter(J=='NH3U'&L!='NENNH3U',!str_detect(L,'NEN')) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_wrap(cols=vars(J)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_NH3U)
g_vx_MOH <- df_vx2 %>% 
  filter(J=='MOH'&L!='NENMOH',!str_detect(L,'NEN')) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_wrap(cols=vars(J)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_MOH)
g_vx <- g_vx_HVC + g_vx_NH3 + g_vx_NH3U + g_vx_MOH
plot(g_vx)

# by L
g_vx_HVC <- df_vx2 %>% 
  filter(J=='HVC'&str_detect(L,'NEN')&L!='NENHVC') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_HVC)
g_vx_NH3 <- df_vx2 %>% 
  filter(J=='NH3'&str_detect(L,'NEN')&L!='NENNH3') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_NH3)
g_vx_NH3U <- df_vx2 %>% 
  filter(J=='NH3U'&str_detect(L,'NEN')&L!='NENNH3U') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_NH3U)
g_vx_MOH <- df_vx2 %>% 
  filter(J=='MOH'&str_detect(L,'NEN')&L!='NENMOH') %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=L),stat='identity') +
  facet_wrap(vars(Sc)) +
  # facet_grid(rows=vars(Sc),cols=vars(J)) +
  scale_fill_brewer(palette='Set3')
plot(g_vx_MOH)
g_vx <- g_vx_HVC + g_vx_NH3 + g_vx_MOH
plot(g_vx)


# Process energy ----------------------------------------------------------

# historical
df_historical <- data.frame(H=c(2010,2015,2020),
                            value=c(357.56+76.84+190.14,407.48+159.28+219.07,408.43+225.56+248.30))

df_process <- ve_l %>%
  filter(I=='NEN') %>% 
  group_by(K,H,Sc) %>% 
  summarise(value=sum(ve_l))

# Emission ----------------------------------------------------------------

# historical
df_historical <- data.frame(H=c(2010,2015,2020),
                            value=c(357.56+76.84+190.14,407.48+159.28+219.07,408.43+225.56+248.30))

# total
df_ve1 <- ve_l %>%
  filter(I=='NEN') %>% 
  group_by(K,H,Sc) %>% 
  summarise(value=sum(ve_l)) %>% 
  ungroup() %>% 
  filter(str_detect(K,'BMS|COL|OIL|NGS|NGL'),Sc=='Baseline') %>% 
  mutate(emf=case_when(str_detect(K,'BMS')~0,
                     str_detect(K,'COL')~94.6,
                     str_detect(K,'OIL')~77.4,
                     str_detect(K,'NGS')~56.1,
                     str_detect(K,'NGL')~68.2)) %>% 
  mutate(flow=case_when(str_detect(K,'.{3}F')~'Feedstock',
                        str_detect(K,'.{3}P')~'Process',
                        TRUE~'Energy')) %>% 
  group_by(H,Sc,flow) %>% 
  summarise(value=sum(value*emf)/10^3) %>% # kgCO2 -> 
  mutate(flow=factor(flow,levels=c('Feedstock','Process','Energy')))
g_emi_total <- df_ve1 %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=flow),stat='identity') +
  geom_point(data=df_historical,aes(x=H,y=value),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3')
plot(g_emi_total)


# future 
df_ve2 <- ve_l %>%
  filter(I=='NEN') %>% 
  group_by(K,H,Sc) %>% 
  summarise(value=sum(ve_l)) %>% 
  ungroup() %>% 
  mutate(Y5=5*floor(H/5-401)+2005) %>%
  group_by(K,Y5,Sc) %>% 
  summarise(value=mean(value)) %>% 
  ungroup() %>% 
  filter(str_detect(K,'BMS|COL|OIL|NGS|NGL')) %>% 
  mutate(emf=case_when(str_detect(K,'BMS')~0,
                       str_detect(K,'COL')~94.6,
                       str_detect(K,'OIL')~77.4,
                       str_detect(K,'NGS')~56.1,
                       str_detect(K,'NGL')~68.2)) %>% 
  mutate(flow=case_when(str_detect(K,'.{3}F')~'Feedstock',
                        str_detect(K,'.{3}P')~'Process',
                        TRUE~'Energy')) %>% 
  group_by(Y5,Sc,flow) %>% 
  summarise(value=sum(value*emf)/10^3) %>% # kgCO2 -> 
  mutate(flow=factor(flow,levels=c('Feedstock','Process','Energy')))

g_emi_total <- df_ve2 %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value,fill=flow),stat='identity') +
  geom_point(data=df_historical,aes(x=H,y=value),color='indianred1',fill='white',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g_emi_total)
