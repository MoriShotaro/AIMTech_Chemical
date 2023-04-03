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
ddir <- paste0(root,'/data/GEER/')
odir <- paste0(root,'/output/GEER/')

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

R33_R5 <- read.csv(paste0(ddir,'define/R33_R5.csv'),header=FALSE) %>% 
  rename(R=1,R5=2)

# Histrical data ----------------------------------------------------------

# USGS NH3 ------------
year <- c(2005:2014,2016:2017,2019,2021)
ddir2 <- paste0(root,'/data/GEER/historical/USGS_NH3/')

USGS_all <- foreach(i=year,.combine='rbind') %do% {
  tmp <- read_xls(paste0(ddir2,'myb1-',i,'-nitro.xls'),sheet=13,skip=5) %>% 
    select(!starts_with('..')&!contains('e')) %>% 
    rename(Country=1) %>%
    filter(Country=='Total') %>% 
    mutate(Country=str_replace_all(Country,'Country or locality2','Country')) %>% 
      mutate(across(-Country,~str_replace_all(.,'--','0'))) %>%
      mutate(across(-Country,~na_if(.,'NA'))) %>%
      mutate(across(-Country,~as.numeric(.))) %>%
    pivot_longer(cols=-Country,names_to='Year',values_to='value',names_transform=as.numeric) %>%
    mutate(Record=i)
  assign(paste0('df',i),tmp)
} %>% bind_rows()

USGS_NH3 <- USGS_all %>% 
  group_by(Country,Year) %>% 
  summarise(Record=max(Record)) %>% 
  left_join(USGS_all) %>% select(-Record) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  filter(Year>=2005&Year<=2020) %>% 
  mutate(value=value*17/14/1000) # thousand t-N -> Mt-NH3
save(USGS_NH3, file=paste0(odir,'USGS_NH3.RData'))

# NEDO HVC -----------

ddir2 <- paste0(root,'/data/GEER/historical/NEDO_HVC/')

df_ethylene <- read_xlsx(paste0(ddir2,'04_2019syouhinbetudeta.xlsx'),sheet=2,col_names=FALSE) %>%
  slice(63) %>% select(-1,-18,-19) %>% rename(Sr1=1,Sr2=2) %>% replace_na(list(Sr1='',Sr2='')) %>% 
  unite(col=Sr,c(1,2),sep='') %>% drop_na() %>% 
  mutate(across(c(2:15),~as.numeric(.))) %>% 
  pivot_longer(cols=-c(Sr),names_to='Y',names_prefix='...',names_transform=as.numeric,values_to='value') %>% 
  mutate(Y=Y+2006) %>% 
  mutate(Product='ethylene')
df_propylene <- read_xlsx(paste0(ddir2,'04_2019syouhinbetudeta.xlsx'),sheet=13,col_names=FALSE) %>% 
  slice(63) %>% select(-1,-18,-19) %>% rename(Sr1=1,Sr2=2) %>% replace_na(list(Sr1='',Sr2='')) %>% 
  unite(col=Sr,c(1,2),sep='') %>% drop_na() %>% 
  mutate(across(c(2:15),~as.numeric(.))) %>% 
  pivot_longer(cols=-c(Sr),names_to='Y',names_prefix='...',names_transform=as.numeric,values_to='value') %>% 
  mutate(Y=Y+2006) %>% 
  mutate(Product='propylene')
df_benzene <- read_xlsx(paste0(ddir2,'04_2019syouhinbetudeta.xlsx'),sheet=17,col_names=FALSE) %>%
  slice(63) %>% select(-1,-18,-19) %>% rename(Sr1=1,Sr2=2) %>% replace_na(list(Sr1='',Sr2='')) %>% 
  unite(col=Sr,c(1,2),sep='') %>% drop_na() %>% 
  mutate(across(c(2:15),~as.numeric(.))) %>% 
  pivot_longer(cols=-c(Sr),names_to='Y',names_prefix='...',names_transform=as.numeric,values_to='value') %>% 
  mutate(Y=Y+2006) %>% 
  mutate(Product='benzene')
df_toluene <- read_xlsx(paste0(ddir2,'04_2019syouhinbetudeta.xlsx'),sheet=18,col_names=FALSE) %>%
  slice(63) %>% select(-1,-18,-19) %>% rename(Sr1=1,Sr2=2) %>% replace_na(list(Sr1='',Sr2='')) %>% 
  unite(col=Sr,c(1,2),sep='') %>% drop_na() %>% 
  mutate(across(c(2:15),~as.numeric(.))) %>% 
  pivot_longer(cols=-c(Sr),names_to='Y',names_prefix='...',names_transform=as.numeric,values_to='value') %>% 
  mutate(Y=Y+2006) %>% 
  mutate(Product='toluene')
df_xylene <- read_xlsx(paste0(ddir2,'04_2019syouhinbetudeta.xlsx'),sheet=19,col_names=FALSE) %>%
  slice(63) %>% select(-1,-18,-19) %>% rename(Sr1=1,Sr2=2) %>% replace_na(list(Sr1='',Sr2='')) %>% 
  unite(col=Sr,c(1,2),sep='') %>% drop_na() %>% 
  mutate(across(c(2:15),~as.numeric(.))) %>% 
  pivot_longer(cols=-c(Sr),names_to='Y',names_prefix='...',names_transform=as.numeric,values_to='value') %>% 
  mutate(Y=Y+2006) %>% 
  mutate(Product='xylene')

NEDO_HVC <- bind_rows(df_ethylene,df_propylene,df_benzene,df_toluene,df_xylene) %>% 
  group_by(Sr,Y) %>% 
  summarise(value=sum(value)) %>% 
  filter(Y<=2018)
save(NEDO_HVC, file=paste0(odir,'NEDO_HVC.RData'))


# MOH ----------

ddir2 <- paste0(root,'/data/GEER/historical/MMSA_MOH/')

mmsa2020 <- read_xlsx(paste0(ddir2,'MMSA2020.xlsx'),skip=1) %>%
  slice(8) %>% 
  select(3:7) %>% 
  pivot_longer(c(1:5),names_to='Y',values_to='value') %>% 
  filter(Y%in%c(2015,2016))
mmsa2022 <- read_xlsx(paste0(ddir2,'MMSA2022.xlsx'),skip=1) %>%
  slice(8) %>% 
  select(3:8) %>% 
  pivot_longer(c(1:6),names_to='Y',values_to='value')

MMSA_MOH <- bind_rows(mmsa2020,mmsa2022) %>% 
  mutate(Y=as.numeric(Y)) %>% 
  filter(Y<=2020)
save(MMSA_MOH, file=paste0(odir,'MMSA_MOH.RData'))

# Input data --------------------------------------------------------------

df <- rgdx.param(paste0(ddir,'input/dem_chem_extension.gdx'),'serv_t') %>%
  rename(R=i1,I=i2,J=i3,H=i4) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(H=as.numeric(H)) %>% 
  filter(H<=2050) %>% 
  select(-I)

# Plot --------------------------------------------------------------------

# NH3
g_NH3 <- df %>%
  filter(J%in%c('NEN_NH3','NEN_NH3U'),H<=2020) %>% 
  left_join(R33_R5) %>% 
  group_by(R5,H) %>% 
  summarise(value=sum(value)) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=R5),stat='identity') +
  geom_path(data=USGS_NH3,aes(x=Year,y=value),color='indianred1',size=1) +
  geom_point(data=USGS_NH3,aes(x=Year,y=value),color='indianred1',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set2')
plot(g_NH3)

# HVC
g_HVC <- df %>%
  filter(J=='NEN_HVC',H<=2020) %>% 
  left_join(R33_R5) %>% 
  group_by(R5,H) %>% 
  summarise(value=sum(value)) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=R5),stat='identity') +
  geom_path(data=NEDO_HVC,aes(x=Y,y=value/1000),color='indianred1',size=1) +
  geom_point(data=NEDO_HVC,aes(x=Y,y=value/1000),color='indianred1',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set2')
plot(g_HVC)

# MOH
g_MOH <- df %>%
  filter(J=='NEN_MOH',H<=2020) %>% 
  left_join(R33_R5) %>% 
  group_by(R5,H) %>% 
  summarise(value=sum(value)) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=R5),stat='identity') +
  geom_path(data=MMSA_MOH,aes(x=Y,y=value/1000),color='indianred1',size=1) +
  geom_point(data=MMSA_MOH,aes(x=Y,y=value/1000),color='indianred1',shape=21,size=3,stroke=1) +
  scale_fill_brewer(palette='Set2')
plot(g_MOH)


g <- df %>% 
  left_join(R33_R5) %>% 
  group_by(R5,J,H) %>% 
  summarise(value=sum(value)) %>% 
  filter(H%in%seq(2005,2050,5)) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=R5),stat='identity') +
  scale_fill_brewer(palette='Set2') +
  facet_wrap(vars(J),scales='free')
plot(g)

g <- df_high %>% 
  left_join(R33_R5) %>% 
  group_by(R5,J,H) %>% 
  summarise(value=sum(value)) %>% 
  filter(H%in%seq(2005,2050,5)) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=value,fill=R5),stat='identity') +
  scale_fill_brewer(palette='Set2') +
  facet_wrap(vars(J),scales='free')
plot(g)
