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
ddir <- paste0(root,'/data/GEER/output/global2050CCU/2304072249/gams_output/gdx_primary/')
ddir2 <- paste0(root,'/data/GEER/output/global2050CCU/2304072249/main/')
odir <- paste0(root,'/output/GEER/plot/0407/')

# R33_R5 <- read.csv(paste0(ddir,'define/R33_R5.csv'),header=FALSE) %>% 
#   rename(R=1,R5=2)


## Input data -------------------------------------------------------------
# Load GDX file -----------------------------------------------------------

cpol <- c('Baseline','NPi500','NPi700','NPi1000')
techpol <- c('Default','limCCS','Biofueloff')

list_Sc <- expand.grid(cpol=cpol,techpol=techpol) %>% 
  mutate(Sc=paste0(cpol,'_',techpol)) %>% 
  mutate(Sc=str_remove_all(Sc,'_Default')) 

cpol_techpol <- list_Sc %>%
  filter(!str_detect(Sc,'Baseline_')&Sc!='NPi500_limCCS') %>% 
  select(Sc)

Sc <- cpol_techpol$Sc

vr_l <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'vr_l') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i) %>% 
    filter(I=='NEN')
} %>% bind_rows()

bn_t1 <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'bn_t1') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i) %>% 
    filter(I=='NEN')
} %>% bind_rows()


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

# Main --------------------------------------------------------------------

df_invc <- vr_l %>% 
  select(-I) %>% 
  left_join(bn_t1) %>% 
  mutate(value=bn_t1*vr_l) %>% 
  filter(!str_detect(L,'NEN')) %>% 
  mutate(J2=case_when(str_detect(L,'HVC')~'HVC',
                      str_detect(L,'MOH')~'Methanol',
                      str_detect(L,'NH3')~'Ammonia')) %>% 
  group_by(L,H,Sc,J2) %>% 
  summarise(value=sum(value)) %>% 
  group_by(L,Sc,J2) %>% 
  complete(H=c(2005:2050)) %>% 
  ungroup() %>% 
  mutate(Y5=5*floor(H/5-401)+2005) %>%
  replace_na(list(value=0)) %>% 
  group_by(Sc,Y5,J2) %>% 
  summarise(value=mean(value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=Sc,values_from=value) %>% 
  mutate(across(-c(Y5,J2),~.-Baseline)) %>% 
  pivot_longer(cols=-c(Y5,J2),names_to='Sc',values_to='value') %>% 
  left_join(list_Sc) %>% 
  mutate(techpol=as.character(techpol)) %>% 
  mutate(techpol=recode(techpol,'Biofueloff'='limBio')) %>% 
  mutate(cpol=str_remove(cpol,'NPi')) %>% 
  mutate(cpol=case_when(cpol!='Baseline'~paste0(cpol,'C'),
                        TRUE~cpol),
         techpol=case_when(techpol=='limCCS'~'NoCCS',
                           TRUE~techpol)) %>% 
  mutate(techpol=factor(techpol,levels=c('Default','NoCCS','limBio')))
  

df_invc_total <- df_invc %>% 
  group_by(Sc,Y5) %>% 
  summarise(value=sum(value)) %>% 
  left_join(list_Sc) %>% 
  mutate(techpol=as.character(techpol)) %>% 
  mutate(techpol=recode(techpol,'Biofueloff'='limBio')) %>% 
  mutate(cpol=str_remove(cpol,'NPi')) %>% 
  mutate(cpol=case_when(cpol!='Baseline'~paste0(cpol,'C'),
                        TRUE~cpol),
         techpol=case_when(techpol=='limCCS'~'NoCCS',
                           TRUE~techpol)) %>% 
  mutate(techpol=factor(techpol,levels=c('Default','NoCCS','limBio')))

g <- df_invc %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value,fill=J2),stat='identity')
plot(g)

g <- df_invc_total %>% 
  ggplot() +
  geom_line(aes(x=Y5,y=value,color=techpol,group=Sc,linetype=cpol),size=0.5) +
  geom_point(aes(x=Y5,y=value,color=techpol,shape=cpol),size=1.5,stroke=1,fill='white') +
  scale_color_manual(values=c('indianred2','palegreen3','deepskyblue3'),guide=guide_legend(ncol=2)) +
  scale_linetype_manual(values=c('solid','solid','solid'),guide=guide_legend(ncol=2)) +
  scale_shape_manual(values=c(22,21,24)) +
  scale_y_continuous(limits=c(0,NA)) +
  MyTheme +
  theme(legend.position='bottom',
        legend.text = element_text(size = 10))
plot(g)
