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
ddir <- paste0(root,'/data/GEER/output/global2050CCU/2304021523/gams_output/gdx_primary/')
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

cpol <- c('Baseline','NPi500','NPi700','NPi1000')
# Sc <- c('Baseline','NPi700_limBioCCS')
# Sc <- c('Baseline','SR15_below')
# Sc <- c('Baseline','NPi500_limBioCCS','NPi700_limBioCCS','NPi1000_limBioCCS')
# Sc <- c('BaU')
techpol <- c('Default')

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
    filter(I=='OIL')
} %>% bind_rows()

ve_l <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'ve_l') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i) %>% 
    filter(I=='OIL')
} %>% bind_rows()

vq_l <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'vq_l') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i) %>% 
    filter(I=='OIL')
} %>% bind_rows()

e_t <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'e_t') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i) %>% 
    filter(I=='OIL')
} %>% bind_rows()

gas_t1 <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'gas_t1') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i) %>% 
    filter(I=='OIL')
} %>% bind_rows()


# Main --------------------------------------------------------------------

df <- vx_l %>% 
  filter(L%in%c('OIL_F_FED','OIL_F_ADD','OIL_F_ADJ'))

g <- df %>% 
  ggplot() +
  geom_bar(aes(x=H,y=vx_l,fill=L),stat='identity') +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sc))
plot(g)
