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
ddir <- paste0(root,'/data/GEER/output/global2010/2303261725/gams_output/gdx_primary/')
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

R33_R5 <- read.csv(paste0(root,'/data/check/define/R33_R5.csv'),header=FALSE) %>% 
  rename(R=1,R5=2)


# Input data --------------------------------------------------------------

Sc <- c('BaU')

vx_l <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'vx_l') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i)
} %>% bind_rows()

ve_l <- foreach(i=Sc,.combine='rbind') %do% {
  tmp <- rgdx.param(paste0(ddir,i,'.gdx'),'vx_l') %>% 
    mutate(across(where(is.factor),~as.character(.))) %>% 
    mutate(H=as.numeric(H)) %>% 
    mutate(Sc=i)
} %>% bind_rows()


# Main --------------------------------------------------------------------


