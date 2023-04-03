
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
library(fuzzyjoin)
library(gdxrrw)

# setting -----------------------------------------------------------------

root <- getwd()
ddir <- paste0(root,'/data/IEA_EneBal/')
odir <- paste0(root,'/output/IEA_EneBal/')

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

# input data --------------------------------------------------------------

country_code <- countrycode::codelist %>% 
  select(country.name.en.regex,iso3c)

R230_106 <- read_delim(paste0(ddir,'define/230_106.map'),skip=2,col_names=FALSE,delim='.') %>%
  rename(iso3c=1, R106=2) %>% 
  mutate(iso3c=str_remove_all(iso3c,' ')) %>% mutate(R106=str_remove_all(R106,' '))

R106_33 <- read_delim(paste0(ddir,'define/R106_32.map'),col_names=FALSE,delim='.') %>% 
  rename(R106=1, R33=2) %>% 
  mutate(R106=str_remove_all(R106,' ')) %>% mutate(R33=str_remove_all(R33,' '))

POP <- rgdx.param(paste0(ddir,'define/serv_global_SSP2.gdx'),'ind_t') %>%
  rename(R33=1,Sv=2,Year=3,ind=4) %>%
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  filter(Year<=2018,Sv!='GDP_PPP') %>%
  filter(Sv=='GDP_MER',R33=='JPN')

df_Ref <- read_csv(paste0(ddir,'OilRefineries.csv')) %>% 
  rename(R=1,H=2)

df_Ref2 <- df_Ref %>% 
  filter(H%in%c(2005:2020)) %>% 
  slice(193:2592) %>% select(1,2,24:45) %>% 
  regex_left_join(country_code,by=c(R='country.name.en.regex'),ignore_case=TRUE) %>% 
  mutate(iso3c=case_when(R=='Republic of Turkiye'~'TUR',
                         TRUE~iso3c)) %>% 
  select(R,iso3c,everything()) %>% select(-country.name.en.regex) %>% 
  drop_na(iso3c) %>%
  mutate(across(-c(R,iso3c),~as.numeric(.))) %>% 
  left_join(R230_106) %>% 
  left_join(R106_33) %>%
  select(-iso3c,-R106) %>% 
  pivot_longer(-c(R,H,R33),names_to='Product',values_to='value') %>% 
  group_by(R33,Product,H) %>%
  summarise(value=sum(value)) %>% 
  filter(value>=0) %>% 
  pivot_wider(names_from=Product,values_from=value,values_fill=0) %>% 
  ungroup() %>% 
  drop_na(R33) %>%
  mutate(Total=apply(.[,c(3:24)],1,sum)) %>% 
  mutate(across(-c(R33,H,Total),~./Total)) %>% 
  select(R33,H,Naphtha) %>% 
  mutate(Naphtha=if_else(is.nan(Naphtha),0,Naphtha)) %>% 
  bind_rows(data.frame(R33='XOC',H=2005:2020,Naphtha=5)) %>% 
  group_by(R33) %>% 
  summarise(Naphtha=mean(Naphtha)) %>% 
  mutate(YEAR='2005') %>% 
  pivot_wider(names_from=YEAR,values_from=Naphtha)
ls_HVC <- wgdx.reshape(df_Ref2,symDim=2,symName="naphtha_yield",setsToo=FALSE)
wgdx.lst(gdxName=paste0(odir,"/naphtha_yield.gdx"),ls_HVC,squeeze=FALSE)

