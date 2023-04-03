
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

# setting -----------------------------------------------------------------

root <- getwd()
ddir <- paste0(root,'/data/MMSA/')
odir <- paste0(root,'/output/MMSA/')

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



# Setting -----------------------------------------------------------------

country_code <- countrycode::codelist %>% 
  select(country.name.en.regex,iso3c)


R230_106 <- read_delim(paste0(ddir,'define/230_106.map'),skip=2,col_names=FALSE,delim='.') %>%
  rename(iso3c=1, R106=2) %>% 
  mutate(iso3c=str_remove_all(iso3c,' ')) %>% mutate(R106=str_remove_all(R106,' '))

R106_33 <- read_delim(paste0(ddir,'define/R106_32.map'),col_names=FALSE,delim='.') %>% 
  rename(R106=1, R33=2) %>% 
  mutate(R106=str_remove_all(R106,' ')) %>% mutate(R33=str_remove_all(R33,' '))

R106_17 <- read_delim(paste0(ddir,'define/region17.map'),col_names=FALSE,delim='.') %>%
  rename(iso3c=1,Sr=2) %>%
  mutate(iso3c=str_remove_all(iso3c,'\t')) %>%  mutate(Sr=str_remove_all(Sr,'\t')) %>% 
  filter(Sr=='CIS')


# Input data --------------------------------------------------------------

SSP2 <- rgdx.param(paste0(ddir,'serv_global_SSP2.gdx'),'ind_t') %>%
  rename(R33=1,Sv=2,Y=3,value=4) %>%
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Y=as.numeric(Y)) %>% 
  filter(Sv!='GDP_PPP') %>% 
  pivot_wider(names_from=Sv,values_from=value) %>%
  mutate(GDP_CAP=GDP_MER/POP)

IEA_NEU <- read_csv(paste0(ddir,'IEA_NEU_ITE.csv')) %>% 
  slice(-1) %>% slice(745:10044) %>% rename(Sr=...1,Y=PRODUCT) %>% 
  mutate(across(-c(Sr),~as.numeric(.))) %>% 
  filter(Y%in%c(2005:2020)) %>% 
  transmute(Sr,Y,value=`Coal and coal products`+`Natural gas`) %>%
  regex_full_join(country_code,c(Sr='country.name.en.regex'),ignore_case=TRUE) %>% 
  drop_na() %>% rename(R106=iso3c) %>% 
  full_join(R106_33) %>% 
  drop_na() %>%
  group_by(R33,Y) %>% 
  summarise(value=sum(value)) %>%
  group_by(Y) %>%
  mutate(share=value/sum(value)) %>% 
  select(-value)


mmsa1 <- read_xlsx(paste0(ddir,'MMSA2020.xlsx'),skip=1) %>%
  slice(8) %>% 
  select(3:7) %>% 
  pivot_longer(c(1:5),names_to='Y',values_to='value') %>% 
  filter(Y%in%c(2015,2016))

mmsa2 <- read_xlsx(paste0(ddir,'MMSA2022.xlsx'),skip=1) %>%
  slice(8) %>% 
  select(3:8) %>% 
  pivot_longer(c(1:6),names_to='Y',values_to='value')

# df_CHN <- bind_rows(mmsa1,mmsa2) %>%
#   mutate(Y=as.numeric(Y)) %>% 
#   mutate(CHN=value*57700/88428.09) %>% 
#   mutate(R33=rep('CHN')) %>% 
#   select(Y,R33,CHN) %>% 
#   rename(value=CHN) %>% 
#   filter(Y!=2021,Y!=2022) 
# 
# df_rest <- IEA_NEU %>% 
#   filter(Y%in%c(2015:2020)) %>% 
#   left_join(bind_rows(mmsa1,mmsa2) %>% mutate(Y=as.numeric(Y))) %>% 
#   mutate(value=(value-value*57700/88428.09)*share) %>% 
#   select(-share) %>% 
#   drop_na()

df <- IEA_NEU %>%
  filter(Y%in%c(2015:2020)) %>%
  left_join(bind_rows(mmsa1,mmsa2) %>% mutate(Y=as.numeric(Y))) %>%
  mutate(value=value*share/1000) %>% # Mt MOH
  select(-share) %>%
  drop_na()
  

g <- df %>% 
  ggplot()+
  geom_area(aes(x=Y,y=value,fill=R33))
plot(g)


# Regression --------------------------------------------------------------

R33_flag <- df %>% 
  group_by(R33) %>% 
  summarise(value=sum(value)) %>% 
  filter(value>0) %>% 
  distinct(R33)

df_future <- df %>% 
  left_join(SSP2) %>% 
  mutate(value_CAP=value/POP)

df_regression <- df %>% 
  left_join(SSP2) %>% 
  mutate(value_CAP=value/POP) %>% 
  filter(R33%in%R33_flag$R33)

g <- df_regression %>%
  ggplot() +
  geom_point(aes(x=GDP_CAP,y=value_CAP,color=R33))
plot(g)

model_wo_dummy <- nls(value_CAP~a/(1+exp(-c*(GDP_CAP-b))),start=list(a=71.48773,b=4,c=0.2),trace=T,data=df_regression)


a <- summary(model_wo_dummy)$coefficient[1]
b <- summary(model_wo_dummy)$coefficient[2]
c <- summary(model_wo_dummy)$coefficient[3]
df_obsprd <- data.frame(obs=df_regression$value_CAP,prd=a/(1+exp(-c*(df_regression$GDP_CAP-b))))
# dummy_mean <- mean(summary(lm(value_CAP~GDP_CAP+R33,data=df_future))$coefficients[3:34])
R2 <- summary(lm(prd~obs,data=df_obsprd))$r.squared

g <- df_regression %>%
  ggplot() +
  geom_point(aes(x=GDP_CAP,y=value_CAP,color=R33)) +
  stat_function(fun=function(x) a/(1+exp(-c*(x-b)))) +
  scale_x_continuous(limits=c(0,NA)) +
  scale_y_continuous(limits=c(0,NA))
plot(g)


# Future extension --------------------------------------------------------

df_extension <- df_future %>%
  full_join(SSP2) %>%
  mutate(value_CAP=case_when(is.na(value_CAP) & Y%in%c(2015:2020)~0,
                   TRUE~value_CAP)) %>% 
  group_by(R33) %>%
  complete(Y=full_seq(c(2005,2100),1)) %>% 
  mutate(value_CAP=na_locf(value_CAP,option='locf')) %>% 
  mutate(value_CAP_prd=case_when(Y>2020 & R33%in%R33_flag$R33 ~ a/(1+exp(-c*(GDP_CAP-b))),
                                 TRUE ~ value_CAP)) %>% 
  mutate(error=value_CAP-value_CAP_prd) %>% 
  mutate(value_prd=value_CAP_prd*POP)

g <- df_extension %>%
  ggplot() +
  geom_area(aes(x=Y,y=value_prd,fill=R33))
plot(g)

df_MOH <- df_extension %>% 
  select(R33,Y,value_prd) %>% 
  mutate(value_prd=value_prd,J='NEN_MOH',I='NEN') %>% # Mt-HVC
  rename(R=R33,H=Y,value=value_prd) %>% 
  select(R,I,J,H,value)
