
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
ddir <- paste0(root,'/data/NEDO_HVC/')
odir <- paste0(root,'/output/NEDO_HVC/')

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

year <- c(2005:2014,2016:2017)

country_list <- read.table(paste0(ddir,'define/country_list.txt')) %>% 
  mutate(Country2=paste0(Country,'e')) %>% rename(Country1=1)

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

df_ethylene <- read_xlsx(paste0(ddir,'04_2019syouhinbetudeta.xlsx'),sheet=2,col_names=FALSE) %>%
  slice(35:63) %>% select(-1,-18,-19) %>% rename(Sr1=1,Sr2=2) %>% replace_na(list(Sr1='',Sr2='')) %>% 
  unite(col=Sr,c(1,2),sep='') %>% drop_na() %>% 
  mutate(across(c(2:15),~as.numeric(.))) %>% 
  pivot_longer(cols=-c(Sr),names_to='Y',names_prefix='...',names_transform=as.numeric,values_to='value') %>% 
  mutate(Y=Y+2006) %>% 
  mutate(Product='ethylene')

df_propylene <- read_xlsx(paste0(ddir,'04_2019syouhinbetudeta.xlsx'),sheet=13,col_names=FALSE) %>% 
  slice(35:63) %>% select(-1,-18,-19) %>% rename(Sr1=1,Sr2=2) %>% replace_na(list(Sr1='',Sr2='')) %>% 
  unite(col=Sr,c(1,2),sep='') %>% drop_na() %>% 
  mutate(across(c(2:15),~as.numeric(.))) %>% 
  pivot_longer(cols=-c(Sr),names_to='Y',names_prefix='...',names_transform=as.numeric,values_to='value') %>% 
  mutate(Y=Y+2006) %>% 
  mutate(Product='propylene')

df_benzene <- read_xlsx(paste0(ddir,'04_2019syouhinbetudeta.xlsx'),sheet=17,col_names=FALSE) %>%
  slice(35:63) %>% select(-1,-18,-19) %>% rename(Sr1=1,Sr2=2) %>% replace_na(list(Sr1='',Sr2='')) %>% 
  unite(col=Sr,c(1,2),sep='') %>% drop_na() %>% 
  mutate(across(c(2:15),~as.numeric(.))) %>% 
  pivot_longer(cols=-c(Sr),names_to='Y',names_prefix='...',names_transform=as.numeric,values_to='value') %>% 
  mutate(Y=Y+2006) %>% 
  mutate(Product='benzene')

df_toluene <- read_xlsx(paste0(ddir,'04_2019syouhinbetudeta.xlsx'),sheet=18,col_names=FALSE) %>%
  slice(35:63) %>% select(-1,-18,-19) %>% rename(Sr1=1,Sr2=2) %>% replace_na(list(Sr1='',Sr2='')) %>% 
  unite(col=Sr,c(1,2),sep='') %>% drop_na() %>% 
  mutate(across(c(2:15),~as.numeric(.))) %>% 
  pivot_longer(cols=-c(Sr),names_to='Y',names_prefix='...',names_transform=as.numeric,values_to='value') %>% 
  mutate(Y=Y+2006) %>% 
  mutate(Product='toluene')

df_xylene <- read_xlsx(paste0(ddir,'04_2019syouhinbetudeta.xlsx'),sheet=19,col_names=FALSE) %>%
  slice(35:63) %>% select(-1,-18,-19) %>% rename(Sr1=1,Sr2=2) %>% replace_na(list(Sr1='',Sr2='')) %>% 
  unite(col=Sr,c(1,2),sep='') %>% drop_na() %>% 
  mutate(across(c(2:15),~as.numeric(.))) %>% 
  pivot_longer(cols=-c(Sr),names_to='Y',names_prefix='...',names_transform=as.numeric,values_to='value') %>% 
  mutate(Y=Y+2006) %>% 
  mutate(Product='xylene')

df_europe <- read_xlsx(paste0(ddir,'03_2019plant.xlsx'),sheet=13,col_names=FALSE) %>%
  select(1,2) %>% drop_na(...2) %>% rename(Sr=1,Sc=2)
df_europe[1:14,1] <- 'Western Europe'; df_europe[15:21,1] <- 'Other Europe'

df_me_africa <- read_xlsx(paste0(ddir,'03_2019plant.xlsx'),sheet=19,col_names=FALSE) %>%
  select(1,2) %>% drop_na(...2) %>% slice(-1,-3,-10,-15) %>% rename(Sr=1,Sc=2)
df_me_africa[1:7,1] <- 'Middle East'; df_me_africa[8:11,1] <- 'Africa'

df_latinamerica <- read_xlsx(paste0(ddir,'03_2019plant.xlsx'),sheet=37,col_names=FALSE) %>%
  select(1,2) %>% drop_na(...2) %>% slice(3:5) %>% rename(Sr=1,Sc=2)
df_latinamerica[1:3,1] <- 'Other Latin America'

df_remap <- bind_rows(df_europe,df_me_africa,df_latinamerica) %>% 
  mutate(iso3c=c('BEL','FRA','DEU','GRC','ITA','NLD','ESP','GBR','PRT','AUT','FIN','NOR','CHE','SWE',
                 'BGR','SVK','CZE','HUN','POL','ROU','SRB',
                 'IRN','ISR','KWT','QAT','OMN','ARE','TUR',
                 'DZA','EGY','NGA','ZAF',
                 'ARG','COL','VEN')) %>%
  slice(-21) %>% select(-Sc) %>% 
  bind_rows(data.frame(Sr=rep('Other Europe'),iso3c=c('SRB','BIH','HRV','MKD','SVN','MNE'))) %>% 
  bind_rows(data.frame(Sr=rep('Other Oceania'),iso3c=c('NZL','XOC')))

df_all <- bind_rows(df_ethylene,df_propylene,df_benzene,df_toluene,df_xylene) %>% 
  group_by(Sr,Y) %>% 
  summarise(value=sum(value)) %>%
  mutate(Sr=str_remove_all(Sr,'[^\x01-\x7E]')) %>% 
  mutate(Sr=str_remove_all(Sr,'[0-9]')) %>% 
  mutate(Sr=str_remove_all(Sr,'^ *')) %>% 
  mutate(Sr=recode(Sr,'China including HK'='China'))

df <- df_all %>%
  pivot_wider(names_from=Sr,values_from=value) %>% 
  mutate(Y,`Other Oceania`=Oceania-Australia,`Other Europe`=`Europe`-`Western Europe`,
         `Middle East`=`Middle East`-`Saudi Arabia`) %>%
  select(-Asia,-Oceania,-Europe,-`Total Production`,-`North America`,-`Latin America`) %>% 
  pivot_longer(cols=-c(Y),names_to='Sr',values_to='value')

Region_NEDO <- df %>% 
  distinct(Sr) %>% 
  regex_full_join(country_code,by=c(Sr='country.name.en.regex'),ignore_case=TRUE) %>% 
  drop_na(Sr) %>% select(-2) %>%
  full_join(R106_17) %>% 
  slice(-33) %>% 
  full_join(df_remap) %>% 
  drop_na(iso3c)

# Oil product input as non-energy use in chemical sector
df_IEA <- read_csv(paste0(ddir,'IEA_EneBal.csv')) %>% 
  slice(-1) %>% rename(Sc=1) %>% slice(13:157) %>% select(1,52:60) %>% 
  mutate(across(-c(Sc),~str_replace_all(.,'\\.\\.','0') %>% as.numeric(.))) %>% 
  regex_left_join(country_code,by=c(Sc='country.name.en.regex'),ignore_case=TRUE) %>% 
  drop_na(iso3c) %>% 
  select(-Sc,-country.name.en.regex) %>% 
  pivot_longer(cols=-c(iso3c),names_to='Y',names_transform=as.numeric,values_to='value')

# Share in NEDO region
df_remap_share <- df_IEA %>% 
  full_join(Region_NEDO) %>% 
  drop_na(Sr) %>% 
  select(Sr,iso3c,Y,value) %>% 
  complete(iso3c,Y) %>% 
  drop_na(Y,Sr) %>% 
  group_by(Sr,Y) %>% 
  mutate(share=value/sum(value)) %>% 
  replace_na(list(share=1)) %>% 
  select(-value) %>% 
  pivot_wider(names_from=Y,values_from=share) %>% 
  mutate(`2019`=`2018`,`2020`=`2018`,`2021`=`2018`,`2022`=`2018`,`2023`=`2018`) %>% 
  pivot_longer(cols=-c(iso3c,Sr),names_to='Y',names_transform=as.numeric,values_to='share')

df2 <- df %>% 
  left_join(df_remap_share) %>% 
  mutate(value=value*share) %>% 
  select(iso3c,Y,value) %>% 
  drop_na(iso3c) %>% 
  left_join(R106_33,by=c('iso3c'='R106')) %>% 
  mutate(R33=case_when(iso3c=='SRB'~'XENI',
                       iso3c=='BIH'~'XENI',
                       iso3c=='MKD'~'XENI',
                       iso3c=='BEL'~'XE15',
                       iso3c=='MNE'~'XENI',
                       TRUE~R33)) %>% 
  bind_rows(data.frame(iso3c=c(NA,NA,NA),Y=c(2010,2010,2010),value=c(0,0,0),R33=c('XEA','XOC','XSA'))) %>% 
  complete(R33) %>% 
  group_by(R33,Y) %>% 
  summarise(value=sum(value))


# Regression by using country dummy ---------------------------------------

SSP2 <- rgdx.param(paste0(ddir,'serv_global_SSP2.gdx'),'ind_t') %>%
  rename(R33=1,Sv=2,Y=3,value=4) %>%
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Y=as.numeric(Y)) %>% 
  filter(Sv!='GDP_PPP') %>% 
  pivot_wider(names_from=Sv,values_from=value) %>%
  mutate(GDP_CAP=GDP_MER/POP)

R33_17 <- read.delim(paste0(ddir,"define/region17_enduse.map"),header=F,sep='\t') %>% 
  select(-V2) %>% rename(R33=1,R17=2)
R33_17[30,1] <- "XE3"; R33_17[33,1] <- "XNF"


df_future <- df2 %>% 
  left_join(SSP2) %>% 
  mutate(value_CAP=value*1000/POP) %>% 
  mutate(R33=as.factor(R33))
df_future_all <- df2 %>% 
  left_join(SSP2) %>% 
  mutate(value_CAP=value*1000/POP) %>% 
  mutate(R33=as.factor(R33))


# df_fit <- df_future %>%
#   ungroup() %>%
#   group_by(R33) %>%
#   summarise(GDP_CAP1=max(GDP_CAP),value_CAP1=max(value_CAP),GDP_CAP2=mean(GDP_CAP),value_CAP2=mean(value_CAP)) %>%
#   # mutate(log_value=log(value_CAP1)) %>%
#   # mutate(log_value=replace(log_value, which(is.infinite(log_value) & (log_value < 0)), 0)) %>%
#   left_join(df_slope) %>% 
#   mutate(slope=replace(slope, which(is.nan(slope)), 0)) %>%
#   mutate(slope=replace(slope, which(slope<0), 0)) %>% 
#   mutate(a=case_when(R33=='ARG'~log(17.1338910+1.7590406*(50-17.1338910)),
#                      TRUE~value_CAP1+slope*(50-GDP_CAP1)-17.1338910+1.7590406*(50-14.2024571))) %>%
#   mutate(a=replace(a, which(is.infinite(a) & (a < 0)), 0)) %>%
#   mutate(a=replace(a, which(is.nan(a)), 0)) %>%
#   mutate(a2=value_CAP1+slope*(50-GDP_CAP1)) %>%
#   mutate(a2=replace(a2, which(is.infinite(a2) & (a2 < 0)), 0)) %>% 
#   mutate(a2=replace(a2, which(is.nan(a2)), 0))
# Rlist <- df_fit %>% distinct(R33)

# df_gdx <- df_future %>% 
#   rename(CNS=value,GDP=GDP_MER,GDPP=GDP_CAP) %>% 
#   select(-value_CAP) %>% 
#   pivot_longer(cols=-c(R33,Y),names_to='Sv',values_to='value') %>% 
#   pivot_wider(names_from='Y',values_from='value')
# 
# wgdx.reshape(df_gdx,symDim=3,symName='DATA',tName='Y',gdxName = paste0(odir,'/HVCprod.gdx'),setsToo=F)

#  filter(R33!='KOR',R33!='NZL',R33!='XAF',R33!='ZAF',R33!='XCS',R33!='XEWI',R33!='AUS')
# 
# df_future2 <- makedummies(df_future, basal_level = TRUE)
# 


df_slope <- levels(df_future_all$R33) %>% 
  purrr::map(function(i){
    hoge <- df_future_all %>% 
      filter(R33==i)
    slope <- lm(value_CAP~GDP_CAP,data=hoge)$coefficients[[2]]
    data.frame(R33=i,slope=slope)
  }) %>% bind_rows() %>% replace_na(list(slope=0))

df_slope[[1]]


df_future <- df2 %>% 
  left_join(SSP2) %>% 
  mutate(value_CAP=value*1000/POP) %>% 
  mutate(R33=as.factor(R33))

R33_flag <- df_future %>% 
  group_by(R33) %>% 
  summarise(value=sum(value)) %>% 
  filter(value>0) %>% 
  distinct(R33)

df_regression <- df_future %>% 
  filter(R33%in%R33_flag$R33)

# df_fit2 <- df_future %>%
#   ungroup() %>%
#   group_by(R33) %>%
#   summarise(GDP_CAP1=max(GDP_CAP),value_CAP1=max(value_CAP),GDP_CAP2=mean(GDP_CAP),value_CAP2=mean(value_CAP)) %>%
#   # mutate(log_value=log(value_CAP1)) %>%
#   # mutate(log_value=replace(log_value, which(is.infinite(log_value) & (log_value < 0)), 0)) %>%
#   left_join(df_slope) %>% 
#   mutate(slope=replace(slope, which(is.nan(slope)), 0)) %>%
#   mutate(slope=replace(slope, which(slope<0), 0)) %>% 
#   mutate(a=case_when(slope>7~2*value_CAP2,
#                      slope>1~5/4*value_CAP2,
#                      TRUE~value_CAP2)) %>% 
#   mutate(b=case_when(slope>7~GDP_CAP2,
#                      slope>1~2/3*GDP_CAP2,
#                      TRUE~GDP_CAP2/2))
# Rlist <- df_fit %>% distinct(R33)

# df_fit[5,8] <- 4 

model_wo_dummy <- nls(value_CAP~a/(1+exp(-c*(GDP_CAP-b))),start=list(a=71.48773,b=4,c=0.2),trace=T,data=df_regression)
summary(model_wo_dummy)

# model_w_dummy <- gnls(value_CAP~exp(a/(1+exp(-c*(GDP_CAP-b)))),params=list(a~R33,b+c~1),
#                       start=c(df_fit$a2,
#                               rep(coef(model_wo_dummy)[[2]],length(levels(df_future$R33))-31),
#                               coef(model_wo_dummy)[[3]]),
#                       control = list(method = "L-BFGS-B"),
#                       data=df_future)



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
  group_by(R33) %>%
  complete(Y=full_seq(c(2005,2100),1)) %>% 
  mutate(value_CAP=na_locf(value_CAP,option='locf')) %>% 
  mutate(value_CAP_prd=case_when(Y>2023 & R33%in%R33_flag$R33 ~ a/(1+exp(-c*(GDP_CAP-b))),
                             TRUE ~ value_CAP)) %>% 
  mutate(error=value_CAP-value_CAP_prd) %>% 
  mutate(value_prd=value_CAP_prd*POP)

g <- df_extension %>%
  ggplot() +
  geom_area(aes(x=Y,y=value_prd,fill=R33))
plot(g)

df_HVC <- df_extension %>% 
  select(R33,Y,value_prd) %>% 
  mutate(value_prd=value_prd/10^6,J='NEN_HVC',I='NEN') %>% # Mt-HVC
  rename(R=R33,H=Y,value=value_prd) %>% 
  select(R,I,J,H,value)

# model_w_dummy <- gnls(value_CAP~a/(1+exp(-c*(GDP_CAP-b))),params=list(a~R33,b~1,c~1),
#                       start=c(df_fit2$a,
#                               # df_fit2$b,
#                               coef(model_wo_dummy)[[2]],
#                               coef(model_wo_dummy)[[3]]),
#                       control = list(method = "L-BFGS-B"),
#                       data=df_future,trace=T)
# 
# 
# plot(df_future$GDP_CAP,df_future$value_CAP)
# 
# model_w_dummy <- gnls(value_CAP~a/(1+exp(-c*(GDP_CAP-b))),params=list(a~R33,b~1,c~1),
#                       start=c(df_fit2$a,
#                               # df_fit2$b,
#                               coef(model_wo_dummy)[[2]],
#                               coef(model_wo_dummy)[[3]]),
#                       control = list(method = "L-BFGS-B"),
#                       data=df_future,trace=T)
# 
# 
# plot(df_future$GDP_CAP,df_future$value_CAP)
# 
# model_w_dummy <- gnls(value_CAP~exp(a/(1+exp(-c*(GDP_CAP-b)))),params=list(a~R33,b~R33,c~1),
#                       start=c(c(6.55,6.55),
#                               c(-1,9),
#                               0.22),
#                       control = list(method = "L-BFGS-B"),
#                       data=df_future)


# alpha <- summary(nls(value_CAP~a*exp(-b/GDP_CAP),start=c(a=1,b=1),data=df_future))$coefficients[1]
# beta <- summary(nls(value_CAP~a*exp(-b/GDP_CAP),start=c(a=1,b=1),data=df_future))$coefficients[2]
# R2 <- data.frame(ovs=df_future$value_CAP,prd=alpha*exp(-beta/df_future$GDP_CAP))
#
# g <- df_future %>%
#   ggplot() +
#   geom_point(aes(x=GDP_CAP,y=value_CAP,color=R33)) +
#   stat_function(fun=function(x) alpha*exp(-beta/x)) +
#   annotate('text',x=Inf,y=-Inf,label=paste0('R2 = ',summary(lm(R2$prd~R2$ovs))$r.squared),hjust=1,vjust=-1)
# plot(g)
#
#
# df_future2 <- df2 %>%
#   left_join(R33_17) %>%
#   left_join(SSP2) %>%
#   group_by(R17,Y) %>%
#   summarise(value=sum(value),POP=sum(POP),GDP_MER=sum(GDP_MER)) %>%
#   mutate(value_CAP=value/POP,GDP_CAP=GDP_MER/POP)
#
# alpha <- summary(nls(value_CAP~a*exp(-b/GDP_CAP),start=c(a=1,b=1),data=df_future2))$coefficients[2]
# beta <- summary(nls(value_CAP~a*exp(-b/GDP_CAP),start=c(a=1,b=1),data=df_future2))$coefficients[1]
# R2 <- summary(lm(value_CAP~GDP_CAP+R33,data=df_future))$r.squared
#
# g <- df_future2 %>%
#   ggplot() +
#   geom_point(aes(x=GDP_CAP,y=value_CAP,color=R17)) +
#   stat_function(fun=function(x) alpha*exp(-beta/x)) +
#   annotate('text',x=Inf,y=-Inf,label=paste0('R2 = ',summary(lm(R2$prd~R2$ovs))$r.squared),hjust=1,vjust=-1)
# plot(g)
#
# #
# alpha <- summary(lm(value_CAP~GDP_CAP+R33,data=df_future))$coefficient[2]
# beta <- summary(lm(value_CAP~GDP_CAP+R33,data=df_future))$coefficient[1]
# dummy_mean <- mean(summary(lm(value_CAP~GDP_CAP+R33,data=df_future))$coefficients[3:34])
# R2 <- summary(lm(value_CAP~GDP_CAP+R33,data=df_future))$r.squared
# 
# g <- df_future %>%
#   ggplot() +
#   geom_point(aes(x=GDP_CAP,y=value_CAP,color=R33)) +
#   stat_function(fun=function(x) alpha*x+beta+dummy_mean) +
#   annotate('text',x=Inf,y=-Inf,label=paste0('R2 = ',R2),hjust=1,vjust=-1)
# plot(g)
# 
# df_coeff <- data.frame(R33=df_future %>% distinct(R33),dummy=c(0,summary(lm(value_CAP~GDP_CAP+R33,data=df_future))$coefficients[3:34]))


