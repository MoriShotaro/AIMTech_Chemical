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
ddir <- paste0(root,'/data/IEA2009/')
odir <- paste0(root,'/output/IEA2009/')

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


# Input data --------------------------------------------------------------

# demand kg/cap
HVC_low <- read_xlsx(paste0(ddir,'IEA2009_Table4_4.xlsx'),sheet=1) %>% 
  rename(R=...1) %>% mutate(J='NEN_HVC',Sc='low') %>% 
  pivot_longer(cols=-c(R,J,Sc),names_to='Y',values_to='value',names_transform=as.numeric)
HVC_high <- read_xlsx(paste0(ddir,'IEA2009_Table4_4.xlsx'),sheet=2) %>% 
  rename(R=...1) %>% mutate(J='NEN_HVC',Sc='high') %>% 
  pivot_longer(cols=-c(R,J,Sc),names_to='Y',values_to='value',names_transform=as.numeric)
NH3_low <- read_xlsx(paste0(ddir,'IEA2009_Table4_4.xlsx'),sheet=3) %>% 
  rename(R=...1) %>% mutate(J='NEN_NH3',Sc='low') %>% 
  pivot_longer(cols=-c(R,J,Sc),names_to='Y',values_to='value',names_transform=as.numeric)
NH3_high <- read_xlsx(paste0(ddir,'IEA2009_Table4_4.xlsx'),sheet=4) %>% 
  rename(R=...1) %>% mutate(J='NEN_NH3',Sc='high') %>% 
  pivot_longer(cols=-c(R,J,Sc),names_to='Y',values_to='value',names_transform=as.numeric)
MOH_low <- read_xlsx(paste0(ddir,'IEA2009_Table4_4.xlsx'),sheet=5) %>% 
  rename(R=...1) %>% mutate(J='NEN_MOH',Sc='low') %>% 
  pivot_longer(cols=-c(R,J,Sc),names_to='Y',values_to='value',names_transform=as.numeric)
MOH_high <- read_xlsx(paste0(ddir,'IEA2009_Table4_4.xlsx'),sheet=6) %>% 
  rename(R=...1) %>% mutate(J='NEN_MOH',Sc='high') %>% 
  pivot_longer(cols=-c(R,J,Sc),names_to='Y',values_to='value',names_transform=as.numeric)

df_IEA2009 <- bind_rows(HVC_low,HVC_high,NH3_low,NH3_high,MOH_low,MOH_high) %>% 
  group_by(R,J,Sc) %>% 
  complete(Y=seq(2005,2100,1)) %>% 
  mutate(value=na_interpolation(value)) %>% 
  rename(value_CAP=value)

g <- df_IEA2009 %>% 
  filter(Y%in%c(2005:2050),Sc=='low') %>% 
  ggplot() +
  geom_path(aes(x=Y,y=value_CAP,color=R)) +
  facet_wrap(vars(J))
plot(g)

# region definition
R33_IEA2009 <- read_csv(paste0(ddir,'IEA2009_R33.csv'),col_names=FALSE) %>% 
  rename(R33=1,R=2)

# population
POP <- rgdx.param(paste0(ddir,'serv_global_SSP2.gdx'),'ind_t') %>%
  rename(R33=1,Sv=2,Y=3,value=4) %>%
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Y=as.numeric(Y)) %>% 
  filter(Sv=='POP') %>% 
  select(-Sv) %>% rename(POP=value)

# future extention
df_demand <- POP %>% 
  left_join(R33_IEA2009) %>%
  left_join(df_IEA2009) %>% 
  mutate(value=value_CAP*POP/10^6) # thousand kg -> Mt

# output to GDX file
df_demandGDX <- df_demand %>% 
  filter(Sc=='low') %>% 
  select(-POP,-R,-value_CAP,-Sc) %>% 
  mutate(I='NEN') %>% rename(R=R33,H=Y) %>% 
  pivot_wider(names_from=H,values_from=value) %>% 
  select(R,I,J,everything())

df_demandGDX_high <- df_demand %>% 
  filter(Sc=='low') %>% 
  select(-POP,-R,-value_CAP,-Sc) %>% 
  mutate(I='NEN') %>% rename(R=R33,H=Y) %>% 
  pivot_wider(names_from=H,values_from=value) %>% 
  select(R,I,J,everything())

# other chemical demand
df_other <- expand.grid(R=distinct(df_demandGDX,R)$R,H=c(2005:2100),J='NEN_OTH',value=0,I='NEN')

# output to GDX file w/ other
df_demandGDX2 <- df_demand %>% 
  filter(Sc=='low') %>% 
  select(-POP,-R,-value_CAP,-Sc) %>% 
  mutate(I='NEN') %>% rename(R=R33,H=Y) %>% 
  bind_rows(df_other) %>% 
  pivot_wider(names_from=H,values_from=value) %>% 
  select(R,I,J,everything())
wgdx.reshape(df_demandGDX2,symDim=4,symName="serv_t",setsToo=FALSE,gdxName=paste0(odir,'dem_chem_extention.gdx'))

df_demandGDX2_high <- df_demand %>% 
  filter(Sc=='high') %>% 
  select(-POP,-R,-value_CAP,-Sc) %>% 
  mutate(I='NEN') %>% rename(R=R33,H=Y) %>% 
  bind_rows(df_other) %>% 
  pivot_wider(names_from=H,values_from=value) %>% 
  select(R,I,J,everything())
wgdx.reshape(df_demandGDX2_high,symDim=4,symName="serv_t",setsToo=FALSE,gdxName=paste0(odir,'dem_chem_extension_high.gdx'))

# output to GDX file w/ other + only methanol is high demand
df_demandGDX3 <- df_demand %>% 
  filter(Sc=='low'&J%in%c('NEN_HVC','NEN_NH3') | Sc=='high'&J=='NEN_MOH') %>% 
  select(-POP,-R,-value_CAP,-Sc) %>% 
  mutate(I='NEN') %>% rename(R=R33,H=Y) %>% 
  bind_rows(df_other) %>% 
  pivot_wider(names_from=H,values_from=value) %>% 
  select(R,I,J,everything())
wgdx.reshape(df_demandGDX3,symDim=4,symName="serv_t",setsToo=FALSE,gdxName=paste0(odir,'dem_chem_extension.gdx'))

# output to GDX file w/ other + only methanol is high demand + consideration of urea production
df_demandGDX4 <- df_demand %>% 
  filter(Sc=='low'&J%in%c('NEN_HVC','NEN_NH3') | Sc=='high'&J=='NEN_MOH') %>% 
  select(-POP,-R,-value_CAP,-Sc) %>% 
  pivot_wider(names_from=J,values_from=value) %>% rename(NEN_NH3_TOTAL=NEN_NH3) %>% 
  mutate(NEN_NH3=NEN_NH3_TOTAL*(1-0.483),NEN_NH3U=NEN_NH3_TOTAL*0.483) %>% 
  select(-NEN_NH3_TOTAL) %>% 
  pivot_longer(cols=-c(R33,Y),names_to='J',values_to='value') %>% 
  mutate(I='NEN') %>% rename(R=R33,H=Y) %>% 
  bind_rows(df_other) %>% 
  pivot_wider(names_from=H,values_from=value) %>% 
  select(R,I,J,everything())
wgdx.reshape(df_demandGDX4,symDim=4,symName="serv_t",setsToo=FALSE,gdxName=paste0(odir,'dem_chem_extension.gdx'))



# Global value adjustment -------------------------------------------------

df_adj <- df_demandGDX4 %>%
  pivot_longer(cols=-c(R,I,J),names_to='H',values_to='value',names_transform=as.numeric) %>% 
  group_by(I,J,H) %>% 
  summarise(value=sum(value))

df_IEA2022 <- data.frame(H=c(2005:2020),
                         HVC=c(123,129,137,130,131,139,142,144,150,153,159,164,170,177,182,185),
                         MOH=c(115,124,136,139,145,173,172,186,196,224,245,259,276,292,326,314),
                         NH3=c(111,113,118,117,116,120,124,127,133,131,135,134,134,137,139,142)) %>% 
  mutate(HVC=HVC/123,MOH=MOH/115,NH3=NH3/111) %>% 
  mutate(HVC=HVC*277.61777,MOH=MOH*38.87917,NH3=NH3*(66.64178+81.45107)) %>% 
  pivot_longer(cols=c(HVC,MOH,NH3),names_to='J',values_to='value') %>% 
  group_by(H) %>% 
  summarise(value=sum(value))

# Plot --------------------------------------------------------------------

g <- df_demand %>% 
  group_by(J,Sc,Y) %>% 
  summarise(value=sum(value)) %>% 
  ggplot() +
  geom_path(aes(x=Y,y=value,color=Sc)) +
  facet_wrap(vars(J))
plot(g)

g <- df_demand %>% 
  filter(Sc=='low') %>%
  group_by(Y,R33) %>% 
  summarise(value=sum(value)) %>% 
  ggplot() +
  geom_area(data=df_IEA2022,aes(x=H,y=value)) +
  geom_bar(aes(x=Y,y=value,fill=R33),stat='identity')
plot(g)


# Feedstock constraint ----------------------------------------------------

R33 <- distinct(df_demand,R33)

NH3_feed <- R33 %>% 
  mutate(NENNH3G=case_when(R33=='JPN'~1,
                             R33=='USA'~0.97,
                             R33=='BRA'~1,
                             R33=='CAN'~1,
                             R33=='CHN'~0.22,
                             R33=='IND'~0.70,
                             R33=='KOR'~0,
                             R33=='XME'~1,
                             R33=='XE15'~0.815,
                             TRUE~0.71),
         NENNH3C=case_when(R33=='JPN'~0,
                             R33=='USA'~0,
                             R33=='BRA'~0,
                             R33=='CAN'~0,
                             R33=='CHN'~0.78,
                             R33=='IND'~0,
                             R33=='KOR'~0,
                             R33=='XME'~0,
                             R33=='XE15'~0,
                             TRUE~0.21)) %>% 
  mutate(NENNH3O=1-NENNH3G-NENNH3C)

NH3U_feed <- R33 %>% 
  mutate(NENNH3UG=case_when(R33=='JPN'~1,
                             R33=='USA'~0.97,
                             R33=='BRA'~1,
                             R33=='CAN'~1,
                             R33=='CHN'~0.22,
                             R33=='IND'~0.70,
                             R33=='KOR'~0,
                             R33=='XME'~1,
                             R33=='XE15'~0.815,
                             TRUE~0.71),
         NENNH3UC=case_when(R33=='JPN'~0,
                             R33=='USA'~0,
                             R33=='BRA'~0,
                             R33=='CAN'~0,
                             R33=='CHN'~0.78,
                             R33=='IND'~0,
                             R33=='KOR'~0,
                             R33=='XME'~0,
                             R33=='XE15'~0,
                             TRUE~0.21)) %>% 
  mutate(NENNH3UO=1-NENNH3UG-NENNH3UC)

HVC_feed <- R33 %>% 
  mutate(NENHVCO=case_when(R33=='JPN'~0.96,
                             R33=='USA'~0.38,
                             R33=='BRA'~0.99,
                             R33=='CAN'~0.11,
                             R33=='CHN'~1,
                             R33=='IND'~0.68,
                             R33=='KOR'~1,
                             R33=='XME'~0.21,
                             R33=='XE15'~0.8775,
                             TRUE~0.65)) %>% 
  mutate(NENHVCG=1-NENHVCO)

MOH_feed <- R33 %>% 
  mutate(NENMOHG=case_when(R33=='JPN'~1,
                             R33=='USA'~0.71,
                             R33=='BRA'~1,
                             R33=='CAN'~1,
                             R33=='CHN'~0,
                             R33=='IND'~0.69,
                             R33=='KOR'~1,
                             R33=='XME'~1,
                             R33=='XE15'~0.8125,
                             TRUE~0.47),
         NENMOHC=case_when(R33=='JPN'~0,
                             R33=='USA'~0,
                             R33=='BRA'~0,
                             R33=='CAN'~0,
                             R33=='CHN'~1,
                             R33=='IND'~0,
                             R33=='KOR'~0,
                             R33=='XME'~0,
                             R33=='XE15'~0.0125,
                             TRUE~0.53)) %>% 
  mutate(NENMOHO=1-NENMOHG-NENMOHC)


# Check -------------------------------------------------------------------

chem_shr <- bind_cols(NH3_feed,HVC_feed %>% select(-R33),MOH_feed %>% select(-R33)) %>% 
  pivot_longer(cols=-c(R33),names_to='Sv',values_to='value',names_prefix='N_') %>% 
  mutate(J=case_when(str_detect(Sv,'NH3')~'NEN_NH3',
                     str_detect(Sv,'HVC')~'NEN_HVC',
                     TRUE~'NEN_MOH')) %>% 
  rename(shr=value)

check_shr <- chem_shr %>% 
  pivot_wider(names_from=Sv,values_from=shr,values_fill=0) %>% 
  mutate(Total=NH3_NGS+NH3_COL+NH3_OIL+HVC_OIL+HVC_GAS+MOH_NGS+MOH_COL+MOH_OIL)

# Plot --------------------------------------------------------------------

g <- df_demand %>% 
  left_join(chem_shr,by=c('R33','J')) %>% 
  mutate(prod=value*shr) %>% 
  select(R33,Y,J,Sc,Sv,prod) %>% 
  group_by(Y,J,Sc,Sv) %>% 
  summarise(value=sum(prod)) %>% 
  filter(Sc=='low',Y<=2020&Y>=2005) %>% 
  ggplot() +
  geom_bar(aes(x=Y,y=value,fill=Sv),stat='identity',position='fill') +
  facet_wrap(vars(J),scales='free')
plot(g)

g <- df_demand %>% 
  left_join(chem_shr,by=c('R33','J')) %>% 
  mutate(prod=value*shr) %>% 
  select(R33,Y,J,Sc,Sv,prod) %>% 
  group_by(Y,J,Sc,Sv) %>% 
  summarise(value=sum(prod)) %>% 
  filter(Sc=='low',Y<=2020&Y>=2005) %>% 
  ggplot() +
  geom_bar(aes(x=Y,y=value,fill=R33),stat='identity',position='fill') +
  facet_wrap(vars(J),scales='free')
plot(g)

g <- df_demand %>% 
  left_join(chem_shr,by=c('R33','J')) %>% 
  mutate(prod=value*shr) %>% 
  select(R33,Y,J,Sc,Sv,prod) %>% 
  group_by(Y,J,Sc,Sv) %>% 
  summarise(value=sum(prod)) %>% 
  filter(Sc=='low',Y<=2050) %>% 
  ggplot() +
  geom_area(aes(x=Y,y=value,fill=Sv)) +
  facet_wrap(vars(J),scales='free')
plot(g)


# Output to GDX file ------------------------------------------------------

ls_HVC <- wgdx.reshape(HVC_feed,symDim=2,symName="feedshare",tName="L",setsToo=FALSE)
wgdx.lst(gdxName=paste0(odir,"/HVC_feedshare.gdx"),ls_HVC,squeeze=FALSE)
ls_NH3 <- wgdx.reshape(NH3_feed,symDim=2,symName="feedshare",tName="L",setsToo=FALSE)
wgdx.lst(gdxName=paste0(odir,"/NH3_feedshare.gdx"),ls_NH3,squeeze=FALSE)
ls_NH3U <- wgdx.reshape(NH3U_feed,symDim=2,symName="feedshare",tName="L",setsToo=FALSE)
wgdx.lst(gdxName=paste0(odir,"/NH3U_feedshare.gdx"),ls_NH3U,squeeze=FALSE)
ls_MOH <- wgdx.reshape(MOH_feed,symDim=2,symName="feedshare",tName="L",setsToo=FALSE)
wgdx.lst(gdxName=paste0(odir,"/MOH_feedshare.gdx"),ls_MOH,squeeze=FALSE)
