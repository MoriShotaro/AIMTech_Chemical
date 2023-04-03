
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

# setting -----------------------------------------------------------------

root <- getwd()
ddir <- paste0(root,'/data/USGS_Nitrogen/')
odir <- paste0(root,'/output/USGS_Nitrogen/')

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
  select(country.name.en,iso3c)
  

R230_106 <- read_delim(paste0(ddir,'define/230_106.map'),skip=2,col_names=FALSE,delim='.') %>%
  rename(iso3c=1, R106=2) %>% 
  mutate(iso3c=str_remove_all(iso3c,' ')) %>% mutate(R106=str_remove_all(R106,' '))

R106_33 <- read_delim(paste0(ddir,'define/R106_32.map'),col_names=FALSE,delim='.') %>% 
  rename(R106=1, R33=2) %>% 
  mutate(R106=str_remove_all(R106,' ')) %>% mutate(R33=str_remove_all(R33,' '))


df_all <- foreach(i=year,.combine='rbind') %do% {
  tmp <- read_xls(paste0(ddir,'myb1-',i,'-nitro.xls'),sheet=13,skip=5) %>%
    select(!starts_with('..')&!contains('e')) %>%
    rename(Country=1) %>% mutate(Country=str_replace_all(Country,'Country or locality2','Country')) %>%
    mutate(Country=str_remove_all(Country,'[0-9]')) %>% 
    mutate(Country=str_remove_all(Country,' $')) %>% 
    mutate(Country=str_remove_all(Country,',$')) %>%
    slice(1:(str_which(Country,'See footnotes')-1),(str_which(Country,'Country')+1):str_which(Country,'Total$')) %>% 
    left_join(country_list,by=c('Country'='Country2')) %>% 
    mutate(Country=case_when(
      is.na(Country1)~Country,
      TRUE~Country1
    )) %>% select(-Country1) %>% 
    mutate(across(-Country,~str_replace_all(.,'--','0'))) %>% 
    mutate(across(-Country,~na_if(.,'NA'))) %>% 
    mutate(across(-Country,~as.numeric(.))) %>%
    pivot_longer(cols=-Country,names_to='Year',values_to='value') %>% 
    mutate(Record=i)
  assign(paste0('df',i),tmp)
} %>% bind_rows()

df_203 <- df_all %>%
  group_by(Country,Year) %>% 
  summarise(Record=max(Record)) %>% 
  left_join(df_all) %>% select(-Record) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  filter(Country!='Total',Year>=2005) %>%
  complete(Year=full_seq(c(2005,2017),1)) %>% 
  group_by(Country) %>%
  mutate(value=na_locf(value,option='locf')) %>% 
  mutate(Country=recode(Country,
                        'Bosnia and Herzegovina'='Bosnia & Herzegovina',
                        'Burma'='Myanmar (Burma)',
                        'Czech Republic'='Czechia',
                        'Korea, North'='North Korea',
                        'Korea, Republic of'='South Korea',
                        'Serbia and Montenegro'='Serbia',
                        'Trinidad and Tobago'='Trinidad & Tobago'))

df_106 <- df_203 %>%
  left_join(country_code,by=c('Country'='country.name.en')) %>%
  left_join(R230_106) %>% 
  mutate(R106=case_when(
    Country=='Serbia'~'XYU',
    TRUE~R106
  ))

df_33 <- df_106 %>% 
  left_join(R106_33) %>% 
  group_by(R33,Year) %>% 
  summarise(value=sum(value)*17/14/1000) # thousand t-N -> Mt-NH3



# Plot --------------------------------------------------------------------

g <- df_33 %>%
  group_by(Year) %>% 
  summarise(value=sum(value)) %>%
  ggplot() +
  geom_path(aes(x=Year,y=value)) +
  scale_x_continuous(breaks=seq(2005,2017,2)) +
  scale_y_continuous(limits=c(0,NA))
plot(g)

g <- df_33 %>%
  ggplot() +
  geom_area(aes(x=Year,y=value,fill=R33)) +
  scale_x_continuous(breaks=seq(2005,2017,2)) +
  scale_y_continuous(limits=c(0,NA))
plot(g)


# Future projection -------------------------------------------------------

R33_17 <- read.delim(paste0(ddir,"/region17_enduse.map"),header=F,sep='\t') %>% 
  select(-V2) %>% rename(R33=1,R17=2)
R33_17[30,1] <- "XE3"; R33_17[33,1] <- "XNF"

ind_t <- rgdx.param(paste0(ddir,"/serv_global_SSP2.gdx"),"ind_t") %>%
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(t=as.numeric(t)) %>%
  rename(R33=R,Ind=.j,Y=t,value=ind_t)

Fer_Use_CGE <- rgdx.param(paste0(ddir,'/global_17_IAMC.gdx'),'IAMC_Template') %>%
  filter(SCENARIO=="SSP2_BaU_NoCC",VEMF=="Fer_Use_Nit") %>% select(-SCENARIO) %>%
  pivot_wider(names_from=YEMF,values_from=IAMC_Template) %>% slice(1:17) %>%
  pivot_longer(cols=-c(REMF,VEMF),names_to="Y",values_to="Vc") %>%
  mutate(Y=as.numeric(Y)) %>% rename(R17=REMF,Variable=VEMF) %>%
  select(-Variable) %>% rename(Fer_Use_Nit=Vc)

Fer_Use <- Fer_Use_CGE %>%
  left_join(R33_17) %>%
  left_join(SSP2) %>%
  group_by(Y,R17) %>%
  mutate(Fer_Use_Nit=Fer_Use_Nit*POP/sum(POP)*17/14*10) %>% # Tg-N -> Tg->NH3 = Mt-NH3
  ungroup() %>% select(-R17,-POP,-GDP_MER,-GDP_CAP) %>%
  group_by(R33) %>% nest() %>%
  mutate(data=purrr::map(data,~{
    approx(.$Y,.$Fer_Use_Nit,xout=c(2005:2100)) %>% bind_rows()
  })) %>%
  mutate(data=purrr::map(data,~{
    mutate(.,y=y/filter(.,x==2017)$y)
  })) %>% 
  unnest(data) %>%
  rename(Year=x,v17=y)



df_33_future <- df_33 %>% 
  group_by(R33) %>% 
  complete(Year=full_seq(c(2005,2100),1)) %>% 
  mutate(value=na_locf(value,option='locf'),Sv='NH3') %>% 
  rename(value1=value) %>% 
  left_join(Fer_Use) %>% 
  mutate(value2=value1*v17) %>% 
  mutate(value=case_when(Year<=2017~value1,
                         TRUE~value2))

df_NH3 <- df_33_future %>% 
  select(R33,Year,value) %>% 
  mutate(J='NEN_NH3',I='NEN') %>% # Mt-HVC
  rename(R=R33,H=Year,value=value) %>% 
  select(R,I,J,H,value)

# Plot --------------------------------------------------------------------

# g <- df_33_future %>%
#   group_by(Year) %>% 
#   summarise(value=sum(value)) %>%
#   ggplot() +
#   geom_path(aes(x=Year,y=value)) +
#   scale_y_continuous(limits=c(0,NA))
# plot(g)
# 
# g <- df_33_future %>%
#   ggplot() +
#   geom_area(aes(x=Year,y=value,fill=R33)) +
#   scale_y_continuous(limits=c(0,NA))
# plot(g)


# Urea production data ----------------------------------------------------

country_code <- countrycode::codelist %>% 
  select(country.name.en.regex,iso3c)

df_urea <- read_csv(paste0(ddir,'FAOSTAT_UREA.csv')) %>% 
  filter(Item=='Urea',Element=='Production') %>% 
  select(-1,-2,-4,-5,-6,-7,-8) %>% 
  pivot_longer(cols=-c(Area),names_to='Year',values_to='value',names_prefix='Y',names_transform=as.numeric) %>% 
  filter(Year%in%c(2005:2017)) %>% 
  drop_na() %>% 
  mutate(Area=case_when(str_detect(Area,'rkiye')~'Turkey',
                        TRUE~Area)) %>% 
  group_by(Area) %>% 
  complete(Year=full_seq(c(2005,2017),1)) %>% 
  group_by(Area) %>%
  mutate(value=na_locf(value,option='locf')) %>% 
  regex_full_join(country_code,c(Area='country.name.en.regex'),ignore_case=TRUE) %>% 
  drop_na(Year,iso3c) %>% 
  left_join(R230_106) %>% 
  mutate(R106=case_when(
    Area=='Serbia'~'XYU',
    TRUE~R106
  )) %>% 
  left_join(R106_33) %>% 
  group_by(R33,Year) %>% 
  summarise(value=sum(value)*17/60/10^6) %>% # ton Urea -> Mt NH3
  rename(value_Urea=value)



# NH3_urea ----------------------------------------------------------------

df <- df_33 %>% 
  left_join(df_urea) %>% 
  mutate(NH3_urea=value_Urea/value)


# Compare to previous data ------------------------------------------------

# df_prev <- rgdx.param(paste0(ddir,'dem_Chem.gdx'),'serv_t') %>% 
#   rename(R33=1,J=3,Year=4,value_p=5) %>%
#   mutate(across(where(is.factor),~as.character(.))) %>% 
#   mutate(Year=as.numeric(Year)) %>% 
#   filter(J=='NEN_NH3',Year%in%c(2005:2017)) %>% select(R33,Year,value_p)
# 
# df_cmp <- df_33 %>% 
#   left_join(df_prev) %>%
#   rename(USGS=value,Estimated=value_p) %>% 
#   pivot_longer(cols=-c(R33,Year),names_to='variable',values_to='value')
# 
# g1 <- df_cmp %>%
#   group_by(Year,variable) %>% 
#   summarise(value=sum(value)) %>%
#   ggplot() +
#   geom_line(aes(x=Year,y=value,color=variable)) +
#   geom_point(aes(x=Year,y=value,color=variable,shape=variable)) +
#   scale_x_continuous(breaks=seq(2005,2017,2)) +
#   scale_y_continuous(limits=c(0,NA)) +
#   labs(y='Ammonia production (Mt/yr)') +
#   MyTheme +
#   theme(legend.position='none')
# plot(g1)
# 
# g2 <- df_cmp %>%
#   filter(Year==2017) %>% 
#   ggplot() +
#   geom_point(aes(x=R33,y=value,color=variable,shape=variable)) +
#   scale_y_continuous(limits=c(0,NA)) +
#   labs(y='Ammonia production (Mt/yr)') +
#   MyTheme +
#   theme(legend.position=c(0.8,0.8))
# plot(g2)
# 
# g <- g1 + g2 + plot_layout(width=c(1,3))
# plot(g)
# ggsave(paste0(odir,'USGS_Estimated.png'),width=9,height=4)
