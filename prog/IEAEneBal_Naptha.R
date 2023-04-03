
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

df <- read_csv(paste0(ddir,'japan.csv')) %>%
  select(1,2,17) %>%
  rename(FLOW=1,Year=2,value=3) %>% 
  filter(FLOW=='Industry') %>% 
  mutate(Year=as.numeric(Year))

POP <- rgdx.param(paste0(ddir,'define/serv_global_SSP2.gdx'),'ind_t') %>%
  rename(R33=1,Sv=2,Year=3,ind=4) %>%
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  filter(Year<=2018,Sv!='GDP_PPP') %>%
  filter(Sv=='GDP_MER',R33=='JPN')

df2 <- df %>% 
  left_join(POP) %>% 
  filter(Year>=2005,Year!=2019) %>% 
  mutate(ind=as.numeric(ind),value=as.numeric(value))

g <- df2 %>% 
  ggplot() +
  geom_point(aes(x=ind,y=value,color=Year)) +
  scale_x_continuous(limits=c(0,NA)) +
  scale_y_continuous(limits=c(0,NA))
plot(g)

%>% 
  filter(FLOW!='COUNTRY') %>% rename(COUNTRY=1) %>% 
  select(1,31,61) %>% 
  slice(13:157) %>%
  regex_left_join(country_code,by=c(COUNTRY='country.name.en.regex'),ignore_case=TRUE) %>% 
  select(COUNTRY,iso3c,everything()) %>% select(-country.name.en.regex) %>% 
  drop_na(iso3c) %>%
  mutate(across(-c(COUNTRY,iso3c),~as.numeric(.))) %>% 
  left_join(R230_106) %>% 
  left_join(R106_33) %>%
  select(-iso3c,-R106) %>%
  pivot_longer(-c(COUNTRY,R33),names_to='variable',values_to='value') %>% 
  group_by(R33,variable) %>%
  summarise(value=sum(value)) %>%
  drop_na(R33) %>% 
  pivot_wider(names_from=variable,values_from=value) %>%
  mutate(value=`Non-energy use`*100/`Total final consumption`)


refinery_yield_2014 <- read_csv(paste0(ddir,'2014_OilRefineries.csv')) %>% 
  filter(PRODUCT!='COUNTRY') %>% rename(COUNTRY=1) %>% 
  select(1,23:44) %>% 
  slice(13:157) %>%
  regex_left_join(country_code,by=c(COUNTRY='country.name.en.regex'),ignore_case=TRUE) %>% 
  select(COUNTRY,iso3c,everything()) %>% select(-country.name.en.regex) %>% 
  drop_na(iso3c) %>%
  mutate(across(-c(COUNTRY,iso3c),~as.numeric(.))) %>% 
  left_join(R230_106) %>% 
  left_join(R106_33) %>%
  select(-iso3c,-R106) %>% 
  pivot_longer(-c(COUNTRY,R33),names_to='Product',values_to='value') %>% 
  group_by(R33,Product) %>%
  summarise(value=sum(value)) %>%
  filter(value>=0) %>% 
  pivot_wider(names_from=Product,values_from=value,values_fill=0) %>% 
  drop_na(R33) %>%
  ungroup() %>% 
  mutate(Total=apply(.[,c(2:22)],1,sum)) %>% 
  mutate(across(-c(R33,Total),~.*100/Total)) %>% 
  mutate(Year=2014)

refinery_yield_2015 <- read_csv(paste0(ddir,'2015_OilRefineries.csv')) %>% 
  filter(PRODUCT!='COUNTRY') %>% rename(COUNTRY=1) %>% 
  select(1,23:44) %>% 
  slice(13:157) %>%
  regex_left_join(country_code,by=c(COUNTRY='country.name.en.regex'),ignore_case=TRUE) %>% 
  select(COUNTRY,iso3c,everything()) %>% select(-country.name.en.regex) %>% 
  drop_na(iso3c) %>%
  mutate(across(-c(COUNTRY,iso3c),~as.numeric(.))) %>% 
  left_join(R230_106) %>% 
  left_join(R106_33) %>%
  select(-iso3c,-R106) %>% 
  pivot_longer(-c(COUNTRY,R33),names_to='Product',values_to='value') %>% 
  group_by(R33,Product) %>%
  summarise(value=sum(value)) %>%
  filter(value>=0) %>% 
  pivot_wider(names_from=Product,values_from=value,values_fill=0) %>% 
  drop_na(R33) %>%
  ungroup() %>% 
  mutate(Total=apply(.[,c(2:22)],1,sum)) %>% 
  mutate(across(-c(R33,Total),~.*100/Total)) %>% 
  mutate(Year=2015)

refinery_yield_2016 <- read_csv(paste0(ddir,'2016_OilRefineries.csv')) %>% 
  filter(PRODUCT!='COUNTRY') %>% rename(COUNTRY=1) %>% 
  select(1,23:44) %>% 
  slice(13:157) %>%
  regex_left_join(country_code,by=c(COUNTRY='country.name.en.regex'),ignore_case=TRUE) %>% 
  select(COUNTRY,iso3c,everything()) %>% select(-country.name.en.regex) %>% 
  drop_na(iso3c) %>%
  mutate(across(-c(COUNTRY,iso3c),~as.numeric(.))) %>% 
  left_join(R230_106) %>% 
  left_join(R106_33) %>%
  select(-iso3c,-R106) %>% 
  pivot_longer(-c(COUNTRY,R33),names_to='Product',values_to='value') %>% 
  group_by(R33,Product) %>%
  summarise(value=sum(value)) %>%
  filter(value>=0) %>% 
  pivot_wider(names_from=Product,values_from=value,values_fill=0) %>% 
  drop_na(R33) %>%
  ungroup() %>% 
  mutate(Total=apply(.[,c(2:22)],1,sum)) %>% 
  mutate(across(-c(R33,Total),~.*100/Total)) %>% 
  mutate(Year=2016)

refinery_yield_2017 <- read_csv(paste0(ddir,'2017_OilRefineries.csv')) %>% 
  filter(PRODUCT!='COUNTRY') %>% rename(COUNTRY=1) %>% 
  select(1,23:44) %>% 
  slice(13:157) %>%
  regex_left_join(country_code,by=c(COUNTRY='country.name.en.regex'),ignore_case=TRUE) %>% 
  select(COUNTRY,iso3c,everything()) %>% select(-country.name.en.regex) %>% 
  drop_na(iso3c) %>%
  mutate(across(-c(COUNTRY,iso3c),~as.numeric(.))) %>% 
  left_join(R230_106) %>% 
  left_join(R106_33) %>%
  select(-iso3c,-R106) %>% 
  pivot_longer(-c(COUNTRY,R33),names_to='Product',values_to='value') %>% 
  group_by(R33,Product) %>%
  summarise(value=sum(value)) %>%
  filter(value>=0) %>% 
  pivot_wider(names_from=Product,values_from=value,values_fill=0) %>% 
  drop_na(R33) %>%
  ungroup() %>% 
  mutate(Total=apply(.[,c(2:22)],1,sum)) %>% 
  mutate(across(-c(R33,Total),~.*100/Total)) %>% 
  mutate(Year=2017)

refinery_yield_2018 <- read_csv(paste0(ddir,'2018_OilRefineries.csv')) %>% 
  filter(PRODUCT!='COUNTRY') %>% rename(COUNTRY=1) %>% 
  select(1,23:44) %>% 
  slice(13:157) %>%
  regex_left_join(country_code,by=c(COUNTRY='country.name.en.regex'),ignore_case=TRUE) %>% 
  select(COUNTRY,iso3c,everything()) %>% select(-country.name.en.regex) %>% 
  drop_na(iso3c) %>%
  mutate(across(-c(COUNTRY,iso3c),~as.numeric(.))) %>% 
  left_join(R230_106) %>% 
  left_join(R106_33) %>%
  select(-iso3c,-R106) %>% 
  pivot_longer(-c(COUNTRY,R33),names_to='Product',values_to='value') %>% 
  group_by(R33,Product) %>%
  summarise(value=sum(value)) %>%
  filter(value>=0) %>% 
  pivot_wider(names_from=Product,values_from=value,values_fill=0) %>% 
  drop_na(R33) %>%
  ungroup() %>% 
  mutate(Total=apply(.[,c(2:22)],1,sum)) %>% 
  mutate(across(-c(R33,Total),~.*100/Total)) %>% 
  mutate(Year=2018)

refinery_yield <- refinery_yield_2018 %>% 
  bind_rows(refinery_yield_2017) %>% 
  bind_rows(refinery_yield_2016) %>% 
  bind_rows(refinery_yield_2015) %>% 
  bind_rows(refinery_yield_2014) %>%
  mutate(Year=as.character(Year))

refinery_yield_mean <- refinery_yield %>%
  group_by(Year) %>% 
  summarise(Naphtha=sum(Naphtha*Total)/sum(Total))

g <- refinery_yield %>% 
  ggplot() +
  geom_hline(data=refinery_yield_mean,aes(color=Year,yintercept=Naphtha),size=0.4,alpha=0.8) +
  geom_point(aes(x=R33,y=Naphtha,color=Year,size=Naphtha*Total),alpha=0.8) +
  scale_color_brewer(palette='Set1') +
  labs(y=paste0('Yield of Naphtha (%)')) +
  theme(
    axis.text.x=element_text(size = 10,angle=45, vjust=1, hjust=0.8, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    legend.title = element_blank()
  ) +
  MyTheme
plot(g)
ggsave(paste0(odir,'Naphtha_Yield.png'),width=9,height=5)

g <- oil_product %>% 
  ggplot() +
  geom_point(aes(x=R33,y=value,size=`Non-energy use`),shape=21,stroke=1.5) +
  theme(
    axis.text.x=element_text(size = 10,angle=45, vjust=1, hjust=0.8, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    legend.title = element_blank()
  )
plot(g)
ggsave(paste0(odir,'OilProducts_nonene.png'),width=10,height=6)



hoge <- refinery_yield %>% 
  distinct(R33)

fuga <- R106_33 %>% 
  distinct(R33) %>%
  anti_join(hoge)
