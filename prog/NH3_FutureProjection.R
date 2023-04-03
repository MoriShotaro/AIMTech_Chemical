# library -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)
library(foreach)
library(imputeTS)
library(countrycode)
library(gdxrrw)

# setting -----------------------------------------------------------------

root <- getwd()
ddir <- paste0(root,'/data/NH3_FutureProjection/')
odir <- paste0(root,'/output/NH3_FutureProjection/')


# Input data --------------------------------------------------------------

SSP2 <- rgdx.param(paste0(ddir,'serv_global_SSP2.gdx'),'ind_t') %>%
  rename(R33=1,Sv=2,Year=3,value=4) %>%
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  filter(Year<=2017,Sv!='GDP_PPP') %>% 
  pivot_wider(names_from=Sv,values_from=value) %>% 
  mutate(GDP_CAP=GDP_MER/POP)

R33_17 <- read.delim(paste0(ddir,"/region17_enduse.map"),header=F,sep='\t') %>% 
  select(-V2) %>% rename(R33=1,R17=2)
R33_17[30,1] <- "XE3"; R33_17[33,1] <- "XNF"


# R230_106 <- read_delim(paste0(ddir,"/230_106.map"),delim=" . ",comment="*",col_names=FALSE) %>% rename(R230=1,R106=2)
# R106_32 <- read_delim(paste0(ddir,"/R106_32.map"),delim=" . ",comment="*",col_names=FALSE) %>% rename(R106=1,R33=2)
# R106_17 <- read_delim(paste0(ddir,"/region17.map"),delim=".",col_names=FALSE) %>% rename(R106=1,R17=2) %>% mutate(across(starts_with("R"),~str_replace_all(.,"\t","")))
# RegionMap <- R230_106 %>% left_join(R106_32) %>% left_join(R106_17) %>% drop_na()
# R33_17 <- RegionMap %>% select(R33,R17) %>% distinct(R33,.keep_all=TRUE)
# 
# 
Fer_Use_CGE <- rgdx.param(paste0(ddir,'/global_17_IAMC.gdx'),'IAMC_Template') %>%
  filter(SCENARIO=="SSP2_BaU_NoCC",VEMF=="Fer_Use_Nit") %>% select(-SCENARIO) %>%
  pivot_wider(names_from=YEMF,values_from=IAMC_Template) %>% slice(1:17) %>%
  pivot_longer(cols=-c(REMF,VEMF),names_to="Y",values_to="Vc") %>%
  mutate(Y=as.numeric(Y)) %>% rename(R17=REMF,Variable=VEMF) %>%
  select(-Variable) %>% rename(Fer_Use_Nit=Vc)
# 
Fer_Use <- Fer_Use_CGE %>%
  left_join(R33_17) %>%
  left_join(SSP2) %>%
  group_by(Y,R17) %>%
  mutate(Fer_Use_Nit=Fer_Use_Nit*POP/sum(POP)*17/14*10) %>% # Tg-N -> Tg->NH3 = Mt-NH3
  ungroup() %>% select(-R17,-POP,-GDP_MER,-GDP_CAP) %>%
  group_by(R33) %>% nest() %>%
  mutate(data=map(data,~{
    approx(.$Y,.$Fer_Use_Nit,xout=c(2005:2017)) %>% bind_rows()
  })) %>%
  unnest(data) %>%
  rename(Year=x,Value=y)


# Regression Production per cap vs. GDP per cap ---------------------------

df <- df_33 %>% 
  left_join(SSP2) %>% 
  mutate(PRO_CAP=value/POP)
  # left_join(Fer_Use) %>% 
  # mutate(USE_CAP=Value/POP)
alpha <- summary(nls(PRO_CAP~a*exp(-b/GDP_CAP),start=c(a=1,b=1),data=df))$coefficients[1]
beta <- summary(nls(PRO_CAP~a*exp(-b/GDP_CAP),start=c(a=1,b=1),data=df))$coefficients[2]
R2 <- data.frame(ovs=df$PRO_CAP,prd=alpha*exp(-beta/df$GDP_CAP))

g <- df %>% 
  ggplot() +
  geom_point(aes(x=GDP_CAP,y=PRO_CAP,color=R33,size=value)) +
  stat_function(fun=function(x) alpha*exp(-beta/x)) +
  annotate('text',x=Inf,y=-Inf,label=paste0('R2 = ',summary(lm(R2$prd~R2$ovs))$r.squared),hjust=1,vjust=-1)
plot(g)

df_17 <- df_33 %>% 
  left_join(R33_17) %>% 
  left_join(SSP2) %>% 
  mutate(PRO_CAP=value/POP) %>%
  group_by(R17,Year) %>% 
  summarise(value=sum(value),PRO_CAP=sum(PRO_CAP),GDP_CAP=sum(GDP_CAP))
alpha <- summary(nls(PRO_CAP~a*exp(-b/GDP_CAP),start=c(a=1,b=1),data=df_17))$coefficients[1]
beta <- summary(nls(PRO_CAP~a*exp(-b/GDP_CAP),start=c(a=1,b=1),data=df_17))$coefficients[2]
R2 <- data.frame(ovs=df_17$PRO_CAP,prd=alpha*exp(-beta/df_17$GDP_CAP))

g <- df_17 %>% 
  ggplot() +
  geom_point(aes(x=GDP_CAP,y=PRO_CAP,color=R17,size=value)) +
  stat_function(fun=function(x) alpha*exp(-beta/x)) +
  annotate('text',x=Inf,y=-Inf,label=paste0('R2 = ',summary(lm(R2$prd~R2$ovs))$r.squared),hjust=1,vjust=-1)
plot(g)

# Regression Production vs. Population ------------------------------------

df <- df_33 %>% 
  left_join(SSP2)
alpha <- summary(nls(value~a*exp(-b/POP),start=c(a=100,b=10000),data=df))$coefficients[1]
beta <- summary(nls(value~a*exp(-b/POP),start=c(a=100,b=10000),data=df))$coefficients[2]
R2 <- data.frame(ovs=df$value,prd=alpha*exp(-beta/df$POP))

g <- df %>% 
  ggplot() +
  geom_point(aes(x=POP,y=value,color=R33)) +
  stat_function(fun=function(x) alpha*exp(-beta/x)) +
  annotate('text',x=Inf,y=-Inf,label=paste0('R2 = ',summary(lm(R2$prd~R2$ovs))$r.squared),hjust=1,vjust=-1)
plot(g)
