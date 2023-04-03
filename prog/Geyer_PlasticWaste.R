
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

# setting -----------------------------------------------------------------

root <- getwd()
ddir <- paste0(root,'/data/Geyer_Plastic/')
odir <- paste0(root,'/output/Geyer_Plastic/')

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

df <- data.frame(Sector=c('P1','P2','P3','P4','P5','P6','P7'),
                 Share=c(0.448,0.067,0.188,0.038,0.119,0.008,0.132),
                 mean=c(13,0.5,35,8,3,20,5),
                 sd=c(3,0.1,7,2,1,3,1.5)) %>%
  mutate(mu=log(mean^2 / sqrt(mean^2 + sd^2)),sigma=sqrt(log(1 + (sd^2 / mean^2))))


g <- ggplot(data=data.frame(Year=seq(0,70,0.1)),aes(Year)) +
  mapply(function(data,mu,sigma,s) stat_function(fun=dlnorm,args=list(meanlog=mu,sdlog=sigma),aes_q(color=s)), 
         df$mu, df$sigma, df$Sector)
plot(g)

g <- ggplot(data=data.frame(Year=seq(0,70,0.01)),aes(Year)) +
  mapply(function(mu, sigma, s) stat_function(fun=dlnorm,args=list(meanlog=mu,sdlog=sigma),aes_q(color=s)), 
         df$mu, df$sigma, df$Sector) +
  scale_x_log10()
plot(g)
ggsave(paste0(odir,'test.png'),width=10,height=10)

g <- ggplot(data=data.frame(x=c(0,70)),aes(x)) + stat_function(fun=dnorm,args=list(mean=13,sd=3))
plot(g)

ggplot() +
  stat_function(fun = dlnorm, args = list(meanlog = 2.5390069, sdlog = 0.2277824)) +
  scale_x_log10() 
