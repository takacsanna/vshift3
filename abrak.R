library(tidyverse)
library(ggplot2)

df <- read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/ossz3.csv")
gps <- c("RU", "CN","USA")
df2 <- df[df$gp %in% gps,]
hun <- df2[df2$country == "Hungary",]
summary(hun)
p<-hun %>% 
  ggplot(aes(x=year, y=salience, color=gp))+
  geom_line()+
  labs(title = "A magyar parlament gyakoriságai\n2000 és 2019 között",
       y = "Relatív gyakoriság", x = "Év")+
  guides(color=guide_legend(title="Nagyhatalmak"))
cz <- df2[df2$country == "Czechia",]

q<- cz %>% 
  ggplot(aes(x=year, y=salience, color=gp))+
  geom_line()+
  labs(title = "A cseh parlament gyakoriságai\n2000 és 2019 között",
       y = "Relatív gyakoriság", x = "Év")+
  guides(color=guide_legend(title="Nagyhatalmak"))

cro <- df2[df2$country == "Croatia",]

r<- cro %>% 
  ggplot(aes(x=year, y=salience, color=gp))+
  geom_line()+
  labs(title = "A horvát parlament gyakoriságai\n2000 és 2019 között",
       y = "Relatív gyakoriság", x = "Év")+
  guides(color=guide_legend(title="Nagyhatalmak"))

ggpubr::ggarrange(p,q,r)

hkina <- hun[hun$gp=="CN",]
x<-hkina %>% ggplot(aes(x=year, y=log_senti))+
  geom_point()+
  geom_smooth()+
  labs(title = "A magyar parlament kínai hangulata\n2000 és 2019 között",
       y = "Hangulat", x = "Év")
ckina <- cz[cz$gp=="CN",]
y<-ckina %>% ggplot(aes(x=year, y=log_senti))+
  geom_point()+
  geom_smooth()+
  labs(title = "A cseh parlament kínai hangulata\n2000 és 2019 között",
       y = "Hangulat", x = "Év")
crok <- cro[cro$gp=="CN",]
z<-crok %>% ggplot(aes(x=year, y=log_senti))+
  geom_point()+
  geom_smooth()+
  labs(title = "A horvát parlament kínai hangulata\n2000 és 2019 között",
       y = "Hangulat", x = "Év")
ggpubr::ggarrange(x,y,z)

hru <- hun[hun$gp=="RU",]
a<-hru %>% ggplot(aes(x=year, y=log_senti))+
  geom_point()+
  geom_smooth()+
  labs(title = "A magyar parlament orosz hangulata\n2000 és 2019 között",
       y = "Hangulat", x = "Év")
czru <- cz[cz$gp=="RU",]
b<-czru %>% ggplot(aes(x=year, y=log_senti))+
  geom_point()+
  geom_smooth()+
  labs(title = "A cseh parlament orosz hangulata\n2000 és 2019 között",
       y = "Hangulat", x = "Év")
crok <- cro[cro$gp=="RU",]
c<-crok %>% ggplot(aes(x=year, y=log_senti))+
  geom_point()+
  geom_smooth()+
  labs(title = "A horvát parlament orosz hangulata\n2000 és 2019 között",
       y = "Hangulat", x = "Év")
ggpubr::ggarrange(a,b,c)

husa <- hun[hun$gp=="USA",]
l<-husa %>% ggplot(aes(x=year, y=log_senti))+
  geom_point()+
  geom_smooth()+
  labs(title = "A magyar parlament USA hangulata\n2000 és 2019 között",
       y = "Hangulat", x = "Év")
czru <- cz[cz$gp=="USA",]
m<-czru %>% ggplot(aes(x=year, y=log_senti))+
  geom_point()+
  geom_smooth()+
  labs(title = "A cseh parlament USA hangulata\n2000 és 2019 között",
       y = "Hangulat", x = "Év")
crok <- cro[cro$gp=="USA",]
n<-crok %>% ggplot(aes(x=year, y=log_senti))+
  geom_point()+
  geom_smooth()+
  labs(title = "A horvát parlament USA hangulata\n2000 és 2019 között",
       y = "Hangulat", x = "Év")
ggpubr::ggarrange(l,m,n)
