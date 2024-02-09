library(tidyverse)
gc()

hun <- read.csv("C:/Users/Anna/Documents/tk/vshift3/sentiment/hun_sentiment_yrs.csv")
fdi_hun<-read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/fdi_hun.csv")
import_hun <-read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/import_hun.csv")
export_hun<-read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/export_hun.csv")


hun<-hun %>% 
  filter(
    gp != "OTHER"
  ) %>% 
  rename(year=cal_yr)

fdi_hun <- fdi_hun %>% 
  mutate(
    gp = case_when(country == "Franciaország" ~ "FR", country == "Németország" ~ "DE", 
                   country == "Egyesült Királyság" ~ "UK", country == "Oroszország" ~ "RU", 
                   country == "Kína" ~ "CN", country == "Egyesült Államok" ~ "USA")
  )
#millió USD

hun2<-merge(hun, fdi_hun, by=c("gp", "year"))
import_hun <- import_hun %>% 
  mutate(
    gp = case_when(country == "France" ~ "FR", country == "Germany" ~ "DE", 
                   country == "United Kingdom" ~ "UK", country == "Russian Federation" ~ "RU", 
                   country == "China" ~ "CN", country == "United States" ~ "USA")
  )
unique(import_hun$gp)
hun2<-merge(hun, import_hun, by=c("gp", "year"))
hun3<-hun2 %>% 
  select(
    c(1,2,4:10,"import")
  )
export_hun<-export_hun %>% 
  mutate(
    gp = case_when(country == "France" ~ "FR", country == "Germany" ~ "DE", 
                   country == "United Kingdom" ~ "UK", country == "Russian Federation" ~ "RU", 
                   country == "China" ~ "CN", country == "United States" ~ "USA")
  ) %>% 
  rename(export=value,
         year=name)
hun4<-merge(hun3,export_hun, by=c("gp","year")) %>% 
  select(-c("country"))
write.csv(hun4, "C:/Users/Anna/Documents/tk/vshift3/tisztitott/hun_panel.csv")

##############################################################################################################cseh    

cz <- read.csv("C:/Users/Anna/Documents/tk/vshift3/sentiment/cz_sentiment_yrs.csv")
fdi_cz<-read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/fdi_cz.csv")
import_cz <-read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/import_cz.csv")
export_cz<-read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/export_cz.csv")


cz<-cz %>% 
  filter(
    gp != "OTHER"
  ) %>% 
  rename(year=cal_yr)
unique(fdi_cz$country)
fdi_cz <- fdi_cz %>% 
  mutate(
    gp = case_when(country == "France" ~ "FR", country == "Germany" ~ "DE", country == "Russia" ~ "RU",
                   country == "United Kingdom" ~ "UK", country == "Russian Federation" ~ "RU", 
                   country == "China" ~ "CN", country == "United States" ~ "USA")
  )
fdi_cz <- fdi_cz %>% 
  mutate(
    fdi_flow = fdi_flow/1000
  )
cz2<-merge(cz, fdi_cz, by=c("gp", "year"))
import_cz <- import_cz %>% 
  mutate(
    gp = case_when(country == "France" ~ "FR", country == "Germany" ~ "DE", 
                   country == "United Kingdom" ~ "UK", country == "Russian Federation" ~ "RU", 
                   country == "China" ~ "CN", country == "United States" ~ "USA")
  )
unique(import_cz$gp)
cz2<-merge(cz, import_cz, by=c("gp", "year"))
cz3<-cz2 %>% 
  select(
    c(1,2,4:10,"import")
  )
export_cz<-export_cz %>% 
  mutate(
    gp = case_when(country == "France" ~ "FR", country == "Germany" ~ "DE", 
                   country == "United Kingdom" ~ "UK", country == "Russian Federation" ~ "RU", 
                   country == "China" ~ "CN", country == "United States" ~ "USA")
  )

cz4<-merge(cz3,export_cz, by=c("gp","year")) %>% 
  select(-c("country"))
write.csv(cz4, "C:/Users/Anna/Documents/tk/vshift3/tisztitott/cz_panel.csv")


############################################################################################################horvát    

cro <- read.csv("C:/Users/Anna/Documents/tk/vshift3/sentiment/cro_sentiment_yrs.csv")
fdi_cro<-read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/fdi_cro.csv")
import_cro <-read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/import_cro.csv")
export_cro<-read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/export_cro.csv")


cro<-cro %>% 
  filter(
    gp != "OTHER"
  ) %>% 
  rename(year=cal_yr)
unique(fdi_cro$country)
fdi_cro <- fdi_cro %>% 
  mutate(
    gp = case_when(country == "FRANCE" ~ "FR", country == "GERMANY" ~ "DE",
                   country == "UNITED KINGDOM" ~ "UK", country == "RUSSIAN FEDERATION" ~ "RU", 
                   country == "CHINA" ~ "CN", country == "UNITED STATES" ~ "USA")
  )
cro2<-merge(cro, fdi_cro, by=c("gp", "year"))
import_cro <- import_cro %>% 
  mutate(
    gp = case_when(country == "France" ~ "FR", country == "Germany" ~ "DE", 
                   country == "United Kingdom" ~ "UK", country == "Russian Federation" ~ "RU", 
                   country == "China" ~ "CN", country == "United States" ~ "USA")
  )
unique(import_cro$gp)
cro2<-merge(cro, import_cro, by=c("gp", "year"))
cro3<-cro2 %>% 
  select(
    c(1,2,4:10,"import")
  )

export_cro<-export_cro %>% 
  mutate(
    gp = case_when(country == "France" ~ "FR", country == "Germany" ~ "DE", 
                   country == "United Kingdom" ~ "UK", country == "Russian Federation" ~ "RU", 
                   country == "China" ~ "CN", country == "United States" ~ "USA")
  )

cro4<-merge(cro3,export_cro, by=c("gp","year")) %>% 
  select(-c("country"))
write.csv(cro4, "C:/Users/Anna/Documents/tk/vshift3/tisztitott/cro_panel.csv")


###################################################################################################összekapcsolás

hun <- read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/hun_panel.csv")
cz <- read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/cz_panel.csv")
cro <- read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/cro_panel.csv")

hun2 <-hun %>% 
  mutate(
    merleg = export - import,
    eu = ifelse(hun$year<2004, 0, 1)
  ) %>% 
  group_by(gp) %>% 
  mutate(
    #n_fdi = scale(fdi_flow),
    n_merleg = scale(merleg),
    n_import = scale(import),
    n_export = scale(export)
  )
hun2 %>% 
  ggplot(aes(x=year, y=n_merleg, color=gp))+
  geom_line()
  
cz2 <- cz %>%  
  mutate(
    merleg = export - import,
    eu = ifelse(cz$year<2004, 0, 1)
  ) %>% 
  group_by(gp) %>% 
  mutate(
    #n_fdi = scale(fdi_flow),
    n_merleg = scale(merleg),
    n_import = scale(import),
    n_export = scale(export)
  )

cz2 %>% 
  ggplot(aes(x=year, y=fdi_flow, color=gp))+
  geom_line()

cro2 <- cro %>% 
  mutate(
    merleg = export - import,
    eu = ifelse(cro$year<2013, 0, 1)
  ) %>% 
  group_by(gp) %>% 
  mutate(
    #n_fdi = scale(fdi_flow),
    n_merleg = scale(merleg),
    n_import = scale(import),
    n_export = scale(export)
  )

cro2 %>% 
  ggplot(aes(x=year, y=n_merleg, color=gp))+
  geom_line()

ossz <- rbind(hun2, cz2, cro2) %>% 
  select(-c(1))

leftright <- readxl::read_excel("C:/Users/Anna/Documents/tk/vshift3/nyers/leftright.xlsx") %>% 
  pivot_longer(-1) %>% 
  rename(
    year = ...1,
    country.x = name
    )

ossz2 <- merge(ossz, leftright, by = c("year", "country.x")) %>% rename(leftright = value)

evek <- c(1997, 2001, 2005, 2009, 2013, 2017, 2021)
ossz2 <- ossz2 %>% 
  mutate(
    valasztasi_ev = ifelse(ossz2$year %in% evek, 1, 0)
  )
ossz2$year <- as.character(ossz2$year)

write.csv(ossz2, "C:/Users/Anna/Documents/tk/vshift3/tisztitott/ossz2.csv")

df <- read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/ossz2.csv")
df2 <- readRDS("C:/Users/Anna/Documents/tk/vshift3/nyers/stm_hun_cze_cro.rds")
df <- df %>% 
  select(-c(log_senti)) %>% 
  rename(country = country.x)
df2 <- df2 %>% 
  select(c("cal_yr", "country", "gp","log_senti","salience")) %>% 
  rename(year = cal_yr)
df3 <- merge(df, df2, by = c("year", "country", "gp"))
df <- df3
write.csv(df, "C:/Users/Anna/Documents/tk/vshift3/tisztitott/ossz3.csv")

