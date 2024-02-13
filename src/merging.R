gc()

library(tidyverse)
library(here)

here()

hun <- read.csv(here("data_input/sentiment_hun.csv"))
fdi_hun <- read.csv(here("data_input/fdi_hun.csv"))
import_hun <- read.csv(here("data_input/import_hun.csv"))
export_hun <- read.csv(here("data_input/export_hun.csv"))


hun <- hun %>%
  filter(
    gp != "OTHER"
  )

fdi_hun <- fdi_hun %>% 
  mutate(
    gp = case_when(country == "Franciaország" ~ "FR",
                   country == "Németország" ~ "DE", 
                   country == "Egyesült Királyság" ~ "UK",
                   country == "Oroszország" ~ "RU", 
                   country == "Kína" ~ "CN",
                   country == "Egyesült Államok" ~ "USA")
  )

hun2 <- merge(hun, fdi_hun, by=c("gp", "year"))

import_hun <- import_hun %>%
  mutate(
    gp = case_when(country == "France" ~ "FR",
                   country == "Germany" ~ "DE",
                   country == "United Kingdom" ~ "UK",
                   country == "Russian Federation" ~ "RU",
                   country == "China" ~ "CN",
                   country == "United States" ~ "USA")
  )

unique(import_hun$gp)
hun2 <- merge(hun, import_hun, by=c("gp", "year"))
hun3 <- hun2 %>% 
  select(
    -c("X","country.y")
  ) %>% 
  rename(country = country.x)

export_hun <- export_hun %>%
  mutate(
    gp = case_when(country == "France" ~ "FR",
                   country == "Germany" ~ "DE",
                   country == "United Kingdom" ~ "UK",
                   country == "Russian Federation" ~ "RU",
                   country == "China" ~ "CN",
                   country == "United States" ~ "USA")
  ) %>%
  rename(export=value,
         year=name)

hun4 <- merge(hun3, export_hun, by=c("gp","year")) %>%
  select(-c("country.y")) %>%
  rename(country = country.x)

##############################################################################################################cz    

cz <- read.csv(here("data_input/sentiment_cz.csv"))
fdi_cz <- read.csv(here("data_input/fdi_cz.csv"))
import_cz <- read.csv(here("data_input/import_cz.csv"))
export_cz <- read.csv(here("data_input/export_cz.csv"))

cz <- cz %>% 
  filter(
    gp != "OTHER"
  )


fdi_cz <- fdi_cz %>%
  mutate(
    gp = case_when(country == "France" ~ "FR",
                   country == "Germany" ~ "DE",
                   country == "Russia" ~ "RU",
                   country == "United Kingdom" ~ "UK",
                   country == "Russian Federation" ~ "RU", 
                   country == "China" ~ "CN",
                   country == "United States" ~ "USA")
  )

fdi_cz <- fdi_cz %>%
  mutate(
    fdi_flow = fdi_flow/1000
  )
unique(fdi_cz$gp)

cz2 <- merge(cz, fdi_cz, by = c("gp", "year"))

import_cz <- import_cz %>%
  mutate(
    gp = case_when(country == "France" ~ "FR",
                   country == "Germany" ~ "DE",
                   country == "United Kingdom" ~ "UK",
                   country == "Russian Federation" ~ "RU",
                   country == "China" ~ "CN",
                   country == "United States" ~ "USA")
  )
unique(import_cz$gp)
cz2 <- merge(cz, import_cz, by = c("gp", "year"))
cz3 <- cz2 %>%
  select(
    -c("X", "country.y")
  )

export_cz <- export_cz %>% 
  mutate(
    gp = case_when(country == "France" ~ "FR",
                   country == "Germany" ~ "DE",
                   country == "United Kingdom" ~ "UK",
                   country == "Russian Federation" ~ "RU",
                   country == "China" ~ "CN",
                   country == "United States" ~ "USA")
  )

cz4 <- merge(cz3,export_cz, by=c("gp","year")) %>%
  select(-c("country")) %>%
  rename(country = country.x)


############################################################################################################cro    

cro <- read.csv(here("data_input/sentiment_cro.csv"))
fdi_cro<-read.csv(here("data_input/fdi_cro.csv"))
import_cro <-read.csv(here("data_input/import_cro.csv"))
export_cro<-read.csv(here("data_input/export_cro.csv"))


cro<-cro %>%
  filter(
    gp != "OTHER"
  )

unique(fdi_cro$country)
fdi_cro <- fdi_cro %>%
  mutate(
    gp = case_when(country == "FRANCE" ~ "FR",
                   country == "GERMANY" ~ "DE",
                   country == "UNITED KINGDOM" ~ "UK",
                   country == "RUSSIAN FEDERATION" ~ "RU",
                   country == "CHINA" ~ "CN",
                   country == "UNITED STATES" ~ "USA")
  )
cro2 <- merge(cro, fdi_cro, by = c("gp", "year"))
import_cro <- import_cro %>%
  mutate(
    gp = case_when(country == "France" ~ "FR",
                   country == "Germany" ~ "DE",
                   country == "United Kingdom" ~ "UK",
                   country == "Russian Federation" ~ "RU",
                   country == "China" ~ "CN",
                   country == "United States" ~ "USA")
  )
unique(import_cro$gp)
cro2 <- merge(cro, import_cro, by=c("gp", "year"))
cro3 <- cro2 %>%
  select(
    -c("X","country.y")
  )

export_cro <- export_cro %>%
  mutate(
    gp = case_when(country == "France" ~ "FR",
                   country == "Germany" ~ "DE",
                   country == "United Kingdom" ~ "UK",
                   country == "Russian Federation" ~ "RU",
                   country == "China" ~ "CN",
                   country == "United States" ~ "USA")
  )

cro4 <- merge(cro3,export_cro, by = c("gp","year")) %>%
  select(-c("country")) %>%  rename(country = country.x)


###################################################################################################merge

hun4 <- hun4 %>% 
  mutate(
    balance = export - import,
    eu = ifelse(hun$year < 2004, 0, 1)
  ) %>%
  group_by(gp) %>%
  mutate(
    #n_fdi = scale(fdi_flow),
    n_balance = scale(balance),
    n_import = scale(import),
    n_export = scale(export)
  ) %>%
  ungroup()

## election years -1
yrs <- c(1997, 2001, 2005, 2009, 2013, 2017, 2021)
hun4 <- hun4 %>% 
  mutate(
    election_1 = ifelse(hun2$year %in% yrs, 1, 0)
  )


cz4 <- cz4 %>%  
  mutate(
    balance = export - import,
    eu = ifelse(cz$year<2004, 0, 1)
  ) %>%
  group_by(gp) %>%
  mutate(
    #n_fdi = scale(fdi_flow),
    n_balance = scale(balance),
    n_import = scale(import),
    n_export = scale(export)
  ) %>%
  ungroup()

yrs <- c(1995, 1997, 2001, 2005, 2009, 2012, 2016, 2020)
cz4 <- cz4 %>%
  mutate(
    election_1 = ifelse(cz2$year %in% yrs, 1, 0)
  )

cro4 <- cro4 %>% 
  mutate(
    balance = export - import,
    eu = ifelse(cro$year<2013, 0, 1)
  ) %>% 
  group_by(gp) %>%
  mutate(
    #n_fdi = scale(fdi_flow),
    n_balance = scale(balance),
    n_import = scale(import),
    n_export = scale(export)
  ) %>%
  ungroup()

yrs <- c(1996, 1999, 2004, 2008, 2013, 2018)
cro4 <- cro4 %>%
  mutate(
    election_1 = ifelse(cro2$year %in% yrs, 1, 0)
  )

merged_1 <- rbind(hun4, cz4, cro4) %>%
  select(-c(1))

leftright <- readxl::read_excel(here("data_input/leftright.xlsx")) %>%
  pivot_longer(-1) %>%
  rename(
    year = ...1,
    country = name
    )

merged_2 <- merge(merged_1, leftright, by = c("year", "country")) %>%
  rename(right_wing_gov = value)

write.csv(merged_2, here("data_inter/data.csv"))
