library(tidyverse)
library(readxl)
setwd("C:/Users/Anna/Documents/tk/vshift3/nyers")


countries = c("China", "France", "Germany", "Russian Federation", "United Kingdom", "United States")
################################magyar
import_hun <- readxl::read_excel("wits_import_hun.xlsx", sheet = 2)

import_hun2 <- import_hun %>% 
  select(-c(1, 3, 4, 5)) %>% 
  rename(
    country = `Partner Name`
  ) %>% 
  filter(
    country %in% countries
  ) %>% 
  pivot_longer(-1) %>% 
  set_names("country", "year", "import")


write_csv(import_hun2, "C:/Users/Anna/Documents/tk/vshift3/tisztitott/import_hun.csv")

export_hun <- readxl::read_excel("wits_export_hun.xlsx", sheet = 2)
export_hun2 <- export_hun %>% 
  select(-c(1, 3, 4, 5)) %>% 
  rename(
    country = `Partner Name`
  ) %>% 
  filter(
    country %in% countries
  ) %>% 
  pivot_longer(-1)

write_csv(export_hun2, "C:/Users/Anna/Documents/tk/vshift3/tisztitott/export_hun.csv")

################################cseh
import_cz <- readxl::read_excel("wits_import_czech.xlsx", sheet = 2)

import_cz2 <- import_cz %>% 
  select(-c(1, 3, 4, 5)) %>% 
  rename(
    country = `Partner Name`
  ) %>% 
  filter(
    country %in% countries
  ) %>% 
  pivot_longer(-1) %>% 
  set_names("country", "year", "import")

write_csv(import_cz2, "C:/Users/Anna/Documents/tk/vshift3/tisztitott/import_cz.csv")

export_cz <- readxl::read_excel("wits_export_czech.xlsx", sheet = 2)
export_cz2 <- export_cz %>% 
  select(-c(1, 3, 4, 5)) %>% 
  rename(
    country = `Partner Name`
  ) %>% 
  filter(
    country %in% countries
  ) %>% 
  pivot_longer(-1) %>% 
  set_names("country", "year", "export")


write_csv(export_cz2, "C:/Users/Anna/Documents/tk/vshift3/tisztitott/export_cz.csv")

################################horvát
import_cro <- readxl::read_excel("wits_import_cro.xlsx", sheet = 2)

import_cro2 <- import_cro %>% 
  select(-c(1, 3, 4, 5)) %>% 
  rename(
    country = `Partner Name`
  ) %>% 
  filter(
    country %in% countries
  ) %>% 
  pivot_longer(-1) %>% 
  set_names("country", "year", "import")

write_csv(import_cro2, "C:/Users/Anna/Documents/tk/vshift3/tisztitott/import_cro.csv")

export_cro <- readxl::read_excel("wits_export_cro.xlsx", sheet = 2)
export_cro2 <- export_cro %>% 
  select(-c(1, 3, 4, 5)) %>% 
  rename(
    country = `Partner Name`
  ) %>% 
  filter(
    country %in% countries
  ) %>% 
  pivot_longer(-1) %>% 
  set_names("country", "year", "export")


write_csv(export_cro2, "C:/Users/Anna/Documents/tk/vshift3/tisztitott/export_cro.csv")
