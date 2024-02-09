library(tidyverse)
library(readxl)
setwd("C:/Users/Anna/Downloads")

##########################################################magyar

fdi_hun <- readxl::read_excel("C:/Users/Anna/Downloads/fdi_hun.xls")

orszagok <- c("Egyesült Államok", "Oroszország", "Egyesült Királyság", "Németország", "Franciaország", "Kína")

fdi_hun2 <- fdi_hun %>% 
  janitor::row_to_names(1) %>% 
  pivot_longer(-1) %>% 
  set_names("country", "year", "fdi_flow") %>% 
  filter(
    country %in% orszagok
  )
write.csv(fdi_hun2, "fdi_hun.csv")

##########################################################horvát

fdi_cro <- readxl::read_excel("C:/Users/Anna/Downloads/e-u5.xlsx")

fdi_cro2 <- fdi_cro %>% 
  janitor::row_to_names(3) 

countries = c("GERMANY", "RUSSIAN FEDERATION", "UNITED STATES", "FRANCE","UNITED KINGDOM", "CHINA")

#fdi_cro2$`2022.` <- as.numeric(fdi_cro2$`2022.`)
#fdi_cro2<-rename(fdi_cro2, `2022`=`2022.`)
fdi_cro2<-fdi_cro2[,c(1:31)]

fdi_cro2 <- fdi_cro2 %>% 
  pivot_longer(-1) %>% 
  set_names("country", "year", "fdi_flow") %>% 
  filter(
    country %in% countries
  )
write.csv(fdi_cro2, "fdi_cro.csv")

############################################################cseh

file_names <- list.files() %>% 
  keep(~ str_starts(., "FDI_4.4. Flows_Countries_I_"))


for (i in seq_along(file_names)) {
  file <- file_names[i]
  xls_sheets <- excel_sheets(file)
  if (any(xls_sheets == "USD")) {
    df <- read_excel(file, sheet = "USD")
    assign(paste0("df", i), df, envir = .GlobalEnv)
  } else {
    cat(paste("Warning: Sheet not found in", file, "\n"))
  }
}

#a 2000-es file hibás

countries = c("Germany", "France", "United Kingdom", "Russian Federation", "United States", "China")
years = c(2001:2013)
df_list <- list(df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14)
final_dfs <- list()

for (i in seq_along(df_list)) {
  df <- df_list[[i]]
  df_new <- df %>%
    janitor::row_to_names(7) %>%
    select(c(2, "Total")) %>%
    set_names("country", "fdi_flow") %>%
    mutate(year = years[i]) %>%
    filter(country %in% countries)
  final_dfs[[paste0("df_new_", i)]] <- df_new
}

final_df <- do.call(rbind, final_dfs)

final_df2 <- final_df[order(final_df$country),]
write.csv(final_df2, "fdi_cz_01-14.csv")

fdi_cz <- read.csv("C:/Users/Anna/Documents/tk/vshift3/nyers/fdi_cz_2.csv")
countries = c("Germany", "France", "United Kingdom", "Russia", "United States", "China")

fdi_cz2 <- fdi_cz[(fdi_cz$R1_POPIS_LNG %in% countries),]
fdi_cz2 <- fdi_cz2[(fdi_cz2$S1_POPIS_LNG == "Sum"),]
fdi_cz2 <- fdi_cz2[(fdi_cz2$MENA == "USD"),]

unique(fdi_cz2$R1_POPIS_LNG)

fdi_cz2 <- fdi_cz2 %>% 
  select(c(1,5,7)) %>% 
  set_names("year", "country", "fdi_flow") %>% 
  mutate(
    fdi_flow = fdi_flow*1000
  )
write.csv(fdi_cz2, "C:/Users/Anna/Documents/tk/vshift3/nyers/fdi_cz_14-21.csv")

cz1 <- read_csv("C:/Users/Anna/Documents/tk/vshift3/nyers/fdi_cz_01-14.csv")
cz2 <- read_csv("C:/Users/Anna/Documents/tk/vshift3/nyers/fdi_cz_14-21.csv")

cz3<-rbind(cz1, cz2)
cz3 <- cz3[order(cz3$country),] %>% 
  select(-c(1))

write.csv(cz3, "C:/Users/Anna/Documents/tk/vshift3/tisztitott/fdi_cz.csv")

###############################################oecd

fdi <- read.csv("C:/Users/Anna/Documents/tk/vshift3/nyers/fdi_oecd.csv")

fdi_hun2 <- fdi %>% 
  filter(
    Reporting.country == "Hungary",
    Partner.country.territory %in% countries,
    Currency == "US Dollar"
  ) %>% 
  select(c("Year", "Value", "Partner.country.territory", "Measurement.principle"))
unique(fdi_cz2$Partner.country.territory)

fdi_cz2 <- fdi %>% 
  filter(
    Reporting.country == "Czechia",
    Partner.country.territory %in% countries,
    Currency == "US Dollar"
  ) %>% 
  select(c("Year", "Value", "Partner.country.territory", "Measurement.principle"))
