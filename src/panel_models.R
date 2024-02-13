gc()

library(plm)
library(here)
library(tidyverse)

df <- read.csv(here("data_inter/data.csv"))

########################sentiment

#cn
cn <- df %>% 
  filter(gp == "CN")

p_sent_cn <- plm(log_senti ~ n_export + n_import + leftright + valasztasi_ev + eu, data = cn, model = "pooling")
summary(p_sent_cn)

f_sent_cn <- plm(log_senti ~ n_export + n_import + leftright + valasztasi_ev  + eu, data = cn, model = "within", effect = "individual", index=c("country", "year"))
summary(f_sent_cn)

pooltest(p_sent_cn, f_sent_cn)

#usa
usa <- df %>% 
  filter(gp == "USA")

p_sent_usa <- plm(log_senti ~ n_export + n_import + leftright + valasztasi_ev + eu, data = usa, model = "pooling")
summary(p_sent_usa)

f_sent_usa <- plm(log_senti ~ n_export + n_import + leftright + valasztasi_ev + eu, data = usa, model = "within", index=c("country", "year"))
summary(f_sent_usa)

pooltest(p_sent_usa, f_sent_usa)

#ru
ru <- df %>% 
  filter(gp == "RU")

p_sent_ru <- plm(log_senti ~ n_export + n_import + leftright + valasztasi_ev + eu, data = ru, model = "pooling")
summary(p_sent_ru)

f_sent_ru <- plm(log_senti ~ n_export + n_import + leftright + valasztasi_ev + eu, data = ru, model = "within", index=c("country", "year"))
summary(f_sent_ru)

pooltest(p_sent_ru, f_sent_ru)

#######################################salience

#cn
p_sal_cn <- plm(salience ~ n_export + n_import + leftright + valasztasi_ev + eu, data = cn, model = "pooling")
summary(p_sal_cn)

f_sal_cn <- plm(salience ~ n_export + n_import + leftright + valasztasi_ev  + eu, data = cn, model = "within", effect = "individual", index=c("country", "year"))
summary(f_sal_cn)

pooltest(p_sal_cn, f_sal_cn)

#usa
p_sal_usa <- plm(salience ~ n_export + n_import + leftright + valasztasi_ev + eu, data = usa, model = "pooling")
summary(p_sal_usa)

f_sal_usa <- plm(salience ~ n_export + n_import + leftright + valasztasi_ev + eu, data = usa, model = "within", index=c("country", "year"))
summary(f_sal_usa)

pooltest(p_sal_usa, f_sal_usa)

#ru
p_sal_ru <- plm(salience ~ n_export + n_import + leftright + valasztasi_ev + eu, data = ru, model = "pooling")
summary(p_sal_ru)

f_sal_ru <- plm(salience ~ n_export + n_import + leftright + valasztasi_ev + eu, data = ru, model = "within", index=c("country", "year"))
summary(f_sal_ru)

pooltest(p_sal_ru, f_sal_ru)
