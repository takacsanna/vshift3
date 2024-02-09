library(plm)
library(tidyverse)

df <- read.csv("C:/Users/Anna/Documents/tk/vshift3/tisztitott/ossz3.csv")
df$year <- relevel(factor(df$year), ref = "2013")

########################szentiment
kina <- df %>% 
  filter(gp == "CN")

p_sent_kina <- plm(log_senti ~ n_export + n_import + leftright + valasztasi_ev + eu, data = kina, model = "pooling")
summary(p_sent_kina)

f_sent_kina <- plm(log_senti ~ n_export + n_import + leftright + valasztasi_ev  + eu, data = kina, model = "within", effect = "individual", index=c("country", "year"))
summary(f_sent_kina)

pooltest(p_sent_kina, f_sent_kina)
#erősen elvetem H0-t, fixhatású

usa <- df %>% 
  filter(gp == "USA")

p_sent_usa <- plm(log_senti ~ n_export + n_import + leftright + valasztasi_ev + eu, data = usa, model = "pooling")
summary(p_sent_usa)

f_sent_usa <- plm(log_senti ~ n_export + n_import + leftright + valasztasi_ev + eu, data = usa, model = "within", index=c("country", "year"))
summary(f_sent_usa)

pooltest(p_sent_usa, f_sent_usa)
#jobb a fix

ru <- df %>% 
  filter(gp == "RU")

p_sent_ru <- plm(log_senti ~ n_export + n_import + leftright + valasztasi_ev + eu, data = ru, model = "pooling")
summary(p_sent_ru)

f_sent_ru <- plm(log_senti ~ n_export + n_import + leftright + valasztasi_ev + eu, data = ru, model = "within", index=c("country", "year"))
summary(f_sent_ru)

pooltest(p_sent_ru, f_sent_ru)

#######################################salience

#kína
p_sal_kina <- plm(salience ~ n_export + n_import + leftright + valasztasi_ev + eu, data = kina, model = "pooling")
summary(p_sal_kina)

f_sal_kina <- plm(salience ~ n_export + n_import + leftright + valasztasi_ev  + eu, data = kina, model = "within", effect = "individual", index=c("country", "year"))
summary(f_sal_kina)

pooltest(p_sal_kina, f_sal_kina)

#usa
p_sal_usa <- plm(salience ~ n_export + n_import + leftright + valasztasi_ev + eu, data = usa, model = "pooling")
summary(p_sal_usa)

f_sal_usa <- plm(salience ~ n_export + n_import + leftright + valasztasi_ev + eu, data = usa, model = "within", index=c("country", "year"))
summary(f_sal_usa)

pooltest(p_sal_usa, f_sal_usa)

#oroszország
p_sal_ru <- plm(salience ~ n_export + n_import + leftright + valasztasi_ev + eu, data = ru, model = "pooling")
summary(p_sal_ru)

f_sal_ru <- plm(salience ~ n_export + n_import + leftright + valasztasi_ev + eu, data = ru, model = "within", index=c("country", "year"))
summary(f_sal_ru)

pooltest(p_sal_ru, f_sal_ru)
