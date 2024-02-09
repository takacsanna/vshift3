library(tidyverse)
library(foreign)
library(nnet)
library(reshape2)

stm <- read_rds("C:/Users/Anna/Downloads/sentiment.rds")
stm <- stm[(!is.na(stm$gov_opp) | !is.na(stm$left_right)),]
stm <- stm[(stm$left_right != "NA"),]
stm <- stm[(stm$gov_opp != "NA"),]
stm <- stm %>% 
  mutate(
    pos_senti = ifelse(stm$log_senti <= 0, 0, 1)
  )
stm$multi_senti <- ifelse(stm$log_senti > 0.5, 1, ifelse(stm$log_senti >= -0.5 & stm$log_senti <= 0.5, 0, -1))

stm <- stm %>% 
  mutate(
    tavolsag = 2004 - cal_yr
  )

valasztasok <- as.Date(c("1998-05-10", "2002-04-07",
                         "2006-04-09", "2010-04-11", 
                         "2014-04-06", "2018-04-08", "2022-04-03"))


stm <- stm %>%
  mutate(valasztas = ifelse(((valasztasok - stm$date) <= 365), 1, 0))

###ols a bővebb west csoportra
westb <- stm[(stm$gp_grp_westb == "West"),]

linreg_westb <- lm(log_senti ~ gov_opp + left_right + nmention + cal_yr + valasztas, data = westb)
summary(linreg_westb)

#0.009

###ols a szűkebb west csoportra

westa <- stm[(stm$gp_grp_westa == "West"),]

linreg_westa <- lm(log_senti ~ gov_opp + left_right + nmention + cal_yr + valasztas, data = westa)
summary(linreg_westa)

#0.02

###logisztikus regresszió

logit_westa <- glm(pos_senti ~ gov_opp + left_right + nmention + tavolsag + valasztas, data = westa, family = "binomial")
summary(logit_westa)
nullm <- glm(pos_senti~1, data = westa, family = "binomial")
1-logLik(logit_westa)/logLik(nullm)

#0.029: westa

logit_westb <- glm(pos_senti ~ gov_opp + left_right + nmention + tavolsag + valasztas, data = westb, family = "binomial")
summary(logit_westb)
nullm <- glm(pos_senti~1, data = westb, family = "binomial")
1-logLik(logit_westb)/logLik(nullm)

#0.014: westb

###multinomiális logit
multi_westa <- multinom(multi_senti ~ gov_opp + left_right + nmention + tavolsag + valasztas, data = westa)
summary(multi_westa)
head(round(fitted(multi_westa), 2))

westa$senti_pred <- predict(multi_westa, newdata = westa, "class")
tab <- table(westa$multi_senti, westa$senti_pred)
round((sum(diag(tab))/sum(tab))*100,2)
#52.63% accuracy

###################################################panel
