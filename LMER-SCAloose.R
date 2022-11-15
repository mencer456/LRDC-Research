library(nlme)
library(lme4)
library(sjPlot)

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)

df=read.csv('C:/Users/Hanna/Documents/GitHub/LRDC-Research/SCA_Merge/SCAloose.csv')
View(df)

#W is the number of words in the essay, so that will be our y
LMER=lme4::lmer(W~L1+as.factor(level_id)+(1|anon_id)+(1|question_id)+
                  S+VP+C+T+DC+CT+CP+CN+
                  MLS+MLT+MLC+
                  C.S+VP.T+C.T+DC.C+DC.T+T.S+CT.T+CP.T+CP.C+CN.T+CN.C,
                REML=FALSE,data=df)
LMER
#compared against Chinese and Level 3

# W	S	VP	C	T	DC	CT	CP	CN	MLS	MLT	MLC	C/S	VP/T	C/T	DC/C	DC/T	T/S	CT/T	CP/T	CP/C	CN/T	CN/C


summary(LMER)
par(mfrow=c(2,2))
plot(LMER)

sjPlot::plot_model(LMER)


#interact lvlid and lang
intLMER=lme4::lmer(W~L1*as.factor(level_id)+(1|anon_id)+(1|question_id)+
                  S+VP+C+T+DC+CT+CP+CN+
                  MLS+MLT+MLC+
                  C.S+VP.T+C.T+DC.C+DC.T+T.S+CT.T+CP.T+CP.C+CN.T+CN.C,
                REML=FALSE,data=df)
intLMER

summary(intLMER)
par(mfrow=c(2,2))
plot(intLMER)

sjPlot::plot_model(intLMER)


#WORDTYPE
wtLMER=lme4::lmer(wordtype_len~L1+as.factor(level_id)+(1|anon_id)+(1|question_id)+
                  S+VP+C+T+DC+CT+CP+CN+
                  MLS+MLT+MLC+
                  C.S+VP.T+C.T+DC.C+DC.T+T.S+CT.T+CP.T+CP.C+CN.T+CN.C,
                REML=FALSE,data=df)
wtLMER
#compared against Chinese and Level 3



summary(wtLMER)
par(mfrow=c(2,2))
plot(wtLMER)

sjPlot::plot_model(wtLMER)


#int wordtype
intwtLMER=lme4::lmer(wordtype_len~L1*as.factor(level_id)+(1|anon_id)+(1|question_id)+
                     S+VP+C+T+DC+CT+CP+CN+
                     MLS+MLT+MLC+
                     C.S+VP.T+C.T+DC.C+DC.T+T.S+CT.T+CP.T+CP.C+CN.T+CN.C,
                   REML=FALSE,data=df)
intwtLMER

summary(intwtLMER)
par(mfrow=c(2,2))
plot(intwtLMER)

sjPlot::plot_model(intwtLMER)
