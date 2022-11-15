library(nlme)
library(lme4)
library(sjPlot)

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)

df=read.csv('C:/Users/Hanna/Documents/GitHub/LRDC-Research/SCA_Merge/SCAloose.csv')
View(df)

LMER=lme4::lmer(text3_len~L1+as.factor(level_id)+(1|anon_id)+(1|question_id)+
                  W+S+VP+C+T+DC+CT+CP+CN+
                  MLS+MLT+MLC+
                  C/S+VP/T+C/T+DC/C+DC/T+T/S+CT/T+CP/T+CP/C+CN/T+CN/C
                ,REML=FALSE,data=df)
LMER


# W	S	VP	C	T	DC	CT	CP	CN	MLS	MLT	MLC	C/S	VP/T	C/T	DC/C	DC/T	T/S	CT/T	CP/T	CP/C	CN/T	CN/C

