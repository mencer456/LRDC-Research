library(nlme)
library(lme4)
library(sjPlot)

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)

df=read.csv('C:/Users/Hanna/Documents/GitHub/LRDC-Research/CSV/same_probe.csv')
View(df)

unique(df$question_id)
tapply(df$answer_id, df$native_language)

LMER=lme4::lmer(text3_len~L1+as.factor(level_id)+(1|anon_id)+(1|question_id),REML=FALSE,data=df)
LMER

test=lme4::lmer(text3_len~L1*as.factor(level_id)+(1|anon_id)+(1|question_id),REML=FALSE,data=df)
summary(test)

library(lmerTest) #show p val


summary(LMER)
par(mfrow=c(2,2))
plot(LMER)
#glmer=glmer(text3_len~L1+level_id+(1|anon_id)+(1|question_id),data=df)

sjPlot::plot_model(LMER)
