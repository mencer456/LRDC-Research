#setup
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

df=read.csv('C:\\Users\\Hanna\\Documents\\GitHub\\LRDC-Research\\CSV\\participant_averages.csv')
head(df)

test=read.csv('C:\\Users\\Hanna\\Documents\\GitHub\\LRDC-Research\\CSV\\whole.csv')
head(test)

summary(df)

#creating two way anova tests
two.way=aov(text_len~as.factor(native_language)+as.factor(level_id),data=test)
summary(two.way)

interaction=aov(text_len~as.factor(native_language)*as.factor(level_id),data=test)
summary(interaction)

#plotting the testt
par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(interaction)
par(mfrow=c(1,1))

#determining differences
tukey.two.way=TukeyHSD(two.way)
tukey.two.way

tukey.int=TukeyHSD(interaction)
tukey.int

#test

plot=aov(text_len~as.factor(native_language):as.factor(level_id),data=test)
plot.test=TukeyHSD(plot)
plot.test
