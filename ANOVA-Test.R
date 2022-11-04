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
plot(plot.test,las=1)

#make dataframe with group labels
mean.len.data=test%>%
  group_by(native_language,level_id)%>%
  summarise(
    len=mean(text_len)
  )

mean.len.data$group=c('a','b','b','b','b','b','b','b','c')
mean.len.data

two.way.plot=ggplot(test,aes(x=native_language,y=text_len,group=level_id))+
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

two.way.plot
#add standard err an means
newtwo.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2,color='maroon') +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange',color='maroon') +
  geom_point(data=mean.len.data, aes(x=native_language, y=len,color='pink'))

newtwo.way.plot
facet=newtwo.way.plot+
  facet_wrap(~level_id)
facet

rev.two.way.plot=ggplot(test,aes(x=level_id,y=text_len,group=native_language))+
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

rev.two.way.plot

new.revtwo.way.plot <- rev.two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2,color='maroon') +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange',color='maroon') +
  geom_point(data=mean.len.data, aes(x=level_id, y=len,color='pink'))+
  facet_wrap(~native_language)

new.revtwo.way.plot
