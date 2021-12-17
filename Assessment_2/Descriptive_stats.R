# 2) A)  mean, median, mode, trimmed mean
#Unique ID : E7321008
x<-c(100, 95, 95, 90, 85, 75, 65, 60, 55)
mean(x)
median(x)
library(modeest)
mode=mfv(x)
print(mode)
#trimmed mean
mean(x,trim=0.5)


#2 b)
#Unique ID : E7321008
ch<-data.frame(Patient=c(1:15),
                  Before=c(9.1,8.0,7.7,10.0,9.6,7.9,9.0,7.1,8.3,9.6,8.2,9.2,7.3,8.5,9.5),
                  After=c(8.2,6.4,6.6,8.5,8.0,5.8,7.8,7.2,6.7,9.8,7.1,7.7,6.0,6.6,8.4))
View(ch)
#Median and the IQR of the cholesterol measurements for the patients BEFORE treatment
median(ch$Before)
IQR(ch$Before)
#Median and the IQR of the cholesterol measurements for the patients AFTER treatment
median(ch$After)
IQR(ch$After)


# 2) C)
#Unique ID : E7321008
#installing packages "ggpubr" and "rstatix" for hypothesis testing
install.packages("ggpubr")
library(ggpubr)
install.packages("rstatix")
library(rstatix)
#installing package "MASS" for the dataset 'birthwt'
install.packages("MASS")
library(MASS)
birthwt
#one way anova to do hypothesis testing because we use it to compare dependent and independent
#H0 : smoking mother has no impact on weight of the baby during birth
aov1=aov(birthwt$bwt~birthwt$smoke)
summary(aov1)
#null hypothesis is rejected because p-value is lesser than 0.05
#visualizing the plot
library(ggplot2)
library(tidyverse)
ggplot(birthwt,aes(x=smoke,y=bwt,col=smoke))+
  geom_point() 
#hence smoking mothers have impact on weight of the baby during birth