library(mvtnorm)
library(expm)
library(plyr)
library(tsibble)
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")  
install.packages("tidyr")
install.packages("delta")
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(tidyverse)
install.packages("MASS")
library(MASS)
library(matlib)
library(rgl)
library(car)

#Task13

set.seed(2020)

N = 25
vec.delta.simple = rep(0,100)
vec.delta.correct = rep(0,100)
vec.delta.correct.poly = rep(0, 100)

si_k = 0
co_k = 0

for(i in 1:100){
  
  x = runif(N,  -50, 100)
  
  mu=45+0.1*x+0.0005*x^2+5e-7*x^3+5e-11*x^4+5e-13*x^5 
  y=mu+rnorm(N,sd=10) 
  model.correct=lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5))
  # better use poly
  model.correct.poly=lm(y~poly(x,degree=5))
  
  fitted.vals.poly=predict(model.correct.poly,newdata=data.frame(x=x))
  
  model.simple=lm(y~x)
  
  si=mean(abs(model.simple$fitted.values-mu))
  co=mean(abs(model.correct$fitted.values-mu))
  
  if(si < co){
    
    si_k = si_k + 1
    
  } else {
    
    co_k = co_k + 1
    
  }
  
  if(i<6){
    # data
    #plot(x,y)
    param1=model.correct$coefficients
    param2=model.simple$coefficients
    # true relationship
    #curve(45+0.1*x+0.0005*x^2+5e-7*x^3+5e-11*x^4+5e-13*x^5,add=TRUE,col="blue")
    # correct model
    #curve(param1[1]+param1[2]*x+param1[3]*x^2+param1[4]*x^3+param1[5]*x^4+param1[6]*x^5,add=TRUE,col="red")
    # simple model
    #curve(param2[1]+param2[2]*x,add=TRUE,col="green")
    # correct model fitted with poly
    x.grid=seq(min(x),max(x),length.out = 100)
    y.pred=predict(model.correct.poly,newdata=data.frame(x=x.grid))
    #lines(x.grid,y.pred, col="orange")
    plot(model.correct$residuals,add=T,col="blue")
    plot(model.simple$residuals,add = T, col="red")
  }
  
  vec.delta.correct[i]=mean(abs(model.correct$fitted.values-mu))
  vec.delta.correct.poly[i]=mean(abs(fitted.vals.poly-mu))
  vec.delta.simple[i]=mean(abs(model.simple$fitted.values-mu))
  
}
mean(vec.delta.simple)
sd(vec.delta.simple)

#####
#Task 14
#####

load("/Users/thetruetae/Desktop/RWTH/SS21/ADA/RLab/R-Lab4/Survey1.RData")

# (b)
model.survey=lm(DimSelf~DimEmotion+DimBody, data = data.survey) 


# (c)
# check the fit of the model
par(mfrow=c(2,2))
plot(model.survey)
par(mfrow=c(1,1))
plot(cooks.distance(model.survey))

# test of normality
shapiro.test(model.survey$residuals)

# (d)
plot3d(x=data.survey$DimEmotion,y=data.survey$DimBody,z=data.survey$DimSelf,xlab="DimEmotion",ylab="DimBody",zlab="DimSelf")
planes3d(a=model.survey$coefficient[2],b=model.survey$coefficient[3],c=-1,d=model.survey$coefficients[1],alpha=0.5)

# (e)
summary(model.survey)

print("correct =")
print(mean(abs(model.correct$fitted.values-mu)))
print("simple =")
print(mean(abs(model.simple$fitted.values-mu)))

########
#Task 15
#########

# (a)
load("/Users/thetruetae/Desktop/RWTH/SS21/ADA/RLab/R-Lab4/Solar.RData")

# (b)
boxplot(solar$Pmax ~ solar$batch)

solar.aov = aov(solar$Pmax ~ solar$batch)

# (d)
# check the fit of the model
par(mfrow=c(2,2))
plot(solar.aov)
par(mfrow=c(1,1))
plot(cooks.distance(solar.aov))

#test of normality
shapiro.test(residuals(solar.aov))

#Levene-Test of equal variances
leveneTest(solar.aov)

# alternative: Levene-Test "by hand"
Median.Grp = tapply(solar$Pmax,solar$batch,median)
Z = abs(solar$Pmax - Median.Grp[solar$batch])
summary(aov(Z ~ solar$batch))

# (e)
summary(solar.aov) 

# (f)
# pairwise comparison with Tukey-Test
# since the model is balanced (each group of a factor level has the same length)
solar.Tuk = TukeyHSD(solar.aov,conf.level=0.9)
solar.Tuk 

# plot of the computed confidence intervals
plot(solar.Tuk)


############
#
# Task 16
#
#############

# (a)
ToothGrowth
len=ToothGrowth$len
supp=ToothGrowth$supp
dose=as.factor(ToothGrowth$dose)

# (b)
boxplot(len~supp*dose)

# (c)
model=lm(len~supp*dose)

# (d)
# check the fit of the model
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
plot(cooks.distance(model))

shapiro.test(residuals(model))

leveneTest(model) 

# (e)
summary(model)

# (f)
anova(model)

# (g)
model.new=lm(len~dose+supp)
