library(mvtnorm)
library(expm)
library(plyr)
library(tsibble)
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")  
install.packages("tidyr")
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(tidyverse)
install.packages("MASS")
library(MASS)
library(matlib)


#Task9
bisamples <- rbinom(12, 30, 0.7)
x <- 1:12
barplot(bisamples)
mean_sam <- mean(bisamples)
var_sam <- var(bisamples)
bigt <- sqrt(30)*(mean_sam - 12*0.7)/sqrt(12*0.7*0.3)

bigtit <- function(m, n, p) {

  bisamples <- rbinom(n, m, p)
  
  return(sqrt(m)*(mean(bisamples) - n*p)/sqrt(n*p*(1-p)))
  
}

m=30
n=12
p=0.7
x<-rbinom(n=m,size=n,prob=p)


freq.table=table(x)
# freq.table is of class "table"
observed.vals=as.numeric(names(freq.table))
# add all possible values (not just observed values) to the vector
plot.vals=0:max(observed.vals)
comb.table=matrix(0,2,length(plot.vals))
colnames(comb.table)=plot.vals
rownames(comb.table)=c("sample",sprintf("Binom(%2.1i, %2.1f) pmf",n, p))
# compute proportions for the observed values
comb.table[1,observed.vals+1]=freq.table/m
# compute probabilities for all possible values
comb.table[2,]=dbinom(plot.vals,size=n,prob=p)
barplot(comb.table,beside=TRUE,col=c("blue","red"),ylab="relative frequency / probability",xlab="value",legend.text=TRUE,args.legend=list(x="topleft"))

kuka <- matrix(0,2,length(plot.vals))
colnames(kuka) = plot.vals
rownames(kuka) = c("sample", sprintf("Binom(%2.1i, %2.1f) pmf", n, p))
kuka[1, observed.vals + 1] = freq.table/m
kuka[2,] = dbinom(plot.vals, size = n, prob = p)

barplot(comb.table,beside=TRUE,col=c("blue","red"),ylab="relative frequency / probability",xlab="value",legend.text=TRUE,args.legend=list(x="topleft"))

k = 10000

m.vec = c(5,30,500)

size = 12
prob = 0.7
z = seq(-4,4,0.01)

for(m in m.vec){
  
  T.vec = c()
  for(i in 1:k){
    
    T.vec = c(T.vec, bigtit(m,n,p))
    
  }
  
  hist(T.vec, nclass = 16, freq = FALSE)
  lines(z, dnorm(z), col = "red", lty = 3)
  
}

solar = read.csv2("/Users/thetruetae/Desktop/RWTH/SS21/ADA/RLab/R-Lab3/Solar.csv", stringsAsFactors = TRUE)
solar$batch = as.factor(solar$batch)


pairs(~Pmax+Imax+Umax+Isc+Uoc,data=solar,col=c("red","blue","green","orange")[solar$batch])

boxplot(Uoc~batch, data=solar,xaxt="n",main="Boxplot for Uoc")
axis(1, at=1:4)

plot(solar$Pmax,solar$Isc,col=c("red","blue","green","orange")[solar$batch])
legend(x="topleft", legend = levels(solar$batch), col=c("red","blue","green","orange"), pch=1)
X = solar$Pmax[!is.na(solar$Isc) & solar$batch == 3]
Y = solar$Isc[!is.na(solar$Isc) & solar$batch == 3]

reg = lm(Y~X)
reg.par.lm = reg$coef

reg.par.mat
reg.par.lm
abline(reg.par.lm,col="black")

# (e)
# batch 1
X.1 = solar$Pmax[!is.na(solar$Isc)&solar$batch=="1"]
Y.1 = solar$Isc[!is.na(solar$Isc)&solar$batch=="1"]
reg1 = lm(Y.1~X.1)
reg.par1= reg1$coefficients
abline(reg.par1,col="red")
# batch 4
X.4 = solar$Pmax[!is.na(solar$Isc)&solar$batch=="4"]
Y.4 = solar$Isc[!is.na(solar$Isc)&solar$batch=="4"]
reg4 = lm(Y.4~X.4)
reg.par4= reg4$coefficients
abline(reg.par4,col="orange")
# alternative using the package ggplot2
ggplot(solar)+geom_point(aes(Pmax,Isc,col=batch))+geom_abline(aes(intercept=reg.par.lm[1],slope=reg.par.lm[2]))+geom_abline(aes(intercept=reg.par1[1],slope=reg.par1[2]),col="red")+geom_abline(aes(intercept=reg.par4[1],slope=reg
                                                                                                                                                                                                    .par4[2]),col="orange")+scale_color_manual(values=c("red", "blue", "green", "orange"))
# (f)
solar$Isc = round(ifelse(is.na(solar$Isc),predict(reg,newdata=data.frame(X=solar$Pmax)),solar$Isc), digits=3)
# alternative using the package dplyr
solar %>% mutate(
  Isc=round(ifelse(is.na(Isc),predict(reg,newdata=data.frame(X=solar$Pmax)),Isc),digits=3)
)
# (g)
save(solar,file="/Users/thetruetae/Desktop/RWTH/SS21/ADA/RLab/R-Lab4/Solar.RData")




rent.data = read.csv2("/Users/thetruetae/Desktop/RWTH/SS21/ADA/RLab/R-Lab3/rent.csv", stringsAsFactors = TRUE)

plot(rent.data$space,rent.data$rent.sqm)
rent.lm1 = lm(rent.sqm~space,data=rent.data)
abline(rent.lm1,col="red")
# the simple linear regression obviously does not capture the structure behind the data
# => transform the data points to find a better model
# the scatterplot is similar to a plot of f(x)=1/x
# => use the reciprocal of space instead:
plot(1/rent.data$space,rent.data$rent.sqm)
# we can see a clear linear structure and model it now using a linear regression

rent.lm2 = lm(rent.sqm~I(1/space),data=rent.data)
# the operator / is used in "formula" to construct the design matrix of complex models
# => the formula rent.sqm~1/space is identical to rent.sqm~1
# Use the function I if you want to use an expression of a variable, e.g. 1/space, as predictor
# here this leads to the predictor I(1/space)
abline(rent.lm2,col="red")
# the linear model with the predictor 1/space
plot(rent.data$space,rent.data$rent.sqm)
abline(rent.lm1,col="red")
curve(rent.lm2$coefficients[1]+rent.lm2$coefficients[2]/x,col="blue",add=TRUE)
# the new regression has a clearly better fit

meanpr <- mean(predict(rent.lm2))
varpr <- var(predict(rent.lm2))
respr <- var(residuals(rent.lm2))

