library(ggplot2)
library(mvtnorm)
library(MASS)
library(dplyr)
library(tidyr)
library(gridExtra)
library(car)
library(datasets)
install.packages("VGAM")
library(VGAM)
library(vcdExtra)
library(Matrix)

set.seed(518)

######################
#########TASK1########
######################
# a) i define this
a = c(1,2,3)
b = c(3, -5, 0)
A = matrix(c(3, 1.5, 1/8, 5, -pi, -6, -1, exp(2.5), 9), 
           nrow = 3,
           ncol = 3)

B = matrix(c(1, -2, 3, -2, 4, -6, 3, -6, 9), 
           nrow = 3,
           ncol = 3)
#ii)
# A %*% a
# t(a) %*% b

#iii
#component-wise operation
# + ,-, *, /

#iv 
# construct the new matrix by
# 1. add the first two columns
# 2. add the first two rows
# > B
# [,1] [,2] [,3]
# [1,]    1   -2    3
# [2,]   -2    4   -6
# [3,]    3   -6    9
# > rbind(B[1,] + B[2,], B[3,])
# [,1] [,2] [,3]
# [1,]   -1    2   -3
# [2,]    3   -6    9
# > cbind(rbind(B[1,] + B[2,], B[3,])[,1] + rbind(B[1,] + B[2,], B[3,])[,2] ,rbind(B[1,] + B[2,], B[3,])[,3])
# [,1] [,2]
# [1,]    1   -3
# [2,]   -3    9
#b) Use cut and label data
x = c(5,2,6,4,1,2,2,5,4,4,6,4,2,5,5,3,6,1,4,5)

# set x -> 1, if x = 1, 2‚
# set x -> 2, if x = 3, 4
# set x -> 3, if x = 5, 6

cuts = cut(x/2, breaks = c(0,1,2,3), include.lowest = TRUE, label = c(1,2,3))

######################
#########TASK2########
######################
#a)
v1 = c(TRUE, TRUE, FALSE, TRUE, FALSE)
v2 = 1:6
v3 = 5:10

#b)

v4 = v1*1

#c)
k = 0
for(i in 1:length(v2)) { 
  
  k = k +  (v2[i]*v3[i])^i

}
print(k)

# to get the power of values corresponding their index
# i.e. a^1 b^2 c^3 d^4 e^5 
# 
#sum((v2*v3)^c(1:length(v2)))

#d)

# by using loop
short_len =  if (length(v2) < length(v4)) length(v2) else length(v4)

for(i in 1:short_len) { 
  
  if(v2[i]> v4[i]){
    
    print(v2[i])
    break
    
  }
  
}

# without loop
# which(v2[1:short_len] - v4 > 0)[1]

mywhich <- function(a1, a2){
  
  samelen = length(a1) == length(a2)

  if(samelen){
    
    return( sum((a1*a2)^c(1:length(a1))))
    
  } else {
    
    short_len =  if (length(v2) < length(v4)) length(v2) else length(v4)
    
    return(which(a1[1:short_len] - a2[1:short_len] > 0)[1])

  }
  
}

######################
#########TASK3########
######################
#i)
credits.df <- read.table("R-Lab-Datasets/credits.wsv", header = TRUE, sep = " ")
#ii)
#credits.df[2,]

# exclude from the 1st and 8th column
# credits.df[, -(1:8)]
#iii)
mean(credits.df$amount)
median(credits.df$amount)
summary(credits.df$amount)


my.sd <- function(data, corrected=FALSE){
  
  len = if(corrected) length(data) else length(data)-1
  
  return( sqrt(sum((data - mean(data))^2)/len))
  
}

######################
#########TASK4########
######################

#a)

mu = 5
sd = 2

rnorm10 = rnorm(10, mean = mu, sd = sd)

rnorm50 = rnorm(50, mean = mu, sd = sd)

rnorm100 = rnorm(100, mean = mu, sd = sd)

#b)
ss
# case rnorm10

pnorm(7, mean(rnorm10), sd(rnorm10)) - pnorm(3, mean(rnorm10), sd(rnorm10))

pnorm(9, mean(rnorm10), sd(rnorm10)) - pnorm(5, mean(rnorm10), sd(rnorm10))


pnorm(7, mean(rnorm50), sd(rnorm50)) - pnorm(3, mean(rnorm50), sd(rnorm50))

pnorm(9, mean(rnorm50), sd(rnorm50)) - pnorm(5, mean(rnorm50), sd(rnorm50))

pnorm(7, mean(rnorm100), sd(rnorm100)) - pnorm(3, mean(rnorm100), sd(rnorm100))

pnorm(9, mean(rnorm100), sd(rnorm100)) - pnorm(5, mean(rnorm100), sd(rnorm100))

#c) quantiles

qnorm(0.3, mean(rnorm10), sd(rnorm10))
qnorm(0.3, mean(rnorm50), sd(rnorm50))
qnorm(0.3, mean(rnorm100), sd(rnorm100))
qnorm(0.3, mu, sd)

qnorm(0.5, mean(rnorm10), sd(rnorm10))
qnorm(0.5, mean(rnorm50), sd(rnorm50))
qnorm(0.5, mean(rnorm100), sd(rnorm100))
qnorm(0.5, mu, sd)

qnorm(0.75, mean(rnorm10), sd(rnorm10))
qnorm(0.75, mean(rnorm50), sd(rnorm50))
qnorm(0.75, mean(rnorm100), sd(rnorm100))
qnorm(0.75, mu, sd)

# sample size n=10, 50, 100
for(n in c(10, 50, 100)){
  print("Sample size:")
  print(n)
  # (a)
  
  # generate random numbers, remember: sd=sqrt(sigma^2)
  X = rnorm(n, mean=5, sd = 2)
  
  # (b)
  
  # proportion of random numbers in the interval
  h1 = mean(3 <= X & X <= 7)
  h2 = mean(5 <= X & X <= 9)
  # TRUE is interpreted as 1 and FALSE is interpreted as 0
  
  # probability of the normal distribution
  p = pnorm(c(7,9), mean = 5, sd = 2) - pnorm(c(3,5), mean = 5, sd = 2)
  
  print(list(proportion=c(h1, h2), probability = p))
  
  # (c)
  
  # empirical quantiles
  q1 = quantile(X, probs=c(0.3, 0.5, 0.75))
  # quantiles of the normal distribution
  q2 = qnorm(c(0.3, 0.5, 0.75), mean=5, sd = 2)
  
  print(list(emp_quantile=q1, quantile=q2))
}

######################
#########TASK5########
######################

# n = 30, mu = 5, sig = 4(sd = 2)
n30 = rnorm(30, 5, 2)
n30d = dnorm(n30, 5, 2)

# n = 100, mu = 5, sig = 4(sd = 2)
n100 = rnorm(100, 5, 2)
n100d = dnorm(n100, 5, 2)

# n = 300, mu = 5, sig = 4(sd = 2)
n300 = rnorm(300, 5, 2)
n300d = dnorm(n300, 5, 2)

hist(n300, freq = FALSE)
lines(density(n300), col = "red")
lines(n300, dnorm(n300, mean = 5, sd = 2), col = "blue")

######################
#########TASK6########
######################

#a)
set.seed(98989)
sample_size = 100                                       
sample_meanvector = c(0, 0, 0, 0)                                   
sample_covariance_matrix = diag(4)

# create bivariate normal distribution
sample_distribution = mvrnorm(n = sample_size,
                               mu = sample_meanvector, 
                               Sigma = sample_covariance_matrix)
#b)
new.mu = c(1,0,2,-1)

sigma1 = matrix(c(4,2,2,3,2,3,2,1,2,2,5,2,3,1,2,3), ncol = 4)

sigma2 = matrix(c(4.5, 4.75, 2, 2.25, 4.75, 5.25, 2.75, 3.25, 2, 2.75, 2.75, 3.5, 2.25, 3.25, 3.5, 4.5), ncol = 4)

#c)
SVD.sigma1.sq = svd(sigma1)$u %*% diag(sqrt(svd(sigma1)$d), 4, 4) %*% t(svd(sigma1)$v)

sigma1.trans = t(new.mu + SVD.sigma1.sq %*% t(sample_distribution))

SVD.sigma2.ev = replace(svd(sigma2)$d, 
                        svd(sigma2)$d < sqrt(.Machine$double.eps), 
                        0)
SVD.sigma2.sq = svd(sigma2)$u %*% diag(SVD.sigma2.ev) %*% t(svd(sigma2)$v)

sigma2.trans = t(new.mu + SVD.sigma2.sq%*% t(sample_distribution))

#d) scatterplot matrix for the multi dimensional matrix
pairs(sample_distribution)

#e) the last check!

ginv.sig1 = ginv(sigma1)

inv.sig1 = solve(sigma1)

ginv.sig1 = ginv(sigma2)

#inv.sig1 = solve(sigma2)

######################
#########TASK7########
######################

#a)
survey1a = read.csv2("R-Lab-Datasets/Survey1a.csv", header = TRUE, sep = ";")

survey1b = read.csv2("R-Lab-Datasets/Survey1b.csv", header = TRUE, sep = ";")

#b)
# try to use regular expression
Diminx = grep("Dim+", colnames(survey1a), perl = TRUE, value = FALSE)

Meaninx = grep("Mean+", colnames(survey1a), perl = TRUE, value = FALSE)

for( i in c(Diminx, Meaninx)){
  
  survey1a[,i] = as.numeric(survey1a[,i])
  survey1b[,i] = as.numeric(survey1b[,i])
  
}

#c)

survey1 = rbind(survey1a, survey1b)
survey1 = survey1[!duplicated(survey1),]

# we see the columns DimBody, DimSelf, DimFamily, and MeanScore have NA
#colSums(is.na(survey1)) > 0

survey1$DimBody[is.na(survey1$DimBody)]<-mean(survey1$DimBody,na.rm=TRUE)
survey1$DimSelf[is.na(survey1$DimSelf)]<-mean(survey1$DimSelf,na.rm=TRUE)
survey1$DimFamily[is.na(survey1$DimFamily)]<-mean(survey1$DimFamily,na.rm=TRUE)
survey1$MeanScore[is.na(survey1$MeanScore)]<-mean(survey1$MeanScore,na.rm=TRUE)

#d)
ggplot(survey1, aes(Age, DimSchool, color = Sex)) +
  geom_point()

#e)
ggplot(survey1, aes(Sex,DimFriends, color = Sex)) + geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)

#f)
write.csv(survey1,"survey1_task7.csv", row.names = FALSE)

######################
#########TASK8########
######################
#a)
credits_task8 = read.csv2("R-Lab-Datasets/credits.wsv", header = TRUE, sep = " ")

#b)
credits_task8$gastarb = credits_task8$gastarb * -1 + 3

#c)
nrow(credits_task8)

#cuts = cut(x/2, breaks = c(0,1,2,3), include.lowest = TRUE, label = c(1,2,3))

credits_task8$dtime = as.numeric(cut(credits_task8$time, 
                          breaks = c(seq(0, 54, by = 6), Inf), 
                          include.lowest = TRUE,
                          label = 10:1))

credits_task8$damount = as.numeric(cut(credits_task8$amount, 
                          breaks = c(0, 500, 1000, 
                                     1500,2500,  5000, 
                                     7500,10000,15000,20000,Inf), 
                          include.lowest = TRUE,
                          label = 10:1))

credits_task8$dage = as.numeric(cut(credits_task8$age, 
                          breaks = c(0, 
                                    25,
                                    39,
                                    59,
                                    64,
                                    Inf), 
                          include.lowest = TRUE,
                          label = 1:5))
#summed <- rowSums(zscore[, c(1, 2, 3, 5)])
#account, dtime, behavior, usage, damount, savings, employment, rate, famgen, guaran, residence, finance, dage, furthcred, home, prevcred, job, pers, phone, gastarb.
#credits_task8$simple.score = 
# dataf[, c('A', 'B', 'Cost')]

credits_task8$simple.score = rowSums(credits_task8[, c("account", "dtime", "behavior", "usage", "damount", "savings", "employment", "rate", "famgen", "guaran", "residence", "finance", "dage", "furthcred", "home", "prevcred", "job", "pers", "phone", "gastarb")])

write.csv(credits_task8,"R-Lab-Datasets/credits_task8.csv", row.names = FALSE)

######################
#########TASK9########
######################

#a) get the random sample
rbinom_task9 = rbinom(30, 12, 0.7)

#b) construct the bar plot
barplot(table(rbinom_task9))

#c) calculate mean and variance of the sample
# and write them in the figure

T.mnp = sqrt(30)*((mean(rbinom_task9) - 12*0.7)/sqrt(12*0.7*0.3))

# (d)
calc.T<-function(m, n, p){
  x<-rbinom(n=m,size=n, prob=p)
  x.bar=mean(x)
  s2=var(x)
  T_mnp=sqrt(m)*((x.bar-n*p)/sqrt(n*p*(1-p)))
  return(T_mnp)
}

# (e)
k=10000
m.vec=c(5,30,500)
size=12
p = 0.7
z = seq(-4,4,0.01) #for the plot of dnorm
for(m in m.vec){
  T.vec=c()
  for(i in 1:k){
    T.vec=c(T.vec,calc.T(m,n,p))
  }
  hist(T.vec,nclass=16,freq=FALSE) #histogram with 16 breaks
  lines(z,dnorm(z),col="red",lty=3) #add density of standard normal distribution on the interval from -4 to 4
}



######################
#########TASK10########
######################

#a) import the data
solar_task10 = read.csv2("R-Lab-Datasets/Solar.csv", header = TRUE, sep = ";")

# batch to type factor
solar_task10$batch = as.factor(solar_task10$batch)

#b) scatter plot

pairs(~Pmax+Imax+Umax+Isc+Uoc,
      data=solar_task10, 
      col = solar_task10$batch,
      main="Solar scatter plot")

#c)
ggplot(solar_task10, aes(batch, Uoc, color = batch)) + geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)

#d)

pairs(~Pmax+Isc,
      data=solar_task10, 
      col = solar_task10$batch,
      main="Solar scatter plot Pmax and Isc")
#i) compute the parameter by using I.4.6 and I.4.9
# Isc∼Pmax
# insert the vector of ones
new_pmax = cbind(rep(1, nrow(solar_task10)), solar_task10$Pmax)

# achieve the parameter manually
parameter_manual = ginv(new_pmax) %*% solar_task10$Isc[!is.na(solar_task10$Isc)]

X = solar_task10$Pmax[!is.na(solar_task10$Isc)]
Y = solar_task10$Isc[!is.na(solar_task10$Isc)]

# ii)
# fit the linear model
fit = lm(Y ~ X)

plot(solar_task10$Pmax,solar_task10$Isc,col=c("red","blue","green","orange")[solar_task10$batch])

reg.par.lm = fit$coefficients
# 
# > fit$coefficients
# (Intercept)        Pmax 
# 4.49334242  0.03713745 

abline(fit, col = "orange")

Pmax.df = data.frame(solar_task10$Pmax)
predicted_Isc = predict(fit, newdata = solar_task10)
batch4 = which(solar_task10$batch == 4)
batch1 = which(solar_task10$batch == 1)

fit.batch1 = lm(solar_task10$Isc[batch1] ~ solar_task10$Pmax[batch1], data = solar_task10)

abline(fit.batch1$coefficients, col = "blue")

abline(lm(solar_task10$Isc[batch4] ~ solar_task10$Pmax[batch4], data = solar_task10), col = "red")


# e) predict the regression of missing values in Isc
Isc_NA = which(is.na(solar_task10$Isc) == TRUE)
solar_task10$Isc[Isc_NA] = predicted_Isc[predicted_Isc]

write.csv(solar_task10,"R-Lab-Datasets/solar_task10.csv", row.names = FALSE)

#######################
#########TASK11########
#######################

#a)
rent_task11 = read.csv2("R-Lab-Datasets/rent.csv", header = TRUE, sep = ";")

#b)
plot(rent_task11$space, rent_task11$rent.sqm)
lm.rent = lm(rent.sqm ~ space , data = rent_task11)
abline(lm.rent, col = "red")

#c)
plot(1/rent_task11$space,rent_task11$rent.sqm)
lm.rent.2 = lm(rent.sqm ~ 1/space , data = rent_task11)
abline(lm.rent.2, col = "blue")

#######################
#########TASK12########
#######################

#a)
cars_task12 = read.table("R-Lab-Datasets/cars2.dat", header = TRUE, sep = " ")

#b)
plot(cars_task12$speed, cars_task12$dist)

#c)
speed = cars_task12$speed
dist = cars_task12$dist

lm.cars2.qd = lm(dist ~ poly(speed,2))

plot(speed, dist)

speed.qd = (cars_task12$speed)^2

cars_task12$speed2 = speed.qd

speed_secon = seq(min(speed), max(speed), length.out = 100)

speed_secon_grid = data.frame(speed = speed_secon)

predicted.dist = predict(lm.cars2.qd, speed_secon_grid)

lines(speed_secon, predicted.dist,col = "blue")

#d)

B0 = cbind(1, speed)
B = cbind(B0,  speed^2)

Q0 = B0 %*% solve(t(B0) %*% B0) %*% t(B0)
Q = B %*% solve(t(B) %*% B) %*% t(B)

r0 = 2
r = 3

numerator = t(dist) %*%(Q - Q0) %*% dist
denominator = t(dist) %*%(diag(nrow(Q)) - Q) %*% dist/(nrow(B) - r)

F_statistic = numerator / denominator

lm.cars2.normal = lm(dist ~ speed)

#######################
#########TASK13########
#######################

# μ(x)=45+0.1x+5·10−4x2 +5·10−7x3 +5·10−11x4 +5·10−13x5
# sd = 10



#a) initialize the model 

set.seed(2022)

#vec.delta.simple = runif(100, 0.0, 100)

#vec.delta.correct = runif(100, 0.0, 100)

N=25
# N=1500
vec.delta.simple=rep(0,100)
vec.delta.correct=rep(0,100)
vec.delta.correct.poly=rep(0,100)
for(i in 1:100){
  x=runif(N,0,100)
  #x=runif(N,-50,100)
  
  mu=45+0.1*x+0.0005*x^2+5e-7*x^3+5e-11*x^4+5e-13*x^5
  y=mu+rnorm(N,sd=10)
  model.correct=lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5))
  
  # better use poly
  model.correct.poly=lm(y~poly(x,degree=5))
  fitted.vals.poly=predict(model.correct.poly,newdata=data.frame(x=x))
  
  model.simple=lm(y~x)
  
  if(i<6){
    # data
    plot(x,y)
    param1=model.correct$coefficients
    param2=model.simple$coefficients
    # true relationship
    curve(45+0.1*x+0.0005*x^2+5e-7*x^3+5e-11*x^4+5e-13*x^5,add=TRUE,col="blue")
    # correct model
    curve(param1[1]+param1[2]*x+param1[3]*x^2+param1[4]*x^3+param1[5]*x^4+param1[6]*x^5,add=TRUE,col="red")
    # simple model
    curve(param2[1]+param2[2]*x,add=TRUE,col="green")
    # correct model fitted with poly
    x.grid=seq(min(x),max(x),length.out = 100)
    y.pred=predict(model.correct.poly,newdata=data.frame(x=x.grid))
    lines(x.grid,y.pred, col="orange")
  }
  vec.delta.correct[i]=mean(abs(model.correct$fitted.values-mu))
  vec.delta.correct.poly[i]=mean(abs(fitted.vals.poly-mu))
  vec.delta.simple[i]=mean(abs(model.simple$fitted.values-mu))
}

mean(vec.delta.simple)
mean(vec.delta.correct)


#######################
#########TASK14########
#######################

#a)
survey_task14 = read.csv2("R-Lab-Datasets/survey1_task7.csv", header = TRUE, sep = ",")

#b) create a regression model with the approach
# DimSelf = d + a DimEmotion + b DimBody

DimSe = as.integer(survey_task14$DimSelf)
DimEmo = as.integer(survey_task14$DimEmotion)
DimBo = as.integer(survey_task14$DimBody)

DimSelf.fit = lm(DimSe ~ DimEmo + DimBo)

#c)-1 plot
#i) residual vs fitted
residuals = DimSelf.fit$residuals

fitted_values = DimSelf.fit$fitted.values

plot(residuals, fitted_values)

#ii) standardized residual vs fitted
DimSelf.rstandard = rstandard(DimSelf.fit)
plot(sqrt(abs(DimSelf.rstandard)), fitted_values)

#iii) quantiles of the standardized residuals vs the expected quantiles

qqnorm(DimSelf.rstandard, col = "blue")
qqline(DimSelf.rstandard, col = "red")

#iv)
# leverage: a measure of how far away the independent variable values
# of an observation are from of the other observations

# get the leverage
leverages = hatvalues(DimSelf.fit)
plot(DimSelf.rstandard, leverages)

plot(cooks.distance(DimSelf.fit))

# another option
par(mfrow =c(2,2))
plot(DimSelf.fit)
par(mfrow=c(1,1))
plot(cooks.distance(DimSelf.fit))

#d) Null hypothesis b = 0 => DimBody

hypo.fit = lm(DimSe ~ DimEmo)

Y = DimSe

ones2 = rep(1, nrow(survey_task14))

B0 = cbind(ones2, DimEmo)

B = cbind(B0, DimBo)

Q = B %*% solve(t(B)%*%B, t(B))
Q0 = B0 %*% solve(t(B0)%*%B0, t(B0))
n = nrow(survey_task14)
r = 3
r0 = 2
numer2 = Y %*% (Q-Q0) %*% Y / (r-r0)
denom2 = Y %*% (diag( nrow(survey_task14))) %*% Y / (n-r)
F_statistic = numer2 / denom2

reject = F_statistic > 0.05

#denom2 = t(Y) %*% (diag(n) - )
# 
# > anova(hypo.fit, DimSelf.fit)
# Analysis of Variance Table
# 
# Model 1: DimSelf ~ DimEmotion
# Model 2: DimSelf ~ DimEmotion + DimBody
# Res.Df     RSS Df Sum of Sq      F Pr(>F)
# 1     74 11271.4                           
# 2     59  8362.3 15    2909.1 1.3684 0.1935


#######################
#########TASK15########
#######################

#a)
race_task15 = read.table("R-Lab-Datasets/Races.dat", header = TRUE)

timeW.fit = lm(timeW ~ climb + distance, data = race_task15)

#b)

timeW.fit2 = lm(timeW ~ distance, data = race_task15)
# 
# Analysis of Variance Table
# 
# Model 1: timeW ~ distance
# Model 2: timeW ~ climb + distance
# Res.Df   RSS Df Sum of Sq     F    Pr(>F)    
# 1     66 30686                                 
# 2     65 12675  1     18011 92.36 4.223e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# reject the null hypothesis that climb is not sigfinicant

distance = race_task15$distance
climb = race_task15$climb
ones_task15 = rep(1, nrow(race_task15))

mens_dat = cbind(ones_task15, climb, distance)

predicted_timeM = mens_dat %*% timeW.fit$coefficients
# which(race_task15["timeW"] == 490.05)
# [1] 41
# > predicted_timeM[41]
# [1] 456.1487
# 

# #d)
# > cor(race_task15$timeM, race_task15$timeW)
# [1] 0.9958732

cor(Races$timeW,Races$timeM)
# we have strong positive correlation 
# this indicates that in a race where the males need more time, the females will also need more time

#e)
fit.timeMW = lm(timeM ~ -1 + timeW, data = race_task15)

#######################
#########TASK16########
#######################

florida_task16 = read.table("R-Lab-Datasets/Florida.dat", header = TRUE)

urban.fit = lm(Crime ~ HS, data = florida_task16)
urban.fit2 = lm(Crime ~ Urban + HS, data = florida_task16)

# Adjusting for urbanization, the effect of education changes sign: 
#the crime rate tends to decrease as education increases.
# Hence, we have a positive marginal correlation when we ignore urbanization 
# This phenomenon of association reversal between 
#marginal and conditional associations is called "simpsons paradox"

#######################
#########TASK17########
#######################

UN_task17 = read.table("R-Lab-Datasets/UN.dat", header = TRUE)

Internet.fit = lm(Internet ~
                    GDP+
                    HDI+
                    GII+
                    Fertility+
                    CO2+
                    Homicide+
                    Prison, data = UN_task17)

Internet.GDP.fit = lm(Internet ~ GDP,data = UN_task17)

# the p-value of GDP is essentially 0 when it is the sole explanatory variable
# when we add the other variables, the SE of the GDP effect increases 
#from 0.1217 to 0.290680
# the p-value increases to 0.13856  ( > 0.05) when we add the other variables 
#to the model
# the dramatic change in the SE for GDP and the 
#lack of statistical significance for the conditional effects is 
#due to the high correlation 

# because of the multicollinearity, 
#we can attain nearly as large and R^2 value in predicting 
#the response with a reduced set of explanatory variables 
# the fact that the effect of GDP is so different 
#in the multiple regression model compared with the bivariate model is 
#caused by the multicollinearity, 
# meaning that GDP "overlaps" considerably with other explanatory variables. 
#Hence, the effects on the multiple regression model are not significant even 
# if the effect is highly significant marginally

#######################
#########TASK18########
#######################
solar_task18 = read.table("R-Lab-Datasets/solar_task10.csv", header = TRUE, sep = ",")

solar_task18$batch = as.factor(solar_task18$batch)

#b) boxplot for Pmax for each batch
ggplot(solar_task18, aes(x = batch, y =  Pmax, fill=(batch), group = batch)) + geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)
#or this
#solar_boxplot = boxplot(solar_task18$Pmax ~ solar_task18$batch)

#anova of the Pmax corresponding for each batch
solar_aov = aov(solar_task18$Pmax ~ solar_task18$batch)

#levene test 
levene.test.solar = leveneTest(solar_task18$Pmax ~ solar_task18$batch)

#plot the 4 
# fitted vs residuals
# fitted vs standard residuals
# normal QQ
# residual vs leverage
par(mfrow = c(2,2))

plot(solar_aov)

par(mfrow = c(1,1))

#shapiro test
shapiro.test(residuals(solar_aov))

#Tukey test and its plot
plot(TukeyHSD(solar_aov, conf.level=0.9))

#######################
#########TASK19########
#######################

#a) load toothgrowth data
tooth.data = ToothGrowth

#to factorize the dose
tooth.data$dose = as.factor(tooth.data$dose)

#b) 
par(mfrow = c(1,1))
box_tooth_len_dose = boxplot(tooth.data$len ~ tooth.data$dose * tooth.data$supp)

#c) fit a model for len based on supp and dose

len.fit = lm(len ~ dose * supp, data = tooth.data)

#d)
par(mfrow = c(2,2))
plot(len.fit)

par(mfrow = c(1,1))
aov.len = aov(len.fit)

#f) pairwise t test and fit another model by 
# excluding less effective parameter
new.len.fit = lm(len ~ dose + supp, data = tooth.data)
pairwise.t.test(tooth.data$len, tooth.data$dose, p.adjust.method = "bonferroni")

#######################
#########TASK20########
#######################

my.lm = function(formula, data){
  
  Y = data[,as.character(formula[[2]])]
  X = model.matrix(formula, data = data)
  
  parameter.name = colnames(data)
  model.with.intercep=any(colnames(X)=="(Intercept)")
  X=matrix(X,ncol=ncol(X))
  p.columns=ncol(X)
  n.observations=nrow(X)
  degrees.of.freedom=n.observations-p.columns
  
  qr.decomp.X=qr(X)
  beta.hat=qr.solve(qr.decomp.X,y)
  beta.hat=matrix(beta.hat)
  mu.hat=X%*%beta.hat
  residuals=y-mu.hat
  SSE=sum((residuals)**2)
  y.star=ifelse(model.with.intercep,mean(y),0)
  TSS=sum((y-y.star)**2)
  sigma.hat.2=SSE/degrees.of.freedom
  R.2=(TSS-SSE)/TSS
  beta.hat.cov=sigma.hat.2 * chol2inv(qr.decomp.X$qr)
  
  rownames(beta.hat)=param.names
  rownames(beta.hat.cov)=param.names
  colnames(beta.hat.cov)=param.names
  
  erg=list(beta.hat=beta.hat,beta.hat.cov=beta.hat.cov,R.2=R.2,sigma.hat.2=sigma.hat.2)
  
}

#######################
#########TASK21########
#######################

set.seed(2020)

#a)
unif.sample = runif(50, 0, 40)
#b)
norm.sample = rnorm(50, 15, 10)
#c)
exp.sample = (rexp(50, 1)-1)*5
#d)
beta1 = 35
beta2 = 0.5
beta3 = -0.1

betas = c(beta1, beta2, beta3)

mu = beta1 + unif.sample*beta2 + norm.sample*beta3
Y = mu + exp.sample

#e)
pseudo_fit = lm(Y ~ unif.sample + norm.sample)

beta.hat = pseudo_fit$coefficients

#f)
diff = sum((beta.hat - betas)^2)

#######################
#########TASK22########
#######################

#a) i)
# define the exponential family function
my.exp.fam = function(a, b, my.c, pi){
  
  return(my.func = function(y, mu){
    
    return( (exp(y*mu - b(mu))/(a(pi)) + my.c(y, pi)))
    
  })
  
}

#a) ii
# define the EXP family by using T, h, and B
my.exp.fam2 = function(my.T, h, B, eth){
  
  return(my.func2 = function(y, mu){
    
    return(h(y, mu)*exp(sum(eth(mu) * my.T(y)) - B(mu)))
    
  })
  
}
# define the a function
my.a = function(pi){
  
  if(pi >= 0){
    
    return(pi)
    
  }else{
    
    warning("it should be bigger than 0!")
    
  }
  
  
}
# define the b function
my.b = function(theta){
  
  if(theta < 0){
    
    return(-log(1-exp(theta)))
    
  }else{
    
    warning("this function expects to have theta less than 0")
    
  }
  
}
# define the c function
my.c = function(y, pi){
  
  return(0)
  
}

new.expfam = my.exp.fam(my.a, my.b, my.c, 1)
# our target area to estimate the prob
x = 0:6
# theta
theta = -0.8
# run the function
new.expfam(x, theta)

# generate a matrix
comb.table = matrix(0, 2, 7)
# set the column names with 0,1,2,...,6
colnames(comb.table) = x
# set the row names with own pdf and real
rownames(comb.table) = c("expfam", "dgeom")
# set column values with the densitys
comb.table[1,] = new.expfam(x, theta)
comb.table[2,] = dgeom(x, 1-exp(theta))
# barplot
barplot(comb.table, 
        beside = TRUE, 
        col = c("blue", "red"),
        ylab = "probablity",
        xlab = "value",
        legend.text = TRUE,
        args.legend = list(x="topright"))

n = 200

# we now get the 200 random sample
# and estimate the density directly
# retrieve the sample
geom_sample = table(rgeom(200, 1-exp(theta)))
# filter the unique values
observed.vals = as.numeric(names(geom_sample))

plot.vals = 0:max(observed.vals)
# generate the combination table to plot
# set the size of table
comb.table = matrix(0, 2, length(plot.vals))
# set the columns
colnames(comb.table) = plot.vals
# set the row names
rownames(comb.table) = c("rgeom", "own geom")
# assign the density values into the matrix
comb.table[1, observed.vals + 1] = geom_sample/200
comb.table[2, ] = new.expfam(plot.vals, theta)
# plot'em
barplot(comb.table, 
        beside = TRUE, 
        col = c("blue", "red"),
        ylab = "probablity",
        xlab = "value",
        legend.text = TRUE,
        args.legend = list(x="topright"))

# Task 24

# (a)
n=10
beta=2
alpha_true=2
y=rgamma(n, beta, alpha_true)

newton_increment<-function(alpha,x,beta=2){
  n <- length(x)
  score <- n*beta/alpha - sum(x)
  hessian <- (-n*beta/alpha^2)
  return(score/hessian)
}

newton_raphson<-function(x,alpha_0,epsilon=1e-8,delta=100){
  rep=TRUE #should algorithm should be repeated in next step?
  alpha_cur=alpha_0 #current value for alpha
  T.iteration=0 #number of iterations T
  while(rep){ #while rep==TRUE
    alpha_next=alpha_cur-newton_increment(alpha_cur,x) #newton step
    if(abs(alpha_next-alpha_cur) < epsilon*(1+abs(alpha_cur))){ #stopping criterion
      rep=FALSE
      conv=TRUE
    }
    if(delta*abs(alpha_cur) < abs(alpha_next)){ #criterion to check if algorithm diverges
      rep=FALSE
      conv=FALSE
    }
    alpha_cur=alpha_next
    T.iteration=T.iteration+1
  }
  return(list(convergence=conv,alpha=alpha_cur,iterations=T.iteration))
}
print(newton_raphson(y,2))

#(b)
for(n in c(10,100,500,1000)){
  alpha=rep(0, 1000)
  conv_ind = rep(FALSE,1000)
  for(i in 1:1000){
    y=rgamma(n, beta, alpha_true)
    result_it_i = newton_raphson(y,2)
    conv_ind[i] = result_it_i$convergence
    alpha[i]= result_it_i$alpha
  }
  # select only alpha estimates for which Newton-Raphson converged
  alpha = alpha[conv_ind] #take the values for alpha which were estimated
  J=n*beta/alpha_true^2 #fisher information 
  cat(mean(alpha),"\t",var(alpha),"\t", J^(-1),"\n") #mean of alpha, sample variance, inverse Fisher information
  alpha_norm=(alpha-mean(alpha))/sd(alpha) #standardize 
  hist(alpha_norm, freq=FALSE)
  curve(dnorm,col="red",add=TRUE) #see theorem ||.2.22, approximate normal distribution
}

#########TASK25########
# a) load data and factorize bin1
windmil.task25 = read.table("R-Lab-Datasets/Windmill.dat", header = TRUE, sep = " ")
windmil.task25$bin1 = as.factor(windmil.task25$bin1)
# b) divide the training and testing data randomly
set.seed(2020)
len.windmill = nrow(windmil.task25)
training.id = which(rbinom(len.windmill, 1, 0.666) == 1)
training.windmill = windmil.task25[training.id, ]
test.windmill = windmil.task25[-training.id, ]
# c) Fit the model  
#Cspd ∼ Spd1 ∗ Spd1Lag1 + Spd2 ∗ Spd2Lag1 + Spd3 ∗ Spd3Lag1 + Spd4 ∗ Spd4Lag1+
# Spd1sin1 + Spd1cos1 + bin1 + Dir1.

complex.formula1 = CSpd ~ Spd1 * Spd1Lag1 + Spd2 * Spd2Lag1 + Spd3 * Spd3Lag1 + Spd4 * 
  Spd4Lag1+Spd1sin1 + Spd1cos1 + bin1 + Dir1

fit1 = lm(CSpd ~ Spd1 * Spd1Lag1 + Spd2 * Spd2Lag1 + Spd3 * Spd3Lag1 + Spd4 * 
            Spd4Lag1+Spd1sin1 + Spd1cos1 + bin1 + Dir1, 
            data = training.windmill)
# > AIC(fit1)
# [1] 3307.401
# > BIC(fit1)
# [1] 3455.71

fit2 = lm(CSpd ~ Spd1 + Spd1Lag1 + Spd2 + Spd2Lag1 + Spd3 + Spd3Lag1 + Spd4 + 
            Spd4Lag1+Spd1sin1 + Spd1cos1 + bin1 + Dir1, 
          data = training.windmill)
# > AIC(fit2)
# [1] 3310.718
# > BIC(fit2)
# [1] 3440.488

# find the best fits using AIC
best.fit = step(fit1, direction = "backward")
# null model can be used to find the best models
lm.pseudo = lm(CSpd ~ 1, data = training.windmill)
best.fit.1.AIC1 = step(lm.pseudo,complex.formula1, direction = "forward")
best.fit.1.AIC2 = step(lm.pseudo,complex.formula1, direction = "both")

# find the best fits using BIC
best.fit = step(fit1, direction = "backward", k = log(n)) 
# null model can be used to find the best models
lm.pseudo = lm(CSpd ~ 1, data = training.windmill)
best.fit.1.BIC1 = step(lm.pseudo,complex.formula1, direction = "forward", k = log(n))
best.fit.1.BIC2 = step(lm.pseudo,complex.formula1, direction = "both", k = log(n))

#f) PRESS
predicted.fit1 = predict(best.fit.1.AIC1, newdata = test.windmill)

PRESS = sum((test.windmill$CSpd - predicted.fit1)^2)

#########TASK27########
#a) load data
transport.task27 = read.csv("R-Lab-Datasets/transportation.csv", header = TRUE, sep = ";")
# boxplot
ggplot(transport.task27, aes(as.factor(transport), time, color = transport)) + geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)
# b)fit the logistic regression
transport.fit = glm(transport ~ time, data = transport.task27, family = "binomial")
#c) fit binary regression using probit
probit.fit = glm(transport ~ time, data = transport.task27, family = binomial(link = "probit"))
# fit binary regression using cloglog
cloglog.fit = glm(transport ~ time, data = transport.task27, family = binomial(link = "cloglog"))

plot(sort(transport.task27$time), sort(fitted(transport.fit)), type="l", col = "green")
lines(sort(transport.task27$time), sort(fitted(probit.fit)), col = "blue")
lines(sort(transport.task27$time), sort(fitted(cloglog.fit)), col = "red")

# predicted probabilities for different link functions
prob.fit = function(model,x,link="logit"){
  pred.lin = model$coefficients[1] + model$coefficients[2] * x
  if(link=="logit"){
    return(exp(pred.lin)/(1 + exp(pred.lin)) )
  }
  if(link=="probit"){
    return(pnorm(pred.lin))
  }
  if(link=="cloglog"){
    return(1-exp(-exp(pred.lin)))
  }
}

# (d)
# divide the data set
transportation.f = transport.task27[transport.task27$sex==1,]
transportation.m = transport.task27[transport.task27$sex==2,]
# fit the model
transp.glm1.f = glm(transport ~ time, family = binomial, data=transportation.f)
transp.glm1.m = glm(transport ~ time, family = binomial, data=transportation.m)
# predicted probabilities
x = seq(-15,60,0.01)
#use written function above for the predictions
prob.pred.f = prob.fit(transp.glm1.f,x) 
prob.pred.m = prob.fit(transp.glm1.m,x)
#write predictions in data frame
prob.pred=data.frame(x=x,prob.pred.f=prob.pred.f, prob.pred.m=prob.pred.m) 

col=c("woman"="red", "man"="blue")
ggplot(prob.pred, aes(x=x))+geom_line(aes(y=prob.pred.f, color="woman"))+
  geom_line(aes(y=prob.pred.m, color="man")) +
  labs(x="time", y="prob",color="Legend")+scale_color_manual(values = col)

# e)
transportation$sex=factor(transportation$sex)
levels(transportation$sex) = c("woman","man")
transp.glm2 = glm(transport ~ time + sex,data=transportation,family = binomial) 
#model where response depends on time AND sex
predicted.transport = ifelse(predict(transp.glm2) > 0.5, 1, 0)
#length(predicted.transport == transportation$transport)

# confusion matrix
confusion.mat = table(transportation$transport ,predicted.transport)
# get the accuracy
sum(diag(confusion.mat))/sum(confusion.mat)

# f)
transp.glm3 = glm(transport ~ time * sex,data=transportation,family = binomial) 
# different ways to compute the p value
1-pchisq(deviance(transp.glm2) - deviance(transp.glm3), 
         df= df.residual(transp.glm2) - df.residual(transp.glm3))
anova(transp.glm3,test="Chisq")
anova(transp.glm2, transp.glm3, test="Chisq")

#########TASK29########
FieldGoal = read.csv2("R-Lab-Datasets/FieldGoal.csv", header = TRUE, sep = ";")
# b) fit the logistic regression model
FG.fit = glm(Good. ~ Dist, data = FieldGoal, family = "binomial")
# c) plot

pred.binom = function(x, coef){
  
  bx = coef[1] + x*coef[2]

  return(exp(bx)/(1 + exp(bx)))
    
}
Good.pred = pred.binom(FieldGoal$Dist, FG.fit$coefficients)
Good.pred = ifelse(Good.pred > 0.5, 1, 0)
field.range = min(FieldGoal$Dist):max(FieldGoal$Dist)
plot(field.range, pred.binom(field.range, FG.fit$coefficients), 
     type = 'l', col = "blue")
# generate the confusion amtrix
confusion.mat = table(FieldGoal$Good. , Good.pred)
# accuracy
sum(diag(confusion.mat))/sum(confusion.mat)

#d) significance test with wald statistics
summary(FG.fit)
# Wald's test statistic
z.squared=(FG.fit$coefficients[2]/summary(FG.fit)$coefficients[2,2])^2
p.val=1-pchisq(z.squared, df=1)
p.val

# (e)
CI=confint(FG.fit, level=0.9) #profile likelihoood CI 
# probability of good kick with 19,39 and 64 yards to the goal
CI.prob=function(x){
  exp(CI[1, ]+CI[2, ]*x)/(1 + exp(CI[1, ]+CI[2,]*x))
}
CI.prob(19)
CI.prob(39)
CI.prob(64)

# f)

model.select = 

# (g) ROC Curves
roc.curve1=roc(Good.~fitted(model.1), data=FG)
roc.curve2=roc(Good.~fitted(model.select), data=FG)
plot.roc(roc.curve1, legacy.axes=TRUE)
plot.roc(roc.curve2, legacy.axes=TRUE, add=TRUE, col="blue")
auc(roc.curve1)
auc(roc.curve2)

#########TASK30########
library(VGAM)
library(vcdExtra)

# (a) generate the contingency table
Hoyt.tab=ftable(Hoyt, col.vars="Status")
Hoyt.tab
Rank=factor(c(rep("Low", 14), rep("Middle", 14), rep("High", 14)))
Occ=factor(rep(c(rep(1, 2), rep(2, 2), rep(3, 2), rep(4, 2), rep(5,2), rep(6,2), rep(7,2)), 3))
Sex=factor(rep(c("Female", "Male"), 21))

#b) fit the baseline category
formula.blc = Hoyt.tab~ Rank*Occ + Rank*Sex + Occ*Sex
vglm.hoyt = vglm(formula.blc, family=multinomial)
  
# (d)
fit2=vglm(Hoyt.tab~Rank*Occ+Occ*Sex,family=multinomial)
# p-value of influence of Sex:Rank
1-pchisq(deviance(fit2)-deviance(fit1), df=df.residual(fit2)-df.residual(fit1))

# (e)
model.selectA=step4(fit1, directions="backward")
model.selectB=step4(fit1, directions="backward", k=log(sum(Hoyt.tab)))


# Task 31
library(VGAM)
library(vcdExtra)

# (a)
Vietnam.tab=ftable(xtabs(Freq~sex+year+response,data=Vietnam), col.vars="response") 
Vietnam.tab #flat contingency table with 4 columns
sex=factor(c(rep("Female",5), rep("Male", 5)))
year=factor(rep(1:5,2))

# (b)
# baseline category log odds
baseline.cat.log.odds<-matrix(0,10,3) #we have 10 rows in Vietnam.tab and 4 columns
for (i in 1:3){
  baseline.cat.log.odds[,i] = log(Vietnam.tab[,i]/Vietnam.tab[,4])
}
baseline.cat.log.odds=xtabs(baseline.cat.log.odds~sex+year)

# cumulative log odds
cum.log.odds<-matrix(0,10,3)
cum.log.odds[,1]=log(Vietnam.tab[,1]/rowSums(Vietnam.tab[,2:4]))
cum.log.odds[,2] = log(rowSums(Vietnam.tab[,1:2])/rowSums(Vietnam.tab[,3:4]))
cum.log.odds[,3]=log(rowSums(Vietnam.tab[,1:3])/Vietnam.tab[,4])
cum.log.odds=xtabs(cum.log.odds~sex+year)

# plot of the log-odds
matplot(baseline.cat.log.odds[1, , ],
        type="p",pch=1,xlab="",
        ylab="Estimated Baseline-Category Log-Odds", 
        xaxt="n",main="Sample Baseline-Category LO for factor level female")
matplot(baseline.cat.log.odds[2, , ],
        type="p",pch=1,xlab="",
        ylab="Estimated Baseline-Category Log-Odds", 
        xaxt="n",main="Sample Baseline-Category LO for factor level male")
matplot(cum.log.odds[1, , ],
        type="p",pch=1,xlab="",
        ylab="Estimated Cumulative Log-Odds", 
        xaxt="n",main="Sample Cumulative LO for factor level female")
matplot(cum.log.odds[2, , ],
        type="p",pch=1,xlab="",
        ylab="Estimated Cumulative Log-Odds", 
        xaxt="n",main="Sample Cumulative LO for factor level male")

#(c)
# cumulative logit model
fit.viet.logit=vglm(Vietnam.tab~sex+year, 
                    family = cumulative(parallel=FALSE, 
                                        link="logitlink"))

# (d)
# cumulative probit model
fit.viet.probit=vglm(Vietnam.tab~sex+year, 
                     family = cumulative(parallel=TRUE, 
                                         link="probitlink"))

# (e)
# cumulative probit model
# Chi-squared 
#need for pearsons X^2
mu=c(predict(fit.viet.probit,type="response")*rowSums(Vietnam.tab))  
X.square.probit<-sum((c(Vietnam.tab)-mu)^2/mu) #pearsons X^2
# p-values
1-pchisq(deviance(fit.viet.probit),df.residual(fit.viet.probit)) 
1-pchisq(X.square.probit,df.residual(fit.viet.probit))

# cumulative logit model
# Chi-squared
mu=c(predict(fit.viet.logit,type="response")*rowSums(Vietnam.tab))
X.square.logit<-sum((c(Vietnam.tab)-mu)^2/mu) 
# p-values
1-pchisq(deviance(fit.viet.logit),df.residual(fit.viet.logit)) 
1-pchisq(X.square.logit,df.residual(fit.viet.logit))


#############
#
# Task 32
#
#############
library(vcd)
library(vcdExtra)

# (a)
GSS
tab.party=xtabs(count~sex+party, data=GSS)
# Add margins
party.margins = addmargins(tab.party)
party.margins 

# (b)
# model of independence
glm.ind = glm(count ~ sex + party, family=poisson, data=GSS)
glm.ind
summary(glm.ind)

# asymptotic p-value
p.val = 1 - pchisq(glm.ind$deviance,df=glm.ind$df.residual)
p.val

# Chi-squared Test
chisq.test(tab.party)

# saturated model
glm.sat = glm(count ~ sex * party, family=poisson, data=GSS)
glm.sat
summary(glm.sat)

# (c)
# Pearsonian residuals and corresponding standardized residuals
res.p = residuals(glm.ind, type="pearson") 
stdres.p = rstandard(glm.ind, type="pearson") 
xtabs(res.p ~ sex + party, data=GSS)
stdres.p.tab = xtabs(stdres.p ~ sex + party, data=GSS)
stdres.p.tab

# Comparison with the statistic of the Chi-squared Test in (b)
sum(res.p^2)
chisq.test(tab.party)$statistic

# deviance residuals and corresponding standardized residuals
res.d = residuals(glm.ind, type="deviance")
stdres.d = rstandard(glm.ind, type="deviance")
xtabs(res.d ~ sex + party, data=GSS)
stdres.d.tab = xtabs(stdres.d ~ sex + party, data=GSS)
stdres.d.tab

# Comparison with the deviance of the log linear model
summary(glm.ind)
sum(res.d^2)

# own implementation
2 * (sum(GSS$count * log(GSS$count/glm.ind$fitted.values))) 

# (d)
# mosaic-plot
# standardized Pearsonian residuals
mosaic(tab.party,gp=shading_Friendly,residuals=stdres.p.tab,
       residuals_type="Std\nresiduals",labeling=labeling_residuals)
# standardized deviance residuals
mosaic(tab.party,gp=shading_Friendly,residuals=stdres.d.tab,
       residuals_type="Std\nresiduals",labeling=labeling_residuals)


  
  
  
  