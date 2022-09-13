library(ggplot2)
library(mvtnorm)
library(MASS)
library(dplyr)
library(tidyr)
library(gridExtra)
library(car)
library(datasets)

########################################
#################Task22#################
########################################

# a.i) exponential family
myEDF <- function(a, b, c, ph){

    return( inner.EDF <- function(y, nu, ph){
      
      return(exp((y*nu - b(nu))/a(ph) + c(y, ph)))
      
    } )

}

# a.ii) k-parameter exponential family
k.param.fam <- function(my.T, h, B){
  
  return( inner.k.param.fam <- function(y, nu){
    
    return(h(y)*exp(c(nu %*% my.T(y)) - B(nu)))
    
  })
  
}

# b)
# a: phi->phi
# b: nu -> -ln(1-exp(nu))
# c: (y, phi) -> 0

my.a <- function(ph){
  
  return(ph)
  
}

my.b <- function(nu){
  
  return(-log(1-exp(nu)))
  
}

my.c <- function(y, ph){
  
  return(0)
  
}

# Task 22 b)
new.EDF <- myEDF(my.a, my.b, my.c, 1)

a.sequence <- seq(0, 200, by=1)

new.EDF.a.sequence <- new.EDF(a.sequence, -0.8, 1)

plot(new.EDF.a.sequence)

x = 0:6
comb.table = matrix(0, 2, 7)
colnames(comb.table) = x
rownames(comb.table) = c("d.exp.dip.fam", "dgeom")

comb.table[1,] = new.EDF(x, -0.8, 1)
comb.table[2,] = dgeom(x, 1-exp(-0.8))

# to generate the barplot of the both 
# exponential dispersion family and
# the original dgeom
barplot(comb.table, 
        beside = TRUE, 
        col = c("blue", "red"), 
        ylab = "probability",
        xlab = "value",
        legend.text = TRUE,
        args.legend = list(x = "topright"))

n = 200
z = rgeom(n, 1-exp(-0.8))
freq.table = table(z)
observed.vals = as.numeric(names(freq.table))

plot.vals=0:max(observed.vals)

comb.table = matrix(0, 2, length(plot.vals))
colnames(comb.table) = plot.vals
rownames(comb.table) = c("Sample", "exp.family")
comb.table[1, observed.vals + 1] = freq.table/n
comb.table[2,] = new.EDF(plot.vals, -0.8, 1)

barplot(comb.table, 
        beside = TRUE, 
        col = c("blue", "red"),
        ylab = "relative frequency / probablity",
        xlab = "value",
        legend.text = TRUE,
        args.legend = list(x = "topright"))

# Task 22 b) was geometric distribution
# so we compare it with the real distribution
#y_dgeom <- dgeom(x_dgeom, prob = 0.5)    
y_dgeom <- dgeom(a.sequence, prob = (1-exp(-0.8)))

plot(y_dgeom)

my.b.2 <- function(nu){
  
  return(-log((-nu)^3 / 2))
  
}

my.c.2 <- function(y, ph){
  
  return(log(y^2))
  
}

# results in gamma distribution with alpha = 2 and beta = 3
new.EDF.2 <- myEDF(my.a, my.b.2, my.c.2, ph)

# we draw again the gamma distribution in the interval 0 to 6
#new.EDF.gamma <- new.EDF.2()

new.EDF.2.gamma <- new.EDF.2(x, -2 ,1)

real.gamma <- dgamma(x, 3, 2)

comb.table.2 = matrix(0, 2, 7)
colnames(comb.table.2) = x
rownames(comb.table.2) = c("d.exp.dip.fam.2", "dgamma")

comb.table.2[1,] = new.EDF.2.gamma
comb.table.2[2,] = real.gamma

curve(new.EDF.2(x, -2 ,1),xlim=c(0,5))
curve(dgamma(x, 3, 2),col="red",add=TRUE)

# n=200
# z=rgamma(n,3,2)
# hist(z,freq=FALSE,ylim=c(0,.8)) #freq=FALSE ensures that we get the probability densities
# curve(dgamma.own(x,theta),col="red",add=TRUE)

z.2 = rgamma(200, 3, 2)
hist(z.2, freq = FALSE, ylim = c(0, .8))
curve(new.EDF.2(x, -2 ,1), col = "red", add = TRUE)

# d)
my.T <- function(y){
  
  return(matrix(c(log(y), y), byrow = TRUE, nrow = 2))
  
}

my.B <- function(nu){
  
  return(log(gamma(nu[1] + 1)) - (nu[1] + 1)*log(-nu[2]))
  
}

my.h <- function(y){
  
  ifelse(x >= 0, 1, 0)
  
}

theta.natural = c(2, -2)

my.EDF.3 <- k.param.fam(my.T, my.h, my.B)

curve(new.EDF.2(x, -2 ,1),xlim=c(0,5))
curve(my.EDF.3(x, theta.natural), col = "red", add = TRUE)


########################################
#################Task24#################
########################################

#a) implement an R-function Newton-Raphson algorithm
# to compute the maximum likelihood estimator of alpha
# for an iid sample 


# we just need to guess the alpha
# because the extimate of beta results in
# mean of x over estimated alpha
a0 = -1

gamma.samples = rgamma(20, 2, 2)

newton.raphson.param <- function(y, alpha, beta = 2){
  
  n = length(y)
  hessian = (-n*beta/alpha^2)
  score = n*beta/alpha - sum(y)
  
  return(score/hessian)
  
}
 
newton_increment<-function(alpha,x,beta=2){
   n <- length(x)
   score <- n*beta/alpha - sum(x)
   hessian <- (-n*beta/alpha^2)
   return(score/hessian)
}

my.newton.raphson <- function(y, alpha, my.eps=1e-8, delta = 100){
  
  rep = TRUE
  alpha.next = alpha
  
  while(rep){
    
    alpha.next = alpha - newton.raphson.param(y, alpha)
    
    if(abs(alpha.next - alpha) < my.eps*(1+abs(alpha))){
      
      rep = FALSE
      conv = TRUE
    
    }
    if(delta*abs(alpha) < abs(alpha.next)){
      
      rep = FALSE
      conv = FALSE
      
    }
    
  }
  
  return(alpha.next)
  
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


########################################
#################Task25#################
########################################

# a) read windmil data
windmil = read.table("Windmill.dat", header=TRUE)

# b) divide the dataset s.t.
# 2/3 rows belong to the training dataset
# 
# dt = sort(sample(nrow(data), nrow(data)*.7))
# train<-data[dt,]
# test<-data[-dt,]

windmil$bin1 = as.factor(windmil$bin1)

set.seed(518518)

CSpd.len = length(windmil$CSpd)

CSpd.binoms = rbinom(CSpd.len, size = 1, prob = 0.67)

windmil.train = windmil[as.logical(CSpd.binoms), ]

windmil.test = windmil[as.logical(1-CSpd.binoms),]

# c) fit the data on the linear model 
# Cspd ∼ Spd1 ∗ Spd1Lag1 + Spd2 ∗ Spd2Lag1 + Spd3 ∗ Spd3Lag1 + Spd4 ∗ Spd4Lag1+
#Spd1sin1 + Spd1cos1 + bin1 + Dir1.
# and compute AIC and BIC

windmil.fit = lm(CSpd ~ Spd1*Spd1Lag1 
                 + Spd2*Spd2Lag1 
                 + Spd3 * Spd3Lag1 
                 + Spd4*Spd4Lag1 
                 + Spd1sin1 
                 + Spd1cos1 
                 + bin1 
                 + Dir1, data = windmil.train)

windmil.fit.AIC = AIC(windmil.fit)
windmil.fit.BIC = BIC(windmil.fit)
#
# broom::glance(windmil.fit)
# # A tibble: 1 x 12
# r.squared adj.r.squared sigma statistic   p.value    df logLik   AIC   BIC deviance df.residual  nobs
# <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
#   1     0.693         0.688  2.10      155. 1.41e-267    16 -2400. 4836. 4926.    4848.        1097  1114
# 

# d) fit the linear model and compute the AIC and BIC
# Cspd ∼ Spd1 + Spd1Lag1 + Spd2 + Spd2Lag1 + Spd3 + Spd3Lag1 + Spd4 + Spd4Lag1+
#   Spd1sin1 + Spd1cos1 + bin1 + Dir1.

# do this backward, forward, and both
windmil.fit.2 <- lm(CSpd ~ Spd1 + Spd1Lag1 + Spd2 + Spd2Lag1 + Spd3 + Spd3Lag1 + Spd4 + Spd4Lag1+Spd1sin1 + Spd1cos1 + bin1 + Dir1, data = windmil.train)

windmil.fit.2.AIC = AIC(windmil.fit.2)
windmil.fit.2.BIC = BIC(windmil.fit.2)

# e) i. step AIC

# we need null model to use fit forward
null.model = lm(CSpd ~ 1, data = windmil.train)
step.windmil.fit.AIC = step(windmil.fit)
step.windmil.fit.AIC.backward = step(windmil.fit, direction = "backward")
step.windmil.fit.AIC.forward = step(null.model,CSpd ~ Spd1*Spd1Lag1 
                                    + Spd2*Spd2Lag1 
                                    + Spd3 * Spd3Lag1 
                                    + Spd4*Spd4Lag1 
                                    + Spd1sin1 
                                    + Spd1cos1 
                                    + bin1 
                                    + Dir1, direction = "forward")
step.windmil.fit.AIC.both = step(null.model, CSpd ~ Spd1*Spd1Lag1 
                                 + Spd2*Spd2Lag1 
                                 + Spd3 * Spd3Lag1 
                                 + Spd4*Spd4Lag1 
                                 + Spd1sin1 
                                 + Spd1cos1 
                                 + bin1 
                                 + Dir1, direction = "both")


step.windmil.fit.BIC.back = step(windmil.fit, direction = "backward", k = log(n))
step.windmil.fit.BIC.for = step(null.model,CSpd ~ Spd1*Spd1Lag1 
                                + Spd2*Spd2Lag1 
                                + Spd3 * Spd3Lag1 
                                + Spd4*Spd4Lag1 
                                + Spd1sin1 
                                + Spd1cos1 
                                + bin1 
                                + Dir1, direction = "forward", k = log(n))
step.windmil.fit.BIC.both = step(null.model, CSpd ~ Spd1*Spd1Lag1 
                                 + Spd2*Spd2Lag1 
                                 + Spd3 * Spd3Lag1 
                                 + Spd4*Spd4Lag1 
                                 + Spd1sin1 
                                 + Spd1cos1 
                                 + bin1 
                                 + Dir1, direction = "both", k = log(n))

# f.i) PRESS
# e).i
# best model based on AIC
# CSpd ~ Spd1 + Spd1Lag1 + Spd2 + Spd2Lag1 + Spd3 + Spd3Lag1 + 
#   Spd4 + Spd4Lag1 + bin1 + Spd2:Spd2Lag1 + Spd4:Spd4Lag1

#sum((data.wind.testing$CSpd-predict(fit2.A, newdata=data.wind.testing))^2)
sum((windmil.text$CSpd - predict(step.windmil.fit.AIC.backward, newdata = windmil.text))^2)
sum((windmil.text$CSpd - predict(step.windmil.fit.AIC.forward, newdata = windmil.text))^2)
sum((windmil.text$CSpd - predict(step.windmil.fit.AIC.both, newdata = windmil.text))^2)

sum((windmil.text$CSpd - predict(step.windmil.fit.BIC.back, newdata = windmil.text))^2)
sum((windmil.text$CSpd - predict(step.windmil.fit.BIC.for, newdata = windmil.text))^2)
sum((windmil.text$CSpd - predict(step.windmil.fit.BIC.both, newdata = windmil.text))^2)


########################################
#################Task26#################
########################################

# get the 25 sample X is uniform distributed between 20 and 80

x.unif = runif(25, min = 20, max = 80)

beta.term = exp(-2 + 0.08*x.unif)

y = rgamma(25, shape = beta.term, rate = 1)

glm.task26 = glm(y ~ x.unif, family=Gamma(link = "log"))

fitted.vals.glm = glm.task26$fitted.values

e = y - fitted.vals.glm

########################################
#################Task27#################
########################################

# a) read the transportaion.csv
transport.csv = read.csv2("transportation.csv", stringsAsFactors=TRUE)

transport.csv = transport.csv[order(transport.csv$time),]
# Create two boxplots of the attribute time, one for each mean of transport.
transport.time.boxplot <- ggplot(mapping = 
                                   aes(x = as.factor(transport.csv$transport),
                                       y = transport.csv$time,
                                       fill=as.factor(transport.csv$transport)),
                                    color = transport.csv$transport,
                                    ) + geom_boxplot() + theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size = 1)) + 
                                  labs( x = "Transport", y = "Time", fill = "Transport") + 
                                  scale_fill_manual(values = c("lightseagreen", "lightsalmon1"))

# b) Fit a logistic regression model, where the attribute transport depends only on time.
logit.fit.transport.time = glm(transport ~ time, data = transport.csv, family = "binomial")

# c).i fit binary regression model from b) using link = probit
logit.fit.transport.time.probit = glm(transport ~ time, data = transport.csv, family = binomial(link = "probit"))

# c).ii fit binary regression model from b) using link = cloglog
logit.fit.transport.time.cloglog = glm(transport ~ time, data = transport.csv, family = binomial(link = "cloglog"))

# plot the line of b) family = binomial
transport.time = transport.csv$time
#sorted.time = order(transport.time)
plot(transport.time, 
     predict(logit.fit.transport.time, type = "response"), 
     type="l", 
     col="darkseagreen3", 
     lwd=2, 
     xlab="time", 
     ylab="transport")

lines(transport.time, 
      predict(logit.fit.transport.time.probit, type = "response"), 
      col="deeppink3", 
      lwd=2)

#lightslateblue
#royalblue2

lines(transport.time, 
      predict(logit.fit.transport.time.cloglog, type = "response"), 
      col="royalblue2", 
      lwd=2)

# d) Fit two logistic regression models as in (b), 
# one for each sex. Plot in ggplot2 the predicted probabilities 
# for using public transport as a function of time on the same plot, 
# using different colors per gender.

# logistic regression based on sex == 1
logit.fit.transport.time.sex1 = glm(transport ~ time, data = transport.csv[c(which(transport.csv$sex == 1)),], family = "binomial")

# logistic regression based on sex == 2
logit.fit.transport.time.sex2 = glm(transport ~ time, data = transport.csv[c(which(transport.csv$sex == 2)),], family = "binomial")

#column names for the following dataframe
cols = c("Time", "Predictions")
# organize sex1, sex2 and their corresponding predicted value into each dataframe
df.trans.time.sex1 = data.frame(transport.csv[c(trans.sex1.idx),]$time, predict(logit.fit.transport.time.sex1, type = "response"))
colnames(df.trans.time.sex1) = cols
df.trans.time.sex2 = data.frame(transport.csv[c(trans.sex2.idx),]$time, predict(logit.fit.transport.time.sex2, type = "response"))
colnames(df.trans.time.sex2) = cols

#ask clemense
#ggplot that plots the predicted value of each sex == 1 and sex == 2
ggplot.transport.sex = ggplot() + 
   geom_line(data = df.trans.time.sex1, aes(x = Time, y = Predictions), color = 'royalblue2') + 
   geom_line(data = df.trans.time.sex2, aes(x = Time, y = Predictions), color = 'darkseagreen3') + 
    scale_color_manual(name = "Sex", values = c('Y1' = 'royalblue2', 'Y2' = 'darkseagreen3')) +
    xlab("Time") + 
    ylab("Transport") + labs(color = "Sex")

# another approach!?!?!??!?!?!?!?
# Issue, one line? but different X-Axis? how to handle ?

# e)
# Fit a logistic regression model, 
# where transport depends on time and sex. 
# Calculate the percentage of correct classified observations according to this model. 
# Compare to the corresponding percentage for the model in (b).

# fit a logistic regression model transport ~ time + sex
logit.fit.transport.time.sex = glm(transport ~ time + sex, data = transport.csv, family = "binomial")

correct.prob.model.e = sum((ifelse(predict(logit.fit.transport.time.sex, type = "response") >= 0.5, 1, 0) & transport.csv$transport)*1)/nrow(transport.csv)
# 0.3193277
correct.prob.model.b = sum((ifelse(predict(logit.fit.transport.time, type = "response") >= 0.5, 1, 0) & transport.csv$transport)*1)/nrow(transport.csv)
# 0.3361345

# f) glm transport depends of time, sex, and their interaction
# goal(y ~ x + z + x:z, data = ...)
# goal(y ~ x*z, data = ...)
logit.fit.transport.f = glm(transport ~ time*sex, data = transport.csv[c(which(transport.csv$sex == 1)),], family = "binomial")
# significant? 


########################################
#################Task28#################
########################################

# a)Download the file credits.csv from RWTHmoodle
# load it as a data frame into your workspace. 
# Divide randomly the data of sample size n = 1000 
# into training data (67 % of the data) 
# with 667 rows and test data (the remaining 33% of the data).

# load the credits.csv
credits.csv.task28 = read.csv2("credits_task28.csv", stringsAsFactors=TRUE, sep = ",")

set.seed(518518)

# divide randonly data

# windmil.train = windmil[as.logical(CSpd.binoms), ]
# 
# windmil.test = windmil[as.logical(1-CSpd.binoms),]

#rbinom(1000, 1, 0.67)

credits.train = credits.csv.task28[as.logical(rbinom(1000, 1, 0.67)), ]

credits.test = credits.csv.task28[as.logical(1 - rbinom(1000, 1, 0.67)), ]

# fit tthe logistic regression model based on
# account, 
# behavior, 
# savings, 
# employment, 
# rate, 
# guaran, 
# finance, 
# furthcred, 
# home, 
# job, 
# time, 
# amount, 
# age.


########################################
#################Task29#################
########################################

library(pROC)

# (a)
FG=read.csv("FieldGoal.csv", sep=";", stringsAsFactors = TRUE)

# (b)
model.1=glm(Good.~Dist, data=FG, family=binomial(link=logit))
model.1

prob.fit = function(x,model){ #computes the fitted probabilities
  exp(model$coefficients[1]+model$coefficients[2]*x)/
    (1 + exp(model$coefficients[1]+model$coefficients[2]*x))
}
x=min(FG$Dist):max(FG$Dist) #vector containing possible values from minimum to maximum value
plot(x, prob.fit(x, model.1), type = "l", xlab="Distance", ylab="prob.fit",ylim=c(0,1))

# (c)
Good.Pred=ifelse(model.1$fitted.values > 0.5, 1, 0)
FG.tab1=table(FG$Good.,Good.Pred) #classification table
sum(diag(FG.tab1))/sum(FG.tab1)

########################################
#################Task32,priority 33, 34#################
########################################

install.packages("VGAM")

library(VGAM)

library(vcdExtra)

##################################################################
#################priority 35, 36, 38, and 37######################
##################################################################

# a) load basketball
basketball = read.csv("Basketball.csv", sep=",", stringsAsFactors = TRUE)

# y1, . . . , y514 be realizations of independent random variables, where yi is representing the number of field goals
# denote with xi the number of games G of the ith player
# Assume that yi is the realization of a Poisson distributed random variable Yi ∼ P(μi) and that the GLM
# log(μi)=α+βxi, i=1,...,514,
# with unknown α, β ∈ R, holds.
# Estimate the parameters α and β using the data set and
# the R-function glm with parameter family=poisson(link=’log’). Create a scatterplot 1
# (x1, y1), . . . , (x514, y514) of G against FG.

basket.model = glm(FG ~ G, data=basketball, family=poisson(link='log'))

# basketball plot of G against FG
plot(basketball$G, 
     basketball$FG, 
     main="Basketball plot of G against FG", 
     xlab="G", ylab="FG", col = 'blue')

# Sample from Yi∗ ∼ P(exp(αˆ + βˆxi)),

# (Intercept)           G 
# 2.71031731  0.04853817 
# plan: fitted or predicted, plug-in into the poisson, and plot'em

fitted.basketball = fitted(basket.model)

pois.fitted.basketball = rpois(length(fitted.basketball), fitted.basketball)

plot(basketball$G, 
     pois.fitted.basketball, 
     main="Basketball plot of G against FG", 
     xlab="G", ylab="FG_predicted", col = 'red')

##################################################################
#################Alligators######################
##################################################################

install.packages("VGAM")

library(VGAM)

library(vcdExtra)

#alligators.dat
gators = read.table("alligators.dat", header=TRUE)

fit = vglm(formula = cbind(y2, y3, y4, y5, y1) ~ size + factor(lake), 
                                                family = multinomial,
                                                data = gators)
