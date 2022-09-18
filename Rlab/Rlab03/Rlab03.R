library(ggplot2)
library(MASS)

######
#Task9
######

# (a) Draw a random sample of size m = 30 from a B(n,p)-distribution, 
# the Binomial distribution with parameter n = 12 and p = 0.7, 
# applying the R-function rbinom.

#Number of observations you want to see
#Number of trials per observation
#probability of success for each trial

binom_sample = rbinom(30, 12, 0.7)

# (b) Construct the bar plot and 
# add the probability mass function (pmf) of the generating distribution.

binom_pmf = table(binom_sample)/30

barplot(binom_pmf)

# (c) Calculate the mean (x ̄) and the variance (s2) of your sample and 
# write them in the title of the figure. Furthermore, calculate

T_m.n.p = sqrt(30) * (mean(binom_sample) - 12*0.7)/sqrt(12*0.7*0.3)

# (d) Write a function with arguments m, n and p 
# which draws a new random sample of size
# m from a B(n,p)-distribution and returns the value of Tm,n,p.

my.t.m.n.p = function(m, n, p){
  
  binom_sample = rbinom(m, n, p)
  
  return(sqrt(m) * (mean(binom_sample) - n*p) / sqrt(n*p*(1-p)))
  
}

# (e) Apply the function from (d) 10,000 times for m ∈ {5, 30, 500}, 
# with n = 12 and p = 0.7. For each m, 
# create a histogram with 16 breaks for the returned values. 
# What do you observe?

'for(i in 1:10000){
  
  for(j in c(5, 30, 500)){
    
    
    
  }
  
}
'

#######
#Task10
#######

# (a) Download the CSV-file Solar.csv from the RWTHmoodle space of the course 
# Applied Data Analysis (Tutorial/Praktikum, no 11.04012). 
# Import the data as a data.frame object into the R workspace and 
# transform the attribute batch to type factor.

solar = read.csv2("Solar.csv", header = TRUE, sep = ";")

solar$batch = as.factor(solar$batch)

# fill na

#solar$Isc[which(is.na(solar$Isc))] = mean(solar$Isc[which(!is.na(solar$Isc))])

# (b) Create a scatterplot matrix of the attributes 
# Pmax, Imax, Umax, Isc and Uoc. 
# Differ- entiate the points by batch using colors.

pairs(solar[,2:6], col = solar$batch)

# (c) Create Box-plots for Uoc for each batch in one figure.

boxplot(solar$Uoc ~ solar$batch, xlab = "batch", ylab = "Uoc")

ggplot(solar, aes(x=batch, y=Uoc, color=batch)) +
  geom_boxplot()

# (d) For the data of Solar.csv, create an (Pmax, Isc) scatterplot. 
# Differentiate the points by
# batch using colors and add a linear regression line. 
# Compute the parameter vector

#scatterplot
plot(solar$Pmax,solar$Isc,col=c("red","blue","green","orange")[solar$batch])
#add legend
legend(x="topleft", legend = levels(solar$batch), col=c("red","blue","green","orange"), pch=1)

# (i) via Example I.4.6 and Theorem I.4.9 of the lecture,

# predict parameter term of Isc based on the Pmax

B0 = solar[which(!is.na(solar$Isc)),]

B = cbind(rep(1, nrow(B0)), B0$Pmax)
#B = solar$Pmax

beta = ginv(t(B) %*% B) %*% t(B) %*% B0$Isc

pmax.fit = lm(Isc ~ Pmax, data = solar)

B1 = cbind(rep(1, nrow(solar)), solar$Pmax)

Isc_fit1 = B1 %*% beta

abline(pmax.fit, col = "black")

na.idx = which(is.na(solar$Isc))

library(tidyverse)

Pmax = data.frame(solar$Pmax)

colnames(Pmax) = "Pmax"

predicted.Isc = predict(pmax.fit, newdata = Pmax)

solar$Isc[na.idx] = predicted.Isc[na.idx]

solar.batch1 = solar[solar$batch == "1",]

solar.batch1 = solar.batch1[which(!is.na(solar.batch1$Isc)),]

lm.batch1 = lm(Isc ~ Pmax, data = solar.batch1)

abline(lm.batch1$coefficients, col = "red")

solar.batch4 = solar[solar$batch == "4",]

solar.batch4 = solar.batch4[which(!is.na(solar.batch4$Isc)),]

lm.batch4 = lm(Isc ~ Pmax, data = solar.batch4)

abline(lm.batch4$coefficients, col = "orange")


#######
#Task11
#######

# (a) Download the CSV-file rent.csv from RWTHmoodle. 
# Import the data as a data.frame object into the R workspace.

rent = read.csv2("rent.csv", header = TRUE, sep = ";")

# (b) Create a scatterplot of the attributes rent.sqm(y-axis) and space(x-axis). 
# Add a linear regression line (you may use the function lm) to the scatterplot.

plot(rent$space, rent$rent.sqm)

lm.sqm = lm(rent.sqm ~ space, data = rent)

lines(rent$space, fitted(lm.sqm) , col = "green")

plot(1/rent$space, rent$rent.sqm)

lm2.sqm = lm(rent.sqm ~ 1/space, data = rent)

lines(rent$space, fitted(lm2.sqm) , col = "red")

#######
#Task12
#######

# (a) Download the white-space-separated file cars2.dat from RWTHmoodle. 
# Import the data as a data.frame object into the R workspace.

cars2 = read.table("cars2.dat", header = TRUE, sep = " ")

# (b) Create a scatterplot of the attributes dist (y-axis) and speed (x-axis) 
# of the cars2 data set.

plot(cars2$speed, cars2$dist, col = "blue")

# (c) Add a quadratic regression curve to the scatterplot 
# by using a linear model with the approach
# dist = a + b · speed + c · speed2

lm.dist = lm(dist ~ poly(speed, degree = 2), data = cars2)

lines(cars2$speed[order(cars2$speed)], fitted(lm.dist)[order(cars2$speed)], col = "red")

B = cbind(rep(1, nrow(cars2)), cars2$speed, (cars2$speed)^2)

Q = B %*% ginv(t(B) %*% B) %*% t(B)

B0 = cbind(rep(1, nrow(cars2)), cars2$speed)

Q0 = B0 %*% ginv(t(B0) %*% B0) %*% t(B0)

F_score = (nrow(cars2) - 3) * t(cars2$dist) %*% (Q - Q0) %*% cars2$dist/(t(cars2$dist) %*% (diag(nrow(cars2)) - Q) %*% cars2$dist)

reject = F_score > qf(1 - 0.05, 1, nrow(cars2) - 3)

# bonus
p.val = 1 - pf(F_score, 1, 47)

reject.alt = p.val < 0.05

# by using anova
lm.dist2 = lm(dist ~ poly(speed, degree = 1), data = cars2)

anova(lm.dist2, lm.dist)