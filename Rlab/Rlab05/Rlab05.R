library(ggplot2)
library(MASS)

#######
#Task18
#######

#(a) Load the .RData file of the pre-processed data of Solar (Task 10) into the R workspace.

solar = read.table("solar_task10.RData", header = TRUE, sep = ",")

solar$batch = as.factor(solar$batch)
# (b) Create four boxplots for the attribute Pmax, one for each batch.
ggplot(solar, aes(x=batch, y=Pmax, color=batch)) +
  geom_boxplot()

# (c) Carry out an analysis of variance for the attribute Pmax 
# regarding the factor batch.

solar.aov = aov(solar$Pmax ~ solar$batch)

'> summary(solar.aov)
Df Sum Sq Mean Sq F value   Pr(>F)    
solar$batch   3  107.8   35.93   11.53 3.71e-07 ***
  Residuals   284  884.8    3.12                     
---
as the p value is smaller than 0.05, there is a significant difference between
groups within batch'

par(mfrow=c(2,2))
plot(solar.aov)
par(mfrow=c(1,1))
plot(cooks.distance(solar.aov))

# test of normality
shapiro.test(residuals(solar.aov))

# the p-value is less than 0.05, 
#then the null hypothesis that the data are normally distributed is rejected.

'Shapiro-Wilk normality test

data:  residuals(solar.aov)
W = 0.99452, p-value = 0.3906
'

library(car)

# (d) Analyze the model assumptions as in Task 14 (c). 
# Additionally, test on level α = 0.05, 
# if there is evidence against the assumption of equal variance in each group of 
# factor level.

# Levene-Test of equal variances
leveneTest(solar.aov)

summary(solar.aov)

par(mfrow=c(1,1))

solar.Tuk = TukeyHSD(solar.aov,conf.level=0.9)
solar.Tuk

# plot of the computed confidence intervals
plot(solar.Tuk)


#######
#Task19
#######
# (a) Consider the data set ToothGrowth from the package datasets 
# and transform the attribute dose to type factor.
tg = ToothGrowth 

tg$dose = as.factor(tg$dose)

# (b) Create a boxplot for the value of len, 
# separated on all factor combinations of supp and dose. 
# What is your impression?
ggplot(tg, aes(x=dose, y=len)) +
  geom_boxplot(aes(fill = supp)) + theme_bw()

# (c) Fit a linear model for len, where supp and dose (inclusive interaction) 
# are explanatory variables.


lm.len = lm(len ~ dose*supp, data = tg)


# (d)
# check the fit of the model
par(mfrow=c(2,2))
plot(lm.len)
par(mfrow=c(1,1))
plot(cooks.distance(lm.len))

shapiro.test(residuals(lm.len))

# The test rejects the hypothesis of normality when the p-value is less than or equal to 0.05.

leveneTest(lm.len)

# Wenn der p-Wert für den Levene-Test größer als 0,05 ist, 
# dann unterscheiden sich die Varianzen nicht signifikant voneinander 
# (d. h., die Homogenitätsannahme der Varianz ist erfüllt). 
# Wenn der p-Wert für den Levene-Test kleiner als .05 ist, 
# gibt es einen signifikanten Unterschied zwischen den Varianzen.

# -> bigger than 0.05: do not reject null-hypothesis
# otherwise, reject H0: Varianz unterscheidet sich signikifant

lm.len.new = lm(len ~ dose + supp, data = tg)
pairwise.t.test(tg$len, tg$dose, p.adjust.method = "bonferroni")

#######
#Task21
#######

set.seed(2020)

df_beta2 = data.frame(matrix(ncol = 3, nrow = 1000))
cols_beta2 = c("N10", "N20", "N50")
colnames(df_beta2) = cols_beta2

df_diff = data.frame(matrix(ncol = 3, nrow = 1000))
cols_diff = c("N10", "N20", "N50")
colnames(df_diff) = cols_diff


for(i in 1:1000){

  runi10 = runif(10, 0, 40)
  runi20 = runif(20, 0, 40)
  runi50 = runif(50, 0, 40)

  rnorm10 = rnorm(10, 15, 10)
  rnorm20 = rnorm(20, 15, 10)
  rnorm50 = rnorm(50, 15, 10)

  err10 = 5*(rexp(10, 1) - 1)
  err20 = 5*(rexp(20, 1) - 1)
  err50 = 5*(rexp(50, 1) - 1)

  be1 = 35
  be2 = 0.5
  be3 = -0.1

  mu1 = be1 + be2*runi10 + be3*rnorm10
  y1 = mu1 + err10

  mu2 = be1 + be2*runi20 + be3*rnorm20
  y2 = mu2 + err20

  mu3 = be1 + be2*runi50 + be3*rnorm50
  y3 = mu3 + err50

  fit.temp1 = lm(y1 ~ runi10 + rnorm10)

  fit.temp2 = lm(y2 ~ runi20 + rnorm20)

  fit.temp3 = lm(y3 ~ runi50 + rnorm50)
  
  df_beta2[i,]$N10 = as.numeric(fit.temp1$coefficients)[3]
  df_beta2[i,]$N20 = as.numeric(fit.temp2$coefficients)[3]
  df_beta2[i,]$N50 = as.numeric(fit.temp3$coefficients)[3]
  
  dif1 = norm(c(be1,be2,be3) - fit.temp1$coefficients, type = "2")
  
  dif2 = norm(c(be1,be2,be3) - fit.temp2$coefficients, type = "2")
  
  dif3 = norm(c(be1,be2,be3) - fit.temp3$coefficients, type = "2")
  
  df_diff[i,]$N10 = dif1
  df_diff[i,]$N20 = dif2
  df_diff[i,]$N50 = dif3
  
}

# (i) Create boxplots of the $β − β$ for the three values of N.

#temp = reshape(data, direction="long", varying=2:4, sep="")
#boxplot(split(temp[,3], temp[,1]))

temp_beta2 = reshape(df_beta2, direction = "long", varying = 2:4, sep = "")
par(mfrow=c(1,1))

density_beta2_10 = density(df_beta2$N10)
density_beta2_20 = density(df_beta2$N20)
density_beta2_50 = density(df_beta2$N50)

plot(density_beta2_10)
curve(dnorm(x, 
            mean = mean(df_beta2$N10),
            sd = sd(df_beta2$N10)),
            add = TRUE,
            col = "red")

plot(density_beta2_20)
curve(dnorm(x, 
            mean = mean(df_beta2$N20),
            sd = sd(df_beta2$N20)),
      add = TRUE,
      col = "blue")

plot(density_beta2_50)
curve(dnorm(x, 
            mean = mean(df_beta2$N50),
            sd = sd(df_beta2$N50)),
      add = TRUE,
      col = "green")