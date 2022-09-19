#######
#Task13
#######

# For values of x in [0, 100], suppose the linear model 
# (Y | X = x) ∼ N (μ(x), σ2) holds with 
#E(Y |X =x)=μ(x)=45+0.1x+5·10−4x2 +5·10−7x3 +5·10−11x4 +5·10−13x5
#  σ = 10.

# (a) Set a seed to 2020 and initialize two numeric vectors vec.delta.simple and
# vec.delta.correct of length 100.

set.seed(2020)

# runif(40, min = -1, max = 1)

vec.delta.simple = rep(0, 100)

vec.delta.correct = rep(0, 100)

for(i in 1:100){
  
  X = runif(25, min = 0, max = 100)
  
  Y = 45 + 0.1*X + 
    5*10^(-4) * X^2 + 
    5*10^(-7) * X^3 + 
    5*10^(-11) * X^4 + 
    5*10^(-13) * X^5 + rnorm(25, sd = 10)
  
  obs = rnorm(25, Y, 10)
  
  mu.simple = lm(Y ~ X)
  mu.correct = lm(Y ~ poly(X, degree = 5))
  
  if(i <= 5){
    
    plot(X, Y)
    lines(X, predict(mu.simple, data.frame(X=X)), col = "blue")
    lines(X[order(X)], predict(mu.correct, data.frame(X=X[order(X)])), col = "red")
    
  }
  
  vec.delta.simple[i] = sum(abs(Y - fitted(mu.simple)))/25
  vec.delta.correct[i] = sum(abs(Y - fitted(mu.correct)))/25
  
}

#######
#Task14
#######

# (a) Load the .RData file of the pre-processed data of Survey1 (Task 7) 
# into the R workspace.

# survey_task7.RData

survey_task7 = read.table("survey_task7.RData", header = TRUE, sep = ",")

# (b) Create a regression model with the approach

lm.survey = lm(DimSelf ~ DimEmotion + DimBody, data = survey_task7)

install.packages("dplyr")
library("dplyr")

par(mfrow=c(2,2))
plot(lm.survey)

"> shapiro.test(fitted(lm.survey))

Shapiro-Wilk normality test

data:  fitted(lm.survey)
W = 0.96368, p-value = 0.0011"

install.packages("rgl")
library("rgl")

# (e) In the model (++) test the hypotheses
# H0 :b=0 versus H1 :b∕=0

'Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  8.95514    6.82362   1.312  0.19165    
DimEmotion   0.54189    0.09100   5.955 2.19e-08 ***
  DimBody      0.25717    0.08775   2.931  0.00398 ** '


#######
#Task15
#######

race = read.table("Races.dat", header = TRUE, sep = "")

# (a) At first step, fit a linear model that predicts women’s record time(timeW) 
# using both distance of the course and climb in elevation 
# as explanatory variables and interpret the resulting coefficients.

lm.timeW = lm(timeW ~ distance + climb, data = race)

# (b) Fit the linear model that predicts timeW using distance 
# as the sole explanatory vari- able. Compare your results with (a).

lm.timeW2 = lm(timeW ~ distance , data = race)

# (c) Show the scatterplot and report the prediction equation. 
# Predict men’s record time for the Highland Fling, 
# for which timeW = 490.05 minutes.
  
lm.men = lm(timeM ~ timeW, data = race)

prd = lm.men$coefficients[1] + lm.men$coefficients[2]*490.05

lm.men2 = lm(timeM ~ -1 + timeW, data = race)

par(mfrow=c(1,1))

plot(race$timeW, race$timeM)

#abline(lm.men2$coefficients, col = "red")
lines(race$timeW, predict(lm.men2, newdata = data.frame(timeW = race$timeW)), col = "red")

#######
#Task16
#######

florida = read.table("Florida.dat", header = TRUE, sep = "")

lm.crime = lm(Crime ~ HS, data = florida)

lm.crime2 = lm(Crime ~ HS + Urban, data = florida)

# Adjusting for urbanization, the effect of education changes sign: the crime rate tends to decrease as education increases.
# Hence, we have a positive marginal correlation when we ignore urbanization 
# This phenomenon of association reversal between marginal and conditional associations is called "simpsons paradox"

#######
#Task17
#######

un = read.table("UN.dat", header = TRUE, sep = "")

lm.internet = lm(Internet ~ 
                   GDP + 
                   HDI + 
                   GII + 
                   Fertility + 
                   CO2 + 
                   Homicide + 
                   Prison , data = un)

# the p-value of GDP is essentially 0 when it is the sole explanatory variable
# when we add the other variables, the SE of the GDP effect increases from 0.1217 to 0.290680
# the p-value increases to 0.13856  ( > 0.05) when we add the other variables to the model
# the dramatic change in the SE for GDP and the lack of statistical significance for the conditional effects is due to the high correlation 

lm.internet2 = lm(Internet ~ GDP, data = un)

cor(un$GDP, un$HDI)

# because of the multicollinearity, we can attain nearly as large and R^2 value in predicting the response with a reduced set of explanatory variables 
# the fact that the effect of GDP is so different in the multiple regression model compared with the bivariate model is caused by the multicollinearity, 
# meaning that GDP "overlaps" considerably with other explanatory variables. Hence, the effects on the multiple regression model are not significant even 
# if the effect is highly significant marginally

