library(MASS)

########################################
#############E-Test 1 Item 3############
########################################

# e)
# reald mental data
mental.2 = read.csv("Mental.dat", sep = "")

# life = X_1,i, ses = X_2,i
multsum.life.ses = mental.2$life * mental.2$ses

sumsumsum = 0

for(i in 1:length(mental.2)){
  
  sumsumsum = sumsumsum + multsum.life.ses[i]^(1/(i^2))
  
}

print(sumsumsum)


########################################
#############E-Test 1 Item 4############
########################################

#Task a
#Load the boston data
data.boston <- Boston

# get the variables of data.boston <- length(data.boston)
# get the number of observation <- nrow(data.boston)

# b) fit the model by the following settings
# fit the lm 
# response : medv
# predictor : the rest
fit.boston.medv <- lm(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat , data = data.boston)

# c) fit the model by the following settings
# response : medv
# predictor : rm and lstat
fit.boston.medv.rm.lstat <- lm(medv ~ rm + lstat , data = data.boston)

# achieve the Sum of Squared Error 
sse.boston.medv.rm.lstate <- sum((fitted(fit.boston.medv.rm.lstat) - Boston
                                 $medv)^2)

#fit.boston.medv.rm.lstate[1], [2], [3]

# task d)
# get the medv value of
# lstat = 10
# rm = 6
medv.lstat10.rm6 <- t(fit.boston.medv.rm.lstat$coefficients) %*% c(1,6,10)


# task e)

fit.medv.lstat.poly <- lm(medv ~ poly(lstat ,degree = 2), data = data.boston)
# coefficients <- fit.medv

# task f)

# fit the simple linear regression model
fit.medv.lstat <- lm(medv ~ lstat , data = data.boston)

# get the residual standard error from model e)
# 
# Residual standard error: 5.524 on 503 degrees of freedom
# Multiple R-squared:  0.6407,	Adjusted R-squared:  0.6393 
# F-statistic: 448.5 on 2 and 503 DF,  p-value: < 2.2e-16

# get the residual standard error from f)

# Residual standard error: 6.216 on 504 degrees of freedom
# Multiple R-squared:  0.5441,	Adjusted R-squared:  0.5432 
# F-statistic: 601.6 on 1 and 504 DF,  p-value: < 2.2e-16

########################################
#############E-Test 1 Item 3############
########################################

mental.data <- read.csv2("Mental.dat", stringsAsFactors = TRUE, header = TRUE, sep = "")

menta.sample.size <- 40

# X1 life Life event score
# X2 ses Socio economic status
# Y impairment

#batch1_idx <- which(new_solar$batch == 1)

# a)-i Y >= 25

# filter out impir >= 25
mental.data.a.i <- mental.data[which(mental.data$impair >= 25),]

# mean for life
mental.data.a.i.mean.life <- mean(mental.data.a.i$life)

# mean for ses
mental.data.a.i.mean.ses <- mean(mental.data.a.i$ses)

# proportion of samples
proportion.impair25 <- length(which(mental.data$impair >= 25))/ nrow(mental.data)

# a)-ii 30 < Y <= 35

mental.data.a.ii <- mental.data[which(mental.data$impair > 30 & mental.data$impair <= 35),]

# mean for life
mental.data.a.ii.mean.life <- mean(mental.data.a.ii$life)

# mean for ses
mental.data.a.ii.mean.ses <- mean(mental.data.a.ii$ses)

# proportion of samples
proportion.impair3035 <- length(which(mental.data$impair > 30 & mental.data$impair <= 35)) / nrow(mental.data)

# b)

fit.impir.life.ses <- lm(impair ~ life + ses , data = mental.data)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 28.22981    2.17422  12.984 2.38e-15 ***
#   life         0.10326    0.03250   3.177  0.00300 ** 
#   ses         -0.09748    0.02908  -3.351  0.00186 ** 


#c)
# filter rows only containing ses >= 70
mental.data.c <- mental.data[which(mental.data$ses >= 70),]

# fit the model based on above

fit.impir.life.ses.70 <- lm(impair ~ life + ses , data = mental.data.c)

#d) compute the correlation

correlation.life.ses <- cor(mental.data$life, mental.data$ses)
# 
# > correlation.life.ses
# [1] 0.123337

# power index
power.index <- 1/c(1:40)^2

# multipying the two columns
#product.life.ses <- (mental.data$life * mental.data$ses)^power.index

ans<- 0

for(i in 1:40){
  
  ans = ans + (mental.data$life[i]*mental.data$ses[i])^(1/i^2)
  
}

########################################
#############E-Test 1 Item 3############
########################################

mental = read.csv("Mental.dat", head = TRUE, sep = "")

# Y >= 25

idx1 = which(mental$impair >= 25)
idx2 = which(mental$impair > 30 & mental$impair <= 35)

mean1.life = mean(mental[idx1,]$life)

mean1.ses = mean(mental[idx1,]$ses)

length(idx1)/nrow(mental)

mean2.life = mean(mental[idx2,]$life)

mean2.ses = mean(mental[idx2,]$ses)

length(idx2)/nrow(mental)

fit.mental = lm(impair ~ life + ses, data = mental)

idx3 = which(mental$ses >= 70)

new_mental = mental[idx3,]

fit.mental2 = lm(impair ~ life + ses, data = new_mental)

########################################
#############E-Test 1 Item 4############
########################################

#sample size 
nrow(Boston)
ncol(Boston)

boston.fit = lm(medv ~ 
                  crim+
                  zn+
                  indus+
                  chas+
                  nox+
                  rm+
                  age+
                  dis+
                  rad+
                  tax+
                  ptratio+
                  black+
                  lstat, 
                  data = Boston)

boston.fit2 = lm(medv ~ rm + lstat, data = Boston)

boston.fit3 =  lm(medv ~ poly(lstat,2), data = Boston)

boston.fit4 =  lm(medv ~ poly(lstat,1), data = Boston)

########################################
#############E-Test 2 Item 3############
########################################

set.seed(2022)
n = 50
x = rnorm(n)
mu = 1 + 2*x - x*x
y = mu + rnorm(n, sd = 2)

# > sd(x)
# [1] 0.8733347

# > mean(mu)
# [1] -0.0218204
# > mean(y)
# [1] 0.7909158

fit1 = lm(y ~ x - 1)
# x 
# 2.898172 

fit2 =  lm(y ~ poly(x, 2))

# > fit2$coefficients
# (Intercept) poly(x, 2)1 poly(x, 2)2 
# 0.7909158  18.9448819  -5.4017402 

mean((fitted(fit1) - mu)^2)
# > mean((fitted(fit1) - mu)^2)
# [1] 1.254949

mean((fitted(fit2) - mu)^2)
# > mean((fitted(fit2) - mu)^2)
# [1] 0.8222109

#model c)

# data:  fit1$residuals
# W = 0.97698, p-value = 0.4326

# data:  fit2$residuals
# W = 0.98168, p-value = 0.6248

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.7909     0.3100   2.551   0.0140 *  
#   poly(x, 2)1  18.9449     2.1921   8.642 2.85e-11 ***
#   poly(x, 2)2  -5.4017     2.1921  -2.464   0.0174 *  
#   ---

########################################
#############E-Test 2 Item 3############
########################################

# set random seed
set.seed(2022)
# the number of random sample 50
n_etest2 <- 50
# random sample of number 50
x <- rnorm(n_etest2)
# mu the function
mu <- 1 + 2*x - x*x
# y
y <- mu + rnorm(n_etest2, sd = 2)

# fit the x value to the linear model to predict the mu without any 
# intercept 
# model Item 3 b) 
fit.x.etest <- lm(y ~ x + 0)

# fit the model by using x to estimate the whole parameters
# model Item 3 c)
fit.x.xx.etest <- lm(y ~  poly(x, 2))

# b)
# predict the value based on the estimated parameters
#predict(model.correct.poly,newdata=data.frame(x=x))
prediction.fit.x.etest <- predict(fit.x.etest)

# c)
# predict the values based on the estimated parameters
prediction.fit.x.xx.etest <- predict(fit.x.xx.etest)

# d)
# sum of squared erros
sse.prediction.fit.x.etest <- mean((prediction.fit.x.etest - mu)^2)

sse.prediction.fit.x.xx.etest <- mean((prediction.fit.x.xx.etest - mu)^2)

#DimSelf.shapiro.test <- shapiro.test(data.frame.survey.task14$DimSelf)

# f)
# shapiro test of model b on level 0.05
shapiro.test.model.b <- shapiro.test(residuals(fit.x.etest))
shapiro.test.model.c <- shapiro.test(residuals(fit.x.xx.etest))

########################################
#############E-Test 3 Item 3############
########################################

#Beetles.txt
btls = read.csv("Beetles.txt", head = TRUE, sep = "")

#a) xi = 1.691

length(which(btls$x == 1.691))

btls.m1 = btls[which(btls$x == 1.691), ]

length(which(btls.m1$y == 1))/nrow(btls.m1)

btls.glm = glm(y ~ x, data = btls, family = "binomial")

sum((fitted(btls.glm) - btls$y)^2)

length((fitted(btls.glm) > 0.5) == TRUE)

btls$predicted = (fitted(btls.glm) > 0.5)*1
length(which(btls$predicted == btls$y))/nrow(btls)

btls.glm2 = glm(y ~ x, data = btls, family = binomial(link = "probit"))

btls$probit = fitted(btls.glm2)

sum((btls$probit - btls$y)^2)


########################################
#############E-Test 3 Item 3############
########################################

# a)
# load the beetles data
btls = read.csv("Beetles.txt", sep = "")

# group the number of beetles of which x = 1.691
table.dose = table(btls$x)
# 
# 1.691 1.724 1.755 1.784 1.811 1.837 1.861 1.884 
# 59    60    62    56    63    59    62    60 
# 

btls.1691 = btls[which(btls$x == 1.691), ]
# sum(btls.1691$y) = 6
#0.1016949

# b) fit the GLM using the canonical link function

btls.glm = glm(y ~ x, data = btls, family = "binomial")

# beetles sse

btls.sse = sum((fitted(btls.glm) - btls$y)^2)
# 59.14444
# AIC = 376.35
# BIC = 

AIC(btls.glm)
BIC(btls.glm)
# > AIC(btls.glm)
# [1] 376.3542
# > BIC(btls.glm)
# [1] 384.7059

# d) confidence interval lower bound and upper bound

# > confint(btls.glm)
# Waiting for profiling to be done...
# 2.5 %    97.5 %
#   (Intercept) -71.46808 -51.09973
# x            28.86777  40.31875
# e)
btls.pred=ifelse(btls.glm$fitted.values > 0.5, 1, 0)

#count the number of rows where predicted and y same
correct_predict = btls[which(btls$y == btls$predicted), ]
# 
# > nrow(correct_predict)/nrow(btls)
# [1] 0.8274428

# f) using probit

btls.glm.probit = glm(y ~ x, data = btls, family = binomial(link = "probit"))

btls.sse = sum((fitted(btls.glm.probit) - btls$y)^2)
# > btls.sse
# [1] 59.2126

########################################
#############E-Test 4 Item 3############
########################################

incomes = c("High Income", "Middle Income", "Low Income")

for_gm_food = c(320, 280, 258)
against_gm_food = c(150, 166, 112)

df_gm_food = data.frame(
                 for_gm_food,
                 against_gm_food)

colnames(df_gm_food) = incomes 


########################################
#############E-Test 4 Item 2############
########################################

#a)
# load data
house = read.table("Houses.txt", sep = "\t")

# factorizing the new
house$new = as.factor(house$new)

#filter the data
task2a = house[which(house$taxes > 3000 & house$bedrooms > 2),]

#b) fit the model; the target variable = 1,0 => binom
house.glm = glm(price ~ size + new + taxes + bedrooms + baths, 
                data = house, family = "binomial")

#d)
price = 1
new = as.factor(1)
size = 1000
taxes = 1200
bedrooms = 2
baths = 2

new_dat = c(1, price, new, size, taxes, bedrooms, baths)
# predict! response!
predict.glm(house.glm, data.frame(new_dat), type = "response")

con_int = confint(house.glm, level = 0.95)

library(pROC)

# area under curve
roc(price ~ fitted(house.glm), data = house)

########################################
#############E-Test 4 Item 4############
########################################

#a) how to generate the contingency table
# define the count
count = c(320, 150, 280, 166, 258, 112)
# against or for gm 3 times each
gm <- rep(c("for", "against"), 3)
# for all the income column; gm, and income appear each two times
income <- c("high", "high", "middle", "middle", "low", "low")
# with x tab, to generate contingency table
contigency =xtabs(count~gm+income)
# to dataframe
data <- data.frame(gm, income, count)
# glm and poisson!!
glm.gm = glm(count ~ gm + income,  data, family = poisson) 
#b) residuals with pearson
residuals.pearson = residuals(glm.gm, type = "pearson")
#b) get the pearson chi squared statistics
chisq.test(contigency)$statistic
#c) Likelihood ratio statistics G^2
glm.gm$deviance
# asymptotic p value
p.value <- 1-pchisq(glm.gm$deviance, glm.gm$df.residual)
# > xtabs(residuals.pearson ~ gm + income)
# income
# gm              High        Low     Middle
# against -13.333333  -1.333333  14.666667
# for      13.333333   1.333333 -14.666667

# d)second model MSat
glm.gm2 = glm(count ~ gm * income, data = data, family = poisson) 
AIC(glm.gm2)#e)
#f)
selected  = step(glm.gm2, direction="backward")


