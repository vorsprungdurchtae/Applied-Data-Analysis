library(ggplot2)
library(mvtnorm)
library(MASS)
library(dplyr)
library(tidyr)
library(gridExtra)
library(car)
library(datasets)

####################
#######TASK18#######
####################

# a) load the data from Task 10
solar.data.task18 <- read.csv2("Solar.csv", stringsAsFactors=TRUE)

solar.data.task18$batch <- as.factor(solar.data.task18$batch)

write.csv(solar.data.task18, file = "Solar-Task-10-Preprocessed.csv", row.names = FALSE)

# b) create Boxplot
#X-Axis : batch
#Y-Axis : Pmax
solar.pmax.boxplot.batches <- ggplot(mapping = aes(x = solar.data.task18$batch,
                                                   y = solar.data.task18$Pmax)) + aes(color = solar.data.task18$batch)  + geom_boxplot() + theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size = 5))

# c) ANOVA
# ANOVA of the data regarding Pmax based on batch
anova.solar.pmax.batches <- aov(Pmax ~ batch, data = solar.data.task18)

# summary of the ANOVA
#summary(anova.solar.pmax.batches)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# batch         3  107.8   35.93   11.53 3.71e-07 ***
#   Residuals   284  884.8    3.12                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# reject the null hypothesis

# d) Levene's test:

# # (c)
# # check the fit of the model
# par(mfrow=c(2,2))
# plot(model.survey)
# par(mfrow=c(1,1))
# plot(cooks.distance(model.survey))
# 
# # test of normality
# shapiro.test(model.survey$residuals) 

# before Levene's test, shapiro test

# set the plot layout
par(mfrow=c(2,2))
# plot the model
#plot(model.survey)

# get the linear model of Pmax - batch
model.solar.task18 <- lm(Pmax ~ batch, data = solar.data.task18)

# plot the model
plot(model.solar.task18)

# now plot for the cooks distance and shapiro test

# plot
par(mfrow = c(1,1))

# plot the cooks distance
plot(cooks.distance(model.solar.task18))

############################################
########ALTERNATIVE-COOK'S-DISTANCE#########
############################################
#plot(lm, 4) -> this generates the cook's distance


# now shapiro test
shapiro.test(model.solar.task18$residuals)
# 
# Shapiro-Wilk normality test
# 
# data:  model.solar.task18$residuals
# W = 0.99452, p-value = 0.3906
# p value is greater than 0.05; therefore, null hypothesis is true

# verify if every or subset of observations still have same variance 
# as the whole
Pmax <- solar.data.task18$Pmax
batch <- solar.data.task18$batch

levene.test.pmax.batch <- leveneTest(Pmax, batch)
# do not reject the null hypothesis

# e) test on the significance level alpha = 0.05 and ASK!!!!
# null hypothesis of equal means of Pmax

#anova.solar.pmax.batches
par(mfrow = c(1, 2))

hist(anova.solar.pmax.batches$residuals)
qqPlot(anova.solar.pmax.batches$residuals, id = FALSE)

# f) Tukey Test!
# everyone does not contain zero; therefore, everyone differe each other significantly
tukey.solar.aov <- TukeyHSD(anova.solar.pmax.batches, conf.level = 0.9)

plot(tukey.solar.aov)
# 
#  we can see that intervals do not contain zero should have significantly big difference between mean

####################
#######TASK19#######
####################

# a) load the ToothGrowth
data.toothgrowth <- ToothGrowth

# transfrom the dose to type factor

data.toothgrowth$dose <- as.factor(data.toothgrowth$dose)

# b) create boxplot for the value len separated on all factor
# combination of supp and dose
# 
# solar.pmax.boxplot.batches <- ggplot(mapping = aes(x = solar.data.task18$batch,
#                                                    y = solar.data.task18$Pmax)) + aes(color = solar.data.task18$batch)  + geom_boxplot() + theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size = 5))
# 

toothgrowth.boxplot <- ggplot(aes(x = supp, y = len), data = data.toothgrowth) + 
    facet_wrap(~ dose) + geom_boxplot(aes(fill = supp))

# c) fit a linear model for len supp and dose explanatory variable

lm.tooth.len.supp.dose.interaction <- lm(len ~ supp + dose + supp*dose, data = data.toothgrowth)
  
# d) analyze the model like the Task 14
# i.   residuals vs fitted values
# ii.  sqrt(abs(standardized residuals)) vs fitted values
# iii. quantiles of the standardized residuals vs the expected quantiles of the standard normal distirbution
# iv.  standardized residuals vs leverage

par(mfrow=c(2,2))
plot(lm.tooth.len.supp.dose.interaction)
par(mfrow=c(1,1))
plot(cooks.distance(lm.tooth.len.supp.dose.interaction))
# this also shows the cooks distance!!!!!
plot(lm.tooth.len.supp.dose.interaction, 4)
# 
# # test of normality
shapiro.test(lm.tooth.len.supp.dose.interaction$residuals) 
# Shapiro-Wilk normality test
# 
# data:  lm.tooth.len.supp.dose.interaction$residuals
# W = 0.98499, p-value = 0.6694
# p value bigger than 0.6694; therefore, we accept the null hypothesis

# also levene's test


# additionally, assumption of equal variance in each group of factor

tooth.bartlett.test <- bartlett.test(len ~ interaction(supp, dose), data = data.toothgrowth)
# Bartlett test of homogeneity of variances
# 
# data:  len by interaction(supp, dose)
# Bartlett's K-squared = 6.9273, df = 5, p-value = 0.2261

# e) influence of the explanatory variables on the value of len AOV
#anova.solar.pmax.batches <- aov(Pmax ~ batch, data = solar.data.task18)
aov.tooth <- aov(len ~ dose, data = data.toothgrowth)
# 
# > summary(aov.tooth)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# dose         2   2426    1213   67.42 9.53e-16 ***
#   Residuals   57   1026      18                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# p-value is less than 0.05; therefore, we conclude that the
# influences differ significantly.
#

# f) test on overall alpha = 0.05 if the interaction of supp and dose
# and mossibly their main effect has an influence on the value of len

alpha <- 0.05

# we have three pair wise t-test for the factors of dose: 0.5, 1, and 2 
# thus, 3(k = 3, k(k-1)/2 = 3)

pairwise.t.test(data.toothgrowth$len, data.toothgrowth$dose, p.adjust.method = "bonferroni")


# g) according to the bonferroni, the interaction does not have any significance


####################
#######TASK20#######
####################

#QR Decomposition!!!!

# a) Using the function model.matrix, create design matrix Xß + e

# error terms are normal distributed with zero mean and sigma squared

####################
#######TASK21#######
####################

# set seed

set.seed(2020)

# repeat this stuff for 1000 times
# N = 50

# array to store the estimated beta2
matrix.beta2 <- matrix(0, nrow = 1000, ncol = 3)

# array to store the difference between
# real parameter vector and
# estimated parameters
# colnames(x) <- c("C1","C2","C3")
matrix.diff.norm <- matrix(0, nrow = 1000, ncol = 3)

colnames(matrix.diff.norm) <- c("10", "20", "50")
colnames(matrix.beta2) <- c("10", "20", "50")

sample.sizes <- c(10, 20, 50)

for (n in 1:3) {
  
  for (i in 1:1000){
  
    # sample from uniform distirbution
    sample.uniform <- runif(sample.sizes[n], min = 0, max = 40)
    
    # sample from normal distribution
    sample.norm <- rnorm(sample.sizes[n], 15, 10)
    
    # sample error term from exponential distribution
    sample.exp <- (rexp(sample.sizes[n], rate = 1)-1)*5

    # generate a sample y1 ... yN
    # mu_i = beta1*1 + beta2*x_1i + beta3*x_2i
  
    beta1 <- 35
    beta2 <- 0.5
    beta3 <- -0.1

    # now generate the pseudo observations
    pseudo_obs <- beta1 + beta2*sample.uniform + beta3 * sample.norm + sample.exp

    # now generate the pseudo dataframe
    # name the columns and column values
    column.uni <- c(sample.uniform)
    column.norm <- c(sample.norm)
    column.obs <- c(pseudo_obs)

    # store them in the dataframe
    df.task.21 <- data.frame(column.obs, column.uni, column.norm)
  
    # fit the model
    lm.pseudo_obs <- lm(column.obs ~ column.uni + column.norm, data = df.task.21)
    # 
    # estimated intercepts for example
    # (Intercept)  column.uni column.norm 
    # 36.2698267   0.4994959  -0.1020958
    # 
  
    # normed value between the real and estimated param
    diff.beta.beta.hat <- norm(c(c(lm.pseudo_obs$coefficients) - c(35, 0.5, -0.1)), type="2") 
  
    # store the beta2 of this iteration into below i-th position
    matrix.beta2[i,n] <- lm.pseudo_obs$coefficients[2]
    matrix.diff.norm[i,n] <- diff.beta.beta.hat
  
  }
  
}

# now get the estimated mean and standard deviation if the beta 2
# mean of beta2
# beta2.mean <- mean(array.beta2)
# st of beta2
# beta2.sd <- sd(array.beta2)

# plot from the array of difference norm
# plot(1:1000, array.diff.norm)

# get the density of beta 2

#beta2.x <- 1:1000
#beta2.density <- dnorm(1:1000, mean = beta2.mean, sd = beta2.sd)
#plot(beta2.x, beta2.density)
# boxplot of the euclidean normed difference between estimations and real param
boxplot.euc.norm.difference <- boxplot(matrix.diff.norm)

# density plot of the beta2 N = 10, 20, and 50 respectively matrix.diff.norm[, 1]
plot(density(matrix.beta2[,1]))
curve(dnorm(x,mean=mean(matrix.beta2[,1]),sd=sd(matrix.beta2[,1])),add=TRUE,col="red")

plot(density(matrix.beta2[,2]))
curve(dnorm(x,mean=mean(matrix.beta2[,2]),sd=sd(matrix.beta2[,2])),add=TRUE,col="red")

plot(density(matrix.beta2[,3]))
curve(dnorm(x,mean=mean(matrix.beta2[,3]),sd=sd(matrix.beta2[,3])),add=TRUE,col="red")

