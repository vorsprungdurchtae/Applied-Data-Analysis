#######
#Task27
#######

# (a) Download the file transportation.csv from RWTHmoodle and load it as a data frame into your workspace. Create two boxplots of the attribute time, one for each mean of transport.

transpt = read.csv("transportation.csv", header = TRUE, sep = ";")

boxplot(transpt$time ~ transpt$transport, xlab = "transport", ylab = "time")

# (b) Fit a logistic regression model, where the attribute transport depends only on time.
# (c)
trans.logit = glm(transport ~ time, data = transpt, family = binomial(link = "logit"))

trans.probit = glm(transport ~ time, data = transpt, family = binomial(link = "probit"))

trans.cloglog = glm(transport ~ time, data = transpt, family = binomial(link = "cloglog"))

# plot the predicted values

plot(sort(transpt$time), sort(fitted(trans.logit)), type="l", col = "green")

plot(transport ~ time, data = transpt)

order.time = order(transpt$time)

lines(sort(transpt$time), sort(fitted(trans.probit)), col = "red")

lines(sort(transpt$time), sort(fitted(trans.cloglog)), col = "blue")

# (d) Fit two logistic regression models as in (b), 
# one for each sex. Plot in ggplot2 the predicted probabilities 
# for using public transport as a function of time on the same plot, 
# using different colors per gender.

# filter out sex == 1
transpt.sex1 = transpt[which(transpt$sex == 1), ]

# fit the logistirc regression with logit
transpt.sex1.logit =  glm(transport ~ time, data = transpt.sex1, family = binomial(link = "logit"))

# fit the logistirc regression with probit
transpt.sex1.probit = glm(transport ~ time, data = transpt.sex1, family = binomial(link = "probit"))

# fit the logistirc regression with cloglog
transpt.sex1.cloglog = glm(transport ~ time, data = transpt.sex1, family = binomial(link = "cloglog"))

plot(transport ~ time, data = transpt.sex1)

order.time.sex1 = order(transpt.sex1$time)

lines(transpt.sex1$time[order.time.sex1], predict(transpt.sex1.logit)[order.time.sex1], col = "blue")

lines(transpt.sex1$time[order.time.sex1], predict(transpt.sex1.probit)[order.time.sex1], col = "red")

lines(transpt.sex1$time[order.time.sex1], predict(transpt.sex1.cloglog )[order.time.sex1], col = "green")

# (e) Fit a logistic regression model, 
# where transport depends on time and sex. 
# Calculate the percentage of correct classified observations 
# according to this model. 
# Compare to the corresponding percentage for the model in (b).

# fit the model transport ~ time + sex
trans.logit.sex.time = glm(transport ~ time + sex, data = transpt, family = binomial(link = "logit"))

transpt$predicted.origin = ifelse(predict(trans.logit.sex.time) > 0.5, 1, 0)

sum(transpt$predicted.origin == transpt$transport)/nrow(transpt)

# 0.7478992

# other way around
# confusion matrix
# confusion.mat = table(transportation$transport ,predicted.transport)
# get the accuracy
# sum(diag(confusion.mat))/sum(confusion.mat)

# (f) Fit a logistic regression model, 
# where transport depends on time, sex and their interaction. 
# Is the interaction statistically significant?

trans.logit.sex.time2 = glm(transport ~ time * sex, data = transpt, family = binomial(link = "logit"))

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept) -2.92422    1.40324  -2.084   0.0372 *
#   time         0.07141    0.07135   1.001   0.3169  
# sex          0.69813    0.83269   0.838   0.4018  
# time:sex     0.02394    0.04460   0.537   0.5914 

# wald statistics
# 0.5367713
wald.stat = 0.02394/0.04460

# wald CI

WaldCI.low =  -0.063476
WaldCI.high = 0.11135

# alternative
1 - pchisq(deviance(trans.logit.sex.time) - deviance(trans.logit.sex.time2),
           df = df.residual(trans.logit.sex.time) - df.residual(trans.logit.sex.time2))

anova(trans.logit.sex.time2, test = "Chisq")
anova(trans.logit.sex.time, trans.logit.sex.time2, tet = "Chisq")
# 
# Analysis of Deviance Table
# 
# Model 1: transport ~ time + sex
# Model 2: transport ~ time * sex
# Resid. Df Resid. Dev Df Deviance
# 1       116     120.85            
# 2       115     120.57  1  0.28499


#######
#Task28
#######

# (a) Download the file credits.csv from RWTHmoodle and 
# load it as a data frame into your workspace.
# Divide randomly the data of sample size n = 1000 
# into training data (67 % of the data) with 667 rows and test data 
# (the remaining 33% of the data).

credits.task28 = read.csv("credits.csv", header = TRUE, sep = ",")

train.idx = rbinom(n = 1000, size = 1, prob = 0.666)

sample.idx = sample(1000, 667)

#credits.train = credits.task28[(train.idx-1)*-1, ]

#credits.test = credits.task28[train.idx, ]

# alternative 
# credits.task28[which(train.idx != 0),]

credits.train = credits.task28[sample.idx,]

credits.test = credits.task28[-sample.idx,]

# (b) Based on the training data fit a logistic regression model, 
# that predicts repayment using the attributes:

repalyment.glm = glm(repayment ~ factor(account) 
                     + factor(behavior) 
                     + factor(savings) 
                     + factor(employment) 
                     + factor(rate) 
                     + factor(guaran) 
                     + finance 
                     + factor(furthcred) 
                     + factor(home) 
                     + factor(job) 
                     + time 
                     + amount 
                     + age, data = credits.train, family = binomial(link = "logit"))

repayment.pred.complex = predict(repalyment.glm, newdata = credits.test, type = "response")

# (c) Select the best model (nested in the model fitted in (b)) 
# in terms of AIC and BIC using a backwards stepwise selection algorithm. 

# select the best model based on BIC backward
repayment.best.glm.BIC = step(repalyment.glm, direction = "backward", k = log(n))

repayment.best.glm.AIC = step(repalyment.glm, direction = "backward")

# Compute the predicted values for the test data based on the model 
# in (b) and both selected models.

predicted.AIC = predict(repayment.best.glm.AIC, newdata = credits.test, type = "response")
predicted.BIC = predict(repayment.best.glm.BIC, newdata = credits.test, type = "response")

# (d) Based on the training data fit a logistic regression model, 
# that predicts repayment using the attribute simple.score 
# (calculated in R-Laboratory 2, Task 8).

repayment.simple = glm(repayment ~ simple.score, data = credits.train, family = binomial(link = "logit"))

repayment.simple.predict = predict(repayment.simple, newdata = credits.test, type = "response")

repayment.simple.score.lin = 1 - credits.test$simple.score/90

sum((repayment.pred.complex - credits.test$repayment)^2)

sum((predicted.AIC - credits.test$repayment)^2)

sum((predicted.BIC - credits.test$repayment)^2)

sum((repayment.simple.predict - credits.test$repayment)^2)

sum((repayment.simple.score.lin - credits.test$repayment)^2)

######
#CRABS
######

crabs = read.table("crabs.dat", header = TRUE)

#######
#Defekt
#######

def = c(20, 17, 12, 9, 7)
nodef = c(80, 83, 88, 91, 93)
machine = 1:5

product = data.frame(machine, def, nodef)

lin.logit = glm(cbind(def, nodef) ~ machine, 
                family = binomial(link = "logit"),
                data = product)






