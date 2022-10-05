#########
#TASK 29
#########

# (a) Download the file FieldGoal.csv from RWTHmoodle 
# and load it as a data frame into your workspace.

FG = read.csv("FieldGoal.csv", header = TRUE, sep = ";")

# (b) Fit a logistic regression model that predicts Good. 
# using Dist as explanatory variable and plot the predicted probabilities 
# for a good kick as a function of Dist.

FG.fit = glm(Good. ~ Dist, data = FG, family = binomial(link = logit))

plot.fit = function(x, coeff){
  
  return( exp(coeff[1] + x*coeff[2])/(1 + exp(coeff[1] + x*coeff[2])) )
  
}

x = min(FG$Dist):max(FG$Dist)

plot(x, 
     plot.fit(x, FG.fit$coefficients), 
     type="l", 
     xlab = "Distance", 
     ylab = "plot.fit", 
     col = "red")

# (c) Calculate the percentage of correct classified observations 
# according to the model in (b).

Good.pred = predict(FG.fit, type = "response") > 0.5

sum(FG$Good. == Good.pred)/nrow(FG)

# (d) Test at significance level α = 0.05 the hypotheses
# H0 :β1 =0 versus H1 :β1 ∕=0

z.squared=(FG.fit$coefficients[2]/summary(FG.fit)$coefficients[2,2])^2
p.val=1-pchisq(z.squared, df=1)
p.val


# (e) Compute 90% profile likelihood confidence intervals 
# for the parameters of intercept and Dist, 
# as well as for the probability of a successful kick 
# when the distance to the goal is 19, 39 and 64 yards.

CI=confint(FG.fit, level=0.9) #profile likelihoood CI 

prof.CI = function(x, coff){
  
  return(exp(coff[1,] + coff[2,]*x)/(1 + exp(coff[1,] + coff[2,]*x)))
  
}

prof.CI(19, CI)
prof.CI(39, CI)
prof.CI(64, CI)

# (f) Fit a logistic regression model, 
# that predicts Good. 
# using the attributes Dist, Blk., Pressure, Roof.type, Altitude and Field. 
# Select the model with smallest AIC 
# using a backwards stepwise selection algorithm. 
# Calculate the percentage of correct classified observations 
# according to this model and compare it with the percentage from (e).

FG2.glm = glm(Good. ~ Dist + Blk. + Pressure + Roof.type + Altitude + Field, data = FG, family = binomial(link = logit))

FG2.AIC = step(FG2.glm, direction = "backward")

Good.pred.2 = predict(FG2.AIC, type = "response") > 0.5

sum(FG$Good. == Good.pred.2)/nrow(FG)

library(pROC)

roc.curve.b = roc(Good. ~ fitted(FG.fit), data = FG)
plot.roc(roc.curve.b, legacy.axes = TRUE, col = "red")

roc.curve.f = roc(Good. ~ fitted(FG2.AIC), data = FG)
plot.roc(roc.curve.f, legacy.axes = TRUE, add=TRUE, col = "blue")

#########
#Alligator
#########

alligator = read.table("alligators.dat", header = TRUE)

fit = vglm(formula = cbind(y2, y3, y4, y5, y1)
           ~ size + factor(lake), family = multinomial,
           data = alligator)


#########
#TASK 30
#########

library(VGAM)
library(vcdExtra)

Hoyt.tab = ftable(Hoyt, col.vars="Status")

Rank=factor(c(rep("Low", 14), rep("Middle", 14), rep("High", 14)))
Occ=factor(rep(c(rep(1, 2), rep(2, 2), rep(3, 2), rep(4, 2), rep(5,2), rep(6,2), rep(7,2)), 3))
Sex=factor(rep(c("Female", "Male"), 21))

hoyt.fit = vglm(Hoyt.tab ~ Rank*Occ + Rank*Sex + Sex*Occ, family = multinomial)

fit.null=vglm(Hoyt.tab~1,family=multinomial)
# p-value of influence of explanatory variables (test versus the null model)
1 - pchisq(deviance(fit.null)-deviance(hoyt.fit),
           df=df.residual(fit.null)-df.residual(hoyt.fit)) 

fit2 = vglm(Hoyt.tab ~ Rank*Occ + Sex*Occ, family = multinomial)
1 - pchisq(deviance(fit.null)-deviance(fit2),
           df=df.residual(fit.null)-df.residual(fit2)) 

hoyt.fit.BIC = step4(hoyt.fit, direction = "backward", k = log(sum(Hoyt.tab)))

hoyt.fit.AIC = step4(hoyt.fit, direction = "backward")

#########
#TASK 31
#########

Vietnam.tab = ftable(xtabs(Freq~sex+year+response,data=Vietnam), col.vars="response")

sex=factor(c(rep("Female",5), rep("Male", 5)))
year=factor(rep(1:5,2))

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
matplot(baseline.cat.log.odds[1, , ],type="p",pch=1,xlab="",ylab="Estimated Baseline-Category Log-Odds", xaxt="n",main="Sample Baseline-Category LO for factor level female")
matplot(baseline.cat.log.odds[2, , ],type="p",pch=1,xlab="",ylab="Estimated Baseline-Category Log-Odds", xaxt="n",main="Sample Baseline-Category LO for factor level male")
matplot(cum.log.odds[1, , ],type="p",pch=1,xlab="",ylab="Estimated Cumulative Log-Odds", xaxt="n",main="Sample Cumulative LO for factor level female")
matplot(cum.log.odds[2, , ],type="p",pch=1,xlab="",ylab="Estimated Cumulative Log-Odds", xaxt="n",main="Sample Cumulative LO for factor level male")

# (c)
# cumulative logit model
fit.viet.logit=vglm(Vietnam.tab~sex+year, 
                    family = cumulative(parallel=FALSE, 
                                        link="logitlink"))

# (d)
fit.viet.probit=vglm(Vietnam.tab~sex+year, 
                    family = cumulative(parallel=TRUE, 
                                        link="probit"))


# (e)
# cumulative probit model
# Chi-squared 
mu=c(predict(fit.viet.probit,type="response")*rowSums(Vietnam.tab))  #need for pearsons X^2
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



