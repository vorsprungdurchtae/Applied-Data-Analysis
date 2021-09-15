#############
#
# Task 24
#
#############
library(ggplot2)
library(pROC)
# (a)
transportation =read.csv("/Users/thetruetae/Desktop/RWTH/SS21/ADA/RLab/R-Lab8/transportation.csv", header = TRUE, sep = ";")
transport = transportation$transport
boxplot(time~transport, data=transportation,names=c("public","car"),xlab="transport",ylab="time (in minutes)")

# (b)
transp.glm1 = glm(transport ~ time, data=transportation,family = binomial)
transp.glm1



# (c)
# other link functions
transp.glm1.cloglog = glm(transport~time,data=transportation,family=binomial(link=cloglog))
transp.glm1.probit = glm(transport~time,data=transportation,family=binomial(link=probit))


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
x = seq(-15,60,0.01)
prob.pred = prob.fit(transp.glm1,x)
plot(x,prob.pred,type="l",col="blue",xlab="time (in minutes)",ylab="predicted probabilities")
lines(x,prob.fit(transp.glm1.cloglog,x,"cloglog"), col="red")
lines(x,prob.fit(transp.glm1.probit,x,"probit"), col="green")
legend("topleft",legend=c("logit","probit", "cloglog"),lty=c(1,1,1),col=c("blue","green", "red"))

# (d)
# devide the data set
transportation.f = transportation[transportation$sex==1,]
transportation.m = transportation[transportation$sex==2,]

transp.glm1.f = glm(transport ~ time, family = binomial, data=transportation.f)
transp.glm1.m = glm(transport ~ time, family = binomial, data=transportation.m)
transp.glm1.f
transp.glm1.m

# predicted probabilities
x = seq(-15,60,0.01)
prob.pred.f = prob.fit(transp.glm1.f,x) #use written function above for the predictions
prob.pred.m = prob.fit(transp.glm1.m,x)
prob.pred=data.frame(x=x,prob.pred.f=prob.pred.f, prob.pred.m=prob.pred.m) #write predictions in data frame

col=c("woman"="red", "man"="blue")
ggplot(prob.pred, aes(x=x))+geom_line(aes(y=prob.pred.f, color="woman"))+geom_line(aes(y=prob.pred.m, color="man")) +
  labs(x="time", y="prob",color="Legend")+scale_color_manual(values = col)

# (e)
# model without interaction
transportation$sex=factor(transportation$sex)
levels(transportation$sex) = c("woman","man")

transp.glm2 = glm(transport ~ time + sex,data=transportation,family = binomial) #model where response depends on time AND sex
transp.glm2

# comparison of predicted values and transport for transp.glm2
transportation$transport = factor(transportation$transport)
transport.pred2 = factor(transp.glm2$fitted.values > 0.5) #predicted values for model 2
levels(transportation$transport) = c("public","car")
levels(transport.pred2) = c("public","car")
transport.tab2=table(transportation$transport,transport.pred2) #classification table
sum(diag(transport.tab2))/sum(transport.tab2)

# comparison of predicted values and transport for transp.glm1
transport.pred1 = factor(transp.glm1$fitted.values > 0.5) #predicted values for model 1
levels(transport.pred1) = c("public","car")
transport.tab1=table(transportation$transport,transport.pred1) #classification table
sum(diag(transport.tab1))/sum(transport.tab1)

# (f)
# model with interaction
transp.glm2.inter = glm(transport ~ time * sex,data=transportation,family = binomial)

# different ways to compute the p value
1-pchisq(deviance(transp.glm2) - deviance(transp.glm2.inter), df= df.residual(transp.glm2) - df.residual(transp.glm2.inter))
anova(transp.glm2.inter,test="Chisq")
anova(transp.glm2, transp.glm2.inter, test="Chisq")



#############
#
# Task 25
#
#############

# (a)
# load the data
data = read.csv("/Users/thetruetae/Desktop/RWTH/SS21/ADA/RLab/R-Lab8/credits.wsv", header = TRUE, sep = " ")

# split into training and test data
training.idx = sample(1000,667)
data.training = data[training.idx,]
data.test = data[-training.idx,]

# (b)
# select logistic regression models and predict the test data
model.complex = glm(repayment~factor(account)+factor(behavior)+factor(savings)+factor(employment)+factor(rate)+factor(guaran)+finance+
                      factor(furthcred)+factor(home)+factor(job)+factor(pers)+time+amount+age,
                    data=data.training,family = binomial(link="logit"))

# (c)
model.selected.AIC = step(model.complex,direction = "backward") #AIC
model.selected.BIC = step(model.complex,direction = "backward",k=log(667)) #BIC
pred.complx=predict(model.complex,newdata = data.test,type = "response") #use prediction with complex model
pred.sel.AIC=predict(model.selected.AIC,newdata = data.test,type = "response") #prediction with best model based on AIC
pred.sel.BIC=predict(model.selected.BIC,newdata = data.test,type = "response") #prediciton with best model based on BIC

# (d)
# predict test data using the simple score 
model.simple.score=glm(repayment~simple.score,data=data.training,family = binomial(link="logit"))
pred.simple.score=predict(model.simple.score,newdata=data.test,type = "response")

# sum of maximum values for all variables in the score
max.score = 10+10+5+4+4+10+5+5+4+4+3+4+4+3+3+4+4+2+2 #add all maximum values for account,dtime,behavior,usage,damount,... see R-Lab 2 Task 8(c)
pred.simple.score.linear = 1-data.test$simple.score/max.score

# (e)
# test the different approaches by a comparison to the actual values in the test data
sum((pred.complx-data.test$repayment)^2)
sum((pred.sel.AIC-data.test$repayment)^2)
sum((pred.sel.BIC-data.test$repayment)^2)
sum((pred.simple.score-data.test$repayment)^2)
sum((pred.simple.score.linear-data.test$repayment)^2)


AIC(transp.glm1)
AIC(transp.glm1.cloglog)
AIC(transp.glm1.probit)

transp.glm1.cloglog1 = glm(transport~time + as.factor(transportation$sex),data=transportation,family=binomial(link=cloglog))

AIC(transp.glm1.cloglog1)
BIC(transp.glm1.cloglog1)


transport.pred3 = factor(transp.glm1.cloglog1$fitted.values > 0.5) #predicted values for model 2
levels(transportation$transport) = c("public","car")
levels(transp.glm1.cloglog1) = c("public","car")
transport.tab3=table(transportation$transport,transport.pred3) #classification table
sum(diag(transport.tab3))/sum(transport.tab3)

newcredit = read.csv("/Users/thetruetae/Desktop/RWTH/SS21/ADA/RLab/R-Lab8/credits.csv", header = TRUE, sep = ",")

newcredit$guaran2 <- newcredit$guaran

newcredit$guaran2[newcredit$guaran2 == 1] <- 1
newcredit$guaran2[newcredit$guaran2 == 2] <- 1
newcredit$guaran2[newcredit$guaran2 != 1] <- 2
set.seed(2021)
sample_size = floor(0.65*nrow(newcredit))
train_size = floor(0.35*nrow(newcredit))

training_data = newcredit[sample(nrow(newcredit), sample_size), ]

modelb = glm(training_data$repayment ~ training_data$time + 
                                       training_data$age + 
                                       training_data$savings + 
                                       training_data$guaran2 + 
                                       as.factor(training_data$account) + 
                                       as.factor(training_data$behavior) + 
                                       as.factor(training_data$rate) + 
                                       as.factor(training_data$finance) + 
                                        as.factor(training_data$furthcred) + 
                                       as.factor(training_data$home) + 
                                       as.factor(training_data$job) + 
                                       as.factor(pers), data=training_data,family = binomial)


model.2=glm(training_data$repayment ~ training_data$time + 
              training_data$age + 
              training_data$savings + 
              training_data$guaran2 + 
              as.factor(training_data$account) + 
              as.factor(training_data$behavior) + 
              as.factor(training_data$rate) + 
              as.factor(training_data$finance) + 
              as.factor(training_data$furthcred) + 
              as.factor(training_data$home) + 
              as.factor(training_data$job) + 
              as.factor(pers), data=training_data,family = binomial(link=logit))
model.select=step(model.2,direction="backward")
#Good.Pred=ifelse(model.select$fitted.values > 0.5, 1, 0)
#FG.tab.select=table(FG$Good.,Good.Pred)
#sum(diag(FG.tab.select))/sum(FG.tab.select)