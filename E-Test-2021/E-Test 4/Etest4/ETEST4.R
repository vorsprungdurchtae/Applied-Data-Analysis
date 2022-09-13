library(MASS)
library("glmnet")

n = nrow(Boston)
set.seed(2021)

# split into training and test data
training.idx = sample(n,n-100)
data.training = Boston[training.idx,]
data.test = Boston[-training.idx,]
nrow(data.training)
nrow(data.test)

model.boston = lm(medv~crim+zn+indus+nox+rm+age+dis+tax+ptratio+black+lstat, data=data.training)
summary(model.boston)
mean(model.boston$fitted.values)
mean(fitted.values(model.boston))


model.lasso<-glmnet(as.matrix(data.training[,c("crim","zn","indus","nox","rm","age","dis",
                                               "tax","ptratio","black","lstat")]),
                    y=data.training$medv,alpha=1,family="gaussian" ,lambda=0.5)
coef(model.lasso)


cv<-cv.glmnet(as.matrix(data.training[,c("crim","zn","indus","nox","rm","age","dis",
                                               "tax","ptratio","black","lstat")]),
                    y=data.training$medv,alpha=1,family="gaussian")
cv$lambda.min
cv$lambda.1se


model.lasso.1sp <- glmnet(as.matrix(data.training[,c("crim","zn","indus","nox","rm","age","dis",
                                                     "tax","ptratio","black","lstat")]),
                          y=data.training$medv,alpha=1,family="gaussian" ,lambda=cv$lambda.1se)
min(coef(model.lasso.1sp))

pred.lasso.1sp = predict(model.lasso.1sp,newx = as.matrix(data.test[,c("crim","zn","indus","nox","rm","age","dis",
                                                               "tax","ptratio","black","lstat")]),
                           type = "response")


model.lasso.min <- glmnet(as.matrix(data.training[,c("crim","zn","indus","nox","rm","age","dis",
                                                     "tax","ptratio","black","lstat")]),
                          y=data.training$medv,alpha=1,family="gaussian" ,lambda=cv$lambda.min)

min(coef(model.lasso.min))

pred.lasso.min = predict(model.lasso.min,newx = as.matrix(data.test[,c("crim","zn","indus","nox","rm","age","dis",
                                                                       "tax","ptratio","black","lstat")]),
                         type = "response")


sum((pred.lasso.1sp-data.test$medv)^2)
sum((pred.lasso.min-data.test$medv)^2)



#ITEM 5
library(datasets)

FCT1 <- ftable(UCBAdmissions)
mean(marginSums(FCT1,margin = 1))
mean(marginSums(FCT1,margin = 2))


#Partb)
FCT.table = as.data.frame(FCT1)

glm.sat = glm(Freq ~ Admit * Gender * Dept, family=poisson, data=FCT.table)
glm.sat
summary(glm.sat)

glm.select  = step(glm.sat, direction="backward")
summary(glm.select)


nothreeway = glm(Freq~ Admit + Gender + Dept, family = poisson, data=FCT.table)
coef(nothreeway)
exp(coef(nothreeway)[3])

res.p = residuals(nothreeway, type="pearson")
mean(res.p)
res.d = residuals(nothreeway, type="deviance")
mean(res.d)