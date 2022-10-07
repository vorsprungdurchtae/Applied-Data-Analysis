#############
#Lung Cancer
#############

lungcancer = read.table("lung_cancer.dat", header = TRUE)

fit <- glm(count ~ factor(histology) + factor(stage) + factor(time),
           family = poisson(link = log), offset = logrisktime, data = lungcancer) 
summary(fit)

#########
#TASK 35
#########

basketball = read.csv("Basketball.csv", header = TRUE, sep = ",")

basketball.fit =  glm(FG ~ G, poisson(link = "log"), data = basketball)
G.x = min(basketball$G):max(basketball$G)

G = basketball$G 
FG = basketball$FG



#curve(exp(basketball.fit$coefficients[1] + x*basketball.fit$coefficients[2]), from = 0, to = 80, add = TRUE, col = "red")
par(mfrow=c(2,2))
set.seed(518)
plot(G, FG)
for(i in 1:3){
  random.sample = rpois(nrow(basketball), basketball.fit$fitted.values)
  plot(G, random.sample)
}
par(mfrow=c(1,1))

#########
#TASK 36
#########

crabs = read.table("Crabs.dat", header = TRUE)

crabs.fit.ZIN1 = glm(y ~ weight + width + color + spine, quasi(link = "log", variance = "mu"), data = crabs)

crabs.fit.ZIN2 = glm(y ~ weight + width + color + spine, quasi(link = "log", variance = "mu^2"), data = crabs)

library("MASS")

NB.glm <- glm.nb(y~weight+color+spine,data=crabs)
summary(NB.glm) 

P.glm<-glm(y~weight+color+spine,family=poisson(link='log'),data=crabs)
summary(P.glm)

ZINB.glm <- zeroinfl(y ~ weight | weight + color, dist="negbin",data=crabs)
summary(ZINB.glm)

#########
#TASK 37
#########

fungal = read.table("fungal.dat", header = TRUE)

fungal$center = as.factor(fungal$center)

fungal.fit = glm(my/m ~ treatment + center, family=binomial, data = fungal)





#prostate.txt

prostate = read.table("prostate.txt", header = TRUE)
prostate.train = prostate[which(prostate$train == TRUE),]
prostate.test = prostate[which(prostate$train == FALSE),]

prostate.fit = lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data = prostate.train)

prostate.penal = glmnet(
  as.matrix(prostate.train[, c("lcavol","lweight","age","lbph","svi","lcp","gleason","pgg45")]), 
  y = prostate.train$lpsa, alpha = 1, family = "gaussian")

plot(prostate.penal)

set.seed(2021)

cv.lasso = cv.glmnet(
  as.matrix(prostate.train[, c("lcavol","lweight","age","lbph","svi","lcp","gleason","pgg45")]), 
  y = prostate.train$lpsa, alpha = 1, family = "gaussian")
