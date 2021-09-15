#######
#Task17
#######

my.lm<-function(formula,data){
  y=data[,as.character(formula[[2]])]
  X=model.matrix(formula,data = data)
  param.names=colnames(X)
  
  model.with.intercep=any(colnames(X)=="(Intercept)")
  X=matrix(X,ncol=ncol(X))
  p.columns=ncol(X)
  n.observations=nrow(X)
  degrees.of.freedom=n.observations-p.columns
  
  qr.decomp.X=qr(X)
  beta.hat=qr.solve(qr.decomp.X,y)
  beta.hat=matrix(beta.hat)
  mu.hat=X%*%beta.hat
  residuals=y-mu.hat
  SSE=sum((residuals)**2)
  y.star=ifelse(model.with.intercep,mean(y),0)
  TSS=sum((y-y.star)**2)
  sigma.hat.2=SSE/degrees.of.freedom
  R.2=(TSS-SSE)/TSS
  beta.hat.cov=sigma.hat.2 * chol2inv(qr.decomp.X$qr)
  
  rownames(beta.hat)=param.names
  rownames(beta.hat.cov)=param.names
  colnames(beta.hat.cov)=param.names
  
  erg=list(beta.hat=beta.hat,beta.hat.cov=beta.hat.cov,R.2=R.2,sigma.hat.2=sigma.hat.2)
}


load("Solar.RData")
model<-my.lm(Pmax~factor(batch)-1,data=solar)
print(model)
print(lm(Pmax~factor(batch)-1,data=solar))

degrees.of.freedom=length(solar$Pmax)-length(model$beta.hat)

t_quantile=qt(0.90,degrees.of.freedom)

printf <- function(...) cat(sprintf(...))

for(i in 1:3){
  contrast=matrix(rep(0,length(model$beta.hat)),nrow = 1)
  contrast[1,i]=-1
  contrast[1,i+1]=1
  CI.center=contrast%*%model$beta.hat
  CI.step.from.center=t_quantile*sqrt(contrast%*%model$beta.hat.cov%*%t(contrast))
  printf("CI for factor level difference of levels %d and %d: [%2.4f,%2.4f]\n",i,i+1,CI.center-CI.step.from.center,CI.center+CI.step.from.center)
}

#######
#Task18
#######
set.seed(2020)
n = 1000
sigma = 5
N_vec = c(10, 20, 50)
beta = c(35, 0.5, -0.1)
estim.true.norm.diff = matrix(NA,n,length(N_vec))
beta2.estims = matrix(NA,n,length(N_vec))

x = runif(10, 0, 100)

for(j in seq_along(N_vec)){
  
  N = N_vec[j]

 for(i in 1:1000){
  
   x1 = runif(N, 0, 100)
   x2 = rnorm(N, 15, 10)
   eps = (rexp(N)-1)*sigma
   mu = beta[1] + beta[2]*x1[i] + beta[3]*x2[i]
   y = mu + eps

   lm.fit = lm(y ~x1 + x2)
   beta.hat = lm.fit$coefficients
   beta2.estims[i, j] = beta.hat[2]
   
   estim.true.norm.diff[i,j] = sqrt(sum((beta-beta.hat)^2))
   
  }

}

model.solar<-lm(Pmax~factor(batch)-1,data=solar)

shapiro.test(model.solar$residuals)
