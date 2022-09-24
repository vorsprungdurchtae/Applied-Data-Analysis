# Task 24
# (a)
n=10
beta=2
alpha_true=2
y=rgamma(n, beta, alpha_true)

newton_increment<-function(alpha,x,beta=2){
  n <- length(x)
  score <- n*beta/alpha - sum(x)
  hessian <- (-n*beta/alpha^2)
  return(score/hessian)
}

newton_raphson<-function(x,alpha_0,epsilon=1e-8,delta=100){
  rep=TRUE #should algorithm should be repeated in next step?
  alpha_cur=alpha_0 #current value for alpha
  T.iteration=0 #number of iterations T
  while(rep){ #while rep==TRUE
    alpha_next=alpha_cur-newton_increment(alpha_cur,x) #newton step
    if(abs(alpha_next-alpha_cur) < epsilon*(1+abs(alpha_cur))){ #stopping criterion
      rep=FALSE
      conv=TRUE
    }
    if(delta*abs(alpha_cur) < abs(alpha_next)){ #criterion to check if algorithm diverges
      rep=FALSE
      conv=FALSE
    }
    alpha_cur=alpha_next
    T.iteration=T.iteration+1
  }
  return(list(convergence=conv,alpha=alpha_cur,iterations=T.iteration))
}
print(newton_raphson(y,2))

#(b)
for(n in c(10,100,500,1000)){
  alpha=rep(0, 1000)
  conv_ind = rep(FALSE,1000)
  for(i in 1:1000){
    y=rgamma(n, beta, alpha_true)
    result_it_i = newton_raphson(y,2)
    conv_ind[i] = result_it_i$convergence
    alpha[i]= result_it_i$alpha
  }
  # select only alpha estimates for which Newton-Raphson converged
  alpha = alpha[conv_ind] #take the values for alpha which were estimated
  J=n*beta/alpha_true^2 #fisher information 
  cat(mean(alpha),"\t",var(alpha),"\t", J^(-1),"\n") #mean of alpha, sample variance, inverse Fisher information
  alpha_norm=(alpha-mean(alpha))/sd(alpha) #standardize 
  hist(alpha_norm, freq=FALSE)
  curve(dnorm,col="red",add=TRUE) #see theorem ||.2.22, approximate normal distribution
}


#######
#Task25
#######

# Let Y ∼ Γ(α,β) be gamma distributed with shape parameter β > 0 and rate parameter α>0,i.e.Y hasthepdf

# Windmill.dat
windmill = read.table("Windmill.dat", header = TRUE)

windmill$bin1 = as.factor(windmill$bin1)

# (b) Divide the data in training and testing data randomly. The training data should consist of about 23 of the rows of Windmill.dat.

binom.idx = rbinom(nrow(windmill), 1, 2/3)

windmill.train = windmill[which(binom.idx == 1),]

# (c) Fit on this data frame a liner model with formula

windmill.fit1 = lm(CSpd ~ Spd1 * Spd1Lag1 + Spd2 * Spd2Lag1 + Spd3 * Spd3Lag1 + Spd4 * Spd4Lag1+
                     Spd1sin1 + Spd1cos1 + bin1 + Dir1, data = windmill)

AIC(windmill.fit1)
BIC(windmill.fit1)

# (d) Next compute the AIC and BIC for the linear model with formula

windmill.fit2 = lm(CSpd ~ Spd1 + Spd1Lag1 + Spd2 + Spd2Lag1 + Spd3 + Spd3Lag1 + Spd4 + Spd4Lag1+
                     Spd1sin1 + Spd1cos1 + bin1 + Dir1, data = windmill)

AIC(windmill.fit2)
BIC(windmill.fit2)

# (e) (i) Search for the linear model with lowest AIC value out of all models nested in the model (c) using a backward, a forward strategy and both. What do you observe?

step(windmill.fit1, direction = "both")
step(windmill.fit1, direction = "forward")
step(windmill.fit1, direction = "backward")

step(windmill.fit1, direction = "both", k = log(n))
step(windmill.fit1, direction = "forward", k = log(n))
step(windmill.fit1, direction = "backward", k = log(n))

# (f) Compare the models selected in (e) (i) and (ii) using the testing data and compute the
# so called predicted residual sum of squares (PRESS)

# i)
# CSpd ~ Spd1 + Spd1Lag1 + Spd2 + Spd2Lag1 + Spd3 + Spd3Lag1 + 
# Spd4 + Spd4Lag1 + Spd1cos1 + bin1 + Spd2:Spd2Lag1 + Spd3:Spd3Lag1

fit.AIC = lm(CSpd ~ Spd1 + Spd1Lag1 + Spd2 + Spd2Lag1 + Spd3 + Spd3Lag1 + 
               Spd4 + Spd4Lag1 + Spd1cos1 + bin1 + Spd2:Spd2Lag1 + Spd3:Spd3Lag1, data = windmill)
PRESS.AIC = sum((windmill$CSpd - fitted(fit.AIC))^2)

# ii)

fit.BIC = lm(CSpd ~ Spd1 + Spd2 + Spd2Lag1 + Spd3 + Spd3Lag1 + Spd4 + Spd4Lag1 + 
               Dir1 + Spd2:Spd2Lag1 + Spd3:Spd3Lag1 + Spd4:Spd4Lag1, data = windmill)

PRESS.BIC = sum((windmill$CSpd - fitted(fit.BIC))^2)

#######
#Task26
#######

#x.unif = runif(25, 20, 80)
#y.unif = rgamma()

#beta.x.unif = exp(0.08*x -2)

#y.gamma = rgamma(25, 1, beta.x.unif)

#fit.y.gamma = glm(y.gamma ~ x.unif, family = "Gamma")

cor.table = data.frame(matrix(nrow = 1, ncol = 4))
cor.columns = c("25","100","1000", "10000")
colnames(cor.table) = cor.columns

row = c(0,0,0,0)

for(i in 1:4){
  
  Ns = c(25, 100, 1000, 10000)
  
  x.unif = runif(Ns[i], 20, 80)
  beta.x.unif = exp(0.08*x - 2)
  y.gamma = rgamma(Ns[i], 1, beta.x.unif)
  fit.y.gamma = glm(y.gamma ~ x.unif, family = "Gamma")
  
  row[i] = cor(y.gamma - fitted(fit.y.gamma), fitted(fit.y.gamma))
  
}




