library(ggplot2)

########
#Task19
########

####
#a
####

set.seed(6969)

myedf <- function(a, b, c, phi){ 
  return(function(x,theta){
    return(exp((x*theta-b(theta))/a(phi) +c(x,phi)))
  })
}


d.nat.exp.fam=function(T.stat,B,h)
{ 
  return(function(x,theta){
    return(exp(c(theta%*%T.stat(x))-B(theta))*h(x))
  })
}

####
#b
####

my.a1 <- function(my.phi){
  
  return(my.phi)
  
}

my.b1 <- function(my.v){
  
  ifelse(my.v<0,-log(1 - exp(my.v)),{
    warning("b is only defined for theta<0. Return 0 for other values of theta.");
    0})
  
}

my.c1 <- function(my.y, my.phi){
  
  return(0)
  
}

dgeom.own <- myedf(my.a1, my.b1, my.c1, 1)

theta = -0.8
x = 0:6
comb.table=matrix(0,2,7)
colnames(comb.table)=x
rownames(comb.table)=c("d.exp.disp.fam","dgeom")

comb.table[1,]=dgeom.own(x,theta)
comb.table[2,]=dgeom(x,1-exp(theta))
barplot(comb.table,beside=TRUE,col=c("blue","red"),ylab="probability",xlab="value",legend.text=TRUE,args.legend=list(x="topright"))

n=200
z=rgeom(n,1-exp(theta))
freq.table=table(z)
# freq.table is of class "table"
observed.vals=as.numeric(names(freq.table))

# add all possible values (not just observed values) to the vector
plot.vals=0:max(observed.vals)

comb.table=matrix(0,2,length(plot.vals))
colnames(comb.table)=plot.vals
rownames(comb.table)=c("sample","exp. family")
# compute proportions for the observed values
comb.table[1,observed.vals+1]=freq.table/n
# compute probabilities for all possible values
comb.table[2,]=dgeom.own(plot.vals,theta)

barplot(comb.table,beside=TRUE,col=c("blue","red"),ylab="relative frequency / probability",xlab="value",legend.text=TRUE,args.legend=list(x="topright"))

###
#c
###

c.a <- functon(phi){
  
  return(phi)
  
}

c.b <- function(theta){
  
  ifelse(theta<0,-log((-theta)^3/2),{
    warning("B is only defined for theta<0. Return 0 for other values of theta.");
    0})
}

c.c <- function(y, theta){
  
  return(log(y^2))
  
}

theta = -2

dgeom.own <- myedf(c.a, c.b, c.c, 1)
curve(dgamma.own(x, theta), xlim=c(0,5))
curve(dgamma(x,3,-theta), col = "red", add=TRUE)

n = 200
z = rgamma(n, 3,2)

hist(z,freq=FALSE,ylim=c(0, .8))
curve(dgamma.own(x,theta),col="red",add=TRUE)


# (d)
T.gamma = function(x){
  return(matrix(c(log(x),x),byrow = TRUE,nrow=2))
}
B.gamma = function(theta){
  return(log(gamma(theta[1]+1))-(theta[1]+1)*log(-theta[2]))
}
h.gamma = function(x){
  ifelse(x>=0,1,0)
}

dgamma.own.alt = d.nat.exp.fam(T.gamma,B.gamma,h.gamma)
theta.natural = c(2 , -2)
curve(dgamma.own(x,theta),xlim=c(0,5))
curve(dgamma.own.alt(x,theta.natural),col="red",add=TRUE)


#######
#Task20
#######


sim = read.csv("/Users/thetruetae/Desktop/RWTH/SS21/ADA/RLab/R-Lab6/Sim1.csv", header = TRUE, sep = ",")

lm_sim = lm(sim$y ~sim$x1 + sim$x2, data = sim)

par(mfrow = c(2,2))
plot(lm_sim)
# Residuals vs fitted
# red line : the function or values to be fitted
# x-axis : fitted values y'
# y axis : residuals y - y'

# Noraml Q-Q
# It shows us the normality of distribution
# x axis : quntile
# y axis : the values to be represented
# our result shows it is positive curved -> left skewed(the opposite is assumed to be right skewed)

#Scale-Location
#the sd residuals should spread along the horizontal red line to ensure the homoscedasticity -> good model
# otherwise it is bad model

#Residuals vs leverage
# this table shows how influence of instnaces according to the cook's distance
# we can see that the #30 is the only influential instance because it has the highest cook's distance among all of them

shapiro.test(lm_sim$residuals)

simfit1 = lm(log(sim$y) ~sim$x1 + sim$x2, data = sim)
plot(simfit1)
shapiro.test(simfit1$residuals)
summary(simfit1)

simfit2 = lm(log(sim$y) ~sim$x1, data = sim)
plot(simfit2)
shapiro.test(simfit2$residuals)
summary(simfit2)
