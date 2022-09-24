#######
#Task22
#######

# (a) Write two R functions 
# which allow the definition of a probability mass function (pmf) 
# or probability density function (pdf) from the exponential dispersion family 
# and from the k-parametric natural exponential family 
# for a (univariate) random variable Y .

# (i) The arguments of the first function should be the functions a, b, c, 
# and the dispersion parameter φ (see Definition II.2.3 in the lecture). 
# The function should return another function with the arguments y and θ 
# whose return value is the value of the pmf/pdf of Y at y 
# for the natural parameter θ.

my.edf.func = function(my.a, my.b, my.c, my.pi){
  
  return(my.edf.pmf = function(y, v){
    
    return(exp((y*v - my.b(v))/my.a(my.pi) + my.c(y, my.pi)))
    
  })
  
}

# (ii) The arguments of the second function 
# should be the functions T, h and B 
# (see Definition II.2.7 
# with ηj(θ) := θj, j = 1,...,k for θ = (θ1,...,θk) ∈ Θ ⊂ Rk).
# The function should return another function with the arguments y 
# and θ whose return value is the value of the pmf/pdf of Y at y 
# for the natural parameter vector θ.

my.k.param = function(my.T, my.h, my.B){
  
  return(my.k.param.func = function(y, my.v){
    
    return(exp(my.v %*% my.T(y) - my.B(my.v)) * my.h(y))
    
  })
  
}

# (b) A pmf of a random variable with values in N0 is 
# given by a member of the exponential dispersion family with
# a = identity function
# b: v -> -ln(1-exp(v))
# c: . -> 0

my.a.identity = function(a){
  
  return(a)
  
}

my.b.ln = function(v){
  
  return(-log(1 - exp(v)))
  
}

my.c.zero = function(y, pi){
  
  return(0)
  
}

my.edf.1 = my.edf.func(my.a.identity, my.b.ln, my.c.zero, 1)

x = 0:6

dgeom.sample = dgeom(x, 1-exp(-0.8))

comb.table = matrix(0, 2, 7)

colnames(comb.table) = x

rownames(comb.table) = c("my.edf.1.pdf", "dgeom")

comb.table[1,] = my.edf.1(x, -0.8)

comb.table[2,] = dgeom(x, 1-exp(-0.8))

barplot(comb.table, 
        beside = TRUE, 
        col = c("blue", "red"),
        ylab = "probability",
        xlab = "value",
        legend.text = TRUE,
        args.legend = list(x = "topright"))

n = 200
z = rgeom(n, 1-exp(-0.8))
freq.table = table(z)
observed.vals = as.numeric(names(freq.table))
plot.vals = 0:max(observed.vals)

comb.table = matrix(0, 2, length(plot.vals))
colnames(comb.table) = plot.vals
rownames(comb.table) = c("sample", "exp.family")
comb.table[1, observed.vals + 1] = freq.table/n
comb.table[2,] = my.edf.1(plot.vals, -0.8)

barplot(comb.table, 
        beside = TRUE, 
        col = c("blue", "red"),
        ylab = "relative frequency / probability",
        xlab = "value",
        legend.text = TRUE,
        args.legend = list(x = "topright")
        )

#c)
my.b.c = function(v){
  
  return(ifelse(v < 0, -log(((-v)^3)/2), {
    warning("b is only defined for theta<0. Return 0 for other values of theta.");
    0}))
  
}

my.c.c = function(y, my.pi){
  
  return(log(y^2))
  
}

natural.param.c = -2
dispers.param.c = 1

edf.c = my.edf.func(my.a.identity, my.b.c, my.c.c, 1)

curve(edf.c(x, -2), xlim=c(0,5))
curve(dgamma(x,3,2),col="red",add=TRUE)

z = rgamma(200, 3, 2)

hist(z, freq = FALSE, ylim = c(0,.8))
curve(edf.c(x, -2), col = "red", add = TRUE)


#d)

my.T.c = function(y){
  
  return(matrix(c(log(y), y), byrow = TRUE, nrow = 2))
  
}

my.h.c = function(y){
  
  return(ifelse(y>0, 1, {warning("y should be greater than 0");0}))
  
}


my.B.c = function(theta){
  
  return(log(gamma(theta[1] + 1)) - (theta[1] + 1)*log(-theta[2]))
  
}

my.func.d = my.k.param(my.T.c, my.h.c, my.B.c)

theta.d = c(2, -2)

curve(my.func.d(x, theta.d), col = "blue", add = TRUE)

#######
#Task23
#######

# (a) Download the data set Sim1.csv from the RWTHmoodle space and load it 
# as data frame into your workspace.

sim1 = read.csv("Sim1.csv", header = TRUE, sep = ",")

# (b) Fit the linear model E(Y ) = Xβ, 
# where the design matrix X contains an intercept 
# and two additonal columns of the variables x1 and x2 
# from the Sim1.csv data set.

fit.sim = lm(y ~ x1 + x2, data = sim1)

par(mfrow=c(2,2))
plot(fit.sim)
par(mfrow=c(1,1))
plot(cooks.distance(fit.sim))

# (c) As alternative approach, 
# fit a linear model with intercept for the transformed response variable 
# log(y) and the explanatory variables x1 and x2. 
# Check the fit of this linear model.

fit.sim.log = lm(log(y) ~ x1 + x2, data = sim1)

par(mfrow=c(2,2))
plot(fit.sim.log)
par(mfrow=c(1,1))
plot(cooks.distance(fit.sim.log))

fit.sim.log2 = lm(log(y) ~ x1, data = sim1)

par(mfrow=c(2,2))
plot(fit.sim.log2)
par(mfrow=c(1,1))
plot(cooks.distance(fit.sim.log2))

EY = rep(NA, length(sim$y))
EY[x1 == 1] = exp(1.5)
EY[x1 == 0] = exp(1)



