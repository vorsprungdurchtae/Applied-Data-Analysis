
n=10
beta=2
alpha_true=2
y=rgamma(n, beta, alpha_true)

mypdf <- function(y, a, b){
  
  ifelse(y > 0, a^b * y^(b-1) * exp(-a*y)/gamma(b),{
    warning("b is only defined for theta<0. Return 0 for other values of theta.");
    0})
  
}

newton_raphson <- function(alpha, x, beta = 2){
  
  n <- length(x)
  score <- n*beta/alpha - sum(x)
  hessian <- -n*beta /alpha^2
  
  return(score/hessian)
  
}

newton_implement <- function(alpha_0, x, beta, eps = 0.00000001, delta=100){
  
  a_cur = alpha_0
  rep = TRUE
  n_iter = 0
  
  while(rep){
    
    a_next = a_cur - newton_raphson(a_cur, x)
    
    if(abs(a_next - a_cur) < eps*(1 + abs(a_cur))){
      
      rep = FALSE
      conv = TRUE
      
    } 
    if(delta*abs(a_cur) < abs(a_next)){
      
      rep = FALSE
      conv = FALSE
      
    }
    a_cur = a_next
    n_iter = n_iter+1
  }
  
  return(list(convergence = conv, alpha = a_cur, iterations = n_iter))
  
}

print(newton_implement(y,2))

for (n in c(10, 100, 500, 1000)){
  
  alpha = rep(0, 1000)
  conv_ind = rep(FALSE, 1000)
  for(i in 1:1000){
    
    y = rgamma(n, beta, alpha_true)
    result_it_i = newton_implement(y, 2)
    conv_ind[i] = result_it_i$convergence
    alpha[i] = result_it_i$alpha
    
  }
  
  alpha = alpha[conv_ind]
  J = n*beta/alpha_true^2
  cat(mean(alpha),"\t",var(alpha),"\t", J^(-1),"\n") 
  alpha_norm=(alpha-mean(alpha))/sd(alpha) 
  hist(alpha_norm, freq=FALSE)
  curve(dnorm,col="red",add=TRUE) 
  
}