######
#Task1
######

#(a) Let a, b ∈ R3, A, B ∈ R3×3 be vectors and matrices, respectively, with values
# (i) Create vectors a, b and matrices A, B in R, which have the same values as
# a, b, A, B and print them on screen.
a = c(1,2,3)
b = c(3, -5, 0)
A = matrix(c(3, 1.5, 1/8, 
           5, -pi, -6, 
           -1, exp(2.5), 9), nrow = 3, ncol = 3)

B = matrix(c(1, -2, 3, 
             -2, 4, -6, 
             3, -6, 9), nrow = 3, ncol = 3)

#(ii) Calculate A·a,B·b,A2,A·B,a⊺ ·bandprintthemonscreen.
# A %*% a
# B %*% b
# A %*% A
# t(a) %*% b

#(iii) Suppose +' , −' , '·, ': denote the respective component-wise operations to +, −, ·, :. H o w c a n y o u r e a l i z e b ⋄' a a n d B ⋄' A i n R f o r ⋄' ∈ { +' , −' , '· , ': } ?
# B + A, B*A ...

#(iv) Create an additional matrix B′ ∈ R2×2, whose elements are obtained from B by summing up the first two rows and columns.
Bprim = cbind(B[,1] + B[,2], B[,3])
Bprim = rbind(Bprim[1,] + Bprim[2,], Bprim[3,])

#B.prime <- matrix(c(sum(B[1:2,1:2]),sum(B[1:2,3]), sum(B[3,1:2]), B[3,3]), nrow = 2)


#(b) Consider the vector
#x <- c(5, 2, 6, 4, 1, 2, 2, 5, 4, 4, 6, 4, 2, 5, 5, 3, 6, 1, 4, 5)
#of the integer values 1 up to 6. Create an additional vector y applying the following

x = c(5, 2, 6, 4, 1, 2, 2, 5, 4, 4, 6, 4, 2, 5, 5, 3, 6, 1, 4, 5)

brks = c(0, 2, 4, 6)
lbs = c(1,2,3)

cut(x, brks, lbs)

######
#Task2
######
#a)
v1 <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
v2 <- 1:6
v3 <- 5:10
#c) Denote by (a1, . . . , a6)′ the vector representing v2 and by (b1, . . . , b6)′ the vector rep- resenting v3. Then, compute the value of ,6i=1(ai · bi)i on two different ways:
#(i) using a loop and (ii) without using a loop

func1 <- function(a, b) {

  result = 0
  
  for(i in 1:length(a)){
    
    result = result + (a[i]*b[i])^i
    
  }
 
  return(result)
   
}
v4 = v1 * 1
# without loop
#  sum((bu*ral)^(1:6))

# d) Search the first index, where v2 has a larger element than v4. Do this with and without using a loop.
# i) w/ loop

short_len = if(length(v2) > length(v4)) length(v4) else length(v2)

for(i in 1:short_len){
  
  if(v2[i] > v4[i]){
    
    ind = i
    break
    
  }
  
}

# ii) w/o loop
which(v2[1:length(v4)] > v4)[1]

# e) Write a function example.function(vec1,vec2), where vec1 and vec2 are numeric vectors representing a ∈ Rd1 and b ∈ Rd2 , which computes the value of the functio

example.function <- function(vec1, vec2) {
  
  result = 0
  
  if(length(vec1) == length(vec2)){
    
     result = sum((vec1*vec2)^c(1:length(vec1)))
    
  } else {
    
    short_len = if(length(vec1) > length(vec2)) length(vec2) else length(vec1)

    result = which(vec1[1:short_len] > vec2[1:short_len])[1]
    
    if(!result){
      
      result = Inf
      
    }
    
  }
  
  return(result)
  
}

######
#Task3
######

# credits.wsv

data.frame = read.table("credits.wsv", header = TRUE)
# Of which data type is amount?
#> class(data.frame$amount)
#[1] "integer"

#Print on screen the second column/row, 
# data.frame[2,] data.frame[,2]
#every column/row apart from the first eight columns/rows and 
# colnames(data.frame[,-(1:8)])
#the first/last six rows of credits.wsv.

# (iii) Calculate the arithmetic mean and median of the variable amount.

#> summary(data.frame$amount)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#250    1366    2320    3271    3972   18424 

my.sd <- function(data, corrected = TRUE) {
  
  result = 0
  
  if(corrected){
    
    result = sqrt(sum((data - mean(data))^2)/(length(data) - 1))
    
  } else {
    
    result = sqrt(sum((data - mean(data))^2)/(length(data)))
    
  }
  
  return(result)
  
}

######
#Task4
######
# (a) Draw random samples of size n = 10, 50, 100 from a N (μ, σ2) distribution with μ = 5 and σ2 = 4.
n10 = rnorm(10, 5, 2)
n50 = rnorm(50, 5, 2)
n100 = rnorm(100, 5, 2) 

# (b) For each sample size n compute the proportion of values that lie in the intervals
#(i) I1 =[3,7]
#> pnorm(7, mean(n10), sd(n10)) - pnorm(3, mean(n10), sd(n10))
#[1] 0.6773296

#> pnorm(7, mean(n50), sd(n50)) - pnorm(3, mean(n50), sd(n50))
#[1] 0.7356676

# > pnorm(7, mean(n100), sd(n100)) - pnorm(3, mean(n100), sd(n100))
# [1] 0.6605202

#(ii) I2 = [5, 9].
# > pnorm(9, mean(n10), sd(n10)) - pnorm(5, mean(n10), sd(n10))
#[1] 0.4753133

#> pnorm(9, mean(n50), sd(n50)) - pnorm(5, mean(n50), sd(n50))
# [1] 0.4785496

#> pnorm(9, mean(n100), sd(n100)) - pnorm(5, mean(n100), sd(n100))
#[1] 0.4133124

#(c) For each sample size n compute the α-quantiles of the generated values for
#(i) α = 0.3 (ii) α = 0.5

#(iii) α = 0.75.
qnorm(0.3, mean(n10), sd(n10))
#[1] 3.935266
qnorm(0.5, mean(n10), sd(n10))
#[1] 4.995745
qnorm(0.75, mean(n10), sd(n10))
#[1] 6.359746






